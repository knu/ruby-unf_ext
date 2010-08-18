#ifndef UNF_TRIE_SEARCHER_HH
#define UNF_TRIE_SEARCHER_HH

#include "char_stream.hh"
#include "node.hh"
#include "../util.hh"
#include <fstream>

namespace UNF {
  namespace Trie {
    // NFの方が適切?
    class Searcher {
    public:
      Searcher(const unsigned* nodes, const char* value=NULL)
	: nodes(reinterpret_cast<const Node*>(nodes)), value(value) {
      }

      // check whether the byte-sequence(code-point) can occur in this Normalization Form
      // return: 
      //   -1=yes, 0=no, 1=maybe # http://www.unicode.org/reports/tr44/#Decompositions_and_Normalization
      int quick_check(const char* key) const {
	return find_value(key,-1);
      }

      int get_canonical_class(const char* key) const {
	return find_value(key,0);
      }
      
      int find_value(const char* key, int default_value) const {
	unsigned node_index=0;
	for(CharStream in(key);; in.read()) {
	  node_index = nodes[node_index].jump(in.peek());
	  if(nodes[node_index].check_char()==in.peek()) {
	    unsigned terminal_index = nodes[node_index].jump('\0');
	    if(nodes[terminal_index].check_char()=='\0')
	      return static_cast<int>(nodes[terminal_index].value());
	  } else
	    return default_value;
	}
      }

      void decompose_one(RangeCharStream in, std::string& buffer) const {
      loop_head:
	const char* beg = in.cur();
	unsigned node_index=0;

	for(;;) {
	  unsigned terminal_index = nodes[node_index].jump('\0');
	  if(nodes[terminal_index].check_char()=='\0') {
	    buffer.append(value+nodes[terminal_index].value());
	    beg = in.cur();
	    break;
	  }
	  
	  unsigned next_index = nodes[node_index].jump(in.read());
	  if(nodes[next_index].check_char()==in.prev())
	    node_index = next_index;
	  else {
	    while(is_utf8_char_start_byte(in.peek())==false)
	      in.read();
	    break;
	  }
	}  
	buffer.append(beg, in.cur());

	if(in.eos()==false)
	  goto loop_head;
      }

      void classify2(const char* key, std::vector<unsigned char>& classes) const {
	CharStream in(key);
	unsigned starter=0;
	unsigned ender=0;
	unsigned char_count=0;

      loop_head:
	unsigned beg = in.cur()-key;
	unsigned node_index=0;
	for(;;){
	  unsigned next_index = nodes[node_index].jump(in.read());
	  
	  if(nodes[next_index].check_char()==in.prev()) {
	    node_index = next_index;

	    unsigned terminal_index = nodes[node_index].jump('\0');
	    if(nodes[terminal_index].check_char()=='\0') {
	      if(char_count==0)
		starter = beg;
	      char_count++;
	      unsigned char klass = nodes[terminal_index].value();
	      ender = in.cur()-key;
	      for(unsigned i=beg; i < ender; i++) 
		classes[i] = klass;
 	      break;
	    }
	    if(in.prev()=='\0') return;
	  } else {
	    if(char_count > 1)
	      bubble_sort(classes, (char*)key, starter, ender);
	    char_count = 0;
	    break;
	  }
	} 

	for(; is_utf8_char_start_byte(in.peek())==false; in.read());

	if(in.peek() != '\0')
	  goto loop_head;

	if(char_count > 1)
	  bubble_sort(classes, (char*)key, starter, ender);
      }

      static void bubble_sort(std::vector<unsigned char>& classes, char* str, unsigned beg, unsigned end) {
	for(unsigned limit=beg, next=end; limit != next;) {
	  limit = next;
	  for(unsigned i=beg+1; i < limit; i++)
	    if(classes[i-1] > classes[i]) {
	      std::swap(classes[i-1], classes[i]);
	      std::swap(str[i-1],     str[i]);
	      next = i;
	    }
	}
      }

      template<class BacktrackableCharStream>
      void longest_common_prefixA_one(BacktrackableCharStream& in, std::string& buf) const {
	const char* beg = in.cur();

	unsigned rest = 0;
	in.skipped.clear();

	unsigned last_matched_index=0;
	const char* tail=NULL;
	unsigned node_index=0;
	unsigned prev_node = 0;
	unsigned char prev_class = 0xFF;
	
	const char* ppp=in.cur();

	for(bool first=true;;) {
	  unsigned terminal_index = nodes[node_index].jump('\0');
	  if(nodes[terminal_index].check_char()=='\0') {
	    last_matched_index  = terminal_index;
	    rest = in.skipped.size();
	    tail = in.cur();
	    if(in.peek()=='\0')
	      break;
	  }

	  if(is_utf8_char_start_byte(in.peek())==true) {
	    if(node_index != 0)
	      first=false;
	    
	    ppp = in.cur();
	    prev_node = node_index;
	    if(prev_class==0 && in.get_class()==0) 
	      prev_class = 0xFF;
	    else
	      prev_class = in.get_class();
	  }

	retry:
	  unsigned next_index = nodes[node_index].jump(in.read());
	  if(nodes[next_index].check_char()==in.prev()) {
	    node_index = next_index;
	  } else if (first==true) {
	    break;
	  } else if (in.next(prev_class, ppp)==true) { // XXX: name
	    node_index = prev_node;
	    ppp = in.cur();
	    goto retry;
	  } else {
	    break;
	  }
	}  	
	if(last_matched_index != 0) {
	  buf.append(value+nodes[last_matched_index].value());
	  buf.append(in.skipped.data(), in.skipped.data()+rest);
	  in.setCur(tail);
	} else {
	  in.setCur(beg+1);
	  while(is_utf8_char_start_byte(in.peek())==false)
	    in.read();
	  //buf.append(beg, in.getCur());
	  in.append(buf, beg, in.cur());
	}
      }
      
      static bool is_utf8_char_start_byte(char byte) {
	if(!(byte&0x80))
	  return true; // ascii
	else if (byte&0x40)
	  return true; // start of a UTF-8 character byte sequence
	return false;
      }

    private:
      const Node* nodes;
      const char* value;
    };
  }
}
#endif
