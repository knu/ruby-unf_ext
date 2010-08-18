#ifndef UNF_TRIE_SEARCHER_HH
#define UNF_TRIE_SEARCHER_HH

#include "char_stream.hh"
#include "node.hh"
#include "../util.hh"

namespace UNF {
  namespace Trie {
    class Searcher {
    public:
      Searcher(const Node* nodes, const char* value=NULL)
	: nodes(nodes), value(value) {}

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

    protected:
      const Node* nodes;
      const char* value;
    }; 
    
    class CanonicalCombiningClass : private Searcher {
    public:
      CanonicalCombiningClass(const unsigned* node_uints)
	: Searcher(Node::from_uint_array(node_uints)) {}
      
      int get_class(const char* str) const { return find_value(str,0); }

      void sort(char* str, std::vector<unsigned char>& classes) const {
	CharStream in(str);
	unsigned sort_beg=0;
	unsigned sort_end=0;
	unsigned unicode_char_count=0;

      loop_head:
	unsigned beg = in.cur()-str;
	
	for(unsigned node_index=0;;){
	  node_index = nodes[node_index].jump(in.read());
	  
	  if(nodes[node_index].check_char()==in.prev()) {
	    unsigned terminal_index = nodes[node_index].jump('\0');
	    if(nodes[terminal_index].check_char()=='\0') {
	      if((unicode_char_count++)==0)
		sort_beg = beg;
	      sort_end = in.cur()-str;
	      
	      unsigned char klass = nodes[terminal_index].value();
	      for(unsigned i=beg; i < sort_end; i++) 
		classes[i] = klass;
 	      break;
	    }
	  } else {
	    if(unicode_char_count > 1)
	      bubble_sort(str, classes, sort_beg, sort_end);
	    unicode_char_count = 0;
	    break;
	  }
	} 
	Util::eat_until_utf8_char_start_point(in);

	if(in.eos()==false)
	  goto loop_head;

	if(unicode_char_count > 1)
	  bubble_sort(str, classes, sort_beg, sort_end);
      }      

    private:
      void bubble_sort(char* str, std::vector<unsigned char>& canonical_classes, unsigned beg, unsigned end) const {
	for(unsigned limit=beg, next=end; limit != next;) {
	  limit = next;
	  for(unsigned i=beg+1; i < limit; i++)
	    if(canonical_classes[i-1] > canonical_classes[i]) {
	      std::swap(canonical_classes[i-1], canonical_classes[i]);
	      std::swap(str[i-1], str[i]);
	      next = i;
	    }
	}
      }
    };

    class NormalizationForm : private Searcher {
    public:
      NormalizationForm(const unsigned* node_uints, const char* value=NULL)
	: Searcher(Node::from_uint_array(node_uints), value) {} 

      // TODO: comment
      int quick_check(const char* key) const { return find_value(key,-1); }

      void decompose(RangeCharStream in, std::string& buffer) const {
      loop_head:
	const char* beg = in.cur();

	for(unsigned node_index=0;;) {
	  node_index = nodes[node_index].jump(in.read());
	  if(nodes[node_index].check_char()==in.prev()) {
	    unsigned terminal_index = nodes[node_index].jump('\0');
	    if(nodes[terminal_index].check_char()=='\0') {
	      buffer.append(value+nodes[terminal_index].value());
	      beg = in.cur();
	      break;
	    }
	  } else {
	    Util::eat_until_utf8_char_start_point(in);
	    buffer.append(beg, in.cur());
	    break;
	  }
	}  

	if(in.eos()==false)
	  goto loop_head;
      }

      void compose(CharStreamForComposition& in, std::string& buf) const {
	in.skipped.clear();

	const char* const beg = in.cur();
	const char* current_char_head = in.cur();
	const char* last_matched_tail = NULL;
	
	unsigned node_index = 0;
	unsigned last_skip_tail_offset = 0;
	unsigned last_matched_index = 0;
	
	unsigned retry_root_node = 0;
	unsigned char retry_root_class = 0xFF;

	for(bool first=true;;) {
	  if(Util::is_utf8_char_start_byte(in.peek())) {
	    if(node_index != 0)
	      first=false;
	    current_char_head = in.cur();
	    
	    retry_root_node = node_index;
	    if(retry_root_class==0 && in.get_canonical_class()==0) 
	      retry_root_class = 0xFF;
	    else
	      retry_root_class = in.get_canonical_class();
	  }

	retry:
	  unsigned next_index = nodes[node_index].jump(in.read());
	  if(nodes[next_index].check_char()==in.prev()) {
	    // succeeded
	    node_index = next_index;
	    unsigned terminal_index = nodes[node_index].jump('\0');
	    if(nodes[terminal_index].check_char()=='\0') {
	      last_matched_index  = terminal_index;
	      last_skip_tail_offset = in.skipped.size();
	      last_matched_tail = in.cur();
	      if(in.eos())
		break;
	    }
	  } else if (first==true) {
	    // no retry if current point is a part of first starter
	    break;
	  } else if (in.next_combining_char(retry_root_class, current_char_head)==true) { 
	    // back previous code-point and retry
	    node_index = retry_root_node;
	    current_char_head = in.cur();
	    goto retry;
	  } else {
	    break;
	  }
	}  
	
	if(last_matched_index != 0) {
	  // append composed unicode-character and skipped combining-characters
	  buf.append(value+nodes[last_matched_index].value());
	  buf.append(in.skipped.data(), in.skipped.data()+last_skip_tail_offset);
	  in.setCur(last_matched_tail);
	} else {
	  // append one unicode-character
	  in.setCur(Util::nearest_utf8_char_start_point(beg+1));
	  in.append(buf, beg, in.cur()); // XXX: name
	}
      }
    };
  }
}

#endif
