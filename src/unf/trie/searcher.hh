#ifndef UNF_TRIE_SEARCHER_HH
#define UNF_TRIE_SEARCHER_HH

#include "char_stream.hh"
#include "node.hh"
#include "node2.hh"
#include "builder.hh"
#include "../util.hh"

namespace UNF {
  namespace Trie {
    class Searcher {
    public:
      Searcher(const Node* nodes, const Node2* nodes2, const unsigned root, const unsigned* vals, const char* strs, const char* value=NULL)
	: nodes(nodes), nodes2(nodes2), root(root), vals(vals), strs(strs), value(value) {}

      unsigned find_value(const char* key, int default_value) const {
        Node2 node = nodes2[root];
        int id = node.inc_id(-1);

	for(CharStream in(key);; in.read(), id=node.inc_id(id)) {
          if(node.is_terminal())                     return vals[id];
          if(in.eos())                               return default_value;
          if(node.check_encoded_children(in)==false) return default_value;
            
	  node = nodes2[node.jump(in.peek())];
	  if(node.check_char() != in.peek())
            return default_value;
	}
      } 

    protected:
      const Node* nodes;
      const Node2* nodes2;
      const unsigned root;
      const unsigned* vals;
      const char* strs;
      const char* value;
    }; 
    
    class CanonicalCombiningClass : private Searcher {
    public:
      CanonicalCombiningClass(const unsigned* node_uints, const unsigned* nodes, unsigned root, const unsigned* vals, const char* strs)
	: Searcher(Node::from_uint_array(node_uints), Node2::from_uint_array(nodes), root, vals, strs) {}
      
      unsigned get_class(const char* str) const { return find_value(str,0); }

      void sort(char* str, std::vector<unsigned char>& classes) const {
	CharStream in(str);
	unsigned sort_beg=0;
	unsigned sort_end=0;
	unsigned unicode_char_count=0;

      loop_head:
	unsigned beg = in.cur()-str;
	Node2 node = nodes2[root];
        int id = node.inc_id(-1);
	for(;; id=node.inc_id(id)) {
          if(node.is_terminal()) {
            if((unicode_char_count++)==0)
		sort_beg = beg;
	      sort_end = in.cur()-str;
	      
	      unsigned char klass = vals[id];
	      for(unsigned i=beg; i < sort_end; i++) 
		classes[i] = klass;
 	      break;
          }

          if(in.eos() || node.check_encoded_children(in)==false)
            goto failed;
          
          node = nodes2[node.jump(in.read())];
          if(node.check_char() != in.prev())
            goto failed;
          continue;
        failed:
          if(unicode_char_count > 1)
            bubble_sort(str, classes, sort_beg, sort_end);
          unicode_char_count = 0;
          break;
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
      NormalizationForm(const unsigned* node_uints, const unsigned* nodes, unsigned root, const unsigned* vals,  const char* strs, const char* value=NULL)
	: Searcher(Node::from_uint_array(node_uints), Node2::from_uint_array(nodes), root, vals, strs, value) {} 

      // TODO: set的な検索は専用メソッドを設ける
      bool quick_check(const char* key) const { return find_value(key,0xFFFFFFFF)==0xFFFFFFFF; }

      void decompose(RangeCharStream in, std::string& buffer) const {
      loop_head:
	const char* beg = in.cur();

        Node2 node = nodes2[root];
        int id = node.inc_id(-1);
	for(;; id=node.inc_id(id)) {
          if(node.is_terminal()) {
            word_append2(buffer, strs, vals[id]);
            beg = in.cur();
            break;
          }

          if(in.eos() || node.check_encoded_children(in)==false) 
            goto failed;
          
	  node = nodes2[node.jump(in.read())];
	  if(node.check_char() == in.prev())
            continue;
        failed:
          Util::eat_until_utf8_char_start_point(in);
          buffer.append(beg, in.cur());
          break;
	}  

	if(in.eos()==false)
	  goto loop_head;
      }

      void compose(CharStreamForComposition& in, std::string& buf) const {
	in.init_skipinfo();

	const char* const beg = in.cur();
	const char* current_char_head = in.cur();
        unsigned composed_char_info = 0;
	
	unsigned node_index = 0;
	unsigned retry_root_node = 0;
	unsigned char retry_root_class = 0;

	for(bool first=true;;) {
	  if(Util::is_utf8_char_start_byte(in.peek())) {
	    if(node_index != 0)
	      first=false;
	    current_char_head = in.cur();

	    retry_root_node = node_index;
	    retry_root_class = in.get_canonical_class();
	  }

	retry:
	  unsigned next_index = nodes[node_index].jump(in.read());
	  if(nodes[next_index].check_char()==in.prev()) {
	    // succeeded
	    node_index = next_index;
	    unsigned terminal_index = nodes[node_index].jump('\0');
	    if(nodes[terminal_index].check_char()=='\0') {
	      composed_char_info = nodes[terminal_index].value();
              
	      in.mark_as_last_valid_point();
	      if(in.eos() || retry_root_class > in.get_canonical_class())
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
	
	if(composed_char_info != 0) {
	  // append composed unicode-character and skipped combining-characters
          word_append(buf, value, composed_char_info);
	  in.append_skipped_chars_to_str(buf);
	  in.reset_at_marked_point();
	} else {
	  // append one unicode-character
	  in.setCur(Util::nearest_utf8_char_start_point(beg+1));
	  in.append_read_char_to_str(buf, beg);
	}
      }

    private:
      static void word_append(std::string& buffer, const char* base, unsigned info) {
        unsigned offset = info & MappingKey::OFFSET_BITMASK;
        unsigned length = info >> MappingKey::OFFSET_BITLEN;
        buffer.append(base+offset, base+offset+length);
      }

      static void word_append2(std::string& buffer, const char* base, unsigned info) {
        unsigned offset = info & 0xFFFFFF;
        unsigned length = info >> 24;
        buffer.append(base+offset, base+offset+length);
      }
    };
  }
}

#endif
