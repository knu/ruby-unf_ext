#ifndef UNF_TRIE_NODE2_HH
#define UNF_TRIE_NODE2_HH

#include "char_stream.hh"

namespace UNF {
  namespace Trie {
    class Node2 {
    public:
      unsigned jump(unsigned char ch) const { return base() + ch; }
      unsigned value() const { return base(); }
      unsigned check_char() const { return high & 0xFF; }
      unsigned type() const { return (low >> 29) & 3; }
      bool is_terminal() const { return (low >> 31) != 0; }
      unsigned sibling_total() const {
        switch(type()) {
        case 0:
          return high>>24;
        case 1:
          return high>>16;
        default:
          return high>>8;
        }
      }

      unsigned inc_id(unsigned id) const {
        return id + (is_terminal() ? 1 : 0) + sibling_total();
      }

      template<class T>
      bool check_encoded_children(T& in) const {
        switch(type()) {
        case 0:
          //          std::cout << "@ " << (int)in.peek() << std::endl;
          // std::cout << "# " << (int)enc_chck(0) << ", " << (int)enc_chck(1) << std::endl;
          return ((enc_chck(0)==0 || (enc_chck(0) == in.read() && !in.eos())) &&
                  (enc_chck(1)==0 || (enc_chck(1) == in.read() && !in.eos())));
        case 1:
          return enc_chck(0)==0 || (enc_chck(0) == in.read() && !in.eos());
        default:
          return true;
        }
      }

      unsigned char enc_chck(unsigned index) const {
        return (high >> (8*index+8)) & 0xFF;
      }

      static const Node2* from_uint_array(const unsigned* node_uints)
      { return reinterpret_cast<const Node2*>(node_uints); }

      bool operator==(const Node2& x) const {
        return high == x.high && low == x.low;
      }
      
    private:
      unsigned base() const { return low & 0x1FFFFFFF; }

    private:
      unsigned high;
      unsigned low;
    };
  }
}

#endif
