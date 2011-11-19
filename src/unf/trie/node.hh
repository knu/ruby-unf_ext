#ifndef UNF_TRIE_NODE_HH
#define UNF_TRIE_NODE_HH

namespace UNF {
  namespace Trie {
    class Node {
    public:
      Node() : data(0xFFFFFFFF) {}
      
      void set_base_index(unsigned base_index) { data = (data&0xFF000000)+(base_index&0x00FFFFFF); }
      void set_value(unsigned value) { set_base_index(value); }
      void set_check_char(unsigned char ch) { data = (ch << 24) + base(); }

      bool is_unused() const { return data==0xFFFFFFFF; }

      unsigned jump(unsigned char ch) const { return base() + ch; }
      unsigned value() const { return base(); }
      unsigned check_char() const { return data>>24; }
      unsigned to_uint() const { return data; }

      static const Node* from_uint_array(const unsigned* node_uints)
      { return reinterpret_cast<const Node*>(node_uints); }

    private:
      unsigned base() const { return data & 0xFFFFFF; }

    private:
      // TODO: bit-fieldを使ってみる
      unsigned data;
    };
  }
}

#endif
