#ifndef UNF_TRIE_BUILDER_HH
#define UNF_TRIE_BUILDER_HH

#include "node_allocator.hh"
#include "char_stream.hh"
#include "node.hh"
#include <fstream>
#include <cstring>
#include <string>
#include <iomanip>

namespace UNF {
  namespace Trie {
    /**
     * Trie key class
     */
    struct Key {
      Key(const std::string& key) : buf(key), cs(buf.c_str()) {}
      
      bool operator<(const Key& k) const { return strcmp(cs.cur(), k.cs.cur()) < 0; }
      unsigned char read() { return cs.read(); }     
      unsigned char prev() const { return cs.prev(); }
      unsigned char peek() const { return cs.peek(); } 
      void reset() { cs.setCur(buf.c_str()); }
      virtual void set_node_value(Node& node) = 0;

      std::string buf;
      CharStream cs;
    };
    
    struct AttrKey : public Key {
      AttrKey(const std::string& key, unsigned attr) : Key(key), attr(attr) {}
      virtual void set_node_value(Node& node) { node.set_value(attr); }

      unsigned attr;
    };
    typedef std::vector<AttrKey> AttrKeyList;
    
    struct MappingKey : public Key {
      static const unsigned OFFSET_BITLEN = 18;
      static const unsigned OFFSET_BITMASK = 0x3FFFF;

      MappingKey(const std::string& key, const std::string& value, std::string& buffer) 
	: Key(key) {
        value_info = (value.size()<<OFFSET_BITLEN) | buffer.size();
	buffer += value;
      }
      MappingKey(const std::string& key, const std::string& value, unsigned value_info) 
	: Key(key) {
        this->value_info = (value.size()<<OFFSET_BITLEN) | (value_info&OFFSET_BITMASK);
      }
      
      virtual void set_node_value(Node& node) { node.set_value(value_info); }

      unsigned value_info;
    };
    typedef std::vector<MappingKey> MappingKeyList;
    
    /**
     * Builder class
     */
    template<class KeyList>
    class Builder {
    public:
      Builder(KeyList& keys) 
	: keys(keys), node_size(count_node(keys)*1.5), alloc(node_size) {
	nodes = new Node[node_size];
      }
      ~Builder() {
	delete [] nodes;
      }
      
      Builder& build() {
	build_impl(0, keys.size(), 0);
	return *this;
      }
      
      void output_nodes_cpp_array(std::ofstream& out, const char* prefix) {
	if(node_size > 0xFF)
	  while(nodes[node_size-0xFF].is_unused())
	    node_size--;
	
	out << "const unsigned " << prefix << "_NODES[]={" << std::endl;
	for(unsigned i=0; i < node_size; i++) {
	  out << "0x" << std::hex << std::setw(8) << std::setfill('0') << std::right << std::hex << nodes[i].to_uint();
	  if(i+1 < node_size) out << ',';
	  if((i+1)%10==0)     out << std::endl;
	}
	out << "};" << std::endl << std::endl;
      }
      
    private:
      void build_impl(std::size_t beg, std::size_t end, int root_node) {
	if(end-beg == 1) {
	  if(nodes[root_node].check_char()!='\0') {
	    for(; keys[beg].peek()!='\0'; keys[beg].read())
	      root_node = set_node(root_node, alloc.allocate(keys[beg].peek()), keys[beg].peek());
	    root_node = set_node(root_node, alloc.allocate('\0'), '\0');
	  }
	  keys[beg].set_node_value(nodes[root_node]);
	  return;
	}
	
	std::vector<unsigned char> children;
	std::vector<std::size_t>   ranges;
	do {
	  children.push_back(keys[beg].peek());
	  ranges.push_back(beg);
	  beg = end_of_same_node(keys, beg, end);
	} while (beg != end);
	ranges.push_back(end);

	int base_node = alloc.allocate(children);
	for(std::size_t i=0; i < children.size(); i++)
	  build_impl(ranges[i], ranges[i+1], set_node(root_node, base_node, children[i]));
      }

      int set_node(int node, int base_node, unsigned char child) {
	int next   = base_node + child;
	nodes[node].set_base_index(base_node);
	nodes[next].set_check_char(child);
	return next;
      }

      unsigned end_of_same_node(KeyList& keys, std::size_t beg, std::size_t end) {
	unsigned char ch = keys[beg].read();
	std::size_t cur  = beg+1;
	for(; cur < end && ch == keys[cur].peek(); cur++)
	  keys[cur].read();
	return cur;
      }

      unsigned count_node(KeyList& keys) {
	unsigned count = count_node_impl(keys,0,keys.size());
	for(std::size_t i = 0; i < keys.size(); i++)
	  keys[i].reset();
	return count;
      }

      unsigned count_node_impl(KeyList& keys, std::size_t beg, std::size_t end) {
	if(end-beg == 1) {
	  unsigned count=1;
	  while(keys[beg].read()!='\0')
	    count++;
	  return count;
	}
	
	std::vector<std::size_t>   ranges;
	do {
	  ranges.push_back(beg);
	  beg = end_of_same_node(keys, beg, end);
	} while (beg != end);
	ranges.push_back(end);

	unsigned count=ranges.size()-1;
	for(std::size_t i=0; i < ranges.size()-1; i++)
	  count += count_node_impl(keys, ranges[i], ranges[i+1]);
	return count;
      }

    private:
      KeyList& keys;
      unsigned node_size;
      Node* nodes;
      NodeAllocator alloc;
    };
  }
}

#endif
