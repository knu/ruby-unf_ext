#ifndef UNF_TRIE_NODE_ALLOCATOR_HH
#define UNF_TRIE_NODE_ALLOCATOR_HH

#include <vector>

namespace UNF {
  namespace Trie {
    class NodeAllocator {
      typedef unsigned NodeIndex;
      typedef std::vector<unsigned char> UCharList;
      typedef std::vector<bool> BitSet;

    private:
      struct ForwardLink {
	ForwardLink(NodeIndex i) : idx(i), next(NULL) {}
    
	NodeIndex append(NodeIndex beg_idx, unsigned num) {
	  ForwardLink* cur=this;
	  while(cur->next) cur=cur->next;
      
	  for(NodeIndex i=beg_idx; i < beg_idx+num; i++)
	    cur = cur->next = new ForwardLink(i);
	  return beg_idx+num-1;
	}
    
	void remove_next() {
	  ForwardLink* tmp=next; next=next->next; delete tmp;
	}
    
	void delete_all_tail() {
	  for(ForwardLink* tmp=next; next; tmp=next) {
	    next=next->next;
	    delete tmp;
	  }
	}
    
	NodeIndex    idx;  
	ForwardLink* next; 
      };

    public:
      static const unsigned PER_LINK_SIZE=0x10000;
  
    public:
      NodeAllocator(unsigned node_size) : root(0), used_base(node_size) {
	last_idx = root.append(1,PER_LINK_SIZE);
      }
      ~NodeAllocator() {
	root.delete_all_tail();
      }
  
      int allocate(unsigned char code) {
	static UCharList codes(1);  // XXX: thread unsafe
	codes[0] = code;
	return allocate(codes);
      }
  
      int allocate(const UCharList& codes, ForwardLink* prev=NULL) {
	if(!prev) prev=&root;
	if(!prev->next)
	  last_idx = prev->append(last_idx+1, PER_LINK_SIZE); 

	ForwardLink *cur = prev->next;
	unsigned char min_cd = codes.front();
    
	for(; cur->idx <= min_cd; prev=cur, cur=cur->next);
	for(; cur->next; prev=cur,cur=cur->next) {
	  NodeIndex x = cur->idx - min_cd;
	  if(used_base[x]==false && can_allocate(cur, codes, x)) {
	    used_base[x] = true;
	    alloc(prev,codes,x);
	    return x;
	  }
	}
	return allocate(codes, cur);
      }
  
    private:
      bool can_allocate(ForwardLink* cur, const UCharList& codes, NodeIndex x) const {
	cur=cur->next;
	for(unsigned i=1;i < codes.size(); i++) {
	  for(; cur && cur->idx < x+codes[i]; cur=cur->next);
	  if(!cur || cur->idx > x+codes[i])
	    return false;
	}
	return true;
      }
  
      void alloc(ForwardLink* prev, const UCharList& codes, NodeIndex x) {
	prev->remove_next();
	for(unsigned i=1; i < codes.size(); i++) {
	  for(; prev->next->idx < x+codes[i]; prev=prev->next);
	  prev->remove_next();
	}    
      }
  
    private:
      ForwardLink root;
      NodeIndex   last_idx;
      BitSet      used_base;
    };
  }
}

#endif
