#ifndef UNF_TRIE_CHAR_STREAM_HH
#define UNF_TRIE_CHAR_STREAM_HH

#include <cstring>
#include <vector>
#include <string>
#include "../util.hh"  // XXX:

namespace UNF {
  namespace Trie {
    class CharStream {
    public:
      CharStream(const char* str) : cur_(str) {}
      unsigned char read() { return eos() ? '\0' : *cur_++; }
      unsigned char prev() const { return cur_[-1]; }
      unsigned char peek() const { return *cur_; } 
      const char*   cur() const { return cur_; }
      bool          eos() const { return *cur_ == '\0'; }
      void          setCur(const char* new_cur) { cur_ = new_cur; }

    private:
      const char* cur_;
    };

    class RangeCharStream {
    public:
      RangeCharStream(const char* beg, const char* end) : cur_(beg), end_(end) {}
      unsigned char read() { return eos() ? '\0' : *cur_++; }
      unsigned char prev() const { return cur_[-1]; }
      unsigned char peek() const { return *cur_; } 
      const char*   cur() const { return cur_; }
      const char*   end() const { return end_; }
      bool          eos() const { return cur_ == end_; }

    private:
      const char* cur_;
      const char* end_;
    };

    // TODO: 整理
    class CompoundCharStream {
    public:
      CompoundCharStream(const char* s1, const char* s2) 
	: beg1(s1), beg2(s2), cur1(s1), cur2(s2) {}

      unsigned char read() { return !eos1() ? read1() : read2(); }
      unsigned char peek() const { return !eos1() ? *cur1 : *cur2; }
      unsigned char prev() const { return !eos1() || beg2==cur2 ? cur1[-1] : cur2[-1]; }

      const char* cur() const { return !eos1() ? cur1 : cur2; }
      bool eos() const { return eos1() && eos2(); }
      bool eos1() const { return *cur1=='\0'; }
      bool eos2() const { return *cur2=='\0'; }

      bool range1(const char* p) const {
	return beg1 <= p && p <= cur1;
      }
      
      const char* p1() const { return cur1; }
      const char* beg_2() const { return beg2; }

      unsigned offset() const { return cur1-beg1 + cur2-beg2; }
      void setCur(const char* p) { 
	if(beg1 <= p && p <= cur1) {
	  cur1=p;
	  cur2=beg2;
	} else {
	  cur2=p;
	}
      }

      unsigned over() const { return cur2-beg2; }
      
    private:
      unsigned char read1() { return eos1() ? '\0' : *cur1++; }
      unsigned char read2() { return eos2() ? '\0' : *cur2++; }

    private:
      const char* beg1;
      const char* beg2;
      const char* cur1;
      const char* cur2;
    };

    // TODO: 整理
    class CharStreamForComposition : public CompoundCharStream {
    public:
      CharStreamForComposition (const char* s1, const char* s2, 
				const std::vector<unsigned char>& canonical_classes, 
				std::string& buf)
	: CompoundCharStream(s1, s2), classes(canonical_classes), skipped(buf) {
	skipped.clear();
      }
      
      void append(std::string& s, const char* beg, const char* end) const {
	if(range1(beg) && !range1(end)) {
	  s.append(beg, p1());
	  s.append(beg_2(),end);
	} else {
	  s.append(beg,end);
	}
      }
      
      unsigned char get_prev_canonical_class() const { 
	return offset()-1 < classes.size() ? classes[offset()-1] : 0;
      }

      unsigned char get_canonical_class() const { 
	return offset() < classes.size() ? classes[offset()] : 0;
      }
      
      bool next_combining_char(unsigned char prev_class, const char* ppp) {
	while(Util::is_utf8_char_start_byte(peek()) == false)
	  read();
	
	unsigned char mid_class = get_prev_canonical_class();
	unsigned char cur_class = get_canonical_class();

	if(prev_class < cur_class && mid_class < cur_class) {
	  skipped.append(ppp, cur());
	  return true;
	} else {
	  if(cur_class != 0) {
	    read();
	    return next_combining_char(prev_class,ppp);
	  }
	  return false;
	}
      
      }
      
      const std::vector<unsigned char>& classes;
      std::string& skipped;
    };
  }
}

#endif
