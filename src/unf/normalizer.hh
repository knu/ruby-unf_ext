#ifndef UNF_NORMALIZER_HH
#define UNF_NORMALIZER_HH

#include <vector>
#include <string>
#include <algorithm>
#include <cstring>
#include "trie/searcher.hh"
#include "trie/char_stream.hh"
#include "table.hh"
#include "util.hh"

namespace UNF {
  class Normalizer {
  public:
    Normalizer()
      : nf_d(TABLE::CANONICAL_DECOM_NODES, TABLE::VALUE),
	nf_kd(TABLE::COMPATIBILITY_DECOM_NODES, TABLE::VALUE),
	nf_c(TABLE::CANONICAL_COM_NODES, TABLE::VALUE),
	ccc(TABLE::CANONICAL_CLASS_NODES),
	nf_qc_c(TABLE::NFC_ILLEGAL_NODES),
	nf_qc_kc(TABLE::NFKC_ILLEGAL_NODES)
    {}

    const char* nfd(const char* src) { return decompose(src, nf_d); }
    const char* nfkd(const char* src) { return decompose(src, nf_kd); }
    const char* nfc(const char* src) { return compose(src, nf_qc_c, nf_d); }
    const char* nfkc(const char* src) { return compose(src, nf_qc_kc, nf_kd); }

  private:
    const char* decompose(const char* src, const Trie::Searcher& nf) {
      const char* beg = next_invalid_char(src, nf, true);
      if(*beg=='\0')
	return src;
      
      buffer.assign(src, beg);
      do {
	const char* end = next_starter(beg);
	decompose_one(beg, end, nf, buffer);
	beg = next_invalid_char(end, nf, true);
	buffer.append(end, beg);
      } while(*beg!='\0');
      
      return buffer.c_str();      
    }

    void decompose_one(const char* beg, const char* end, const Trie::Searcher& nf, std::string& buf) {
      unsigned last = buf.size();
      nf.decompose_one(Trie::RangeCharStream(beg,end), buf);
      canonical_combining_class_ordering(buf.data()+last, buf.data()+buf.size());
    }

    const char* compose(const char* src, const Trie::Searcher& nf, const Trie::Searcher& nf_decomp) {
      const char* beg = next_invalid_char(src, nf);
      if(*beg=='\0')
	return src;
      
      buffer.assign(src, beg);
      while(*beg!='\0') {
	const char* end = next_starter(beg);
	buffer2.clear();
	decompose_one(beg, end, nf_decomp, buffer2);
	end = compose_one(buffer2.c_str(), end, buffer);
	beg = next_invalid_char(end, nf);
	buffer.append(end, beg);
      }

      return buffer.c_str();      
    }

    const char* compose_one(const char* starter, const char* rest_starter, std::string& buf) {
      Trie::CharStreamForComposition in(starter, rest_starter, classes, buffer3);
      while(in.eos1()==false) // XXX: name, in.eos1()
	nf_c.longest_common_prefixA_one(in, buf);
      return rest_starter + in.over();
    }

    void canonical_combining_class_ordering(const char* beg, const char* end) {
      classes.assign(end-beg+1, 0);    // +1 is for sentinel value
      ccc.classify2(beg, classes);
    }

    const char* next_invalid_char(const char* src, const Trie::Searcher& nf, bool decomp_phase=false) {
      int last_canonical_class = 0;
      const char* cur = Util::nearest_utf8_char_start_point(src);
      const char* starter = cur;
      
      for(; *cur != '\0'; cur = Util::nearest_utf8_char_start_point(cur+1)) {
	int canonical_class = ccc.get_canonical_class(cur);
	if(last_canonical_class > canonical_class && canonical_class != 0)
	  return starter;

	int ret = nf.quick_check(cur);
	if(ret!=-1) {
	  if(decomp_phase || ret==0)
	    if(canonical_class==0)
	      starter = cur;
	  return starter;
	}

	if(canonical_class==0)
	  starter=cur;

	last_canonical_class = canonical_class;
      }
      return cur;
    }

    const char* next_starter(const char* src) const {
      const char* cur = Util::nearest_utf8_char_start_point(src+1);
      while(ccc.get_canonical_class(cur)!=0)
	cur = Util::nearest_utf8_char_start_point(cur+1);
      return cur;
    }

  private:
    const Trie::Searcher nf_d;
    const Trie::Searcher nf_kd;
    const Trie::Searcher nf_c;
    const Trie::Searcher ccc;
    const Trie::Searcher nf_qc_c;
    const Trie::Searcher nf_qc_kc;
    
    std::string buffer;
    std::string buffer2;
    std::string buffer3;
    std::vector<unsigned char> classes;
  };
}

#endif
