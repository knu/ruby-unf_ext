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
	nf_c_qc(TABLE::NFC_ILLEGAL_NODES),
	nf_kc_qc(TABLE::NFKC_ILLEGAL_NODES),
	ccc(TABLE::CANONICAL_CLASS_NODES)
    {}

    const char* nfd(const char* src) { return decompose(src, nf_d); }
    const char* nfkd(const char* src) { return decompose(src, nf_kd); }
    const char* nfc(const char* src) { return compose(src, nf_c_qc, nf_d); }
    const char* nfkc(const char* src) { return compose(src, nf_kc_qc, nf_kd); }

  private:
    const char* decompose(const char* src, const Trie::NormalizationForm& nf) {
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

    void decompose_one(const char* beg, const char* end, const Trie::NormalizationForm& nf, std::string& buf) {
      unsigned last = buf.size();
      nf.decompose(Trie::RangeCharStream(beg,end), buf);
      char* bufbeg = const_cast<char*>(buf.data());
      canonical_combining_class_ordering(bufbeg+last, bufbeg+buf.size());
    }

    const char* compose(const char* src, const Trie::NormalizationForm& nf, const Trie::NormalizationForm& nf_decomp) {
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
      Trie::CharStreamForComposition in(starter, rest_starter, canonical_classes, buffer3);
      while(in.eos1()==false) // XXX: name, in.eos1()
	nf_c.compose(in, buf);
      return rest_starter + in.over();
    }

    void canonical_combining_class_ordering(char* beg, const char* end) {
      canonical_classes.assign(end-beg+1, 0); // +1 is for sentinel value
      ccc.sort(beg, canonical_classes);
    }

    const char* next_invalid_char(const char* src, const Trie::NormalizationForm& nf, bool decomp_phase=false) {
      int last_canonical_class = 0;
      const char* cur = Util::nearest_utf8_char_start_point(src);
      const char* starter = cur;
      
      for(; *cur != '\0'; cur = Util::nearest_utf8_char_start_point(cur+1)) {
	int canonical_class = ccc.get_class(cur);
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
      while(ccc.get_class(cur)!=0)
	cur = Util::nearest_utf8_char_start_point(cur+1);
      return cur;
    }

  private:
    const Trie::NormalizationForm nf_d;
    const Trie::NormalizationForm nf_kd;
    const Trie::NormalizationForm nf_c;
    const Trie::NormalizationForm nf_c_qc;
    const Trie::NormalizationForm nf_kc_qc;
    const Trie::CanonicalCombiningClass ccc;
    
    std::string buffer;
    std::string buffer2;
    std::string buffer3;
    std::vector<unsigned char> canonical_classes;
  };
}

#endif
