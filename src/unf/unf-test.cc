#include <iostream>
#include <string>
#include <vector>
#include <cstring>
#include "normalizer.hh"

#define NF_TEST(form, correct, target) strcmp(correct.c_str(), form(target.c_str()))==0

/**
 * test function
 */
bool nfd_test(UNF::Normalizer& norm, const std::vector<std::string>& ary) {
  return 
    NF_TEST(norm.nfd, ary[2], ary[0]) &&
    NF_TEST(norm.nfd, ary[2], ary[1]) &&
    NF_TEST(norm.nfd, ary[2], ary[2]) &&
    NF_TEST(norm.nfd, ary[4], ary[3]) &&
    NF_TEST(norm.nfd, ary[4], ary[4]);
}

bool nfkd_test(UNF::Normalizer& norm, const std::vector<std::string>& ary) {
  return 
    NF_TEST(norm.nfkd, ary[4], ary[0]) &&
    NF_TEST(norm.nfkd, ary[4], ary[1]) &&
    NF_TEST(norm.nfkd, ary[4], ary[2]) &&
    NF_TEST(norm.nfkd, ary[4], ary[3]) &&
    NF_TEST(norm.nfkd, ary[4], ary[4]);
}

bool nfc_test(UNF::Normalizer& norm, const std::vector<std::string>& ary) {
  return 
    NF_TEST(norm.nfc, ary[1], ary[0]) &&
    NF_TEST(norm.nfc, ary[1], ary[1]) &&
    NF_TEST(norm.nfc, ary[1], ary[2]) &&
    NF_TEST(norm.nfc, ary[3], ary[3]) &&
    NF_TEST(norm.nfc, ary[3], ary[4]);
}

bool nfkc_test(UNF::Normalizer& norm, const std::vector<std::string>& ary) {
  return 
    NF_TEST(norm.nfkc, ary[3], ary[0]) &&
    NF_TEST(norm.nfkc, ary[3], ary[1]) &&
    NF_TEST(norm.nfkc, ary[3], ary[2]) &&
    NF_TEST(norm.nfkc, ary[3], ary[3]) &&
    NF_TEST(norm.nfkc, ary[3], ary[4]);
}

typedef bool (*TEST_FN_T) (UNF::Normalizer&, const std::vector<std::string>&);

bool nf_test(const std::vector<std::string>& ary, UNF::Normalizer& norm, int entry_id) {
  static const UNF::Normalizer::Form forms[] = {UNF::Normalizer::FORM_NFD, UNF::Normalizer::FORM_NFC,
						UNF::Normalizer::FORM_NFKD,UNF::Normalizer::FORM_NFKC};
  static const TEST_FN_T test_fn[] = {nfd_test, nfc_test, nfkd_test, nfkc_test};
  static const char* form_names[5] = {"NFD ", "NFC ", "NFKD", "NFKC"};

  for(int formid=0; formid < 4; formid++)
    if(test_fn[formid](norm, ary)==false) {
      std::cerr << "Failed(" << entry_id << "): " << form_names[formid] << std::endl;
      for(unsigned i=0; i < ary.size(); i++) 
	std::cerr << " " << ary[i] << "\t -> " << norm.normalize(ary[i].c_str(), forms[formid]) << std::endl;
      return false;
    }
  return true;
}

/**
 * main
 */
int main(int argc, char** argv) {
  if(argc != 1) {
    std::cerr << "Usage: unf-test" << std::endl;
    return 1;
  }

  UNF::Normalizer norm;

  unsigned cnt=0;
  std::vector<std::string> ary;
  std::string line;
  while(std::getline(std::cin, line)) {
    if(ary.size()==5) {
      cnt++;
      if(nf_test(ary, norm, cnt)==false)
	return 1;
      ary.clear();
    } else {
      ary.push_back(line);
    }
  }
  if(ary.size()==5) {
    cnt++;
    if(nf_test(ary, norm, cnt)==false)
      return 1;
  }
  
  std::cerr << "All tests passed!: " << cnt << " entries" << std::endl << std::endl;
  return 0;
}
