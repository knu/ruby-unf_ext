#include <iostream>
#include <string>
#include <vector>
#include <cstring>
#include "normalizer.hh"

#define NF_TEST(form, correct, target) strcmp(correct.c_str(), form(target.c_str()))==0

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
      /*
      std::cout << cnt << std::endl;
      for(unsigned i=0; i < ary.size(); i++) 
	std::cerr << " " << ary[i] << "\t -> " << norm.nfd(ary[i].c_str()) << std::endl;
      */
      if(nfd_test(norm, ary)==false) {
	std::cerr << "Failed(" << cnt << "): NFD" << std::endl;
	for(unsigned i=0; i < ary.size(); i++) 
	  std::cerr << " " << ary[i] << "\t -> " << norm.nfd(ary[i].c_str()) << std::endl;
	return 1;
      } 
      if(nfkd_test(norm, ary)==false) {
	std::cerr << "Failed(" << cnt << "): NFKD" << std::endl;
	for(unsigned i=0; i < ary.size(); i++) 
	  std::cerr << " " << ary[i] << "\t -> " << norm.nfkd(ary[i].c_str()) << std::endl;
	return 1;
      }
      if(nfc_test(norm, ary)==false) {
	std::cerr << "Failed(" << cnt << "): NFC" << std::endl;
	for(unsigned i=0; i < ary.size(); i++) {
	  std::cerr << " " << ary[i] << "\t -> " << norm.nfc(ary[i].c_str());
	  std::cerr << " # ";
	  for(unsigned j=0; j < ary[i].size(); j++) 
	    std::cerr << (int)ary[i][j] << " ";
	  std::cerr << " -> ";
	  const char* s=norm.nfc(ary[i].c_str());
	  for(; *s != '\0'; s++)
	    std::cerr << (int)*s << " ";
	  std::cerr << std::endl;
	}
	return 1;
      } 
      if(nfkc_test(norm, ary)==false) {
	std::cerr << "Failed(" << cnt << "): NFKC" << std::endl;
	for(unsigned i=0; i < ary.size(); i++) {
	  std::cerr << " " << ary[i] << "\t -> " << norm.nfkc(ary[i].c_str());
	  std::cerr << std::endl;
	}
	return 1;
      } 
      /*
      if(nfkc_test(norm, ary)==false) {
	std::cerr << "Failed(" << cnt << "): NFKC" << std::endl;
	for(unsigned i=0; i < ary.size(); i++) 
	  std::cerr << " " << ary[i] << "\t -> " << norm.nfkc(ary[i].c_str()) << std::endl;
	return 1;
      } 
      */
      ary.clear();
      //std::getline(std::cin, line); // eat empty line
    } else {
      ary.push_back(line);
    }
  }
  
  std::cerr << "All tests passed!" << std::endl << std::endl;
  return 0;
}
