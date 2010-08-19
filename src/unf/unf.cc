#include <iostream>
#include <string>
#include <cstring>
#include "normalizer.hh"

bool eq(const char* s1, const char* s2) { return strcmp(s1, s2)==0; }

int main(int argc, char** argv) {
  if(argc != 2) {
  usage:
    std::cerr << "Usage: unf <D|C|KD|KC>" << std::endl;
    return 1;
  }
  
  UNF::Normalizer norm;

  const char* fs = argv[1];
  UNF::Normalizer::Form form;
  if      (eq(fs,"D"))  form = UNF::Normalizer::FORM_NFD;
  else if (eq(fs,"C"))  form = UNF::Normalizer::FORM_NFC;
  else if (eq(fs,"KD")) form = UNF::Normalizer::FORM_NFKD;
  else if (eq(fs,"KC")) form = UNF::Normalizer::FORM_NFKC;
  else                  goto usage;
    
  std::string line;
  while(std::getline(std::cin,line)) 
    std::cout << norm.normalize(line.c_str(), form) << std::endl;

  return 0;
}
