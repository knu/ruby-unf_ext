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
  int form = eq(fs,"D") ? 0 : eq(fs,"C") ? 1 : eq(fs,"KD") ? 2 : eq(fs,"KC") ? 3 : 4;
  if(form==4)
    goto usage;

  std::string line;
  while(std::getline(std::cin,line)) 
    switch(form) {
    case 0: std::cout << norm.nfd(line.c_str()) << std::endl; break;
    case 1: std::cout << norm.nfc(line.c_str()) << std::endl; break;
    case 2: std::cout << norm.nfkd(line.c_str()) << std::endl; break;
    case 3: std::cout << norm.nfkc(line.c_str()) << std::endl; break;
    }

  return 0;
}
