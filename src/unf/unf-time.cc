#include <iostream>
#include <string>
#include <vector>
#include <cstring>
#include "trie/searcher.hh"
#include "normalizer.hh"

#include <sys/time.h>

inline double gettime(){
  timeval tv;
  gettimeofday(&tv,NULL);
  return static_cast<double>(tv.tv_sec)+static_cast<double>(tv.tv_usec)/1000000.0;
}

int main(int argc, char** argv) {
  UNF::Normalizer norm;

  std::cerr << "= read: " << std::endl;
  std::vector<std::string> lines;
  std::string line;
  unsigned total_length=0;
  while(std::getline(std::cin,line)) {
    lines.push_back(line);
    total_length += line.size();
  }
  std::cerr << "  == " << lines.size() << " lines" << std::endl;
  std::cerr << "  == " << "average(line length): " << total_length/lines.size() << " byte" << std::endl;

  std::cerr << "= time: " << std::endl;
  int n=0;
  {
    std::cerr << "  == NFD:  ";
    double beg_t = gettime();
    for(unsigned i=0; i < lines.size(); i++) 
      n += strlen(norm.nfd(lines[i].c_str()));
    std::cerr << gettime()-beg_t << " sec" << std::endl;
  }
  {
    std::cerr << "  == NFC:  ";
    double beg_t = gettime();
    for(unsigned i=0; i < lines.size(); i++) 
      n += strlen(norm.nfc(lines[i].c_str()));
    std::cerr << gettime()-beg_t << " sec" << std::endl;
  }
  {
    std::cerr << "  == NFKD: ";
    double beg_t = gettime();
    for(unsigned i=0; i < lines.size(); i++) 
      n += strlen(norm.nfkd(lines[i].c_str()));
    std::cerr << gettime()-beg_t << " sec" << std::endl;
  }
  {
    std::cerr << "  == NFKC: ";
    double beg_t = gettime();
    for(unsigned i=0; i < lines.size(); i++) 
      n += strlen(norm.nfkc(lines[i].c_str()));
    std::cerr << gettime()-beg_t << " sec" << std::endl;
  }

  std::cerr << "DONE" << std::endl;
  return n;
}
