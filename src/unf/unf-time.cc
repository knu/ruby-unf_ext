#include <iostream>
#include <string>
#include <vector>
#include <cstring>
#include "trie/searcher.hh"
#include "normalizer.hh"

#ifdef __unix__

#include <sys/time.h>
inline double gettime(){
  timeval tv;
  gettimeofday(&tv,NULL);
  return static_cast<double>(tv.tv_sec)+static_cast<double>(tv.tv_usec)/1000000.0;
}

#else

#include <ctime>
inline double gettime() {
  return static_cast<double>(clock())/CLOCKS_PER_SEC;
}

#endif

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

  UNF::Normalizer::Form forms[] = {UNF::Normalizer::FORM_NFD, UNF::Normalizer::FORM_NFC,
				   UNF::Normalizer::FORM_NFKD,UNF::Normalizer::FORM_NFKC};
  const char* form_names[5] = {"NFD ","NFC ","NFKD","NFKC"};
  
  std::cerr << "= time: " << std::endl;
  int n=0;
  for(unsigned formid=0; formid < 4; formid++) {
    std::cerr << "  == " << form_names[formid] << ":  ";
    double beg_t = gettime();
    for(unsigned i=0; i < lines.size(); i++) 
      n += strlen(norm.normalize(lines[i].c_str(), forms[formid]));
    std::cerr << gettime()-beg_t << " sec" << std::endl;    
  }

  std::cerr << "DONE" << std::endl;
  return n;
}
