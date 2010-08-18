#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cstdlib>
#include "trie/builder.hh"

/**
 * macro and function declaration
 */
#define CAN_DEC_FILE      (data_dir+"/canonical-decomposition.def").c_str()
#define COM_DEC_FILE      (data_dir+"/compatibility-decomposition.def").c_str()
#define CAN_COM_FILE      (data_dir+"/canonical-composition.def").c_str()
#define CCC_FILE          (data_dir+"/canonical-combining-class.def").c_str()
#define NFC_ILLEGAL_FILE  (data_dir+"/nfc-illegal-char.def").c_str()
#define NFKC_ILLEGAL_FILE (data_dir+"/nfkc-illegal-char.def").c_str()

bool read_mapping_definition(const char* filepath, const char* prefix, std::ofstream& out, std::string& buffer);
bool read_char_attr_definition(const char* filepath, const char* prefix, std::ofstream& out);

/**
 * main function
 */
int main(int argc, char** argv) {
  if(argc != 3) {
    std::cerr << "Usage: gen-unf-table <table-file(C++ header)> <data-dir>" << std::endl;
    return 1;
  }
  
  const char* table_file = argv[1];
  const std::string data_dir = argv[2];

  std::cerr << "= create table file (C++ header file): " << table_file << std::endl;
  std::ofstream out(table_file);
  if(!out) {
    std::cerr << "Can't open file for writing: " << table_file << std::endl;
    return 1;
  }
  
  // generate C++ header file
  out << "#ifndef UNF_TABLE_HH" << std::endl
      << "#define UNF_TABLE_HH" << std::endl
      << "namespace UNF {" << std::endl
      << "  namespace TABLE {" << std::endl;

  // read definitions and write Node array
  std::string buffer;
  std::cerr << "  == canonical decomposition mapping" << std::endl;
  if(read_mapping_definition(CAN_DEC_FILE, "CANONICAL_DECOM", out, buffer)==false)
    return 1;

  std::cerr << "  == compatibility decomposition mapping" << std::endl;
  if(read_mapping_definition(COM_DEC_FILE, "COMPATIBILITY_DECOM", out, buffer)==false)
    return 1;

  std::cerr << "  == canonical composition mapping" << std::endl;
  if(read_mapping_definition(CAN_COM_FILE, "CANONICAL_COM", out, buffer)==false)
    return 1;

  std::cout << "  == canonical combining class" << std::endl;
  if(read_char_attr_definition(CCC_FILE, "CANONICAL_CLASS", out)==false)
    return 1;

  std::cout << "  == NFC illegal character list" << std::endl;
  if(read_char_attr_definition(NFC_ILLEGAL_FILE, "NFC_ILLEGAL", out)==false)
    return 1;

  std::cout << "  == NFKC illegal character list" << std::endl;
  if(read_char_attr_definition(NFKC_ILLEGAL_FILE, "NFKC_ILLEGAL", out)==false)
    return 1;

  // write value string
  out << "const char VALUE[]={" << std::endl;
  for(unsigned i=0; i < buffer.size(); i++) {
    out << "0x" << std::hex << std::setw(2) << std::setfill('0') << std::right << std::hex 
	<< static_cast<int>(static_cast<unsigned char>(buffer[i]));
    if(i+1 < buffer.size()) out << ',';
    if((i+1)%20==0)         out << std::endl;
  }
  out << "};" << std::endl;

  out << "  }" << std::endl
      << "}" << std::endl
      << "#endif" << std::endl;
  
  std::cout << "DONE" << std::endl;
  return 0;
}

/**
 * function definition
 */
// File format:
//  [FROM STRING]\t[TO STRING]\n
bool read_mapping_definition(const char* filepath, const char* prefix, std::ofstream& out, std::string& buffer) {
  using UNF::Trie::MappingKey;
  using UNF::Trie::MappingKeyList;

  std::ifstream in(filepath);
  if(!in) {
    std::cerr << "Can't open file: " << filepath << std::endl;
    return false;
  }

  MappingKeyList keys;
  for(std::string line; std::getline(in,line);) {
    std::size_t p = line.find('\t');
    std::string key = line.substr(0,p);
    std::string value = line.substr(p+1);
    
    // search sharable value
    //  - using liner search (for simplicity)
    //  - very slow
    std::size_t pos = buffer.find(value+'\0');
    if(pos == std::string::npos)
      keys.push_back(MappingKey(key, value, buffer));
    else
      keys.push_back(MappingKey(key, pos));
  }
  std::sort(keys.begin(), keys.end());

  UNF::Trie::Builder<MappingKeyList>(keys).build().output_nodes_cpp_array(out, prefix);
  return true;
}

// File format:
//  [two-digit hexadecimal] [CHARACTER]
bool read_char_attr_definition(const char* filepath, const char* prefix, std::ofstream& out) {
  using UNF::Trie::AttrKey;
  using UNF::Trie::AttrKeyList;

  std::ifstream in(filepath);
  if(!in) {
    std::cerr << "Can't open file: " << filepath << std::endl;
    return false;
  }

  AttrKeyList keys;
  for(std::string line; std::getline(in,line);) 
    keys.push_back(AttrKey(line.substr(3), strtol(line.substr(0,2).c_str(),NULL,16)));
  std::sort(keys.begin(), keys.end());
  
  UNF::Trie::Builder<AttrKeyList>(keys).build().output_nodes_cpp_array(out, prefix);
  return true;
}
