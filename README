Unicode正規化ライブラリ(NFC,NFD,NFKC,NFKD)。
対応言語: C++(unf)、Ruby(unf-ruby)、Common Lisp(cl-unf)
※ C++及びRuby版はUTF-8を、Common Lisp版はUTF-32を扱う
※ 2011/11追記# Ruby版のUNFであればknuさんによる https://github.com/knu/ruby-unf_ext の使用をお勧めします

---

- unfの開発版
- 変換テーブル生成ソース/データを含む
- 本番用(?)のREADMEおよびMakefileには'.production'が付いている

[変換テーブル生成方法]
# ※ SBCLが必要
# ※ gen-table.lispはいろいろ未整理
$ cd lisp
$ sbcl --script gen-table.lisp ../data/ ../src/unf/table.hh
