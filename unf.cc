#include <ruby.h>

#include "unf/normalizer.hh"

extern "C" {
  VALUE unf_new(VALUE klass);
  VALUE unf_delete(UNF::Normalizer* ptr);
  VALUE unf_normalize(VALUE self, VALUE source, VALUE normalization_form);

  ID FORM_NFD;
  ID FORM_NFC;
  ID FORM_NFKD;
  ID FORM_NFKC;

  void Init_unf() {
    VALUE mdl = rb_define_module("UNF");
   
    VALUE cls = rb_define_class_under(mdl, "Normalizer", rb_cObject);
    rb_define_singleton_method(cls, "new", (VALUE (*)(...))unf_new, 0);
    rb_define_method(cls, "normalize", (VALUE (*)(...))unf_normalize, 2);

    FORM_NFD = rb_intern("nfd");
    FORM_NFC = rb_intern("nfc");
    FORM_NFKD= rb_intern("nfkd");
    FORM_NFKC= rb_intern("nfkc");
  }

  VALUE unf_new(VALUE klass) {
    UNF::Normalizer* ptr;
    VALUE obj = Data_Make_Struct(klass, UNF::Normalizer, NULL, unf_delete, ptr);
    new ((void*)ptr) UNF::Normalizer;
    return obj;
  }

  VALUE unf_delete(UNF::Normalizer* ptr) {
    ptr->~Normalizer();
    ruby_xfree(ptr);
  }

  VALUE unf_normalize(VALUE self, VALUE source, VALUE normalization_form) {
    UNF::Normalizer* ptr;
    Data_Get_Struct(self, UNF::Normalizer, ptr);

    const char* src = StringValuePtr(source);
    const char* rlt;
    ID form_id = SYM2ID(normalization_form);

    if(form_id == FORM_NFD)
      rlt = ptr->normalize(src, UNF::Normalizer::FORM_NFD);
    else if(form_id == FORM_NFC)
      rlt = ptr->normalize(src, UNF::Normalizer::FORM_NFC);
    else if(form_id == FORM_NFKD)
      rlt = ptr->normalize(src, UNF::Normalizer::FORM_NFKD);
    else if(form_id == FORM_NFKC)
      rlt = ptr->normalize(src, UNF::Normalizer::FORM_NFKC);
    else
      rb_raise(rb_eArgError, "Specified Normalization-Form is unknown. Please select one from among :nfc, :nfd, :nfkc, :nfkd.");

    return rb_str_new2(rlt);
  }
}
