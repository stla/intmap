#ifndef _INTMAPHEADER_
#include "intmap_types.h"
#endif

#include "INTMAP.h"

void finalizer_of_intmap(INTMAP* ptr) {
  ptr->intmap.clear();
}

RCPP_MODULE(class_INTMAP) {
  using namespace Rcpp;

  class_<INTMAP>("INTMAP")

      .constructor<Rcpp::IntegerVector, Rcpp::List>()
      .constructor<Rcpp::XPtr<intmapR>>()

      .field("ptr", &INTMAP::ptr)

      .method("size", &INTMAP::size)
      .method("at", &INTMAP::at)
      .method("has_key", &INTMAP::has_key)
      .method("index", &INTMAP::index)
      .method("nth", &INTMAP::nth)
      .method("insert", &INTMAP::insert)
      .method("assign", &INTMAP::assign)
      .method("erase", &INTMAP::erase)
      .method("merase", &INTMAP::merase)
      .method("merge", &INTMAP::merge)
      .method("keys", &INTMAP::keys)
      .method("values", &INTMAP::values)
      .method("toList", &INTMAP::toList)
      .method("extract", &INTMAP::extract)
      .method("extract_inplace", &INTMAP::extract_inplace)
      .method("extract_by_erasing", &INTMAP::extract_by_erasing)
      .method("extract_by_erasing_inplace", &INTMAP::extract_by_erasing_inplace)

      .finalizer(&finalizer_of_intmap);
}
