#ifndef _INTMAPHEADER_
#include "intmap_types.h"
#endif

intmapR intmapNew(Rcpp::IntegerVector keys, Rcpp::List values) {
  intmapR intmap;
  for(R_xlen_t i = 0; i < keys.size(); i++) {
    // SEXP v = values[i];
    intmap.emplace(keys[i], values[i]);
  }
  return intmap;
}

Rcpp::List Just(Rcpp::RObject x) {
  Rcpp::List L = Rcpp::List::create(Rcpp::Named("type") = "just",
                                    Rcpp::Named("content") = x);
  L.attr("class") = "maybe";
  return L;
}

Rcpp::List Nothing() {
  Rcpp::List L = Rcpp::List::create(Rcpp::Named("type") = "nothing");
  L.attr("class") = "maybe";
  return L;
}