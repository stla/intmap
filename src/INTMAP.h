#ifndef _INTMAPHEADER_
#include "intmap_types.h"
#endif

intmapR intmapNew(Rcpp::IntegerVector, Rcpp::List);

class INTMAP {
 public:
  intmapR intmap;
  Rcpp::XPtr<intmapR> ptr;
  INTMAP(Rcpp::IntegerVector keys_, Rcpp::List values_)
      : intmap(intmapNew(keys_, values_)), 
        ptr(Rcpp::XPtr<intmapR>(&intmap, false)) {}
  INTMAP(Rcpp::XPtr<intmapR> ptr_)
      : intmap(*(ptr_.get())), 
        ptr(Rcpp::XPtr<intmapR>(&intmap, false)) {}
  ~INTMAP() {}

  unsigned size() { return intmap.size(); }

  Rcpp::List at(int key) {
    intmapR::iterator it = intmap.find(key);
    if(it != intmap.end()) {
      return Just(it->second);
    } else {
      return Nothing();
    }
  }

  unsigned index(int key) {
    intmapR::iterator it = intmap.find(key);
    if(it != intmap.end()) {
      return intmap.index_of(it) + 1;
    } else {
      return 0;
    }
  }

  bool has_key(int key) { return intmap.contains(key); }

  Rcpp::List nth(const unsigned i) {
    const unsigned s = intmap.size();
    if(i >= s) {
      Rcpp::stop("Index too large.");
    }
    intmapR::iterator it = intmap.nth(i);
    int key = it->first;
    Rcpp::RObject value = it->second;
    return Rcpp::List::create(Rcpp::Named("key") = key,
                              Rcpp::Named("value") = value);
  }

  Rcpp::IntegerVector keys() {
    unsigned s = intmap.size();
    Rcpp::IntegerVector out(s);
    unsigned i = 0;
    for(intmapR::iterator it = intmap.begin(); it != intmap.end(); it++) {
      out[i] = it->first;
      i++;
    }
    return out;
  }

  Rcpp::List values() {
    const unsigned s = intmap.size();
    Rcpp::List out(s);
    unsigned i = 0;
    for(intmapR::iterator it = intmap.begin(); it != intmap.end(); it++) {
      out(i) = it->second;
      i++;
    }
    return out;
  }

  Rcpp::List toList() {
    const unsigned s = intmap.size();
    Rcpp::IntegerVector Keys(s);
    Rcpp::List Values(s);
    unsigned i = 0;
    for(intmapR::iterator it = intmap.begin(); it != intmap.end(); it++) {
      Keys(i) = it->first;
      Values(i) = it->second;
      i++;
    }
    Rcpp::List out;
    out["keys"] = Keys;
    out["values"] = Values;
    return out;
  }

  bool insert(int key, Rcpp::RObject value) {
    std::pair<intmapR::iterator, bool> x = intmap.emplace(key, value);
    return x.second;
  }

  bool assign(int key, Rcpp::RObject value) {
    std::pair<intmapR::iterator, bool> x = intmap.insert_or_assign(key, value);
    return x.second;
  }

  void erase(int key) {
    intmap.erase(key);
  }

  void merase(Rcpp::IntegerVector keys) {
    for(int key : keys) {
      intmap.erase(key);
    }
  }

  Rcpp::XPtr<intmapR> extract(Rcpp::IntegerVector keys) {
    intmapR submap;
    for(int key : keys) {
      intmapR::iterator it = intmap.find(key);
      if(it != intmap.end()) {
        submap.emplace(key, it->second);
      }
    }
    intmapR* submapptr(new intmapR(submap));
    Rcpp::XPtr<intmapR> out(submapptr, false);
    return out;
  }

  void extract_inplace(Rcpp::IntegerVector keys) {
    intmapR submap;
    for(int key : keys) {
      intmapR::iterator it = intmap.find(key);
      if(it != intmap.end()) {
        submap.emplace(key, it->second);
      }
    }
    intmap = submap;
  }

  Rcpp::XPtr<intmapR> extract_by_erasing(Rcpp::IntegerVector keys) {
    intmapR* submapptr = new intmapR(intmap);
    intmapR submap = *submapptr;
    for(intmapR::iterator it = intmap.begin(); it != intmap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()) {
        submap.erase(it->first);
      }
    }
    Rcpp::XPtr<intmapR> out(new intmapR(submap), false);
    delete submapptr;
    return out;
  }

  void extract_by_erasing_inplace(Rcpp::IntegerVector keys) {
    for(intmapR::iterator it = intmap.begin(); it != intmap.end(); it++) {
      if(std::find(keys.begin(), keys.end(), it->first) == keys.end()) {
        intmap.erase(it->first);
      }
    }
  }

  void merge(Rcpp::XPtr<intmapR> intmap2ptr) {
    intmapR intmap2 = *(intmap2ptr.get());
    intmap.merge(intmap2);
  }
};