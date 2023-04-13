#ifndef _INTMAPHEADER_
#define _INTMAPHEADER_

#include <Rcpp.h>
#include <boost/container/flat_map.hpp>

typedef boost::container::flat_map<int, Rcpp::RObject> intmapR;

Rcpp::List Just(Rcpp::RObject);
Rcpp::List Nothing();

#endif