intmap
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/intmap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/intmap/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check-valgrind](https://github.com/stla/intmap/actions/workflows/R-CMD-check-valgrind.yaml/badge.svg)](https://github.com/stla/intmap/actions/workflows/R-CMD-check-valgrind.yaml)
<!-- badges: end -->

*Ordered containers with integer keys.*

------------------------------------------------------------------------

- Create a new integer map.

``` r
imap <- intmap$new(
  keys = c(11, -2), values = list(c("a", "b"), list(333, 4444, 55555))
)
imap
## `intmap` object containing 2 items:
## 
##   "-2" -> list: 333, 4444, 55555
##   "11" -> character vector: a, b
```

- How many entries are there in the map?

``` r
imap$size()
## [1] 2
```

- Get the keys (always ordered) and the values.

``` r
imap$keys()
## [1] -2 11
imap$values()
## [[1]]
## [[1]][[1]]
## [1] 333
## 
## [[1]][[2]]
## [1] 4444
## 
## [[1]][[3]]
## [1] 55555
## 
## 
## [[2]]
## [1] "a" "b"
```

- Get the item of a given key.

``` r
imap$at(5)
## Nothing
imap$at(11)
## Just
## [1] "a" "b"
from_just(imap$at(11))
## [1] "a" "b"
```

- Or use the `get` method with a default value.

``` r
imap$get(5, default = "I am missing.")
## [1] "I am missing."
imap$get(-2)
## [[1]]
## [1] 333
## 
## [[2]]
## [1] 4444
## 
## [[3]]
## [1] 55555
```

- Insert a new entry.

``` r
imap$insert(5, "I am not missing.")
## [1] TRUE
imap
## `intmap` object containing 3 items:
## 
##   "-2" -> list: 333, 4444, 55555
##   "5" -> character: I am not missing.
##   "11" -> character vector: a, b
```

- Get a submap.

``` r
imap$extract(c(5, 11, 99999))
## `intmap` object containing 2 items:
## 
##   "5" -> character: I am not missing.
##   "11" -> character vector: a, b
```

- Merge with another map.

``` r
imap2 <- intmap$new(
  c(-2, 0, 1),
  list("I will be ignored", iris, mtcars)
)
imap$merge(imap2)
imap
## `intmap` object containing 5 items:
## 
##   "-2" -> list: 333, 4444, 55555
##   "0" -> data.frame
##   "1" -> data.frame
##   "5" -> character: I am not missing.
##   "11" -> character vector: a, b
```
