library(intmap)
library(microbenchmark)

# erase
keys <- 1:10000
values <- as.list(letters[1 + keys%%26])

imap <- intmap$new(keys, values)

evens <- keys[keys %% 2L == 0L]
evens <- c(1L, 5000L, 10000L)

microbenchmark(
  intmap = imap$erase(evens), # very slow - except for small evens
  List   = values[-evens],
  times = 5
)

# insert 
keys <- 1:10000
values <- as.list(letters[1 + keys%%26])
imap <- intmap$new(keys, values)

fintmap <- function() {
  for(i in 5000:15000) {
    imap$insert(i, "X", replace = TRUE)
  }
}

flist <- function() {
  for(i in as.character(5000:15000)) {
    values[[i]] <- "X"
  }
}

microbenchmark(
  intmap = fintmap(), # faster
  List   = flist(),
  times = 5
)

# insert again
items <- (-5000):15000
items_char <- as.character(items)

fintmap <- function() {
  for(. in 1:10000) {
    i <- sample(items, size = 1L)
    imap$insert(i, "X", replace = TRUE)
  }
}

flist <- function() {
  for(. in 1:10000) {
    i <- sample(items_char, size = 1L)
    values[[i]] <- "X"
  }
}

microbenchmark(
  intmap = fintmap(), # faster
  List   = flist(),
  times = 5, 
  setup = {
    keys <- 1:10000
    values <- as.list(letters[1 + keys%%26])
    imap <- intmap$new(keys, values)
  }
)

# extract 
keys <- 1:1000
values <- as.list(letters[1 + keys%%26])
imap <- intmap$new(keys, values)

fintmap <- function() {
  imap$extract(500:1500, inplace = FALSE, bydeleting = TRUE)
}

flist <- function() {
  newvalues <- values[as.character(500:1500)]
}

microbenchmark(
  intmap = fintmap(), # slower
  List   = flist(),
  times = 5
)
