library(intmap)
library(microbenchmark)

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
