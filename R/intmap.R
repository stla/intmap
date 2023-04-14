#' @title R6 class representing an ordered map
#'
#' @description A map is given by keys and values.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom methods new
#' @importFrom maybe just nothing from_maybe
intmap <- R6Class(
  
  "intmap",
  
  lock_class = TRUE,
  
  lock_objects = TRUE, 
  
  cloneable = FALSE,
  
  private = list(
    .map = NULL,
    .ptrinit = function(ptr) {
      map <- intmap$new(character(0L), list())
      map[[".__enclos_env__"]][["private"]][[".map"]] <- new("INTMAP", ptr)
      map
    }
  ),
  
  public = list(
    
    #' @description Creates a new \code{intmap} object.
    #'
    #' @param keys keys, an integer vector without \code{NA} value
    #' @param values values, a list of R objects; \code{keys} and 
    #'   \code{values} must have the same length
    #'
    #' @return An \code{intmap} object.
    #'
    #' @examples
    #' intmap$new() # empty map
    #' intmap$new(
    #'   keys = c(4, -2), 
    #'   values = list(c(1, 2), c("a", "b", "c"))
    #' )
    #' # examples with duplicated keys:
    #' intmap$new(
    #'   keys = c(1, 1, 5), 
    #'   values = list(c(1, 2), c(3, 4), "x")
    #' )
    initialize = function(keys = NULL, values) {
      if(length(keys) == 0L) {
        IMAP <- new("INTMAP", integer(0L), list())
      } else {
        keys <- as.integer(keys)
        if(any(is.na(keys))) {
          stop("Keys cannot contain missing values.")
        }
        stopifnot(
          is.list(values),
          length(keys) == length(values)
        )
        ord <- order(keys)
        IMAP <- new("INTMAP", keys[ord], values[ord])
      }
      private[[".map"]] <- IMAP
      invisible(NULL)
    },
    
    #' @description Show instance of an \code{intmap} object.
    #' @param ... ignored
    print = function(...) {
      size <- self$size()
      if(size == 0L) {
        cat("empty `intmap` object\n")
      }else{
        keys_values <- private[[".map"]]$toList()
        keys <- sprintf('"%s"', keys_values[["keys"]])
        values <- vapply(keys_values[["values"]], toString2, character(1L))
        s <- ifelse(size > 1L, "s", "")
        cat(sprintf("`intmap` object containing %d item%s:\n\n", size, s))
        lines <- paste0("  ", keys, " -> ", values)
        cat(lines, sep = "\n")
      }
    },
    
    #' @description Size of the reference map.
    #'
    #' @return An integer, the number of entries.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$size()
    size = function() {
      private[[".map"]]$size()
    },
    
    #' @description Get all keys.
    #'
    #' @return The keys, an integer vector.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$keys()
    keys = function() {
      private[[".map"]]$keys()
    },
    
    #' @description Get all values.
    #'
    #' @return The values, a list of R objects.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$values()
    values = function() {
      private[[".map"]]$values()
    },
    
    #' @description Get all entries of the reference map.
    #'
    #' @return The entries in a dataframe.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$items()
    items = function() {
      L <- private[[".map"]]$toList()
      keys <- L[["keys"]]
      values <- L[["values"]]
      data.frame(key = keys, value = I(values))
    },
    
    #' @description Converts the map to a named list.
    #'
    #' @return A named list (the names are the keys).
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$toList()
    toList = function() {
      L <- private[[".map"]]$toList()
      values <- L[["values"]]
      names(values) <- L[["keys"]]
      values
    },
    
    #' @description Returns the 'maybe' value corresponding to the given key.
    #'
    #' @param key a key (integer)
    #'
    #' @return A \code{maybe} value, either the value corresponding to the key 
    #'   as a 'Just' \code{maybe} value if the key is found, otherwise the 
    #'   'Nothing' \code{maybe} value.
    #' 
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$at(11)
    #' from_just(imap$at(11))
    #' imap$at(4)
    at = function(key) {
      stopifnot(isInteger(key))
      private[[".map"]]$at(as.integer(key))
    },

    #' @description Get the value corresponding to the given key or a default 
    #'   value if this key is missing.
    #'
    #' @param key a key (integer)
    #' @param default a R object, the default value
    #'
    #' @return Either the value corresponding to the key if the key is found, 
    #'   otherwise the \code{default} value.
    #' 
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$get(11, default = 999)
    #' imap$get(4, default = 999)
    get = function(key, default = NULL) {
      stopifnot(isInteger(key))
      from_maybe(private[[".map"]]$at(as.integer(key)), default)
    },
    
    #' @description Returns the index of the given key.
    #'
    #' @param key a key (integer)
    #'
    #' @return The index of the key, or \code{NA} if it is not found.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$index(11)
    #' imap$index(4)
    index = function(key) {
      stopifnot(isInteger(key))
      i <- private[[".map"]]$index(as.integer(key))
      if(i == 0L) {
        NA_integer_
      } else {
        i
      }
    },
    
    #' @description Extract a submap from the reference map.
    #'
    #' @param keys some keys, an integer vector; those which do not belong to 
    #'   the keys of the reference map will be ignored
    #' @param inplace Boolean, whether to update the reference map or 
    #'   to return a new map
    #' @param bydeleting Boolean, whether to construct the submap by 
    #'   deleting the keys which are not in \code{keys} or by starting 
    #'   from the empty submap and adding the entries 
    #'
    #' @return An \code{intmap} object if \code{inplace=FALSE}, 
    #'   otherwise the updated reference map, invisibly.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2, 3), values = list(c("a", "b"), list(3, 4, 5), "X")
    #' )
    #' imap_copy <- imap$copy()
    #' imap$extract(c(11, 3))
    #' imap
    #' imap$extract(c(11, 3), inplace = TRUE)
    #' imap
    #' imap_copy$extract(c(11, 3), bydeleting = TRUE)
    #' imap_copy
    #' imap_copy$extract(c(11, 3), inplace = TRUE, bydeleting = TRUE)
    #' imap_copy
    extract = function(keys, inplace = FALSE, bydeleting = FALSE) {
      stopifnot(isIntegerVector(keys))
      stopifnot(isBoolean(inplace))
      stopifnot(isBoolean(bydeleting))
      keys <- as.integer(keys)
      if(length(keys) == 0L) {
        if(inplace) {
          private[[".map"]] <- new("INTMAP", integer(0L), list())
          invisible(self)
        } else {
          intmap$new(integer(0L), list())
        }
      } else {
        if(bydeleting) {
          if(inplace) {
            private[[".map"]]$extract_by_erasing_inplace(keys)
            invisible(self)
          } else {
            ptr <- private[[".map"]]$extract_by_erasing(keys)
            private[[".ptrinit"]](ptr)
          }
        } else {
          if(inplace) {
            private[[".map"]]$extract_inplace(keys)
            invisible(self)
          } else {
            ptr <- private[[".map"]]$extract(keys)
            private[[".ptrinit"]](ptr)
          }
        }
      }
    },
    
    #' @description Checks whether a key exists in the reference map.
    #'
    #' @param key a key (integer)
    #'
    #' @return A Boolean value.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$has_key(11)
    #' imap$has_key(1)
    has_key = function(key) {
      stopifnot(isInteger(key))
      private[[".map"]]$has_key(as.integer(key))
    },
    
    #' @description Returns the n-th entry of the reference map.
    #'
    #' @param n index, a positive integer
    #' @param stop_if_too_large a Boolean value, whether to stop if \code{n}
    #'   is too large, or to use \code{maybe} values
    #'
    #' @return A list with the key and the value at index \code{n} if 
    #'   \code{stop_if_too_large=TRUE} and \code{n} is not too large, otherwise 
    #'   a \code{maybe} value: either this list wrapped in a 'Just' container, 
    #'   or 'Nothing'.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$nth(2)
    #' imap$nth(2, stop_if_too_large = FALSE)
    #' imap$nth(9, stop_if_too_large = FALSE)
    nth = function(n, stop_if_too_large = TRUE) {
      stopifnot(isPositiveInteger(n))
      stopifnot(isBoolean(stop_if_too_large))
      if(stop_if_too_large) {
        private[[".map"]]$nth(as.integer(n) - 1L)
      }else{
        tryCatch({
          just(private[[".map"]]$nth(as.integer(n) - 1L))
        }, error = function(e) {
          nothing()
        })
      }
    },
    
    #' @description Insert a new entry in the reference map.
    #'
    #' @param key a key (integer)
    #' @param value a value (R object)
    #' @param replace Boolean, whether to replace the value if the key is 
    #'   already present
    #'
    #' @return This updates the reference map and this returns a Boolean value:
    #'   if \code{replace=FALSE}, this returns \code{TRUE} if the value has 
    #'   been inserted (i.e. the given key is new); similarly, if 
    #'   \code{replace=TRUE}, this returns \code{TRUE} if the given key is new 
    #'   (so \code{FALSE} means that the value of the existing key has been 
    #'   replaced).
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap$insert(3, c(6, 7)) # TRUE (insertion)
    #' imap
    #' imap$insert(11, c(8, 9)) # FALSE (no change)
    #' imap
    #' imap$insert(11, c(8, 9), replace = TRUE) # FALSE (replacement)
    #' imap
    insert = function(key, value, replace = FALSE) {
      stopifnot(isInteger(key))
      stopifnot(isBoolean(replace))
      if(replace) {
        private[[".map"]]$assign(as.integer(key), value)
      }else{
        private[[".map"]]$insert(as.integer(key), value)
      }
    },
    
    #' @description Erase the entries of the reference map whose keys are the 
    #'   given ones.
    #'
    #' @param keys some keys, an integer vector; those which do not belong to
    #'   the keys of the reference map are ignored
    #'
    #' @return The reference map, invisibly.
    #'
    #' @examples
    #' imap <- intmap$new(
    #'   keys = c(11, -2, 3), values = list(c("a", "b"), list(3, 4, 5), "X")
    #' )
    #' imap$erase(11)
    #' imap
    #' imap$erase(c(-2, 3))
    #' imap
    erase = function(keys) {
      stopifnot(isIntegerVector(keys))
      if(length(keys) == 1L) {
        private[[".map"]]$erase(as.integer(keys))
      } else if(length(keys) >= 2L) {
        private[[".map"]]$merase(as.integer(keys))
      }
      invisible(self)
    },
    
    #' @description Merge the reference map with another map.
    #'
    #' @param map an \code{intmap} object
    #'
    #' @return The updated reference map, invisibly. Keys of \code{map} that 
    #'   are also keys of the reference map are ignored, i.e. there is no 
    #'   replacement, only insertions.
    #'
    #' @examples
    #' imap1 <- intmap$new(
    #'   keys = c(11, -2), values = list(c("a", "b"), list(3, 4, 5))
    #' )
    #' imap2 <- intmap$new(
    #'   keys = c(11, 3), values = list("X", "Z")
    #' )
    #' imap1$merge(imap2)
    #' imap1
    merge = function(map) {
      stopifnot(inherits(map, "intmap"))
      .map2 <- map[[".__enclos_env__"]][["private"]][[".map"]]
      private[[".map"]]$merge(.map2$ptr)
      invisible(self)
    },
    
    #' @description Copy the reference map.
    #'
    #' @return A copy of the reference map.
    #' 
    #' @examples 
    #' imap <- intmap$new(
    #'   keys = c(11, 3), values = list(TRUE, "Z")
    #' )
    #' true_copy <- imap$copy()
    #' true_copy$erase(11)
    #' imap
    #' naive_copy <- imap
    #' naive_copy$erase(11)
    #' imap
    copy = function() {
      private[[".ptrinit"]](private[[".map"]][["ptr"]])
    }
    
  )
)
