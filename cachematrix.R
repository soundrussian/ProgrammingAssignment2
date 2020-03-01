## A set of functions for caching inverse matrices

#' Constructor for a matrix that caches its inverse.
#' 
#' \code{makeCacheMatrix} returns a list with functions that provide caching mechanism.
#' The caching mechanism does not pollute the current environment with cached data,
#' thanks to R \code{<<-} operator that assigns values to objects from different environment.
#' 
#' It should be used with corresponding \code{cacheSolve} from this file.
#' 
#' @param(x) A matrix which inverse needs to be cached.
#' 
#' @return A list with the following functions and objects:
#'   \code{s} Matrix or \code{NULL}. Cached inverse of matrix \code{x}
#'   \code{set} Function that accepts a matrix, caches it instead of initial matrix and clears inverse cache.
#'   \code{get} Returns cached matrix.
#'   \code{setsolved} Caches the inverse.
#'   \code{getsolved} Returns the cached inverse.
#' 
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolved <- function(solved) s <<- solved
        getsolved <- function() s
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}


#' Calculates inverse of a cached matrix using cache
#' 
#' If there is a cached inverse of the matrix, returns the cached result.
#' Otherwise, calculates the inverse via \code{solve}, caches it, and returns the result.
#' 
#' @param(x) A list returned by \code{makeCacheMatrix} function.
#' 
#' @return The inverse of matrix stored in \code{x}.

cacheSolve <- function(x, ...) {
        s <- x$getsolved()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolved(s)
        s
}
