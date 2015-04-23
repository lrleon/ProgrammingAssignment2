# This program is inspirated by one analogous (written by Roger Peng) 
# for computing the mean of a vector.
#
# In this case, we compute the inverse of a matrix
#
# Usage: two phases
#
# Phase 1: execute my.cached.matrix <- makeCacheMatrix(mat)
#          In this phase the inverse of mat is not computed, but all is
#          arranged for computing it when the function cacheSolve(mat)
#          be called.
#          By example, if we have a matrix mat, then to execute
#
#                my.cached.matrix <- makeCacheMatrix(mat)
#
#          will prepare all the computations for finding the inverse of
#          mat
#
# Phase 2: execute cacheSolve(my.cached.matrix)
#
#          The first time, cacheSolve will invoke to solve in order to
#          obtain the inverse of mat (that is internally referenced in
#          my.cached.matrix) and will return the inverse; but its value
#          (the inverse) will remain internally saved. After, the
#          following times that cacheSolve be called, instead of calling
#          to solve, the method will get the saved result without
#          needing to repeat the computation
#
# NOTE FOR DEVELOPPERS: If you know the Roger Peng version of cachemean,
# please note that we do not use the set function neither the set field
# for the return value of makeCacheMatrix


## prepare a cache of inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
    I <- NULL                                  # inverse is not still computed
    get <- function() x                        # simply return x
    set.inverse <- function(solve) I <<- solve 
    get.inverse <- function() I

        # now set all needed for future solving and caching
    list(get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}



## Return a matrix that is the inverse of 'x' which is a matrix stored
## in cache parameter. If the inverse was already computed and x was not
## modified, then this function retrieves the previous computation
cacheSolve <- function(cache, ...) {
     I <- cache$get.inverse()
     if(!is.null(I)) {                   # is I already computed?
         message("getting cached data")  # yes, simply retrieve it
         return(I)                       # and return it
     }
     mat <- cache$get()        # get the matrix of vector x
     I <- solve(mat, ...)      # compute its inverse
     cache$set.inverse(I)      # save it in the I as deep scope
     I
}
