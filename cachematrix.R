## This module implements a cached version matrix object.
## Currently it implements matrix inversion operation through 
## R's builtin solve ().

## Returns a closure which caches computed values for matrix
makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    getsolved <- function () solved
    setsolved <- function (nsolved) {
        solved <<- nsolved
    }
    list (set = set, get = get, getsolved = getsolved, setsolved = setsolved)
}


## Invoke solve () operation on the cached matrix. 
## This computes inverse of matix if no 2nd argument is provided, 
## else the 2nd argument is used as the RHS of the linear system
## of equations for finding the solution
cacheSolve <- function(cm, ...) {
    tryCatch({
                inv <- cm$getsolved()
                if (!is.null (inv)) {
                    message ("getting from cache")
                    return (inv)
                }
            },
            error = function (e) {
                stop ("E: Provide a cached matrix returned by makeCachedMatrix ()")
            },
            warning = function(w) {
                stop ("W: Provide a cached matrix returned by makeCachedMatrix ()")
            }
    )
    inv <- tryCatch ({
                        message ("computing from scratch")
                        mat <- cm$get()
                        solve(mat, ...)
                     },
                     error = function(e) {
                         message ("Error during matrix solve operation")
                         print (e)
                         NULL
                     },
                     warning = function (w) {
                         message ("Warning during matrix solve operation")
                         print (w)
                         NULL
                     })
    cm$setsolved(inv)
    return (inv)
}
