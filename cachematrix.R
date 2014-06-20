## This function is designed to invert a matrix and cache
## the result

## This first function creates the object that contains the functions
## to do the caching (a list composed of functions)

makeCacheMatrix <- function(x = matrix()) {
                s<-NULL
                set<-function(y) {
                        x<<-y
                        s<<-NULL
                }
                get<-function() x
                setsolve<-function(solve) s <<-solve
                getsolve<-function() s
                list(set = set, get = get, setsolve = setsolve, 
                     getsolve = getsolve)
        }
        
## Does the actual solving of the inverse. If it is cached, it returns the 
## previous solution. If not, it computes it anew and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' - if the cache
        ## is not null (already contains the inverse) then returns the 
        ## message "getting cached data" and returns the cached value ...
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## ... otherwise it will compute the new inverse, cache it, and
        ## return that.
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
