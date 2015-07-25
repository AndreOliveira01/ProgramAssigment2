# R Programming Course
# Programming Assignment 2: Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL 
        set <- function(y) {
                x <<- y
                xinv <<- NULL 
        }
        
        get <- function() x 
        setInv <- function(inv) xinv <<- inv 
        getInv <- function() xinv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) { 
                message("get cache data")
                return(m) 
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}

# Testing
test <- matrix(runif(4,1,100),2,2)
testCached <- makeCacheMatrix(test)


testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
