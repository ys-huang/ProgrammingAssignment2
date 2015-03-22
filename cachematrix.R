
#creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        #storage inverse
        inv <- NULL
        #set a matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        #retrive the matrix
        get <- function() x
        #save inverse matrix
        setinv <- function(inverse) inv<<-inverse
        #retrive inverse matrix
        getinv <- function () inv
        list(set = set,get = get, setinv = setinv, getinv = getinv)
        
}
## check if there is inverse matrix saved, if not, calculate and save it
cacheSolve <- function(x, ...) {
        #check whether have inverse matrix saved
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        #retrive matrix and calculate invers matrix and save it
        data <- x$get()
        inv <-solve(data, ...)
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}

