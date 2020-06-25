## makeCacheMatrix initializes an object by taking a matrix as an argument. It returns a list object which contains 
## 4 functions which can be used to access the value of the matrix and its inverse. cacheSolve is a function which 
## explicitly take the list returned by the makeCacheMatrix function as an argument. It checks whether the inverse is 
## present in the list. If present it returns the value, else it calculates the inverse stores it in the list object
## and then returns the value.

## This function initializes x,m and return a list containing function set, get, setinverse, getinverse 
## which can be accessed using the object list.  

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function return cached inverse invoking the function present in list object if present, else it
## calculates the inverse and stores it in the list object 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
          message("getting cache data")
          return (i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
