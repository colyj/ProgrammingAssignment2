## Author Nicole Johns
## 3/3/2019
## Coursera R Programming course
## Programming Assignment 2

## makeCahceMatrix defines a list that contains 4 functions to be called in 'cacheSolve'
makeCacheMatrix<-function(x=matrix()) { ## define the function & set the default value of argument 'x' as an empty matrix
  m <- NULL ## clears any stored inverse matrix
  set <- function(y) { ## define the 'set' function with argument y
    x <<- y ## set the x matrix in the parent environment to values in y matrix
    m <<- NULL ## clears any stored inverse matrix
    }
  get <- function() x ## gets initial matrix
  setinverse <- function(solve) m <<- solve ## sets inverse matrix 
  getinverse <- function() m ## gets inverse matrix
  list(set = set, get = get, ## assigns names to the items in the list 'makeCacheMatrix' so that they can be reffered to by name & not just position
       setinverse = setinverse,
       getinverse = getinverse)
  }

## cacheSolve either pulls the inverse matrix from cache or calculates it and stores it in cache
cacheSolve <- function(x, ...) { ## define the solve function, set argument x and allow additional arguments from passed item
  m <- x$getinverse() ## call 'getinverse' function to see if there is a stored matrix already
  if(!is.null(m)) { ## if it's not equal to null, aka there IS a stored matrix...
    message("getting cached data")  ## print to the screen this message
    return(m) ## and return that stored matrix
  }
  if(is.null(m)) { ## if the intial call to 'getinverse' returns a null value (aka no stored matrix)
    data <- x$get() ## then call the 'get' function to pass the argument into an object named 'data'
     m <- solve(data, ...) ## calculate the inverse using the solve function & save the inverse to object m
    x$setinverse(m) ## then call 'setinverse' on that object m
     m ## and finally, print that inverse matrix
    }
}
