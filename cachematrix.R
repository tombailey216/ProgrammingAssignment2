## This function allows you to create a special 'matrix' and get the cached inverse if it has already been solved.  

makeCacheMatrix <- function(var1 = matrix()) {
  Inver <- NULL
  set <- function(var2) {
    var1 <<- var2
    Inver <<- NULL
  }
  get <- function() var1
  setInver <- function(inverse) Inver <<- inverse
  getInver <- function() Inver
  list(set = set,
       get = get,
       setInverse = setInver,
       getInverse = getInver)
}

## This function calculates the invers of the special matrix above and allows you to cache the answer
## so that you do not need to rerun the function saving you time & power. If the inverse has already been cached
## and the matrix is the same then a message will appear "retrieving cached data".

cacheSolve <- function(var1, ...) {
  ## Return a matrix that is the inverse of variable 1
  Inver <- var1$getInverse()
  if (!is.null(Inver)) {
    message("retrieving cached data")
    return(Inver)
  }
  Matx <- var1$get()
  Inver <- solve(Matx, ...)
  var1$setInverse(Inver)
  Inver
}

## you can test this by applying the following

set.seed(10000)
r = rnorm(10000)

Assignment2 <- makeCacheMatrix(matrix(r,nrow=100,ncol=100))
Assignment2$get()