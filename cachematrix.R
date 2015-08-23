makeCacheMatrix <- function(mat=matrix()) {
  i <- NULL
  set <- function(y) {              ## set matrix
    mat <<- y
    i <<- NULL
  }
  get <- function() mat             ## get matrix
  setinv <- function(inv) i <<- inv ## set inverse
  getinv <- function() i            ## get inverse
  list(set = set, get = get,        ## list of function
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(mat, ...) {
  
  i <- mat$getinv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- mat$get()
  
  if(ncol(data)==nrow(data))     ## checking for square matrix
  {
    if(det(data)!=0)             ## checking for determinant of matrix
    {
      i <- solve(data, ...)
    }
    else
    {
      print("Can't perform invers as the determinat of matrix is zero")
      i<-NaN
    }
  }
  else
  {
    print("Can't perform invers as it isn't a square matrix")
    i<-NaN
  }
  mat$setinv(i)
  i
  }
