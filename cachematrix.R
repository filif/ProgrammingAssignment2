## (Invertible) matrix caching to save computations


# list of function to:
# set the matrix
# get the matrix
# set the matrix inverse
# get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  setmatrix<-function(solve) m<<- solve
  
  getmatrix<-function() m
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# Return a matrix that is the inverse of 'x'
# If x has been already inverted, fetch the data (saving computation), otherwise compute the invertion
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Fetching cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

