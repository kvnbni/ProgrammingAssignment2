## makeCacheMatrix does 4 steps. It sets the matrix,gets the matrix, set the value of inverse and gets 
##the value of inverse. The caacheSolve() calculates the inverse of the matrix. First it checks if it was
##already computed before, if that's the case then that value is returned and no computation is done else 
##it computes the inverse of the matrix and sets the value of the inverse in the cache via the setinv 
##method.

## makeCacheMatrix for setting the matrix according to the arguments coming in and outputs the matrix when
## we use the get() function. Once the inverse is solved by the second function we set it via the setinv 
##method and using the getinv() method we print it out

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinv<-function(inv) inverse<<-inv
  getinv<-function() inverse
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## When a new matrix value comes in it checks if the computation has already been done else it does the 
##computation and stores the value in the cache using the setinv method and this can be seen when we try 
##to compute the same matrix twice. During the second time the inverse is retreived from the cache

cacheSolve <- function(x, ...) {
        inverse<-x$getinv()
        if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...)
        x$setinv(inverse)
        inverse
}
