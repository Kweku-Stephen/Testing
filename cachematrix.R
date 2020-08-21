## Put comments here that give an overall description of what your
## functions do
## 
## The makeCacheMatrix() function below returns series of functions which work on a square matrix
## The first line of code in the body of the function creates an empty object called m 
## The second function within the body caches the value of a free variable y
## (which I have defined on top of the function) to the empty matrix called x(x is the input variable). 
## it does same for the empty value m (caches m)
##  The subsequent functions within the body do the ff: 
##          get() gets the cached x value,
##          setinerse() inverts x and caches the result to the empty variable m,
##          getinvers() retrieves the value of cached x inverse
##  
##  The function (makeCacheMatrix) basically outputs a list of functions
##  which sets and caches the square matrix
##  which gets the cached matrix(not-inverted)
##  which sets and caches the inverse of the matrix
##  which gets the caches inverse of the matrix
##  

## Write a short comment describing this function

y <- matrix(1:16, nrow = 4, ncol = 4, byrow = F)

makeCacheMatrix <- function(x = matrix(NA, nrow = 4, ncol = 4)) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {get("x", environment(set))}
   
     a <- solve(x)
    setinverse <- function(a) {m <<- a}
    getinverse <- function() {get("m",environment(setinverse))}
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}



## Write a short comment describing this function
## The function makeCacheMatrix() attempts to compute the inverse of x
## it first checks to see if the inverse is already computed in the if statement and if not
## it retrieves the cached x value and computes the inverse using the solve function
## and then caches the inverse using setinverse()

#a <- makeCacheMatrix()
cacheSolve <- function(x,...){
    m <- getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    } #else {
    
    data <- get()
    m <- solve(data)
    setinverse(m)
    m
}



#cacheSolve <- function(x, ...) {
#        ## Return a matrix that is the inverse of 'x'
#}
