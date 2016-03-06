## Caching the Inverse of a Matrix , if it already exits retreive it from cache

##This function creates a special "matrix" object that can cache its inver
makeCacheMatrix <- function(x = matrix()) {
        
        im <- matrix()
        ## declare a new function set to set the value of x 
        set <- function(y){
             x <<- y
             ## initialize the inverse matrix inside the function set
             im <<- matrix()
        }
        
       ## declare function 'get' to return the value of the matrix x to the calling fucntion
       get <- function() x
        
       ## setmatrix assigns the vlaue slv(inverse of x) to im, passed to it by the calling function
       setmatrix <- function(slv)       im <<- slv 
                 
       ## getmatrix function returns the inverse matrix to the calling function
       getmatrix <- function() im
       
       ## return value of the makeCacheMatrix function 
       list(set = set, get = get,
                      setmatrix = setmatrix,
                      getmatrix = getmatrix)
}





## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x= matrix(),  ...) {
  
  ## if matrix is a sqaure matrix proceed with the inversion
  if(nrow(x)==ncol(x)){
     
     cm <- makeCacheMatrix(x)
    ## call the getmatrix function assign the value of the inverse matrix to im using function getmatrix()
       im <- cm$getmatrix()
    
    ## if the value of im is not NA and not null retrun im 
          im_na <- is.na(im)
        if(sum(im_na)==0 && !is.null(im)) {
            message("Getting cached data")
             return(im)
          }
       ## if inverse does not exist then it is calculated using solve
       data <- cm$get()
       im <- solve(data)
       cm$setmatrix(im)
       im
   }
  else {
    message("Please give a sqaure matrix to find the inverse")
 }
  
}
