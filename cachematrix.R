makeCacheMatrix <- function(x = matrix()) {   ## Create a function called "makeCacheMatrix", which takes 
                                              ## one argument and coerces it to be a matrix
  m <- NULL                             ## Initialize the variable "m" as a NULL
  set <- function(y) {                  ## Create a sub-function called "set", which takes an argument "y"
    x <<- y                       ## Part of the "set" sub-function; sets "x" to "y" within the function 
                                  ## "makeCacheMatrix", not just the sub-function
    m <<- NULL                    ## Also part of "set"; initializes "m". We're telling the function that 
                                  ## we've made a new matrix, likely with a different "m" (inverted matrix)
  }
  get <- function() x                   ## Create a sub-function called "get" reporting the value of x. The 
                                        ## "x" comes after the "function()" because this way, the function 
					## takes no arguments, it just prints x
  setim <- function(solve) m <<- solve  ## Create a sub-function called "setim", which works mostly through 
                                        ## its interaction with the cacheSolve function, by applying solve() 
					## to the vector data before using setim to assign a value to m. It 
					## then assigns the result of the solve function to "m". 
  getim <- function() m                 ## Create a sub-function called "getim", which reports the value of "m"
  list(set = set, get = get,            ## Create the list itself. For each element of the list, the part 
                                        ## before the "=" indicates the name or key to be used to call that item, 
       setim = setim,                   ## and the part after identifies the item (in this case, the sub-function) 
       getim = getim)                   ## that the name or key will call.
}

cacheSolve <- function(x, ...) {        ## Calcalute the inverse of the matrix created above
  m <- x$getim()                        ## Check if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                           ## If so, get the inverse from cache
  }
  data <- x$get()                       ## Otherwise, calculate the inverse
  m <- solve(data, ...)
  x$setim(m)                            ## Set the value of the inverse in cache via setim
  m
}

