#MakeCacheMatrix is a function that creates a matrix object capable of storing its inverse
#cacheSolve is a function that accepts a MakeCacheMatrix object, it then checks for a solution, solves and stores the soution inside of the object

makeCacheMatrix <- function(m = matrix()){
  iM <- NULL

  get <- function(){
       m               #returns matrix
  }

  set <- function(new = matrix()){
        m <<- new      #stores matrix outside of work environment
        iM <<- NULL    #overwrites any stored solution
  }

  iset <- function(newiM){
        iM <<- newiM   #stores inverted matrix
  }

  iget <- function(){
        iM             #returns inverted matrix
  }

  list(set = set, get = get, iset = iset, iget = iget)

}

cacheSolve <- function(x, ...){ #x should be an object created by makeCacheMatrix
      iM <- x$iget() #s

      if(!is.null(iM)) {        #if iM not empty 
        message("caching saved you time")
        return(iM)
      }
#else
      temp <- x$get()           #get the original matrix
      iM <- solve(temp, ...)    #solve and store, passing additional arguments
      x$iset(iM)                #stores solution using makeCacheMatrix iset
#return
      iM

}