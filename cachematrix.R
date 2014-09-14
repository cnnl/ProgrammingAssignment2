### The makeCacheMatrix function creates a special object and stores it in an matrix. 
#Futhermore makeCacheMatrix can be seen as an OOP class when x is the object of this function 
#"inv" is an internal variable of the class makeCacheMatrix where the value of this variable consist 
#the object's state matrix and the value of "inv" will change according to the change of the matrix x
###Therefore, makeCacheMatrix creates a special "matrix", which really is a list containing a function to
#get the value of the matrix, set the value of the inverse and get the value of the inverse.



makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL   # "inv" will reverse the x-object is set to null each time the function is called makeCacheMatrix
  
  ### The methods get(), setinverse() e getinverse() are behaviors makeCacheMatrix class that return or 
  #configure any attribute of this class. These functions are used only when the function is called cacheSolve.
  
  get <- function() x                             # This function returns the value of x, in which case the original matrix
  
  setinverse <- function(inverse) inv <<- inverse # This function is called when the first cacheSolve is executed, that is, when "inv" is zero
                                                  # "inv" will store the inverse of matrix and it's reset to NULL every time makeCacheMatrix is called
  
  getinverse <- function() inv                    # This function is called after the execution of cacheSolve and returns the value of the inverse cached
  
  list(get = get,                                 # returns a list that contains all the methods or functions of the matrix object
       setinverse = setinverse,
       getinverse = getinverse) 
}



###The function cacheSolve firstly checks if there is a cached value for the inverse of the object 
#created in makeCacheMatrix, if it exists, it returns the value of the inverse of this object, 
#otherwise it computes the inverse of matrix and stores the inverse on cache.

cacheSolve <- function(x, ...) {      # x is the list that contains the objects with informations of the matrix of makeCacheMatrix got through the methods (e.g.: get())
  
  inv <- x$getinverse()               # accesses the object x and gets the value of the inverse and stores in internal variable "inv"
  
  if(!is.null(inv)) {                 # checks if "inv" is not null, i.e., if there is already a value cached for inv (inverse of the object x)
    message("getting cached data")    # if it exists it sends the message "getting cached data" to the console
    return(inv)                       # returns that value of "inv", which is the inverse value of the last cached
  }
  
  ### if inv is zero, i.e., has no value stored in the cache for the inverse of the object x then the following steps are performed
  
  data <- x$get()         # accesses and gets the data from the x object and stores the variable "data"  
  inv <- solve(data, ...) # computes the inverse of the "data" matrix and stores the variable "inv"
  x$setinverse(inv)       # stores the calculated value for the matrix (see setinverse of makeCacheMatrix) in "inv" variable
  inv                     # When cacheSolve function is executed will return the inverse of the object x (inv)
}
