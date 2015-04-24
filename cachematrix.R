## The goal of these functions if to find the inverse of a matrix as quickly as 
## possible. So, first we store matrices and possibly inverses in makeCacheMatrix
## Then to find the inverse of a stored matrix we call cacheSolve. The function 
## cacheSolve first checks to see if makeCacheMatrix had already computed the inverse
## of the stored matrix. If makeCacheMatrix has stored the inverse then cacheSolve
## doesn't need to recompute the inverse. It simply grabs the inverse from 
## makeCacheMatrix and returns it. If makeCacheMatrix does not have the inverse 
##stored, then cacheSolve solves for the inverse.

## This function creates a list of functions that allows you to define two 
## variables. First, it allows you to set x equal to a matrix. When you do this 
##(by calling the function set) this function sets the other variable inv equal  
## to Null. In essences when you set x equal to a matrix you reset inv to NULL.
## You can then set inv to anything you want using setinv. In particular, you 
## can apply setinv to solve(x) to set inv equal to the inverse of x
## Finally, get and getinv simply return the values of the variables x and inv


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinv <- function(solve)inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function finds the inverse of a matrix (so long as you tell it to do 
##that). Here's what happens when you call cacheSolve(makeCacheMatrix(mat)) where
## mat is a matrix. If the variable inv in makeCacheMatrix is not equal to NULL 
##then this function returns the value of inv as it is assigned in the
## makeCacheMatrix function. If the variable inv equals NULL in the 
## makeCacheMatrix then this function sets
## inv equal to the inverse of the matrix mat and then also applies inv to the 
## function setinv in the list of functions in makeCacheMatrix. It then returns 
## the value of inv.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
