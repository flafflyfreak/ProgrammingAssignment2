## My first function creates a matrix object that can cache its inverse (makeCacheMatrix) and the second one computes the inverse of the matrix created (cacheSolve), but it retrieves the solution from the cache if the matrix hasn't changed.

## The following function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	my_matrix <- NULL
	set <- function (y) {
		x<<-y
		my_matrix<<-NULL
	}
	get<-function()x
	setinv<-function(solve) my_matrix<<- solve
	getinv<-function() my_matrix
	list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## The function below computes the inverse of the special matrix returned by the previous function (makeCacheMatrix). If the inverse has already been calculated and the matrix has not changed), then the function retrieves the solution from the cache.

cacheSolve <- function(x, ...) {
        my_matrix <- x$getinv()
        if(!is.null(my_matrix)) {
                message("getting cached data")
                return(my_matrix)
        }
        data <- x$get()
        my_matrix <- solve(data, ...)
        x$setinv(my_matrix)
        my_matrix
}