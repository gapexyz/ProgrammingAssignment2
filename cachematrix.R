## makeCacheMatrix function creates a special object, in our case a matrix that can cache its inverse.  The function creates a list of 4 functions:set,get,setINV,getINV. Set sets an original matrix and setINV sets the inverted matrix, while get enables to call the original matrix from the function and getINV does the same for the inverted matrix.#



makeCacheMatrix <- function(x = matrix()) {
	    inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x
        setINV<-function(solve)inv<<-solve
        getINV<-function()inv
        list(set=set,get=get,setINV=setINV,getINV=getINV)

}


## cacheSolve the first part of the function checks if there is already 'stored' inverted matrix, if there is such object then inverted matrix is called from the cache. If the matrix inverse has not been yet calculated, the inversion will be executed in the next phase.#

cacheSolve <- function(x, ...) {
	inv<-x$getINV()
     if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
         data<-x$get()
         inv<-solve(data,...)
         x$setINV(inv)
         inv
        
}
