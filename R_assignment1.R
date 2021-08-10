## Creates a special matrix object that can cache its inverse

makeMatrix <- function(x=matrix()){
        inverse <- NULL        ## Initialize the inverse property
        set <- function(matrix){       ##how to set the matrix
                x <<- matrix
                inverse <<- NULL
        }
        get <- function(){             ##how to get the matrix
                x
        }
        setinverse <- function(inverse){      ##how to set the inverse of the matrix
                inverse<<-inverse
        }
        getinverse <- function(){        ##how to get the inverse of the matrix
                inverse
        }
        list(set=set, get=get,        ##return a list of the methods to set/get matrix 
             setinverse=setinverse,
             getinverse=getinverse)
        
}
cacheSolve <- function(m,...){
        x <- m$getinverse()
        if(!is.null(x)){
                message("getting cached data")
                return(x)
        }
        data <- m$get()         ##get the matrix from the object
        x <- solve(data)%*% data     ##calculate the inverse using matrix multiplication
        m$setinverse(x)             ##set the inverse to the object
        x                                    ##return the matrix

}
