## By Juan Pedro Vasquez Lopez
## At first glance it seemed ethereal and too abstract (didn't it?)
## ... but finally, thank G-d, I got it!
##
## USAGE EXAMPLE AFTER PROPER RUN (SOURCED):
## set.seed(12) #to get the same result than me despite rnorm
## a <- makeCacheMatrix() #the list is created
## a$set( matrix(rnorm(36),6,6)  ) #some invertible squared matrix
## a$get() #to check the matrix 
## a$getInverted() #displays NULL because it hasn't yet been calculated
## cacheSolve(a) #inverts the matrix (this first time)
## a$getInverted() #to check te inverse matrix
## cacheSolve(a) #as the matrix set remains intact, so displays the inverse matrix cached (instead of calculate it all over again)


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## initialize where the inverse matrix will be stored as 'public' variable m
        set <- function(y) {
                x <<- y ## stores the matrix to be inverted into 'public' variable x
                m <<- NULL ## at the same time it's a flag for changes in the stored matrix 
        }
        
        get <- function() x ## returns the stored matrix
        setInverted <- function(inverted) m <<- inverted ## stores the inverted matrix to 'public' variable m
        getInverted <- function() m ## returns the stored inverted matrix (or NULL if it hasn't yet been calculated)
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted) ## defines the object returned as a list        
}


## Through this function the matrix can be either inverted or its already inverted matrix displayed
cacheSolve <- function(x, ...) { ## Returns a matrix that is the inverse of 'x'
        m <- x$getInverted()
        if(!is.null(m)) { ## If it has been calculated before, so display cached inverted matrix
                message("getting cached data")
                return(m) #returns cached inveted matrix
        }
        ## Else, get it and actually calculate the inverse of matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setInverted(m) ## Stores the inverted matrix in 'public' variable m
        m ## returns inverted matrix
}
