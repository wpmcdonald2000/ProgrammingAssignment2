# makeCacheMatrix function creates a matrix from input ...
# creates a list of functions to be called by cacheSolve function ...
# and sets inverse as NULL or caches the results of cacheSolve matrix inversion
#  

makeCacheMatrix <- function(m = matrix()) {
        # initialize m_inverse as NULL ... the inverse matrix
        m_inverse <- NULL
        
        # initialize the matrix
        setm <- function(y) {
                m <<- y
                m_inverse <<- NULL
        }
        
        #functions to retrieve created matrix, set values for the inverse
        getm <- function() m
        setinv <- function(inv) m_inverse <<- inv
        getinv <- function() m_inverse
        list(set = setm, getm = getm, setinv = setinv, getinv = getinv)
}

# cacheSolve function takes matrix as input, 
# checks for existing inverse or calculates inverse
# returns inverse

cacheSolve <- function(m, ...) {
        # retrieve the value of getinv in makeCacheMatirx function
        m_inverse <- m$getinv()

        #  Check if inverse matrix has been calculated
        if(!is.null(m_inverse)) {
                # if yes,  return that result
                message("been there, done that, here's what I've got")
                return(m_inverse)
        }
        
        # if not, get matrix from makeCacheMatrix function ...    
        data <- m$getm()
        
        # and calculate the inverse
        m_inverse <- solve(data, ...)
        
        # set the inverse value in makeCacheMarix function
        m$setinv(m_inverse)
        
        m_inverse
}