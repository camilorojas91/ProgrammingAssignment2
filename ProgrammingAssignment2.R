# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize an empty list to store the cache
  cache <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    # Clear the cache whenever the matrix is updated
    cache <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) cache <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() cache
  
  # Return a list containing the functions set, get, setInverse, and getInverse
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}

# Function to compute the inverse of the matrix, with caching mechanism
cacheSolve <- function(x) {
  # Check if the cached inverse exists and return it
  if (!is.null(x$getInverse())) {
    message("Getting cached inverse.")
    return(x$getInverse())
  }
  
  # If the cached inverse does not exist, compute the inverse and cache it
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  return(inverse)
}

# Example usage:
# Create a matrix using makeCacheMatrix function
m <- matrix(c(2, 4, 1, 3), nrow = 2)

# Create a cache matrix object
cacheMatrix <- makeCacheMatrix(m)

# Calculate and cache the inverse of the matrix
inverse_matrix <- cacheSolve(cacheMatrix)

# If the inverse is requested again, it will be fetched from the cache
cached_inverse <- cacheSolve(cacheMatrix)
