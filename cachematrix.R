# makeCacheMatrix creates a special matrix object that can compute its inverse.
# The inverse is computed and returned each time it is requested.

makeCacheMatrix <- function(x = matrix(0, nrow = 3, ncol = 3)) {
  
  # Function to fill the matrix with random values (1 to 10) and reshape it into a 3x3 matrix
  fillSample <- function() {
    # Generate a random sample of 9 numbers from 1 to 10 with replacement
    sampleVector <- sample(1:10, 9, replace = TRUE)
    
    # Create a new matrix with the sampled values (3x3 matrix)
    x <<- matrix(sampleVector, nrow = 3, ncol = 3)
  }
  
  # Function to compute and return the inverse of the matrix
  getInverted <- function() {
    # Compute and return the inverse of the matrix
    return(solve(x))
  }
  
  # Return a list of functions to interact with the matrix object
  return(
    list(
      original = function() x,          # Return the original matrix
      fillSample = fillSample,          # Function to fill the matrix with random values
      getInverted = getInverted        # Function to get the inverse of the matrix
    )
  )
}

# Example usage:
cm <- makeCacheMatrix()   # Create a cached matrix object
cm$fillSample()           # Fill the matrix with random values
print(cm$original())      # Print the original matrix
print(cm$getInverted())   # Get and print the inverse of the matrix

cm<-makeCacheMatrix()
cm$fillSample()
cm$original()
cm$solve()

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
