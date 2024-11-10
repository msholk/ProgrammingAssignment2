# It allows for caching the inverse if the matrix has not changed since the last computation.

makeCacheMatrix <- function(x = matrix(0, nrow = 3, ncol = 3)) {
  invertedMatrix <- NULL              # Initialize the variable to store the inverse of the matrix
  usedToSolveInverted <- NULL         # Initialize the variable to store the matrix used to calc toe inverse
  
  # Function to fill the matrix with random values (1 to 10) and reshape it into a 3x3 matrix
  fillSample <- function() {
    # Generate a random sample of 9 numbers from 1 to 10 with replacement
    sampleVector <- sample(1:10, 9, replace = TRUE)
    
    # Create a new matrix with the sampled values (3x3 matrix)
    x <<- matrix(sampleVector, nrow = 3, ncol = 3)
    usedToSolveInverted <<- NULL
  }
  
  # Function to compute and return the inverse of the matrix
  calcInverted <- function() {
    # Calculate the inverse of the matrix using solve() and return it
    invertedMatrix <<- solve(x)
    usedToSolveInverted <<- x
    return(invertedMatrix)
  }
  
  # Function to get the cached inverted matrix
  getInverted <- function() {
    # Return the cached inverted matrix
    return(invertedMatrix)
  }
  
  # Function to check if the matrix has not changed since it was created
  hasNotChanged <- function() {
    # Check if the current matrix is identical to the original matrix
    return(identical(x, usedToSolveInverted))
  }
  
  # Function to check if the inverse has already been computed
  hasInverted <- function() {
    # Return TRUE if the inverse has been computed (not NULL)
    return(!is.null(invertedMatrix))
  }
  
  # Return a list of functions to interact with the matrix object
  return(
    list(
      getOriginal = function() x,          # Function to get the original matrix
      fillSample = fillSample,             # Function to fill the matrix with random values
      getInverted = getInverted,           # Function to get the cached inverse of the matrix
      calcInverted = calcInverted,         # Function to calculate and cache the inverse
      hasNotChanged = hasNotChanged,       # Function to check if the matrix has not changed
      hasInverted = hasInverted           # Function to check if the inverse has been computed
    )
  )
}

# Example usage:
cm <- makeCacheMatrix()      # Create the matrix object

cm$fillSample()              # Fill the matrix with random values
print(cm$getOriginal())      # Print the original matrix

# Call cacheSolve to either get the cached inverse or calculate it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(x$hasNotChanged() && x$hasInverted()) {
    print("Getting cached matrix")      # Print message to indicate the cached inverse is being used
    return(x$getInverted())            # Return the cached inverse
  }
  
  # If the inverse is not cached, compute and store the inverse
  print("Caching inverted matrix")      # Print message to indicate the matrix inverse is being cached
  return(x$calcInverted())              # Compute and return the inverse
}

# Usage of cacheSolve function
print(cacheSolve(cm))              # Get the inverse, will be computed and cached
print(cacheSolve(cm))              # Get the inverse again, will retrieve cached version

# Modify the matrix and call cacheSolve again
cm$fillSample()                   # Change the matrix by calling fillSample
print(cacheSolve(cm))              # Inverse will be recalculated and cached for the new matrix