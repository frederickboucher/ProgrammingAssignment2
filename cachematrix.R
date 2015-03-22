# makeCacheMatrix module
# create a special matrix that can cache results of time consuming calculations
# To save computing time, the calculation is done only if the matrix has changed


# makeCacheMatrix function
# 
# Holds a matrix and cache results of operations on that matrix
#
makeCacheMatrix <- function(x = matrix())
{	 
	#
	# internal cached matrix inverse value
	#
    	inverseOfMatrix <- NULL	
		

	#
	# Function to set the matrix
	#
    	set <- function(y) 
	{				
		#
		# Verify if new matrix has differents sizes or differents values
		#	
		if(dim(x) != dim(y) || !all(x == y))
		{
			#
			# Matrix has changed, reset our cached inverse value
			#
			inverseOfMatrix <<- NULL				
		}

		#
		# Set the matrix
		#	
	 	x <<- y	
    	}

	
	#
	# Function to retrieve the matrix
	#
    	get <- function()
	{	
		x
	}
	
	
	#
	# Function to set the inverse of the matrix
	#
    	setinverse <- function(inverse)
	{	

		#
		# Cache the inverse value
		#
		inverseOfMatrix <<- inverse	
		
	}

	
	#
	# Function to retrieve the inverse of the matrix
	#
    	getinverse <- function()
	{			
		inverseOfMatrix	
	}
	
	
  
	#
	# Returns the list of functions accessible externally
	#
    	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
			
}


#
# cacheSolve function
# 
# Calculate the inverse of the special matrix: "makeCacheMatrix"
#
cacheSolve <- function(x, ...) 
{
	#
	# Get the inverse
	#
	inv <- x$getinverse()
	
	#
	# Verify if inverse has already been calculated
	#
	if(!is.null(inv))
	{
		#
		# message to user
		#
		message("getting cached data")
		
		#
		# Return the cached inverse
		#
		return(inv)
	}

	#
	# Retrieve the matrix
	#
	data <- x$get()
	
	#
	# Calculate the inverse
	#
	inverseOfM <- solve(data, ...)

	#
	# message to user
	#
	message("getting calculated data")
    
	#
	# this call will refresh the cached inverse value,	
	# and return the inverse
	#
	x$setinverse(inverseOfM)    
	
}
