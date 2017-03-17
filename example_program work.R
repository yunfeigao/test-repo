## The function is to demostrate the use of Cache in Matrix
## There are two functions in play here 
## makeCacheMatrix - This creates the cache list object for the matrix 



## The first function makeCacheMatrix creates a list more like an object that has 4 functions 
## 1. Set - This function rests the cache and also sets the value of matrix
## 2. Get - This function reads the value of the matrix 
## 3. setcacheinv - This functions sets the cache value with the invese matrix
## 4. getcacheinv - This functions get the cache value (stored invese matrix)



makeCacheMatrix <- function(x = matrix()) {
  mcache <- NULL
  set <- function(y) {
    x <<- y
    mcache <<- NULL
  }
  get <- function() x
  setcacheinv <- function(m_inverse) mcache <<- m_inverse
  getcacheinv <- function() mcache
  list(set = set, get = get,
       setcacheinv = setcacheinv,
       getcacheinv = getcacheinv)
}


## The function cacheSolve checks if the results is already cached and return accordingly. 
## This first checkes if the cache is not empty and if not empty returns the one in cache and if it is not 
## the sets the cache and returns the same.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

# Testting
# created two matrix Var1 and h8 and got the cached data when calling the function twice.
# Also demostrated that the cache is returned correctly for each variable
# > var1
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > 
# > var2 <- makeCacheMatrix(var1)
# > cacheSolve(var2)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
#   
#   > h8
# [,1]      [,2]      [,3]       [,4]       [,5]       [,6]       [,7]       [,8]
# [1,] 1.0000000 0.5000000 0.3333333 0.25000000 0.20000000 0.16666667 0.14285714 0.12500000
# [2,] 0.5000000 0.3333333 0.2500000 0.20000000 0.16666667 0.14285714 0.12500000 0.11111111
# [3,] 0.3333333 0.2500000 0.2000000 0.16666667 0.14285714 0.12500000 0.11111111 0.10000000
# [4,] 0.2500000 0.2000000 0.1666667 0.14285714 0.12500000 0.11111111 0.10000000 0.09090909
# [5,] 0.2000000 0.1666667 0.1428571 0.12500000 0.11111111 0.10000000 0.09090909 0.08333333
# [6,] 0.1666667 0.1428571 0.1250000 0.11111111 0.10000000 0.09090909 0.08333333 0.07692308
# [7,] 0.1428571 0.1250000 0.1111111 0.10000000 0.09090909 0.08333333 0.07692308 0.07142857
# [8,] 0.1250000 0.1111111 0.1000000 0.09090909 0.08333333 0.07692308 0.07142857 0.06666667
# > var3 <- makeCacheMatrix(h8)
# > cacheSolve(var3)
# [,1]      [,2]       [,3]       [,4]        [,5]        [,6]        [,7]       [,8]
# [1,]      64     -2016      20160     -92400      221760     -288288      192192     -51480
# [2,]   -2016     84672    -952560    4656960   -11642400    15567552   -10594584    2882880
# [3,]   20160   -952560   11430720  -58212000   149688000  -204324119   141261119  -38918880
# [4,]  -92400   4656960  -58212000  304919999  -800414996  1109908794  -776936155  216215998
# [5,]  221760 -11642400  149688000 -800414996  2134439987 -2996753738  2118916783 -594593995
# [6,] -288288  15567552 -204324119 1109908793 -2996753738  4249941661 -3030050996  856215352
# [7,]  192192 -10594584  141261119 -776936154  2118916782 -3030050996  2175421226 -618377753
# [8,]  -51480   2882880  -38918880  216215998  -594593995   856215351  -618377753  176679358
# > cacheSolve(var3)
# getting cached data
# [,1]      [,2]       [,3]       [,4]        [,5]        [,6]        [,7]       [,8]
# [1,]      64     -2016      20160     -92400      221760     -288288      192192     -51480
# [2,]   -2016     84672    -952560    4656960   -11642400    15567552   -10594584    2882880
# [3,]   20160   -952560   11430720  -58212000   149688000  -204324119   141261119  -38918880
# [4,]  -92400   4656960  -58212000  304919999  -800414996  1109908794  -776936155  216215998
# [5,]  221760 -11642400  149688000 -800414996  2134439987 -2996753738  2118916783 -594593995
# [6,] -288288  15567552 -204324119 1109908793 -2996753738  4249941661 -3030050996  856215352
# [7,]  192192 -10594584  141261119 -776936154  2118916782 -3030050996  2175421226 -618377753
# [8,]  -51480   2882880  -38918880  216215998  -594593995   856215351  -618377753  176679358
# > cacheSolve(var2)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5