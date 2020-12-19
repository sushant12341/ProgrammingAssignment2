## Simple program consist of 2 functions that find the Inverse of a matrix 
# Store the value in cache and retrieve in case the inverse has been calculated.
# it has this special '<<-' super assignment operator used in Current enclosure 
# Purpose of this operators is that the make changes at both current enclosure 
# and parent enclosure.


makeCacheMatrix <- function(x=matrix()){
  m<<-NULL
  set<-function(y=matrix()){
    x<<-y                   #In case you have a new matrix to be computed
    m<<-NULL                # Set the value of x to the current matrix 
  }
  get <- function()x
  setinv <- function(inv)m<<-inv  # Assign the value of inv to the m 
  getinv <- function() m
  
  list('getinv'=getinv,'setinv'=setinv,'get'=get,'set'=set)
}


## This Function will utilize the Cache Function for the  Inverse Data if already
## stored and if Inverse is null then it will utilize the functions in the above 
## Parent function 

cacheSolve <- function(x,...){
  m<-x$getinv()      ## check if the inverse data already present 
  if(!is.null(m)){          
    print('Getting Cached Data')
    return(m)
    
  }
  data<-solve(x$get()) ## in case not , then will compute for the inverse
  x$setinv(data)
}



## Do not Forget to Check if the computed Inverse is Correct or Not 
## Matrix %*% Inv(Matrix) should give a unit matrix 
