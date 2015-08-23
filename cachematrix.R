#######################################################
## Programming Assignment 2 for R Prog Course ID 031  #
## ####################################################

## This Module will enable User to cache the Inverse 
#  Value of a Matrix which can be inverted

## Assumptions : The matrix that is entered is always 
#                invertible


## Function Name : makeCacheMatrix
## Input         : Matrix that is assumed to be always invertible
## Functionality : This Function will take a matrix argument and will 
#                  store it in cache along with its inverse

makeCacheMatrix <- function(matrix_data = matrix())
{
    ## Initialize Matrix Inverse as NULL value
    matrix_inverse_data <- NULL

    ## Nested Function within function with local scope here
    #  The function will set he matrix data to the matrix argument data passed in
    #  Also whenever the matrix data is reset( as in here), the Matrix Inverse value is reset
    #  to NULL
    set_matrix_data <- function(arg)
    {
        matrix_data <<- arg
        matrix_inverse_data <<- NULL
    }

    ## Nested Function within function with local scope here
    #  The function will get matrix elements data 
    get_matrix_data <- function()
    {
        matrix_data
    }

    ## Nested Function within function with local scope here
    #  The function will set matrix inverse value to the argument passed in 
    set_matrix_inverse_value <- function(inv_val)
    {
        matrix_inverse_data <<- inv_val
    }

    ## Nested Function within function with local scope here
    #  The function will return the matrix inverse value
    get_matrix_inverse_value <- function()
    {
        matrix_inverse_data
    }

    ## Create a list which basically acts as list of functions. The result returned is a 
    #  list variable which can be used to access the nested functions with scope within this function
    list(set = set_matrix_data, get = get_matrix_data, 
         set_inv = set_matrix_inverse_value,
         get_inv = get_matrix_inverse_value)
}


## Function Name : cacheSolve
## Input         : Matrix that is assumed to be always invertible
## Input         : Added inputs for future improvements 
## Functionality : This Function will take a matrix argument and will 
#                  return the inverse of the matrix. The inverse will be 
#                  retrieved from cache if already calculated once, else
#                  will be computed and stored in cache for next call

cacheSolve <- function(x, ...)
{
    ## Get the value of Matrix Inverse from Cache
    matrix_inv_res <- x$get_inv()

    ## Check the value of inverse against NULL
    if (is.null(matrix_inv_res))
    {
        ## If the Inverse Value is NULL, then recalculate the inverse
        message("Computing the Inverse for the first time ")
        message("Might Take a while... ")
        matrix_inv_res <- solve(x$get())
        
        ## Set the inverse value in the local cache
        x$set_inv(matrix_inv_res)
        matrix_inv_res
    }
    else
    {
        ## If the Inverse value is not NULL, then the value is present in cache
        ## Return the Cached Value
        message("Inverse of Matrix already in Cache ")
        matrix_inv_res        
    }
}
