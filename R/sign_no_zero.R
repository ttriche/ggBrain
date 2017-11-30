#' Get binary sign of a variable, or array
#'
#' @param x a vector or array
#' @export
#' @seealso \code{\link{sign}}
#' @return a vector or array telling if each element of x is greater than or equal to zero. This array will be of the same dimension as \code{x}, but with elements -1 for negative values of \code{x}, and elements +1 for positive values of \code{x}
#' @examples
#' sign_no_zero(-2:2) #returns -1 -1  1  1  1 
sign_no_zero<-function(x){
	x[x>=0]<-  1
	x[x< 0]<- -1
	return(x)
}

