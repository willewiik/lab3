#' Euclidian algorithm 
#' 
#' Calculates the greatest common divisor between two integers.
#' @param a A integer.
#' @param b A integer.
#' 
#' @details
#' Further information on the euclidian algorithm can be found at 
#' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Wikipedia}	
#' @return Returns a integer of the greatest common divisor.
#' @export

euclidean <- function(a, b){
  if(!is.vector(a))  stop("first argument needs to be an integer!")
  if(length(a) != 1) stop("first argument needs to be an integer")
  if(!a == round(a)) stop("first argument is not an integer")
  if(!is.vector(b))  stop("second argument needs to be an integer!")
  if(length(b) != 1) stop("second argument needs to be an integer")
  if(!b == round(b)) stop("second argument is not an integer")
  
  a <- abs(a)
  b <- abs(b)
  
  greater_num <- max(a, b)
  lesser_num <- min(a, b)
  
  while(lesser_num != 0) {
    remainder <- greater_num %% lesser_num
    greater_num <- lesser_num
    lesser_num <- remainder
  }
  return(greater_num)
}







