#' Example data for dijkstra's function
#'
#' A dataset containing the weight between two nodes
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{starting node}
#'   \item{v2}{end node}
#'   \item{w}{weight between starting and end node}
#' }
#' 
#' @details
#' Data set is used for dijkstra's function, more information about dijkstra's algorithm 
#' can be found at: 
#' \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Wikipedia}	 


wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
