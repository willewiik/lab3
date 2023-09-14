#' Dijkstras algorithm
#' 
#' Calculates the shortest distance from initial node to all other nodes using dijkstra's algorithm.
#' @param graph Data frame consisting of starting locations, end locations and weights.
#' @param init_node Initial starting node.
#' 
#' @details
#' Further information on the dijkstras algorithm can be found at Wikipedia 
#' (https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).
#' @return Returns a vector with shortest distance from starting node to all other nodes.
#' @export
 
dijkstra <-function(graph, init_node){
  if(!is.data.frame(graph))      stop("first argument needs to be a data frame")
  if(!ncol(graph) == 3)          stop("the data frame needs to have 3 columns")
  if(colnames(graph)[1] != "v1") stop("the name of first column has to be v1")
  if(colnames(graph)[2] != "v2") stop("the name of second column has to be v2")
  if(colnames(graph)[3] != "w")  stop("the name of third column has to be w")
  if(!is.vector(init_node))      stop("second argument needs to be a scalar")
  if(!length(init_node) == 1)    stop("second argument needs to be a scalar")
  if(!is.numeric(init_node))     stop("second argument needs to be a scalar")
  if(!any(graph$v1==init_node))  stop("initial node does not exist in graph")
  if(any(graph$w < 0))           stop("weights needs to be larger than 0")
  
  # Assumption: 
  # Places are not ordered: can be 1,3,2
  # Places can be 1,3,5 i.e ot an increment of 1. 
  # There is no graph from places to itself (3 -> 3)
  # Assume there is no "dead" nodes, there is no node that can only be visited but not travelled from. 
  
  # Create result data frame with distance and name of place
  result <- data.frame(distance = rep(Inf, length(unique(graph$v1))),
                       node = unique(graph$v1))
  
  # Define the current node the function is at
  current_node <- init_node
  
  # Define all nodes that has not been visited
  unvisited <- unique(graph$v1)
  unvisited <- unvisited[-which(unvisited == current_node)]
  
  # Starting node to itself it set to 0
  result$distance[which(result$node == current_node)] <- 0

  
  # Distance to all directly reachable nodes from starting location added to result
  temp_data <- graph[graph$v1 == current_node, ]
  possible_nodes <- temp_data$v2 # Possible nodes to visit from current node
  for(i in 1:length(possible_nodes)){
    result$distance[result$node == possible_nodes[i]] <- temp_data$w[i]
  }
  
  # Calculate the shortest distance from initial node to all other nodes
  while(length(unvisited) > 1){
    # The shortest distance from the initial node to the unvisited nodes
    min_dist_next_node <- min(result$distance[result$node %in% unvisited])
    add_dist <- min_dist_next_node
    
    # [1] is needed since several nodes can have same shortest distance
    current_node <- result$node[which(result$distance == min_dist_next_node)[1]]
    
    temp_data <- graph[graph$v1 == current_node, ]
    temp_data$w <- temp_data$w + add_dist 
    
    possible_nodes <- temp_data$v2 # Possible places to visit from current node
    for(i in 1:length(possible_nodes)){
      if(result$distance[result$node == possible_nodes[i]] > temp_data$w[i]){
        result$distance[result$node == possible_nodes[i]] <- temp_data$w[i]
      }
    }
    unvisited <- unvisited[-which(unvisited == current_node)]
  }
  
  return_dist_vector <- result$distance 
  if(is.vector(return_dist_vector)){
    return(return_dist_vector)
  } else{
    return("the result from function is not a vector")
  }
}



