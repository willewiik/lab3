\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    if (!is.data.frame(graph)) 
        stop("first argument needs to be a data frame")
    if (!ncol(graph) == 3) 
        stop("the data frame needs to have 3 columns")
    if (colnames(graph)[1] != "v1") 
        stop("the name of first column has to be v1")
    if (colnames(graph)[2] != "v2") 
        stop("the name of second column has to be v2")
    if (colnames(graph)[3] != "w") 
        stop("the name of third column has to be w")
    if (!is.vector(init_node)) 
        stop("second argument needs to be a scalar")
    if (!length(init_node) == 1) 
        stop("second argument needs to be a scalar")
    if (!is.numeric(init_node)) 
        stop("second argument needs to be a scalar")
    if (!any(graph$v1 == init_node)) 
        stop("initial node does not exist in graph")
    if (any(graph$w < 0)) 
        stop("weights needs to be larger than 0")
    result <- data.frame(distance = rep(Inf, length(unique(graph$v1))), 
        node = unique(graph$v1))
    current_node <- init_node
    unvisited <- unique(graph$v1)
    unvisited <- unvisited[-which(unvisited == current_node)]
    result$distance[which(result$node == current_node)] <- 0
    temp_data <- graph[graph$v1 == current_node, ]
    possible_nodes <- temp_data$v2
    for (i in 1:length(possible_nodes)) {
        result$distance[result$node == possible_nodes[i]] <- temp_data$w[i]
    }
    while (length(unvisited) > 1) {
        min_dist_next_node <- min(result$distance[result$node \%in\% 
            unvisited])
        add_dist <- min_dist_next_node
        current_node <- result$node[which(result$distance == 
            min_dist_next_node)[1]]
        temp_data <- graph[graph$v1 == current_node, ]
        temp_data$w <- temp_data$w + add_dist
        possible_nodes <- temp_data$v2
        for (i in 1:length(possible_nodes)) {
            if (result$distance[result$node == possible_nodes[i]] > 
                temp_data$w[i]) {
                result$distance[result$node == possible_nodes[i]] <- temp_data$w[i]
            }
        }
        unvisited <- unvisited[-which(unvisited == current_node)]
    }
    return_dist_vector <- result$distance
    if (is.vector(return_dist_vector)) {
        return(return_dist_vector)
    }
    else {
        return("the result from function is not a vector")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
