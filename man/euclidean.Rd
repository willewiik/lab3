\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function (a, b) 
{
    if (!is.vector(a)) 
        stop("first argument needs to be a number")
    if (length(a) != 1) 
        stop("first argument needs to be a single number")
    if (!is.numeric(a)) 
        stop("first argument is not a numerical value")
    if (!is.vector(b)) 
        stop("second argument needs to be a number")
    if (length(b) != 1) 
        stop("second argument needs to be a single number")
    if (!is.numeric(b)) 
        stop("second argument is not a numerical value")
    greater_num <- max(abs(a), abs(b))
    lesser_num <- min(abs(a), abs(b))
    while (lesser_num != 0) {
        remainder <- greater_num\%\%lesser_num
        greater_num <- lesser_num
        lesser_num <- remainder
    }
    return(greater_num)
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
