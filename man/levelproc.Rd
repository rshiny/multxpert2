\name{levelproc}
\alias{levelproc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
levelproc(reject, proc, gamma, level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reject}{
%%     ~~Describe \code{reject} here~~
}
  \item{proc}{
%%     ~~Describe \code{proc} here~~
}
  \item{gamma}{
%%     ~~Describe \code{gamma} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
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
function (reject, proc, gamma, level) 
{
    m <- length(reject)
    a <- m - sum(reject)
    ratio <- a/m
    if (ratio == 0) 
        error <- 0
    if (ratio > 0) {
        if (proc == "Holm" | proc == "Hommel" | proc == "Hochberg") {
            error <- gamma + (1 - gamma) * ratio
        }
        else if (proc == "Fallback") {
            accept <- 1 - reject
            sum <- 0
            smallest <- 1
            for (i in 1:length(accept)) {
                if (accept[i] == 1) {
                  if (smallest == 0) {
                    largest <- 1
                    for (j in 1:(i - 1)) {
                      if (accept[j] == 1) 
                        largest <- j
                    }
                    l <- largest
                  }
                  if (smallest == 1) {
                    l <- 0
                    smallest <- 0
                  }
                  sum <- sum + (i - l)
                }
                error <- gamma * (sum/m) + (1 - gamma) * ratio
            }
        }
    }
    nextlevel <- level - error * level
    return(nextlevel)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
