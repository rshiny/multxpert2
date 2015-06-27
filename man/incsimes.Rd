\name{incsimes}
\alias{incsimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
incsimes(p, w, gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{w}{
%%     ~~Describe \code{w} here~~
}
  \item{gamma}{
%%     ~~Describe \code{gamma} here~~
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
function (p, w, gamma) 
{
    k <- length(w[w != 0])
    if (k > 1) {
        temp <- matrix(0, 3, k)
        temp[1, ] <- p[w != 0]
        temp[2, ] <- w[w != 0]
        temp[3, ] <- w[w != 0]/sum(w[w != 0])
        sorted <- temp[, order(temp[1, ])]
        modw <- w[w != 0]
        modw[1] <- 0
        modw[2:k] <- sorted[3, 1:k - 1]
        numer <- sorted[1, ]
        denom <- gamma * sorted[3, ]/(1 - cumsum(modw)) + (1 - 
            gamma) * sorted[2, ]
        incsimes <- min(numer/denom)
    }
    if (k == 1) {
        numer <- p[w != 0]
        denom <- gamma + (1 - gamma) * w[w != 0]
        incsimes <- numer/denom
    }
    if (k == 0) 
        incsimes <- 1
    return(incsimes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
