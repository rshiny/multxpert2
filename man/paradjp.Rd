\name{paradjp}
\alias{paradjp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
paradjp(stat, n, proc = c("Single-step Dunnett", "Step-down Dunnett"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
%%     ~~Describe \code{stat} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{proc}{
%%     ~~Describe \code{proc} here~~
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
function (stat, n, proc = c("Single-step Dunnett", "Step-down Dunnett")) 
{
    if (n <= 0) 
        stop("Sample size must be positive")
    m <- length(stat)
    if (m == 0) 
        stop("No test statistics are specified")
    if (length(n) == 0) 
        stop("No sample size specified")
    nu <- (m + 1) * (n - 1)
    rawp <- 1 - pt(stat, 2 * (n - 1))
    adjp <- rep(0, m)
    if (!all(proc \%in\% c("Single-step Dunnett", "Step-down Dunnett"))) 
        stop("Procedure name is not recognized. ParAdjP function supports only the single-step Dunnett and step-down Dunnett procedures")
    nproc <- length(proc)
    adjp <- matrix(0, m, nproc)
    dimnames(adjp) <- list(NULL, paste(proc, ".adj.pvalue", sep = ""))
    if (is.element("Single-step Dunnett", proc)) {
        adjp[, "Single-step Dunnett.adj.pvalue"] <- sapply(stat, 
            function(x) {
                1 - pdunnett(x, nu, m)
            })
    }
    if (is.element("Step-down Dunnett", proc)) {
        adjptmp <- rep(NA, length(stat))
        or <- order(stat, decreasing = TRUE)
        sorted <- stat[or]
        for (i in 1:m) {
            if (i == 1) {
                adjptmp[1] <- 1 - pdunnett(sorted[1], nu, m)
                maxp <- adjptmp[1]
            }
            if (i > 1 & i < m) {
                adjptmp[i] <- max(maxp, 1 - pdunnett(sorted[i], 
                  nu, m - i + 1))
                maxp <- max(maxp, adjptmp[i])
            }
            if (i == m) {
                adjptmp[m] <- max(maxp, 1 - pt(sorted[m], nu))
            }
        }
        temp <- adjptmp
        adjptmp[or] <- temp
        adjp[, "Step-down Dunnett.adj.pvalue"] <- adjptmp
    }
    result <- data.frame(stat, round(rawp, 4), adjp)
    names(result)[1] <- "Test.statistic"
    names(result)[2] <- "Raw.pvalue"
    return(result = result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
