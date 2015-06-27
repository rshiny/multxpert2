\name{parci}
\alias{parci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parci(stat, n, est, stderror, covprob = 0.975, proc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
%%     ~~Describe \code{stat} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{est}{
%%     ~~Describe \code{est} here~~
}
  \item{stderror}{
%%     ~~Describe \code{stderror} here~~
}
  \item{covprob}{
%%     ~~Describe \code{covprob} here~~
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
function (stat, n, est, stderror, covprob = 0.975, proc) 
{
    m <- length(stat)
    if (m == 0) 
        stop("No test statistics are specified")
    if (m != length(est)) 
        stop("STAT and EST vectors have different lengths")
    if (m != length(stderror)) 
        stop("STAT and STDERROR vectors have different lengths")
    if (covprob >= 1) 
        stop("Simultaneous coverage probability must be <1")
    if (covprob <= 0) 
        stop("Simultaneous coverage probability must be >0")
    if (!all(proc \%in\% c("Single-step Dunnett", "Step-down Dunnett"))) 
        stop("Procedure name is not recognized. ParCI function supports only the single-step Dunnett and step-down Dunnett procedures")
    if (n <= 0) 
        stop("Sample size must be positive")
    nproc <- length(proc)
    cimat <- matrix(0, m, nproc)
    dimnames(cimat) <- list(NULL, paste(proc, ".conf.limit", 
        sep = ""))
    adjpmat <- matrix(0, m, nproc)
    dimnames(adjpmat) <- list(NULL, paste(proc, ".adj.pvalue", 
        sep = ""))
    nu <- (m + 1) * (n - 1)
    result <- paradjp(stat, n, proc)
    alpha <- 1 - covprob
    ci <- rep(0, m)
    zero <- rep(0, m)
    if (is.element("Single-step Dunnett", proc)) {
        adjpmat[, "Single-step Dunnett.adj.pvalue"] <- round(result[, 
            "Single.step.Dunnett.adj.pvalue"], 4)
        reject <- (result[, "Single.step.Dunnett.adj.pvalue"] <= 
            alpha)
        c <- qdunnett(1 - alpha, nu, m)
        cimat[, "Single-step Dunnett.conf.limit"] <- round(est - 
            c * stderror, 4)
    }
    if (is.element("Step-down Dunnett", proc)) {
        adjpmat[, "Step-down Dunnett.adj.pvalue"] <- round(result[, 
            "Step.down.Dunnett.adj.pvalue"], 4)
        reject <- (result[, "Step.down.Dunnett.adj.pvalue"] <= 
            alpha)
        if (sum(reject) == m) {
            c <- qt(1 - alpha, nu)
            cimat[, "Step-down Dunnett.conf.limit"] <- round(pmax(zero, 
                est - c * stderror), 4)
        }
        if (sum(reject) < m) {
            for (i in 1:m) {
                if (reject[i] == 1) 
                  cimat[i, "Step-down Dunnett.conf.limit"] <- 0
                if (reject[i] == 0) {
                  c <- qdunnett(1 - alpha, nu, m - sum(reject))
                  cimat[i, "Step-down Dunnett.conf.limit"] <- round(est[i] - 
                    c * stderror[i], 4)
                }
            }
        }
    }
    result <- data.frame(stat, est, stderror, adjpmat, cimat)
    names(result)[1] <- "Test.statistic"
    names(result)[2] <- "Estimate"
    names(result)[3] <- "Std.error"
    return(result = result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
