\name{pargateadjp}
\alias{pargateadjp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pargateadjp(gateproc, independence, alpha = 0.05, printDecisionRules = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gateproc}{
%%     ~~Describe \code{gateproc} here~~
}
  \item{independence}{
%%     ~~Describe \code{independence} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{printDecisionRules}{
%%     ~~Describe \code{printDecisionRules} here~~
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
function (gateproc, independence, alpha = 0.05, printDecisionRules = FALSE) 
{
    nfams <- length(gateproc)
    if (nfams <= 1) 
        stop("Function requires more than one family of null hypotheses")
    for (i in 1:nfams) {
        pr <- gateproc[[i]]$proc
        if (pr != "Bonferroni" & pr != "Holm" & pr != "Hommel" & 
            pr != "Hochberg" & pr != "Fallback") 
            stop("Procedure name is not recognized. ParGateAdjP function supports only the Bonferroni, Holm, Hommel, Hochberg and fallback procedures")
    }
    if (alpha <= 0) 
        stop("Alpha must be positive")
    if (alpha >= 1) 
        stop("Alpha must be less than 1")
    temp <- gateproc
    for (i in 1:nfams) {
        nhyps <- length(temp[[i]]$rawp)
        adjp <- rep(0, nhyps)
        gateproc[[i]][5] <- list(adjp = adjp)
        for (j in 1:nhyps) {
            upper <- 1
            lower <- 0
            for (k in 1:20) {
                current <- (lower + upper)/2
                res <- pargateeval(temp, current, independence)
                if (independence == TRUE | i == nfams) 
                  reject <- res[[i]][[7]][j]
                if (independence == FALSE & i < nfams) {
                  if (res[[2 * nfams - i]][[6]] > 0) 
                    reject <- res[[2 * nfams - i]][[7]][j]
                  else reject <- res[[i]][[7]][j]
                }
                if (reject == TRUE) 
                  upper <- current
                if (reject == FALSE) 
                  lower <- current
            }
            gateproc[[i]][[5]][j] <- (lower + upper)/2
        }
    }
    count <- 0
    for (i in 1:nfams) {
        count <- count + length(gateproc[[i]]$rawp)
    }
    result <- data.frame()
    k <- 1
    for (i in 1:nfams) {
        nhyps <- length(gateproc[[i]]$rawp)
        for (j in 1:nhyps) {
            result[k, 1] <- gateproc[[i]]$label
            result[k, 2] <- gateproc[[i]]$proc
            result[k, 3] <- gateproc[[i]]$procpar
            result[k, 4] <- round(gateproc[[i]]$rawp[j], 4)
            result[k, 5] <- round(gateproc[[i]][[5]][j], 4)
            k <- k + 1
        }
    }
    names(result)[1] <- "Family"
    names(result)[2] <- "Procedure"
    names(result)[3] <- "Parameter"
    names(result)[4] <- "Raw.pvalue"
    names(result)[5] <- "Adj.pvalue"
    if (printDecisionRules == TRUE) {
        pargaterule(gateproc, alpha, independence)
    }
    return(result = result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
