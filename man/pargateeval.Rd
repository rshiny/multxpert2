\name{pargateeval}
\alias{pargateeval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pargateeval(gateproc, alpha, independence)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gateproc}{
%%     ~~Describe \code{gateproc} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{independence}{
%%     ~~Describe \code{independence} here~~
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
function (gateproc, alpha, independence) 
{
    nfams <- length(gateproc)
    level <- alpha
    for (i in 1:nfams) {
        p <- gateproc[[i]]$rawp
        w <- rep(1/length(p), length(p))
        if (gateproc[[i]]$proc == "Bonferroni") {
            proc <- "Holm"
            gamma <- 0
            gateproc[[i]]$proc <- "Holm"
            gateproc[[i]]$procpar <- 0
        }
        else {
            proc <- gateproc[[i]]$proc
            gamma <- gateproc[[i]]$procpar
        }
        adjp <- pvaltrunc(p, w, proc, gamma)
        rejection <- (adjp <= level)
        gateproc[[i]][5] <- list(adjp = adjp)
        gateproc[[i]][6] <- list(level = level)
        gateproc[[i]][7] <- list(rejection = rejection)
        level <- levelproc(rejection, proc, gamma, level)
    }
    if (independence == FALSE) {
        for (i in (nfams + 1):(2 * nfams - 1)) {
            k <- 2 * nfams - i
            prevrej <- gateproc[[i - 1]][[7]]
            if (sum(prevrej) == length(prevrej)) 
                level <- alpha
            else level <- 0
            label <- gateproc[[k]]$label
            p <- gateproc[[k]]$rawp
            w <- rep(1/length(p), length(p))
            if (gateproc[[k]]$proc == "Bonferroni") 
                proc <- "Holm"
            else proc <- gateproc[[k]]$proc
            gamma <- 1
            adjp <- pvaltrunc(p, w, proc, gamma)
            rejection <- (adjp <= level)
            gateproc[[i]] <- list(label = label, rawp = p, proc = proc, 
                procpar = gamma, adjp = adjp, level = level, 
                rejection = rejection)
        }
    }
    return(gateproc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
