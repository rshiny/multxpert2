\name{pvaltrunc}
\alias{pvaltrunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pvaltrunc(rawp, weight, proc, gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawp}{
%%     ~~Describe \code{rawp} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
}
  \item{proc}{
%%     ~~Describe \code{proc} here~~
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
function (rawp, weight, proc, gamma) 
{
    nhyps <- length(rawp)
    nweis <- length(weight)
    nints <- 2^nhyps - 1
    h <- matrix(0, nints, nhyps)
    for (i in 1:nhyps) {
        for (j in 0:(nints - 1)) {
            k <- floor(j/2^(nhyps - i))
            if (k/2 == floor(k/2)) 
                h[j + 1, i] <- 1
        }
    }
    tempw <- matrix(0, 1, nhyps)
    if (proc == "Holm") {
        holmp <- matrix(0, nints, nhyps)
        for (i in 1:nints) {
            tempw <- (weight * h[i, ]/sum(weight * h[i, ])) * 
                gamma + (1 - gamma) * weight * h[i, ]
            holmp[i, ] <- h[i, ] * bonf(rawp, tempw)
        }
        holm <- rep(0, nhyps)
        for (i in 1:nhyps) holm[i] <- pmin(1, max(holmp[, i]))
        rm(holmp)
        res <- holm
    }
    else if (proc == "Hommel") {
        hommp <- matrix(0, nints, nhyps)
        for (i in 1:nints) hommp[i, ] <- h[i, ] * simes(rawp, 
            weight * h[i, ], gamma)
        hommel <- rep(0, nhyps)
        for (i in 1:nhyps) hommel[i] <- max(hommp[, i])
        rm(hommp)
        res <- hommel
    }
    else if (proc == "Hochberg") {
        hochp <- matrix(0, nints, nhyps)
        for (i in 1:nints) hochp[i, ] <- h[i, ] * incsimes(rawp, 
            weight * h[i, ], gamma)
        hochberg <- rep(0, nhyps)
        for (i in 1:nhyps) hochberg[i] <- max(hochp[, i])
        rm(hochp)
        res <- hochberg
    }
    else if (proc == "Fallback") {
        fallp <- matrix(0, nints, nhyps)
        fallwin <- matrix(0, nhyps, nhyps)
        for (i in 1:nhyps) {
            for (j in 1:nhyps) {
                if (i >= j) 
                  fallwin[i, j] <- 1
            }
        }
        for (i in 1:nints) {
            tempw[] <- 0
            for (j in 1:nhyps) {
                if (h[i, j] == 1) 
                  tempw[j] <- (sum(weight * fallwin[j, ]) - sum(tempw))
                if (h[i, j] == 0) 
                  tempw[j] <- 0
            }
            tempw <- tempw * gamma + (1 - gamma) * weight * h[i, 
                ]
            fallp[i, ] <- h[i, ] * bonf(rawp, tempw)
        }
        fallback <- rep(0, nhyps)
        for (i in 1:nhyps) fallback[i] <- pmin(1, max(fallp[, 
            i]))
        rm(fallp)
        rm(fallwin)
        res <- fallback
    }
    else if (proc == "Fixed-sequence") {
        fixedseq <- rep(0, nhyps)
        fixedseq[1] <- rawp[1]
        if (nhyps > 1) {
            for (i in 2:nhyps) fixedseq[i] <- max(fixedseq[i - 
                1], rawp[i])
        }
        res <- fixedseq
    }
    rm(tempw)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
