\name{pvalci}
\alias{pvalci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pvalci(rawp, est, stderror, weight = rep(1/length(rawp), length(rawp)), covprob = 0.975, proc = c("Bonferroni", "Holm", "Fixed-sequence"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawp}{
%%     ~~Describe \code{rawp} here~~
}
  \item{est}{
%%     ~~Describe \code{est} here~~
}
  \item{stderror}{
%%     ~~Describe \code{stderror} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
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
function (rawp, est, stderror, weight = rep(1/length(rawp), length(rawp)), 
    covprob = 0.975, proc = c("Bonferroni", "Holm", "Fixed-sequence")) 
{
    m <- length(rawp)
    if (m == 0) 
        stop("No p-values are specified")
    for (i in 1:m) {
        if (rawp[i] < 0) 
            stop("P-values must be positive")
        if (rawp[i] > 1) 
            stop("P-values must be less than 1")
    }
    if (m != length(weight)) 
        stop("RAWP and WEIGHT vectors have different lengths")
    if (m != length(est)) 
        stop("RAWP and EST vectors have different lengths")
    if (m != length(stderror)) 
        stop("RAWP and STDERROR vectors have different lengths")
    if (sum(weight) > 1) 
        stop("Sum of hypothesis weights must be <=1")
    for (i in 1:length(weight)) {
        if (weight[i] < 0) 
            stop("Hypothesis weights must be >=0")
    }
    if (covprob >= 1) 
        stop("Simultaneous coverage probability must be <1")
    if (covprob <= 0) 
        stop("Simultaneous coverage probability must be >0")
    if (!all(proc \%in\% c("Bonferroni", "Holm", "Fixed-sequence"))) 
        stop("Procedure name is not recognized")
    nproc <- length(proc)
    cimat <- matrix(0, m, nproc)
    dimnames(cimat) <- list(NULL, paste(proc, ".conf.limit", 
        sep = ""))
    alpha <- 1 - covprob
    result <- pvaladjp(rawp = rawp, weight = weight, alpha = alpha, 
        proc = proc)
    adjpmat <- result[, grep(".adj.pvalue", names(result), value = TRUE)]
    ci <- rep(0, m)
    zero <- rep(0, m)
    if (is.element("Bonferroni", proc)) {
        reject <- (result[, "Bonferroni.adj.pvalue"] <= alpha)
        cimat[, "Bonferroni.conf.limit"] <- est - (stderror * 
            qnorm(1 - (alpha * weight)))
    }
    if (is.element("Holm", proc)) {
        reject <- (result[, "Holm.adj.pvalue"] <= alpha)
        bonfci <- est - (stderror * qnorm(1 - (alpha * weight)))
        if (sum(reject) == m) 
            cimat[, "Holm.conf.limit"] <- pmax(zero, bonfci)
        if (sum(reject) < m) {
            for (i in 1:m) {
                if (reject[i] == 1) 
                  cimat[i, "Holm.conf.limit"] <- 0
                if (reject[i] == 0) {
                  adjalpha <- (alpha * weight[i])/sum(weight[reject == 
                    0])
                  cimat[i, "Holm.conf.limit"] <- est[i] - (stderror[i] * 
                    qnorm(1 - adjalpha))
                }
            }
        }
    }
    if (is.element("Fixed-sequence", proc)) {
        reject <- (result[, "Fixed.sequence.adj.pvalue"] <= alpha)
        if (sum(reject) == 0) {
            cimat[1, "Fixed-sequence.conf.limit"] <- est[1] - 
                stderror[1] * qnorm(1 - alpha)
            for (i in 2:m) cimat[i, "Fixed-sequence.conf.limit"] <- NA
        }
        if (sum(reject) == m) {
            temp1 <- est - stderror * qnorm(1 - alpha)
            cimat[, "Fixed-sequence.conf.limit"] <- min(temp1)
        }
        if (sum(reject) > 0 & sum(reject) < m) {
            cimat[1, "Fixed-sequence.conf.limit"] <- 0
            for (i in 2:m) {
                if (reject[i] == 1) 
                  cimat[i, "Fixed-sequence.conf.limit"] <- 0
                if (reject[i] == 0 & reject[i - 1] == 1) 
                  cimat[i, "Fixed-sequence.conf.limit"] <- est[i] - 
                    stderror[i] * qnorm(1 - alpha)
                if (reject[i] == 0 & reject[i - 1] == 0) 
                  cimat[i, "Fixed-sequence.conf.limit"] <- NA
            }
        }
    }
    result <- data.frame(rawp, est, stderror, weight, adjpmat, 
        cimat)
    names(result)[1] <- "Raw.pvalue"
    names(result)[2] <- "Estimate"
    names(result)[3] <- "Std.error"
    names(result)[4] <- "Weight"
    return(result = result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
