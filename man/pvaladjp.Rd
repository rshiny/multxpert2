\name{pvaladjp}
\alias{pvaladjp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pvaladjp(rawp, weight = rep(1/length(rawp), length(rawp)), alpha = 0.05, proc = c("Bonferroni", "Holm", "Hommel", "Hochberg", "Fixed-sequence", "Fallback"), printDecisionRules = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawp}{
%%     ~~Describe \code{rawp} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{proc}{
%%     ~~Describe \code{proc} here~~
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
function (rawp, weight = rep(1/length(rawp), length(rawp)), alpha = 0.05, 
    proc = c("Bonferroni", "Holm", "Hommel", "Hochberg", "Fixed-sequence", 
        "Fallback"), printDecisionRules = FALSE) 
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
    index <- order(rawp)
    if (alpha <= 0) 
        stop("Alpha must be positive")
    if (alpha >= 1) 
        stop("Alpha must be less than 1")
    nweis <- length(weight)
    if (m != nweis) 
        stop("RAWP and WEIGHT vectors have different lengths")
    if (sum(weight) > 1) 
        stop("Sum of hypothesis weights must be <=1")
    for (i in 1:nweis) {
        if (weight[i] < 0) 
            stop("Hypothesis weights must be >=0")
    }
    if (!all(proc \%in\% c("Bonferroni", "Holm", "Hommel", "Hochberg", 
        "Fixed-sequence", "Fallback"))) 
        stop("Procedure name is not recognized. PvalAdjp function supports the Bonferroni, Holm, Hommel, Hochberg, Fixed-sequence, and Fallback procedures")
    nproc <- length(proc)
    adjp <- matrix(0, m, nproc)
    dimnames(adjp) <- list(NULL, paste(proc, ".adj.pvalue", sep = ""))
    if (is.element("Bonferroni", proc)) {
        adjp[, "Bonferroni.adj.pvalue"] <- pvaltrunc(rawp, weight, 
            "Holm", 0)
    }
    if (is.element("Holm", proc)) {
        adjp[, "Holm.adj.pvalue"] <- pvaltrunc(rawp, weight, 
            "Holm", 1)
    }
    if (is.element("Hommel", proc)) {
        adjp[, "Hommel.adj.pvalue"] <- pvaltrunc(rawp, weight, 
            "Hommel", 1)
    }
    if (is.element("Hochberg", proc)) {
        adjp[, "Hochberg.adj.pvalue"] <- pvaltrunc(rawp, weight, 
            "Hochberg", 1)
    }
    if (is.element("Fixed-sequence", proc)) {
        adjp[, "Fixed-sequence.adj.pvalue"] <- pvaltrunc(rawp, 
            weight, "Fixed-sequence", 1)
    }
    if (is.element("Fallback", proc)) {
        adjp[, "Fallback.adj.pvalue"] <- pvaltrunc(rawp, weight, 
            "Fallback", 1)
    }
    result <- data.frame(round(rawp, 4), round(weight, 4), round(adjp, 
        4))
    names(result)[1] <- "Raw.pvalue"
    names(result)[2] <- "Weight"
    if (printDecisionRules == TRUE) {
        if (length(unique(round(weight, 3))) > 1) 
            stop("Weights must be equal for decision rule calculations to be valid")
        pvalrule(rawp, alpha, proc)
    }
    return(result = result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
