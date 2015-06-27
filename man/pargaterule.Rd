\name{pargaterule}
\alias{pargaterule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 something

%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pargaterule(gateproc, alpha, independence)
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
    gateproc <- pargateeval(gateproc, alpha, independence)
    cat("Hypothesis testing problem\n\n")
    cat("Global familywise error rate=", alpha, "\n", sep = "")
    if (independence == TRUE) 
        cat("Independence condition is imposed (the families are tested from first to last) \n", 
            sep = "")
    if (independence == FALSE) 
        cat("Independence condition is not imposed (the families are tested from first to last and then re-tested from last to first) \n", 
            sep = "")
    cat("\n\n")
    retest <- FALSE
    hyplabel <- vector("list", nfams)
    cumtotal <- 0
    for (i in 1:nfams) {
        la <- round(gateproc[[i]][[6]], 4)
        pp <- round(gateproc[[i]]$procpar, 4)
        cat("Family ", i, " (", gateproc[[i]]$label, ") is tested using", 
            sep = "")
        cat(" ", gateproc[[i]]$proc, " procedure (truncation parameter=", 
            pp, ") at alpha", i, "=", la, ".\n\n", sep = "")
        nhyps <- length(gateproc[[i]]$rawp)
        hyplabel[[i]] <- seq(1:nhyps) + cumtotal
        cumtotal <- cumtotal + nhyps
        rejcount <- sum(gateproc[[i]][[7]])
        for (j in 1:nhyps) {
            rp <- round(gateproc[[i]][[2]][j], 4)
            ap <- round(gateproc[[i]][[5]][j], 4)
            cat("Null hypothesis ", hyplabel[[i]][j], " (raw p-value=", 
                rp, ")", sep = "")
            if (gateproc[[i]][[7]][j] == TRUE) {
                cat(" is rejected.\n\n", sep = "")
            }
            if (la == 0) 
                cat(" is automatically accepted. \n\n", sep = "")
            if (la > 0 & gateproc[[i]][[7]][j] == FALSE) 
                cat(" is accepted.\n\n", sep = "")
        }
        cat("Details on the decision rule for this family can be obtained by running the PValAdjP function for ", 
            gateproc[[i]]$proc, " procedure with gamma=", pp, 
            " and alpha=", la, ".\n\n", sep = "")
        if (rejcount == 0 & i < nfams) {
            cat("No null hypotheses are rejected in Family ", 
                i, " and the parallel gatekeeping procedure cannot pass this family.", 
                sep = "")
            cat(" Testing stops and all remaining null hypotheses are automatically accepted.\n\n\n")
        }
        if (rejcount > 0 & i < nfams) {
            cat("One or more null hypotheses are rejected in Family ", 
                i, " and the parallel gatekeeping procedure passes this family.", 
                sep = "")
            cat(" Based on the error rate function of ", gateproc[[i]]$proc, 
                " procedure (truncation parameter=", pp, "),", 
                sep = "")
            cat(" alpha", i + 1, "=", round(gateproc[[i + 1]][[6]], 
                4), " is carried over to Family ", i + 1, ".\n\n\n", 
                sep = "")
        }
        if (i == nfams & independence == FALSE) {
            if (rejcount == nhyps) {
                retest <- TRUE
                cat("All null hypotheses are rejected in Family ", 
                  i, " and the parallel gatekeeping procedure passes this family.", 
                  sep = "")
                cat(" Retesting begins and alpha", i + 1, "=", 
                  round(alpha, 4), " is carried over to Family ", 
                  i - 1, ".\n\n\n", sep = "")
            }
            if (rejcount < nhyps) {
                cat("Some null hypotheses are accepted in Family ", 
                  i, " and the parallel gatekeeping procedure cannot pass this family.", 
                  sep = "")
                cat(" Retesting will not be performed.\n\n\n", 
                  sep = "")
            }
        }
    }
    if (independence == FALSE & retest == TRUE) {
        for (i in (nfams + 1):(2 * nfams - 1)) {
            k <- 2 * nfams - i
            la <- round(gateproc[[i]][[6]], 4)
            pp <- round(gateproc[[i]]$procpar, 4)
            cat("Family ", k, " (", gateproc[[k]]$label, ") is retested using", 
                sep = "")
            cat(" ", gateproc[[i]]$proc, " procedure (truncation parameter=", 
                pp, ") at alpha", i, "=", la, ".\n\n", sep = "")
            nhyps <- length(gateproc[[i]]$rawp)
            rejcount <- sum(gateproc[[i]][[7]])
            for (j in 1:nhyps) {
                rp <- round(gateproc[[i]][[2]][j], 4)
                ap <- round(gateproc[[i]][[5]][j], 4)
                cat("Null hypothesis ", hyplabel[[k]][j], " (raw p-value=", 
                  rp, ")", sep = "")
                if (gateproc[[i]][[7]][j] == TRUE) {
                  cat(" is rejected.\n\n", sep = "")
                }
                if (la == 0) 
                  cat(" is automatically accepted.\n\n", sep = "")
                if (la > 0 & gateproc[[i]][[7]][j] == FALSE) 
                  cat(" is accepted.\n\n", sep = "")
            }
            cat("Details on the decision rule for this family can be obtained by running the PValAdjP function for ", 
                gateproc[[i]]$proc, " procedure with gamma=", 
                pp, " and alpha=", la, ".\n\n", sep = "")
            if (k > 1) {
                if (rejcount == nhyps) {
                  retest <- TRUE
                  cat("All null hypotheses are rejected in Family ", 
                    k, " and the parallel gatekeeping procedure passes this family and", 
                    sep = "")
                  cat(" alpha", i + 1, "=", round(alpha, 4), 
                    " is carried over to Family ", k - 1, ".\n\n\n", 
                    sep = "")
                }
                if (rejcount < nhyps) {
                  cat("Some null hypotheses are accepted in Family ", 
                    k, " and the parallel gatekeeping procedure cannot pass this family.", 
                    sep = "")
                  cat(" Retesting stops.\n\n\n", sep = "")
                }
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
