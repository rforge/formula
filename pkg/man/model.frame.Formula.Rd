\name{model.frame.Formula}

\alias{terms.Formula}
\alias{model.matrix.Formula}
\alias{model.frame.Formula}

\title{Model Frame/Matrix Construction for Extended Formulas}

\description{
  Computation of model frames and model matrices for extended formulas
  of class \code{Formula}.
}

\usage{
\method{model.frame}{Formula}(formula, \dots, lhs = NULL, rhs = NULL)
\method{model.matrix}{Formula}(object, \dots, lhs = NULL, rhs = 1)
\method{terms}{Formula}(x, \dots, lhs = NULL, rhs = NULL)
}

\arguments{
  \item{formula, object, x}{an object of class \code{Formula}.}
  \item{lhs, rhs}{indexes specifying which elements of the left- and
    right-hand side, respectively, should be employed. \code{NULL}
    corresponds to all parts, \code{0} to none.}
  \item{\dots}{further arguments passed to the respective
    \code{formula} methods.}
}

\details{
  All three model computations leverage the corresponding standard methods.
  Additionally, they allow specification of the part(s) of the left- and righ-hand side
  (LHS and RHS) that should be included in the computation.

  The idea underlying all three model computations is to extract a suitable
  \code{formula} from the more general \code{Formula} and then calling
  the standard \code{\link[stats]{model.frame}}, \code{\link[stats]{model.matrix}},
  and \code{\link[stats]{terms}} methods.
  
  More specifically, if the \code{Formula} has multiple parts on the RHS,
  they are collapsed, essentially replacing \code{|} by \code{+}. If there
  is only a single response on the LHS, then it is kept on the LHS.
  Otherwise all parts of the formula are collapsed on the RHS (because \code{formula}
  objects can not have multiple responses). Hence, for multi-response \code{Formula}
  objects, the (non-generic) \code{\link[stats:model.extract]{model.response}} does not work.
}

\seealso{\code{\link{Formula}},  \code{\link[stats]{model.frame}},
  \code{\link[stats]{model.matrix}}, \code{\link[stats]{terms}}}

\examples{
## artificial example data
set.seed(1071)
dat <- as.data.frame(matrix(round(rnorm(21), digits = 2), ncol = 7))
colnames(dat) <- c("y1", "y2", "y3", "x1", "x2", "x3", "x4")
for(i in 6:7) dat[[i]] <- factor(dat[[i]] > 0, labels = c("a", "b"))

#########################################
## multiple responses and multiple RHS ##
#########################################

## set up Formula
f <- Formula(y1 | y2 + y3 ~ x1 + I(x2^2) | 0 + log(x1) | x3 / x4)
length(f)

## set up full model frame
mf <- model.frame(f, data = dat)
mf

## extract responses
model.frame(f, data = dat, lhs = 1, rhs = 0)
model.frame(f, data = dat, lhs = 2, rhs = 0)
## model.response(mf) does not work!

## extract model matrices
model.matrix(f, data = dat, rhs = 1)
model.matrix(f, data = dat, rhs = 2)
model.matrix(f, data = dat, rhs = 3)
## to use 'mf' instead of 'dat' set:
## attr(mf, "terms") <- NULL

######################################
## single response and two-part RHS ##
######################################

## single response with two-part RHS
f2 <- Formula(y1 ~ x1 + x2 | log(x1))
length(f2)

## set up model frame
mf2 <- model.frame(f2, data = dat)
mf2

## extract single response
model.frame(f2, data = dat, rhs = 0)[,1]
model.response(mf2)
## model.response() works as usual

## extract model matrices
model.matrix(f2, data = dat, rhs = 1)
model.matrix(f2, data = dat, rhs = 2)
}

\keyword{models}
