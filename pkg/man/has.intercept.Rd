\name{has.intercept}

\alias{has.intercept}
\alias{has.intercept.default}
\alias{has.intercept.formula}
\alias{has.intercept.Formula}

\title{Check for Intercept in Formulas}

\description{
  A generic function for assessing whether (parts of) a formula contain(s)
  intercepts.
}

\usage{
has.intercept(object, \dots)

\method{has.intercept}{default}(object, \dots)
\method{has.intercept}{formula}(object, \dots)
\method{has.intercept}{Formula}(object, rhs = NULL, \dots)
}

\arguments{
  \item{object}{an object. Currently, methods are provided for \code{formula}
    and \code{Formula} objects. The default method simply calls
    \code{has.intercept(formula(object), \dots)}.}
  \item{rhs}{index specifying the elements of the right-hand side in
    a \code{Formula} that should be queried. By default all right-hand side
    elements are used.}
  \item{\dots}{further arguments passed to methods.}
}

\value{
  A logical vector.
}

\examples{
has.intercept(y ~ x)
has.intercept(y ~ 0 + x)
has.intercept(Formula(y ~ 0 + x | z))
has.intercept(Formula(y ~ 0 + x | z), rhs = 1)
}

\keyword{methods}
