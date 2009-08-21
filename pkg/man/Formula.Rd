\name{Formula}
\alias{Formula}
\alias{is.Formula}
\alias{formula.Formula}
\alias{terms.Formula}
\alias{update.Formula}
\alias{model.matrix.Formula}
\alias{model.frame.Formula}
\alias{length.Formula}
\alias{as.Formula}
\alias{as.Formula.default}
\alias{as.Formula.formula}
\alias{has.intercept}
\alias{has.intercept.formula}
\alias{has.intercept.Formula}
\alias{print.Formula}

\title{Extended Formulas}

\description{

  The new class \code{Formula} extends the base class
  \code{\link[stats]{formula}} by allowing the use of multi-parts and
  multi-responses formula.  }

\usage{
Formula(object)
\method{formula}{Formula}(x, part = "first",
                          response = NULL, \dots)
\method{terms}{Formula}(x, \dots, part = "first", response = NULL)
\method{update}{Formula}(object, new, \dots)
\method{model.matrix}{Formula}(object, \dots, part = "first")
\method{model.frame}{Formula}(formula, \dots, part = NULL, response = NULL)
\method{length}{Formula}(x)
\method{print}{Formula}(x, ...)

as.Formula(x, \dots)
\method{as.Formula}{default}(x, \dots)
\method{as.Formula}{formula}(x, \dots)

is.Formula(object)

has.intercept(object, ...)
\method{has.intercept}{formula}(object, ...)
\method{has.intercept}{Formula}(object, part = "first", ...)

}

\arguments{
  \item{object, x, formula}{an object. For \code{Formula} it needs to be a
    \code{formula} object.}
  
  \item{part}{this argument specifies the parts of the right hand side
    of the formula that should be used: it can be either a character
    \code{"first"}, \code{"second"}, \code{"both"} or \code{"all"} or a
    numeric vector.}

  \item{response}{this argument specifies which responses shoud be used:
    it can be either a logical (\code{FALSE} means no response and
    \code{TRUE} the first one), a character (\code{"all"}) or a numeric
    vector (eg \code{c(1,3)} or \code{0} for no response.}
  \item{new}{a formula giving a template which specifies how to update.}
  \item{\dots}{further arguments.}
}

\value{
  \code{Formula} returns an object of class \code{Formula}
  which inherits from \code{formula}.
}

\details{  
  \code{Formula} objects extend the basic \code{formula} objects.
  These extensions  include multi-part formulas
  (\code{y ~ x1 + x2 | u1 + u2 + u3 | v1 + v2}),  multiple response
  formulas (\code{y1 + y2 ~ x1 + x2 + x3}) 
  
  The \code{Formula} function coerce a formula to a \code{Formula}
  object by modifying the class attribute and adding three attributes :
  \code{rhs} (a list containing the parts of the right-hand side),
  \code{lhs} (a list containing the responses).
  
  \code{is.Formula} check whether the argument of the function is &
  \code{Formula} object.
  
  \code{as.Formula} is a generic for coercing to \code{Formula}, the
  default method first coerces to \code{formula} and then calls
  \code{Formula}.
  
  Several methods are provided: They all use the \code{formula} method
  for creating a standard formula first. This formula is created using
  some response and some part of the original \code{Formula}. The
  selected responses and parts are indicating using respectively the
  \code{response} and \code{parts} arguments. 


  Hence, the two arguments (\code{response} and \code{part}) are used in the
  \code{terms}, \code{model.frame} and \code{model.matrix} methods.
}

\examples{
data("airquality", package = "datasets")
f <- Ozone + Solar.R ~ Wind + Day | Temp + Day | Month + Day

# create a Formula object from the formula
f2 <- Formula(f)
class(f2)

# update of the Formula
update(f2,.~.-Day|.+Month|.)

# get some parts, with or without the response
formula(f2) # default behavior first response and first part
formula(f2, part = 1, response = 1) # same

formula(f2, part = c(1, 3))
formula(f2, part = c(1, 3), response = 1) # same

formula(f2, part = 2, response = 1)
formula(f2, part = "second", response = 1) # same (deprecated)

formula(f2, part = "all", response = 2) # select all parts
formula(f2, part = "all", response = 0) # a one-sided formula
formula(f2, part = "all", response = FALSE) # same 


# model.frame
head(model.frame(f2, part = c(1, 3), data = airquality))
# with the second response
head(model.frame(f2, part = c(1, 3), response = 2, data = airquality))
# with all parts and no response
head(model.frame(f2, part = "all", response = FALSE, data = airquality))

# model.matrix
head(model.matrix(f2, data = airquality, part = 1))
head(model.matrix(f2, data = airquality, part = "all"))
}

\keyword{classes}
