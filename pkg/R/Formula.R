Formula <- function(object) {

  stopifnot(inherits(object, "formula"))

  # extract correctly the right and the left hand side and deal the
  # case of onepart formula
  rhs <- switch(as.character(length(object)),
                "2" = object[[2]],
                "3" = object[[3]]
                )
  lhs <- switch(as.character(length(object)),
                "2" = NULL,
                "3" = object[[2]]
                )
  # create a list of left-hand side variables (separated by a + in the
  # formula)
  lhs.list <- plus2list(lhs)
  
  # create a list of the parts on the right-hand side of the formula,
  # the parts being separated by a |
  rhs.list <- list()
  if (length(rhs) > 1 && rhs[[1]] == "|"){
    while (length(rhs) > 1 && rhs[[1]] == "|"){
      rhs.list <- c(rhs[[3]], rhs.list)
      rhs <- rhs[[2]]
    }
  }
  rhs.list <- c(rhs, rhs.list)
  last.part <- rhs.list[[length(rhs.list)]]


  rhs.list[[length(rhs.list)]] <- last.part
  structure(object, lhs = lhs.list, rhs = rhs.list,
            class = c("Formula","formula"))
}

as.Formula <- function(x, ...) UseMethod("as.Formula")

as.Formula.default <- function(x, ...) {
  if(!inherits(x, "formula")) x <- as.formula(x)
  Formula(x)
}

as.Formula.formula <- function(x, ...) {

  z <- list(...)
  if(length(z) < 1) return(Formula(x)) else z <- z[[1]]
  #Z# instead of just using the first argument in `...',
  #Z# use all (vectorized or recursively)

  if (length(x) == 3) {
    y <- x[[2]]
    rhs <- x[[3]]
  } else{
    y <- NULL
    rhs <- x[[2]]
  }
  zz <- if (length(z) == 3) z[[3]] else z[[2]]
  #Z# should we throw a warning if there is a left-hand side?
  
  #Z# avoid deparsing and parsing again
  rval <- paste(if(!is.null(y)) paste(deparse(y), collapse = ""),
    " ~ ", paste(deparse(rhs), collapse = ""),
    "|", paste(deparse(zz), collapse = ""))

  as.Formula(rval)
}

is.Formula <- function(object)
  inherits(object, "Formula")

formula.Formula <- function(x, part = "first", response = NULL, ...){
  therhs <- attr(x, "rhs")
  thelhs <- attr(x, "lhs")
  lhs <- response
  rhs <- part
  if (is.character(rhs)){
    if (!rhs %in% c("first","second","both","all")) stop("irrelevant value for rhs")
    rhs <- switch(rhs,
                  "first" = 1,
                  "second" = 2,
                  "both" = c(1,2),
                  "all" = 1:length(x)
                  )
  }
  if (is.character(lhs)){
    if (lhs == "all"){
      lhs <- 1:length(attr(x, "lhs"))
    }
    else{
      stop ("irrelevant value for lhs")
    }
  }
  if (is.logical(lhs)) lhs <- ifelse(lhs, 1, 0)
  if (is.null(lhs)){
    # the default behaviour is to select the first response if any
    if (!is.null(thelhs)) lhs <- paste(deparse(thelhs[[1]])) else lhs <- 0
  }
  else{
    if (max(lhs) > length(thelhs)) stop(paste("only",length(thelhs),"responses available"))
    if (length(lhs) == 1){
      if (lhs == 0){
        lhs <- NULL
      }
      else{
        #YC add a deparse below
        lhs <- paste(deparse(thelhs[[lhs]]))
      }
    }
    else{
      #YC add a deparse below
      lhs <- paste(deparse(thelhs[lhs]),collapse=" + ")
    }
  }
  if (is.null(rhs)){
    rhs <- therhs[1]
  }
  else{
    if (length(rhs) == 1 && rhs < 1) stop("at least one part should be selected")
    if (max(rhs) > length(therhs)) stop(paste("the formula has only", length(therhs), "parts"))
    rhs <- therhs[rhs]
    if (length(rhs) > 1){
      rhs <- paste(rhs,collapse=" + ",sep="")
    }
    else{
      #YC add a collapse = ""
      rhs <- paste(deparse(rhs[[1]]), collapse = "")
    }
  }
  
  if (is.null(lhs)){
    result <- as.formula(paste( " ~ ", rhs))
  }
  else{
    result <- as.formula(paste(lhs, " ~ ", rhs))
  }
  result
}

terms.Formula <- function(x, ..., part = "first", response = NULL) {
  if(is.null(response))
    response <- attr(terms(structure(x, class = "formula")), "response") == 1L
  form <- formula(x, part = part, response = response)
  terms(form, ...)
}

model.frame.Formula <- function(formula, ..., part = NULL, response = NULL) {
  if (is.null(response)) response <- attr(terms(formula), "response") == 1L
  if (is.null(part)) part <- ifelse(length(formula) == 2L, "both", "first")
  form <- formula(formula, part = part, response = response)
  model.frame(form, ...)
}

model.matrix.Formula <- function(object, ..., part = "first") {
  form <- formula(object, part = part)
  model.matrix(form, ...)
  #Z# Should we call model.frame(form, ...) first to avoid missingness problems?
}
  

update.Formula <- function(object, new,...) {
  old <- object
  if (!is.Formula(old)) old <- Formula(old)
  if (!is.Formula(new)) new <- Formula(new)

  old.first <- formula(old, part = "first", response = FALSE)
  old.second <- formula(old, part = "second", response = FALSE)

  new.first <- formula(new, part = "first", response = FALSE)
  new.second <- formula(new, part = "second", response = FALSE)

  new.first <- update(old.first, new.first)
  new.second <- update(old.second, new.second)
  
  #Z# avoid deparsing and parsing again
  result <- paste(paste(deparse(old[[2]]), collapse = ""),
    "~", paste(deparse(new.first[[2]]), collapse = ""),
    "|", paste(deparse(new.second[[2]]), collapse = ""))
  as.Formula(result)
}

update.Formula <- function(object, new,...) {
  new <- Formula(new)
  rhs <- mapply(upcallform,
                attr(object,"rhs"),
                attr(new, "rhs"),
                SIMPLIFY = FALSE)
  lhs.list <- attr(object, "lhs")
  if (!is.null(lhs.list)){
    oldlhs <- as.formula(paste("~",paste(attr(object,"lhs"),collapse = " + ")))
    newlhs <- as.formula(paste("~",paste(attr(new,"lhs"),collapse = " + ")))
    lhs <- update(oldlhs, newlhs)[[2]]
    lhs.list <- plus2list(lhs)
    x <- paste(deparse(lhs)," ~ ",paste(rhs, collapse = " | "))
  }
  else{
    x <- paste(" ~ ", paste(rhs, collapse = " | "))
  }
  structure(as.formula(x), rhs = rhs, lhs = lhs.list,
            class = c("Formula", "formula"))

}

upcallform <- function(x, y){
  x <- formula(paste("~",deparse(x)))
  y <- formula(paste("~",deparse(y)))
  update(x,y)[[2]]
}

plus2list <- function(x){
  x.list <- list()
  if (!is.null(x)){
    x.list <- list()
    if (length(x) == 1) x.list <- list(x) else{
      while (length(x) > 1  && x[[1]] == "+"){
        x.list <- c(x[[3]],x.list)
        x <- x[[2]]
      }
      x.list <- c(x,x.list)
    }
  } else x.list <- NULL
  x.list
}


length.Formula <- function(x) {
  length(attr(x, "rhs"))
}


has.intercept <- function(object, ...) {
  UseMethod("has.intercept")
}

has.intercept.formula <- function(object, ...){
  attr(terms(object), "intercept") == 1L
}

has.intercept.Formula <- function(object, part = "first", ...) {
  if (length(part) > 1) stop("the has.intercept function is relevant for a single part")
  formula <- formula(object, part = part)
  attr(terms(formula), "intercept") == 1L
}

print.Formula <- function(x, ...){
  attributes(x) <- NULL
  print(x)
}
