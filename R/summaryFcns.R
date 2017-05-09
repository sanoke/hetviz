#' Expit scale transformation
#'
#' Also known as the "inverse logit", this is an
#' internal function
#' to transform real line values to
#' logit scale values (i.e., \eqn{(0,1)}).
#' \eqn{expit(x)} is defined as \eqn{ e^x / (1 + e^x) }
#' for any real value.
#'
#' @param x A vector of real values.
#'
#' @return A vector of real values, the same length as the input.
#' 
#' @export
expit <- function(x) { exp(x) / (1 + exp(x)) }


#' Formatting data to be compatible wth plotting
#'
#' This is an
#' internal function
#' to rename covariates within the dataset.
#'
#' @param ds Data to be formatted.
#' @param outcome The column index of the outcome variable.
#' @param treatment The column index of the treatment variable.
#' @param estITE The column index of the estimated ITE variable.
#' @param group The column index of the grouping variable.
#'
#' @return The dataset with values in tact, but column names
#'         changed to be compatible with plotting functions
#' @export
dataCompat <- function(ds,
                       outcome,
                       treatment,
                       estITE = NULL,
                       group) {
    colnames(ds)[outcome]   <- "Y"
    colnames(ds)[treatment] <- "trt"
    colnames(ds)[group]     <- "estGrp"

    if ( !is.null(estITE) ) { colnames(ds)[estITE] <- "mmt" }

    # make the grouping variable into a factor
    ds$estGrp <- as.factor(ds$estGrp)

    return(ds)
}


#' Checking whether a specific covariate is present
#'
#' This is a
#' simple internal function
#' designed to check whether a specific covariate is
#' contained within a list of covariates.
#'
#' @param covarName A scalar string, containing the name of the
#'   covariate of interest.
#' @param covarNames A vector of strings, defining a list of
#'   covariate names to check against.
#'
#' @return A logical scalar, indicating whether the covariate is
#'   contained within the list. Note that by definition,
#'   a \code{NULL} or empty string (\code{""}) will return
#'   \code{TRUE}.
#' @export
covarInDataset <- function(covarName, covarNames) {

  if (nchar(covarName) == 0 | is.null(covarName)) return(TRUE)

  covarName <- trimws(covarName)

  if (covarName %in% covarNames) return(TRUE)

  return(FALSE)
}





#' Produces the ITE quantiles for a specified subgroup
#'
#' \code{groupQuantiles()} is an internal function that
#' takes a dataset and returns quantiles from a
#' specific subgroup. Used in constructing forest plots.
#'
#' @param grpNum A scalar indicating which subgroup to
#'   calculate quantiles from.
#' @param quantiles0 A vector indicating which quantiles to
#'   calculate.
#' @param ds A dataset of a very specific structure, as
#'   defined by \code{dataCompat()}.
#'
#' @return A vector containing the quantiles from the specified
#'   subgroup.
#'
#' @family plotting functions
#' 
#' @export
groupQuantiles <- function(grpNum, ds, quantiles0 = c(0.25, 0.50, 0.75)) {

  subgroup <- ds$mmt[ ds$estGrp == grpNum ]
  
  subgrpQuantiles <- quantile(subgroup, probs = quantiles0 )
  
  whiskerLeft <- as.numeric(subgrpQuantiles["50%"] - 
                            1.5 * (subgrpQuantiles["75%"] - subgrpQuantiles["25%"]))
  whiskerLeft <- ifelse( min(subgroup) > whiskerLeft ,
                         min(subgroup),
                         whiskerLeft)
  
  whiskerRight <- as.numeric(subgrpQuantiles["50%"] + 
                             1.5 * (subgrpQuantiles["75%"] - subgrpQuantiles["25%"]))
  whiskerRight <- ifelse( max(subgroup) < whiskerRight ,
                          max(subgroup),
                          whiskerRight)  

  return( c(wL=whiskerLeft, subgrpQuantiles, wR=whiskerRight) )

}

