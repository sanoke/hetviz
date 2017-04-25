#' Simple simulated data: Effect modification and confounding by EMs
#'
#' @description A simulated dataset of 1500 observations, with 'observed'
#' binary treatment, Normal outcome, covariates, and a few other variables that
#' were used in the generation of the 'observed' treatment and outcome.
#'
#' @format A data frame with 1500 rows and 16 variables:
#' \describe{
#'
#'  \item{trt}{treatment, generated from Bern(expit(0.1 \code{X1}
#'       - 0.1 \code{X2} + 1.1 \code{X3} - 1.1 \code{X4} + 0.4 \code{X5}
#'       - 0.1 \code{E1} + 1.1 \code{E2} - 4 \code{E3} ))}
#'
#'  \item{Y}{outcome, corresponding to the potential outcome
#'       (\code{Y0} or \code{Y1}) associated with the observed treatment}
#'
#'  \item{X1}{confounder, generated from N(0,1)}
#'
#'  \item{X2}{confounder, generated from N(0,1)}
#'
#'  \item{X3}{confounder, generated from N(0,1)}
#'
#'  \item{X4}{confounder, generated from N(0,1)}
#'
#'  \item{X5}{instrument, generated from N(0,1)}
#'
#'  \item{X6}{prognostic variable, generated from N(0,1)}
#'
#'  \item{E1}{effect modifier, generated from Bern(0.5)}
#'
#'  \item{E2}{effect modifier, generated from Bern(0.5)}
#'
#'  \item{E3}{effect modifier, generated from Bern(0.5)}
#'
#'  \item{Y0}{potential outcome,
#'       generated from N(-3.85 + 5 \code{trt}
#'       + 0.5 \code{X1} - 2 \code{X2} - 0.5 \code{X3} + 2 \code{X4}
#'       + \code{X6} - \code{E1} - 2\code{E3} + \code{trt}*\code{E1}
#'       + 4 \code{trt} * \code{E2} - 4 \code{trt}*\code{E3}, 1 )
#'       where \code{trt} is set to 0}
#'
#'  \item{Y1}{potential outcome,
#'       generated from N(-3.85 + 5 \code{trt}
#'       + 0.5 \code{X1} - 2 \code{X2} - 0.5 \code{X3} + 2 \code{X4}
#'       + \code{X6} - \code{E1} - 2\code{E3} + \code{trt}*\code{E1}
#'       + 4 \code{trt} * \code{E2} - 4 \code{trt}*\code{E3}, 1 )
#'       where \code{trt} is set to 1}
#'
#'  \item{trueGrp}{the true average treatment effect in the subgroup
#'       that the observation belongs to}
#'
#'  \item{estGrp}{an integer value in
#'       \{1, 2, ..., 10\} that represents the estimated subgroup that the
#'       observation belongs to. See below for more information on how this value is
#'       assigned. }
#'
#'  \item{mmt}{the estimated ITE, as determined via
#'       regression using Bayesian Additive Regression Trees (as estimated by
#'       \code{bart()} of the \strong{bayesTree} package).}
#'
#' }
#'
#' @section Estimated subgroup assignment:
#'  To estimate what subgroup each
#'  observation belongs to, an individual treatment effect (ITE) was estimated
#'  (stored in \code{mmt}). This empirical distribution of ITEs is then
#'  partitioned into deciles, and an average treatment effect (ATE) estimated in
#'  each. Each of the ten subgroups are arranged in ascending order by the
#'  subgroup-specific ATE then assigned an integer value in \{1, 2, ..., 10\}
#'  (stored in \code{estGrp}).
#'
#' @docType data
#'
#' @references Anoke SC, Normand S-L, Zigler CM (2017). Approaches to treatment
#'  effect heterogeneity in the presence of confounding (in revision).
#'
#' @family simple simulated datasets
#'
#' @seealso \code{\link{simpleDataA}} for data simulated with confounding
#'  and no effect modification, \code{\link{simpleDataB}} for effect modification
#'  and no confounding, and \code{\link{simpleDataC}} for effect modification and
#'  confounding (not by effect modifiers).
"simpleDataD"
