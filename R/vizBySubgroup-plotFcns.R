#' Calculates proportions of individuals in a 2x2 table of (trt, binary covar)
#'
#' \code{collapseProp()} is an internal function that takes a data
#' frame of a particular structure and returns a data frame with proportions
#' that are used in creating bar charts.
#'
#' @param data Any object that can be coerced into a \code{data.frame},
#'   that contains the data needing to be summarized. The first column
#'   is a binary variable named \code{trt}
#'   that indicates each observation's treatment status (0 or 1).
#'   The remaining
#'   columns are binary covariates.
#' @param var An scalar integer that indicates
#'   which column(s) of \code{data} contain the covariates to be
#'   summarized. \emph{This argument is deprecated and will
#'   be removed in later versions.}
#'
#' @return A data frame with three variables:
#'   \itemize{
#'     \item \code{values}, contains the levels of \code{var}
#'     among the treated and the levels among the untreated
#'     \item \code{trt}, an factor variable, indicating
#'     which treatment group the
#'     row corresponds to
#'     \item \code{propor}, the proportion of the entire sample that has that
#      covariate value and treatment value of that row
#'   }
collapseProp <- function(data, var=1) {

  # treat var as a factor variable (eases the cross-tabulation)
  data[,var] <- as.factor(data[,var])

  values_trt  <- levels(as.factor( data[ data$trt==1 , var ] ))
  values_ctrl <- levels(as.factor( data[ data$trt==0 , var ] ))

  # in the treatment group, what proportion of observations have
  #   var = 0 ? what proportion has var = 1 ?
  propor_trt  <- as.numeric(table( data[ data$trt==1 , var ] ) / nrow(data) )

  # in the control group, what proportion of observations have
  #   var = 0 ? what proportion has var = 1 ?
  propor_ctrl <- as.numeric(table( data[ data$trt==0 , var ] ) / nrow(data) )

  # number of levels of var; taking the max in case a level is empty
  #   in the treatment or control group
  num_levels  <- max(length(values_trt), length(values_ctrl))

  trt         <- factor( rep(1:0, each=num_levels) ,
                         levels=1:0,
                         labels=c("treated", "control"))

  data.frame(values = as.factor(c(values_trt, values_ctrl)),
             trt = trt,
             propor = c(propor_trt, propor_ctrl))
}


#' Dynamically produce the code for the "Viz by Subgroup" tab
#'
#' \code{flexPlot.byStratum()} takes a vector of plot names
#' (as strings) and generates the "Viz by Subgroup"
#' tabPanel and placeholders for the constituent plots.
#'
#' @param varNames A vector of plot names to be displayed.
#' @param reset A logical scalar. Should the tab be generated
#'   with no constituent plots?
#'
#' @return A string such that, once evaluated, will generate the
#'   "Viz by Subgroup" tab.
flexPlot.byStratum <- function(varNames="", reset=FALSE) {

  firstPart  <- 'tabPanel("Viz by Subgroup", value="vizSubgroup", br(), '
  part2      <- 'p("Display of the distribution of a specific covariate,
                   across subgroups."),'
  part3      <- 'p("Allows for user to see individual figures for each covariate."),'
  part35     <- 'p("Note: If there are no plots for the selected subgroup, this means there
                       are zero observations in that subgroup."),'
  part4      <- 'fluidRow(column(5, uiOutput("subgroupSelect"), offset=3)),'
  part5      <- 'fluidRow(column(9, textOutput("subgroupSelectInfo"), offset=3)),'
  part7      <- 'uiOutput("subgroupSelect.covars"),'
  lineBreak  <- 'br(),'
  backToTop  <- "HTML('<a href=\"#\" class=\"backToTop\"><back to top of page></a>')"
  lastPart   <- ')'

  selectAll   <- 'div(actionButton("selectAll",  "Select all"),  style="display:inline-block;"),'
  selectNone  <- 'div(actionButton("selectNone", "Select none"), style="display:inline-block;"),'

  varsToPlot <- paste0('ggvis::ggvisOutput("', varNames, '"),', collapse=" ")

  tabPanel0 <- paste0(firstPart,
                      part2,
                      part3,
                      part35,
                      part4,
                      part5,
                      lineBreak,
                      part7,
                      selectAll,
                      selectNone,
                      lineBreak,
                      lineBreak)

  if(reset == TRUE) {
    return( paste0(tabPanel0, backToTop, lastPart) )
  } else {
    return( paste0(tabPanel0, varsToPlot, backToTop, lastPart) )
  }
}


#' Generates figures for 'Viz by Subgroup' tab
#'
#' \code{vizBySubgroup()} is an internal function that
#' takes a vector of covariate data, a vector of (binary) treatment
#' assignments, and generates a plot.
#'
#' @param covarData A vector of numeric covariate data to plot.
#' @param trt A binary numeric vector that indicates each
#'   observation's treatment status (0 or 1).
#' @param covarName A string that contains the name of the covariate
#'   being plotted (for annotating the plot).
#' @param plotHeight A scalar integer that defines the height of
#'   the plot; passed to \code{ggvis()}.
#' @param plotWidth A scalar integer that defines the width of
#'   the plot; passed to \code{ggvis()}.
#' @param trtColor The color of treated observations in the bar
#'   chart. This argument is passed to \code{ggvis()} and can be
#'   any color specification that's acceptable by R (e.g.,
#'   see \href{https://www.stat.ubc.ca/~jenny/STAT545A/block14_colors.html}{here}
#'   for more information).
#' @param ctrlColor The color of control observations in the bar
#'   chart. This argument is passed to \code{ggvis()} and can be
#'   any color specification that's acceptable by R (e.g.,
#'   see \href{https://www.stat.ubc.ca/~jenny/STAT545A/block14_colors.html}{here}
#'   for more information).
#' @param continuous A logical scalar, indicating whether the provided
#'   data is from a continuous distribution.
#'
#' @return If both of the logical arguments are false, the output
#'   will be a \strong{plotly} object containing an empty plot.
#'   Otherwise, the output will be a \strong{ggplot} object
#'   containing the plot.
#'
#' @family plotting functions
vizBySubgroup <- function(covarData,
                          trt,
                          covarName,
                          plotHeight = 200,
                          plotWidth  = 300,
                          trtColor   = "#ffec5c",
                          ctrlColor  = "#e1315b",
                          continuous) {
  if (continuous) {
    # if the var is continuous, generate a histogram
    p <- data.frame(covarData, trt) %>%
           transform(trt=factor(trt, levels=1:0, labels=c("treated", "control")))  %>%
           ggvis::ggvis(~covarData, fill = ~factor(trt), fillOpacity := 0.7) %>%
           group_by(trt) %>%
           layer_histograms() %>%
           #layer_histograms(stack=FALSE) %>%
           add_axis("x", ticks=5, title=covarName) %>%
           scale_nominal("fill", range = c(trtColor, ctrlColor)) %>%
           set_options(width = plotWidth, height = plotHeight) %>%
           add_legend(scales="fill", title="")
  } else {
    # otherwise, generate a barchart
    p <- data.frame(covarData, trt) %>%
           collapseProp() %>%
           ggvis::ggvis(~values, ~propor, fill = ~factor(trt), fillOpacity := 0.7) %>%
           #layer_bars(stack=FALSE) %>%
           layer_bars() %>%
           add_axis("x", title = paste0(covarName, " levels"), title_offset = 30) %>%
           add_axis("y", title = "relative proportions", title_offset = 45, ticks=6) %>%
           scale_numeric("y", domain = c(0,1), clamp = TRUE, nice = FALSE) %>%
           scale_nominal("fill", range = c(trtColor, ctrlColor)) %>%
           set_options(width = plotWidth, height = plotHeight) %>%
           add_legend(scales="fill", title="")
  }

  return(p)
}
