devtools::load_all(pkg = system.file(package = "hetviz"))

MAXPLOTS <- 50 # max number of covariate placeholder plots

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