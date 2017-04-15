#' Generates figure for 'Subgroup Profiles' tab
#'
#' \code{subgroupProfiles()} is an internal function that
#' takes a dataset and returns a plot.
#'
#' @param ds Any object that can be coerced into a \code{data.frame},
#'   that contains data needed for plotting. This dataset is
#'   of a very specific structure, as
#'   defined within \code{dataCompat()}.
#' @param SPplotPts A logical scalar. Should the plot include points?
#' @param SPplotLines A logical scalar. Should the plot include
#'   connecting lines?
#' @param col.outcome A scalar integer, indicating the column of \code{ds}
#'   that contains the outcome.
#' @param col.trt A scalar integer, indicating the column of \code{ds}
#'   that contains the (binary) treatment.
#' @param col.ITE A scalar integer, indicating the column of \code{ds}
#'   that contains the estimated ITEs.
#' @param col.grp A scalar integer, indicating the column of \code{ds}
#'   that contains the grouping indicator.
#' @param cols.covars A integer vector, indicating the columns of \code{ds}
#'   that contain covariates (i.e., excluding treatment, outcome,
#'   grouping var, and estimated ITE (if provided)).
#' @param cols.cts A integer vector, indicating the columns of \code{ds}
#'   that contain continuous variables (including the treatment and outcome
#'   variables, if applicable).
#' @param cols.ctg A integer vector, indicating the columns of \code{ds}
#'   that contain polytomous variables (including the treatment and outcome
#'   variables, if applicable).
#' @param simData A logical scalar, indicating whether the data is the
#'   simple simulated data
#'   (\code{TRUE}) or is more complex (\code{FALSE}).
#'
#' @return If both of the logical arguments are false, the output
#'   will be a \strong{plotly} object containing an empty plot.
#'   Otherwise, the output will be a \strong{ggplot} object
#'   containing the plot.
#'
#' @family plotting functions
subgroupProfiles <- function(ds,
                             SPplotPts   = TRUE,
                             SPplotLines = TRUE,
                             SPcovarGuideLines = FALSE,
                             col.outcome, col.trt, col.ITE, col.grp, cols.covars,
                             cols.cts, cols.ctg,
                             simData) {

  # -- if the user hasn't specified valid options for the plot being
  #    generated, quit early
  if (!SPplotPts & !SPplotLines) { return(plotly::plotly_empty()) }
  
  # updating the progress bar
  incProgress(0.10, detail = "Calculating marginal means and standard errors")

  cols.plotVars <- c(col.outcome, col.trt, cols.covars)
  namesPlotVars <- c("outcome", "treatment", names(ds)[cols.covars])
  plotVars      <- ds[,cols.plotVars]

  numGrp        <- length(unique(ds$estGrp))  # could have used nlevels()
                                              # but I don't want to break anything
  numPlotVar    <- ncol(plotVars)

  covarNames    <- names(plotVars)

  # -- treat the marginal means as parameters, even though
  #    there is some estimation involved
  #    (justify this by the large sample size).
  #
  #    so for each subgroup mean, calculate its distance from
  #    the marginal mean in standard devations.

  # calculate the marginal means and their assoc SEs
  marginalMean  <- colMeans(plotVars)
  marginalSE    <- rep(NA, numPlotVar)
  idx           <- 1   # idx counter for for() loop
  for(k in cols.plotVars) {
    if(k %in% cols.cts) { # if the var is cts
      # pay special attention to how a single column is selected!
      #   because ds is a tibble, extracting a single column
      #   has to be done this way! done in the traditional way
      #   will yield a list.
      marginalSE[idx] <- ifelse( simData, 
                                 sd( ds[,k] )  / sqrt( nrow(ds) ),
                                 sd( ds[[k]] ) / sqrt( nrow(ds) ) )
    } else if(k %in% cols.ctg) { # if the var is binary
      marginalSE[idx] <- sqrt( marginalMean[idx] * (1-marginalMean[idx]) /
                               nrow(ds) )
    } else {
      message(paste0("Unspecified covariate distribution for var",
                     namesPlotVars[idx], "."))
    }
    idx <- idx + 1
  }
  rm(idx)   # done with the counter

  # -- for each group, calculate distance of the subgroup means
  #    from the marginal mean (in standard deviations)
  
  # updating the progress bar
  incProgress(0.10, detail = "Calculating subgroup distance from marginal mean, for each covariate")  

  # each row is a subgroup
  # (the set of var distances from the marginal mean)
  distances <- matrix(nrow=numGrp, ncol=numPlotVar)
  colnames(distances) <- namesPlotVars
  for(j in 1:numGrp) {

    # calculate the covariate means in the subgroup
    subgrpMean <- colMeans( plotVars[ ds$estGrp == j , ] )

    # calculate the distance of each subgroup mean from the
    #  marginal mean, and save the result
    distances[j,] <- (subgrpMean - marginalMean) / marginalSE

  }

  # -- reshape the distance data for plotting
  
  # updating the progress bar
  incProgress(0.10, detail = "Reshaping the distance data for plotting")  

  # y coordinate
  distance <- as.vector(distances)

  # the covariate being plotted
  covar    <- rep( colnames(distances) , each=numGrp )

  # the x coordinate
  # (just an integer for plotting; the x-axis is covariates)
  xCoord   <- rep( 1:ncol(distances) , each=numGrp )

  # the subgroup that the distance value belongs to
  group    <- rep( 1:numGrp , numPlotVar )

  # the marginal mean of each covariate
  # (that distance is calculated from)
  marMean  <- rep( marginalMean , each=numGrp )
  
  plotData <- data.frame(distance = distance,
                         covar = covar,
                         xCoord = as.factor(xCoord),
                         subgroup = as.factor(group),
                         marMean = marMean)
  
  # updating the progress bar
  incProgress(0.10, detail = "Constructing framework for plot")  

  p <- ggplot(plotData, aes(x = xCoord, y = distance,
                            colour = subgroup, group = subgroup)) +
         guides(fill=FALSE) +
         theme_classic() +
         theme(axis.title.x = element_blank(),
               axis.line.x = element_line(lineend="round"),
               axis.text.x = element_text(angle = 60, hjust = 1)) +
         scale_x_discrete(limits = levels(plotData$xCoord),
                          labels = namesPlotVars) +
         ylab("distance (in marginal SEs)")
  
  # setting aesthetics of plot, depending on whether 
  #   the data are simulated
  if (simData) {
    plotOpacity <- 0.8
    lwidth      <- 0.6
    ptSize      <- 1
  } else {
    plotOpacity <- 0.6
    lwidth      <- 0.4
    ptSize      <- 0.5
  }

  if (SPplotPts == TRUE & SPplotLines == TRUE) {
    
    # updating the progress bar
    incProgress(0.10, detail = "Adding points and connecting lines")  

    p <- p + geom_point(alpha = plotOpacity, size = ptSize) + 
             geom_line( alpha = plotOpacity, size = lwidth)

  } else if (SPplotPts == TRUE & SPplotLines == FALSE) {
    
    # updating the progress bar
    incProgress(0.10, detail = "Adding points")  

    p <- p + geom_point(alpha = plotOpacity, size = ptSize)

  } else if (SPplotPts == FALSE & SPplotLines == TRUE) {
    
    # updating the progress bar
    incProgress(0.10, detail = "Adding connecting lines")  

    p <- p + geom_line(alpha = plotOpacity, size = lwidth)

  }
  
  # adding covariate guide lines
  if (SPcovarGuideLines) {
    # updating the progress bar
    incProgress(0.10, detail = "Adding covariate guide lines")  
    
    p <- p + geom_vline(xintercept = plotData$xCoord, 
                        color="black", size=0.1, linetype="dotted") 
    
  }
  
  # margin units are in px; default is 80px and b is for bottom
  p <- plotly::plotly_build(p) %>% plotly::layout(margin = list(b=110))

  return(p)
}
