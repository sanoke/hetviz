#' Generates figure for 'Forest Plot' tab
#'
#' \code{forestPlot()} is an internal function that
#' takes a dataset and returns a plot.
#'
#' @param ds Any object that can be coerced into a \code{data.frame},
#'   that contains data needed for plotting. This dataset is
#'   of a very specific structure, as
#'   defined within \code{dataCompat()}.
#' @param displayMedian A logical scalar. In the case of subgroup-level
#'   plotting (\code{indivPlot = FALSE}), should the median treatment
#'   effect for
#'   each subgroup be displayed?
#' @param displayError A logical scalar. In the case of subgroup-level
#'   plotting (\code{indivPlot = FALSE}), should the error bars for
#'   each subgroup be displayed?
#' @param displayLine A logical scalar. In the case of subgroup-level
#'   plotting (\code{indivPlot = FALSE}), should a line connecting
#'   the median from
#'   each subgroup be displayed?
#' @param displayMarMean A logical scalar. In the case of subgroup-level
#'   plotting (\code{indivPlot = FALSE}), should a vertical line denoting
#'   the marginal mean be displayed?
#' @param displayBoxplot A logical scalar. In the case of individual-level
#'   plotting (\code{indivPlot = TRUE}), should the boxplot for
#'   each subgroup be displayed?
#' @param jitter A logical scalar. In the case of individual-level
#'   plotting (\code{indivPlot = TRUE}), should the plot contain the jittered
#'   individual-level data (typically estimated ITEs?)
#' @param col.outcome A scalar integer, indicating the column of the \code{ds}
#'   that contains the outcome.
#' @param col.trt A scalar integer, indicating the column of the \code{ds}
#'   that contains the (binary) treatment.
#' @param col.ITE A scalar integer, indicating the column of the \code{ds}
#'   that contains the estimated ITEs.
#' @param col.grp A scalar integer, indicating the column of the \code{ds}
#'   that contains the grouping indicator.
#' @param simData A logical scalar, indicating whether the data is the
#'   simple simulated data
#'   (\code{TRUE}) or is more complex (\code{FALSE}).
#' @param indivPlot A logical scalar, indicating whether to generate a plot
#'   at the individual level (\code{TRUE}) or at the subgroup level
#'   (\code{FALSE}).
#'
#' @return If both of the logical arguments are false, the output
#'   will be a \strong{plotly} object containing an empty plot.
#'   Otherwise, the output will be a \strong{ggplot} object
#'   containing the plot.
#'
#' @section Details:
#'   This function calls \code{shiny::incProgress()}, which is used
#'   to increment the progress bar within the UI.
#'
#' @family plotting functions
#' 
#' @export
forestPlot <- function(ds,
                       displayMedian    = TRUE,
                       displayError     = TRUE,
                       displayLine      = TRUE,
                       displayMarMean   = TRUE,
                       displayBoxplot   = TRUE,
                       jitter           = TRUE,
                       col.outcome, col.trt, col.ITE, col.grp,
                       simData,
                       indivPlot        = TRUE) {


  # constant that defines what is considered a 'large' number of groups
  NUMGRPS <- 30

  # constant that defines what is considered a 'large' number of observations
  N       <- 5000

  # determine what kind of plot to generate, based on
  #   the structure of the dataset
  if ( nlevels(as.factor(ds$estGrp)) < NUMGRPS & nrow(ds) < N ) {
    # -- if the dataset is appropriate for a small plot and
    #    that's what the user wants, that's what the user gets!
    if (!is.null(indivPlot)) {
      smallPlot <- indivPlot
    } else { smallPlot <- FALSE }

  } else { smallPlot <- FALSE }

  # -- if the user hasn't specified valid options for the plot being
  #    generated, quit early
  if ( smallPlot ) {
     if(!displayBoxplot & !jitter) { return(plotly::plotly_empty()) }
  } else {
    if (!displayMedian & !displayError & !displayLine) { return(plotly::plotly_empty()) }
  }

  # updating the progress bar
  incProgress(0.10, detail = "Deciding type of plot")

  # plotting strategy depends on whether there are a large number of subgroups,
  #   whether there is a large number of observations,
  #   and whether the user wants plotting at the individual level
  if ( smallPlot ) {
  	  p <- forestPlotGen.fewGroups(ds,
                                   displayBoxplot  = displayBoxplot,
                                   jitter          = jitter,
  	                               simData         = simData)

  } else {
  	  p <- forestPlotGen.manyGroups(ds,
                                   displayMedian  = displayMedian,
                                   displayError   = displayError,
                                   displayLine    = displayLine,
                                   displayMarMean = displayMarMean)
  }

  # updating the progress bar
  incProgress(0.10, detail = "Deciding type of plot")

  return(p)
}


#' Generates individual-level figure for 'Forest Plot' tab
#'
#' \code{forestPlotGen.fewGroups()} is an internal function that
#' takes a dataset and returns a plot.
#'
#' @param ds A dataset of a very specific structure, as
#'   defined within \code{dataCompat()}
#' @param simData A logical scalar, indicating whether the data is the
#'   simple simulated data
#'
#' @inheritParams forestPlot
#'
#' @return If both of the logical arguments are false, the output
#'   will be a \strong{plotly} object containing an empty plot.
#'   Otherwise, the output will be a \strong{ggplot} object
#'   containing the plot.
#'
#' @section Details:
#'   This function calls \code{shiny::incProgress()}, which is used
#'   to increment the progress bar within the UI.
#'
#' @family plotting functions
forestPlotGen.fewGroups  <- function(ds,
                                     displayBoxplot = TRUE,
                                     jitter         = TRUE,
                                     simData) {

  # updating the progress bar
  incProgress(0.10, detail = "Creating underlying plot structure")

  # construction of the forest plot
  p <- ggplot(ds, aes(x=estGrp, y=mmt, group=estGrp, fill=estGrp, color=estGrp)) +
         guides(fill=FALSE) +
         coord_flip() +
         theme_classic() +
         theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
               # axis.title.y = element_blank(),
               axis.line.x = element_line(lineend="round"),
               legend.position="none") +
    	 scale_x_discrete(limits = rev(levels(ds$estGrp))) +
	     xlab("Treatment Effect") +
         ylab("Estimated Subgroup") +
         ylim(min(ds$mmt)-0.1*min(ds$mmt), max(ds$mmt)+0.1*max(ds$mmt))

  if( displayBoxplot == TRUE ) {

    # updating the progress bar
    incProgress(0.30, detail = "Constructing boxplots")

  	p <- p + geom_boxplot(alpha=ifelse(jitter, 0.2, 0.5),
  	                      lwd=0.2, outlier.size=NA, notch=FALSE)
  }

  if (jitter == TRUE) {

    # updating the progress bar
    incProgress(0.30, detail = "Constructing jittered points")

  	p <- p + geom_jitter(width=0.2, pch=21, stroke=0.3, color="black")
  }

  # so the vertical lines are the last layer (for simple simulated data)
  if (simData == 1) {

    # updating the progress bar
    incProgress(0.5, detail = "Adding horizontal guide lines")

    p <- p + geom_hline(yintercept=c(1,2,5,6,9,10), linetype="dotted", lwd=0.1, alpha=0.7)
  }


  # convert into a plotly object, then hide outliers
  # (because adding jittered points on top)
  # https://community.plot.ly/t/ggplotly-ignoring-geom-boxplot-outlier-parameters/2247
  p <- plotly::plotly_build(p)

  # if adding jittered points, hide the boxplot outliers (because they're redundant).
  # to hide the outliers, we need to select the layer that contains them...
  # but the layer index depends on whether there are other elements in the plot.

  # updating the progress bar
  incProgress(0.10, detail = "Final touches")

  if (jitter == TRUE & displayBoxplot == TRUE) {

    p$x$data[1:length(p$x$data)/2] <- lapply(p$x$data[1:length(p$x$data)/2],
                                             FUN = function(x){ x$marker = list(opacity = 0); return(x) })

  } else if (jitter == TRUE & displayBoxplot == FALSE) {

  	# do nothing

  } else if (jitter == FALSE & displayBoxplot == TRUE) {

    errorMsg <- " "

  	# otherwise, beautify outliers (default is an ugly black outline)
    p$x$data[1:length(p$x$data)] <- lapply(p$x$data[1:length(p$x$data)],
                                           FUN = function(x){ x$marker = list(opacity = 0.5); return(x) })
    # **BUG** temp fix for the vertical "true ITE" lines (will come back to this...)
    p$x$data[length(p$x$data)]   <- lapply(p$x$data[length(p$x$data)],
                                            FUN = function(x){ x$marker = list(opacity = 0); return(x) })

  }

  return(p)
}



#' Generates individual-level figure for 'Forest Plot' tab
#'
#' \code{forestPlotGen.manyGroups()} is an internal function that
#' takes a dataset and returns a plot.
#'
#' @param ds A dataset of a very specific structure, as
#'   defined within \code{dataCompat()}.
#'
#' @inheritParams forestPlot
#'
#' @return If an untenable combination
#'   of the logical arguments are false, the output
#'   will be a \strong{plotly} object containing an empty plot.
#'   Otherwise, the output will be a \strong{ggplot} object
#'   containing the plot.
#'
#' @section Details:
#'   This function calls \code{shiny::incProgress()}, which is used
#'   to increment the progress bar within the UI.
#'
#' @family plotting functions
forestPlotGen.manyGroups <- function(ds,
                                     displayMedian  = TRUE,
                                     displayError   = TRUE,
                                     displayLine    = FALSE,
                                     displayMarMean = TRUE) {

  grpNums <- unique(ds$estGrp)

  # updating the progress bar
  incProgress(0.10, detail = "Calculating plotting quantities")

  # generate statistics for plotting
  plotDataStats <- lapply( grpNums , groupQuantiles, ds = ds )

  # updating the progress bar
  incProgress(0.20, detail = "Formatting plotting quantities")

  plotData0     <- matrix(unlist( plotDataStats , use.names = FALSE),
                          byrow    = TRUE,
                          ncol     = 3,
                          dimnames = list(NULL,c("Q25","Q50","Q75")))

  plotData0     <- data.frame( estGrp = rank(-plotData0[,"Q50"]) ,
                               plotData0 ,
                               whisker = 1.5 * (plotData0[,"Q75"] - plotData0[,"Q25"]) )

  # updating the progress bar
  incProgress(0.10, detail = "Creating underlying plot structure")

  # generate the plot
  # (note: plotting this way because error is only added on the y axis)
  p <- ggplot( plotData0 , aes(y = Q50, x = estGrp) ) +
         coord_flip() +
         guides(fill=FALSE) +
         theme_classic() +
         theme(axis.line.y  = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y  = element_blank(),
               axis.line.x  = element_line(lineend="round"),
               legend.position="none") +
    	 #scale_x_discrete(limits = rev(levels(as.factor(plotData0$estGrp)))) +
    	 xlab("subgroup treatment effect (median)") +
    	 ylab("subgroup") + 
    	 ylim(with(plotData0, min(Q25-whisker)), with(plotData0, max(Q75+whisker)))
    	 


   # add the marginal mean (if the user wants it)
   if (displayMarMean == TRUE) {

     # updating the progress bar
     incProgress(0.05, detail = "Adding marginal mean")

   	 p <- p + geom_hline(yintercept = mean(ds$mmt), color="blue", size=0.1)
   }

   # add user specifications to the plot
   if (displayError == TRUE) {

     # updating the progress bar
     incProgress(0.10, detail = "Adding error bars")

   	 p <- p + geom_segment(aes(y=Q25, yend=Q75, x=estGrp, xend=estGrp),
   	                       size = 0.5) +
   	          geom_segment(aes(y=Q25-whisker, yend=Q75+whisker, x=estGrp, xend=estGrp),
   	                       size = 0.1)
   }

   if (displayLine == TRUE) {

     # updating the progress bar
     incProgress(0.10, detail = "Adding connecting line")

   	 p <- p + geom_line(color="red")
   }

   if (displayMedian == TRUE) {

     # updating the progress bar
     incProgress(0.10, detail = "Adding subgroup medians")

   	 p <- p + geom_point(color="red", size=1/log(length(grpNums)), shape=21)
   }

   return(p)

}
