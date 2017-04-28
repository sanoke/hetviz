#' Generate figure for 'Viz by Covariate' tab
#'
#' \code{vizByCovar()} is an internal function that generates a
#' plot containing the covariate profile of a specified
#' covariate.
#'
#' @param ds Any object that can be coerced into a \code{data.frame},
#'   that contains data needed for plotting. This dataset is
#'   of a very specific structure, as
#'   defined within \code{dataCompat()}.
#' @param covar0 A scalar integer, defining which column of the data set
#'   contains the covariate to be plotted.
#' @param VCPplotPts A logical scalar. Should the plot include points?
#' @param VCPplotLines A logical scalar. Should the plot include
#'   a connecting line?
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
#' 
#' @export
vizByCovar <- function(ds,
                       covar0,
                       VCPplotPts   = TRUE,
                       VCPplotLines = FALSE,
                       col.outcome, col.trt, col.ITE, col.grp, cols.covars,
                       cols.cts, cols.ctg,
                       simData) {

  # # -- if the user hasn't specified valid options for the plot being
  # #    generated, quit early
  # if (!VCPplotPts & !VCPplotLines) { return(plotly::plotly_empty()) }

  # updating the progress bar
  incProgress(0.10, detail = "Calculating marginal means and standard errors")
  
  cols.plotVars <- c(col.outcome, col.trt, cols.covars)
  namesPlotVars <- c("outcome", "treatment", names(ds)[cols.covars])
  plotVars      <- ds[,cols.plotVars]
  
  numGrp        <- length(unique(ds$estGrp))  # could have used nlevels()
                                              # but I don't want to break anything
  numPlotVar    <- ncol(plotVars)
  
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

  # calculate the average and standard error in each subgroup, for each covariate
  covarMeans <- covarSEs <- matrix(nrow=numGrp, ncol=numPlotVar)
  colnames(covarMeans) <- colnames(covarSEs) <- namesPlotVars

  # calculate the treatment effect in each group
  TEs <- rep(NA, numGrp)

  xLabels <- NULL
  groupNames <- seq.int(numGrp)
  groupSizes <- summary(ds$estGrp)

  # each row is a subgroup
  for(j in 1:numGrp) {  # for each group...
    
    # updating the progress bar
    incProgress(0.70 / numGrp , 
                detail = paste0("Calculating subgroup distance from marginal mean, for Subgroup ", j))

    # identify who belongs in subgroup j
    subgrp <- plotVars[ ds$estGrp == j , ]
    subgrpSize <- nrow(subgrp)

    # calculate the covariate means in the subgroup
    covarMeans[j,] <- colMeans( subgrp )

    # calculate the TEs in the subgroup
    TEs[j] <- mean( ds$mmt[ ds$estGrp == j ] )

    # calculate SE in each subgroup
    # (which depends on the distribution of the variable)
    idx           <- 1   # idx counter for for() loop
    for(k in cols.plotVars) {  # for each covariate...
      # pay special attention to how a single column is selected!
      #   because ds is a tibble, extracting a single column
      #   has to be done this way! done in the traditional way
      #   will yield a list.
      if(k %in% cols.cts) { # if the covar is cts
        covarSEs[j,idx] <- ifelse( simData, 
                                   sd( subgrp[,idx] ) / sqrt( subgrpSize ),
                                   sd( subgrp[[idx]] ) / sqrt( subgrpSize ) )
          
          
          
          
      } else if(k %in% cols.ctg) { # if the covar is binary
        tempMean <- covarMeans[j,idx]
        covarSEs[j,idx] <- sqrt( tempMean * (1-tempMean) / subgrpSize )
      } else {
        message("Unspecified covariate distribution.")
      }
      idx <- idx + 1
    }
    rm(idx)  # done with the counter
    # if the standard error is very small, round to zero
    covarSEs[j, ][covarSEs[j, ] < 10^(-10)] <- 0

    xLabels <- c(xLabels, substitute("subgrp" ~ groupID ~ "(" ~
                                     n[groupID] ~ "=" ~ groupSize ~ ")",
                                     list(groupID=groupNames[j], groupSize=groupSizes[j])))
  }

  # reshape the data for plotting
  covarMean <- as.vector(covarMeans)
  covarSE   <- as.vector(covarSEs)
  subgroup  <- rep( 1:numGrp , numPlotVar )
  TE        <- rep( TEs , numPlotVar )
  covar     <- rep( namesPlotVars , each=numGrp )
  # xCoord   <- rep( 1:ncol(distances) , each=numGrp )
  # marMean  <- rep( marginalMeans , each=numGrp )

  plotData <- data.frame(covar     = as.factor(covar),
                         subgroup  = as.factor(subgroup),
                         covarMean = covarMean,
                         covarSE   = covarSE,
                         TE        = TE)
  
  # remove unusual data
  plotData <- plotData[ !is.na(plotData$covarMean) ,]
  
  # updating the progress bar
  incProgress(0.10, detail = "Constructing framework for plot")    
  
  # setting aesthetics of plot, depending on whether 
  #   the data are simulated
  if (simData) {
    plotOpacity <- 1
    lwidth      <- 0.8
    ptSize      <- 3
  } else {
    plotOpacity <- 1
    lwidth      <- 0.6
    ptSize      <- 0.8
  }  

  p <- dplyr::filter(plotData, covar == namesPlotVars[covar0]) %>%
         ggplot(aes(x = TE, y = covarMean, group=covar)) +
           geom_hline(yintercept=marginalMean[covar0], color="red", size=1) +
           guides(fill=FALSE) +
           theme_classic() +
           theme(axis.line.x = element_line(lineend="round")) +
           geom_errorbar(aes(ymin = covarMean-covarSE, ymax = covarMean+covarSE),
                         width = 0,                    # width of the error bars
                         #position=position_dodge(0.5),
                         size  = 1,                  # thickness of error bar line
                         alpha = plotOpacity/2,
                         color = "red") +
           ylab(paste0("subgroup-specific average of '", namesPlotVars[covar0], "'")) +
           xlab("subgroup-specific average treatment effect")
  
  if (VCPplotPts == TRUE & VCPplotLines == TRUE) {

    # updating the progress bar
    incProgress(0.10, detail = "Adding points and connecting lines")      
    
    p <- p + geom_point(alpha = plotOpacity, size = ptSize) + 
             geom_line( alpha = plotOpacity, size = lwidth)

  } else if (VCPplotPts == TRUE & VCPplotLines == FALSE) {

    # updating the progress bar
    incProgress(0.10, detail = "Adding points")      
    
    p <- p + geom_point(alpha = plotOpacity, size = ptSize)

  } else if (VCPplotPts == FALSE & VCPplotLines == TRUE) {

    # updating the progress bar
    incProgress(0.10, detail = "Adding connecting lines")     
    
    p <- p + geom_line(alpha = plotOpacity, size = lwidth)

  } 

  return(p)
}
