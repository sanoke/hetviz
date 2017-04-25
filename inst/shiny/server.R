# # # # # # # # # # # # # # # # # # #
# # server.R                        #
#   Server script that contains     #
#   the instructions that the       #
#   computer needs to build the app.#
# # # # # # # # # # # # # # # # # # #

## - - - - - - - - - - - - - - - - - - - - - - - - -
## below: code that's run before the call to shinyServer()...
##        this code is called once (the first time the app is launched)
## - - - - - - - - - - - - - - - - - - - - - - - - -

# source all files in R/
devtools::load_all(pkg = system.file(package = "hetviz"))

# max file upload size is 3GB
options(shiny.maxRequestSize=3*1024^3)

CTGLEVELS <<- 10 # the number of levels such that, more than this,
                 # the covariate is plotted as continuous
invalidVarNames <<- NULL

# -- determines whether a numeric covariate is continuous,
#    based on the global variable defined above
cts <<- function(covar, ctgLevels = CTGLEVELS) {
  return(length(unique(covar)) > ctgLevels)
}

# -- determines whether a given covariate is numeric,
#    a factor, or a string
covarType <<- function(covar) {
  if (is.character(covar)) return("string")
  if (is.factor(covar)) return("factor")
  if (is.numeric(covar)) {
     cts0 <- ifelse(cts(covar), "cts", "ctg")
     return(cts0)
  }
  return(NULL)
}

# - - - - - - - - - - - - - - - - - - -

function(input, output, session) {

  # initialization
  ds          <<- NULL  # analytic dataset
  cols.cts    <<- NULL  # column indices of cts vars
  cols.ctg    <<- NULL  # column indices of categorical vars
  cols.factor <<- NULL
  cols.string <<- NULL
  col.outcome <<- NULL
  col.trt     <<- NULL
  col.grp     <<- NULL
  col.ITE     <<- NULL
  cols.covars <<- NULL

  # hide visualization tabs (to start)
  shinyjs::hide(selector = "#viz li a[data-value=forestPlot],
                            #viz li a[data-value=subgroupProfiles],
                            #viz li a[data-value=vizSubgroup],
                            #viz li a[data-value=covarProfiles],
                            #viz li a[data-value=vizCovar]")
  # hide data preview (to start)
  shinyjs::hide(id = "datasetSimData")

  # what to do when the "start" button is pressed
  observeEvent(input$start, {

  	# if user has selected the 'simple' datagen...
  	if ( input$simData == 1 ) {

  	  if (input$simScenario == 1) {
  	    # confounding and no effect modification
  	  	ds <<- simpleDataA
  	  } else if (input$simScenario == 2) {
  	    # effect modification and no confounding
  	  	ds <<- simpleDataB
  	  } else if (input$simScenario == 3) {
  	    # effect modification and confounding
  	  	ds <<- simpleDataC
  	  } else if (input$simScenario == 4) {
  	    # effect modification and confounding by EMs
  	  	ds <<- simpleDataD
  	  }
  	  
  	  # identify the roles of each variable in the dataset
  	  # (these are by definition; structure of simulated data
  	  #    is known beforehand)
      col.trt     <<- 1
  	  col.outcome <<- 2
  	  col.ITE     <<- 16
  	  col.grp     <<- 15
  	  cols.covars <<- 3:11
  	  
  	  # identify which covars are continuous and which are categorical
  	  cts0       <- apply(ds, 2, cts)
  	  cols.cts  <<- which( cts0)
  	  cols.ctg  <<- which(!cts0)
  	  rm(cts0)

  	  # display visualization tabs
  	  shinyjs::show(selector = "#viz li a[data-value=forestPlot],
  	                            #viz li a[data-value=subgroupProfiles],
  	                            #viz li a[data-value=vizSubgroup],
  	                            #viz li a[data-value=covarProfiles],
  	                            #viz li a[data-value=vizCovar]")
  	  # display data preview
  	  shinyjs::show(id="datasetSimData")
  	}

    # if user has elected to provide their own structured data...
    if ( input$simData == 0 ) {

      # if the user hasn't specified valid variable names, quit early
      if(invalidVarNames) return()

      covarNames <- names(ds)

      # identify the outcome variable
      col.outcome <<- which(covarNames == trimws(as.character(input$outcomeName)),
                           useNames = FALSE)

      # identify the treatment variable
      col.trt     <<- which(covarNames == trimws(as.character(input$treatmentName)),
                            useNames = FALSE)

      # identify the subgroup variable
      col.grp     <<- which(covarNames == trimws(as.character(input$subgroupName)),
                            useNames = FALSE)

      # identify the ITE variable (if any)
      col.ITE     <<- which(covarNames == trimws(as.character(input$ITEname)),
                            useNames = FALSE)

      # identify the covariates and their types
      cols.covars <<- which(!(seq_len(ncol(ds)) %in% c(col.outcome, col.trt, col.grp, col.ITE)),
                            useNames = FALSE)

      # format the data to be compatible with plotting nomenclature
      ds <<- dataCompat(ds,
                        outcome   = col.outcome,
                        treatment = col.trt,
                        estITE    = col.ITE,
                        group     = col.grp)

      # display visualization tabs
      shinyjs::show(selector = "#viz li a[data-value=forestPlot],
                    #viz li a[data-value=subgroupProfiles],
                    #viz li a[data-value=vizSubgroup],
                    #viz li a[data-value=covarProfiles],
                    #viz li a[data-value=vizCovar]")
    }
    
    plotVarIdx    <<- c(col.outcome, col.trt, cols.covars)
    namesPlotVars <<- c("outcome", "treatment", names(ds)[cols.covars])

    # switch to the 'forest plot' tab
    updateTabsetPanel(session, "viz", selected="forestPlot")

  })


  # what to do when the "reset" button is pressed
  observeEvent(input$reset, {

  	# hide visualization tabs
    shinyjs::hide(selector = "#viz li a[data-value=forestPlot],
                              #viz li a[data-value=subgroupProfiles],
                              #viz li a[data-value=vizSubgroup],
                              #viz li a[data-value=covarProfiles],
                              #viz li a[data-value=vizCovar]",
                  anim = TRUE, animType = "fade")

    # hide data preview
    shinyjs::hide(id = "datasetSimData")
    # hide data preview
    shinyjs::hide(id = "datasetUserData")

    # switch to the 'data preview' tab
    updateTabsetPanel(session, "viz", selected="dataPrev")
  })

  userData <- reactive({
    
    if (as.integer(input$userDataType) == 1) {
      # if user is uploading their own data, stored locally
      
      inFile <- input$userDataStrat
      
      if (is.null(inFile)) { return(NULL) }
      
      ds <<- readr::read_delim(inFile$datapath,
                               delim = ',',
                               col_names = TRUE)
      
    } else if (as.integer(input$userDataType) == 2) {
      # if user has provided a URL for their data
      
      if (nchar(input$userDataURL) == 0) { return(NULL) }
      
      ds <<- readr::read_delim(input$userDataURL,
                               delim = ',',
                               col_names = TRUE)
      
    } else if (as.integer(input$userDataType) == 3) {
      # user wants to connect to a SQL database
      
    }



    # identify the type of each covariate
    covarType0       <- apply(ds, 2, covarType)
    cols.cts        <<- which(covarType0 == "cts")
    cols.ctg        <<- which(covarType0 == "ctg")
    cols.factor     <<- which(covarType0 == "factor")
    cols.string     <<- which(covarType0 == "string")
    rm(covarType0)

  })

  # output$datasetUserData
  # - displays user-provided data in tabular form
  output$datasetUserData <- DT::renderDataTable({

    # load user data
    userData()

    # if there are continuous covariates, format them appropriately.
    # otherwise, skip the formatting.
    caption0 <- "Scroll to the left to see all covariates."
    if (is.null(cols.cts) ) {
      ds %>%
        DT::datatable(caption = caption0,
                      options = list( lengthMenu = c(5, 10, 25, 50, 100),
                                      pageLength = 5,
                                      style      = "bootstrap",
                                      searching  = FALSE) )
    } else {
      col.cts0 <-
      ds %>%
        DT::datatable(caption = caption0,
                      options = list( lengthMenu = c(5, 10, 25, 50, 100),
                                      pageLength = 5,
                                      style      = "bootstrap",
                                      searching  = FALSE) ) %>%
                      DT::formatRound(columns = cols.cts, digits = 3)
    }

  }) # - - - end output$datasetUserData


  # output$providedVarsNotFound
  # - checks whether the user has specified variable names
  #   that are actually in the data set.
  output$providedVarsNotFound <- renderText({

    input$treatmentName
    input$outcomeName
    input$subgroupName
    input$ITEname

    errorMsg.validVars <- NULL

    if (is.null(ds)) return(NULL)

    covarNames <- names(ds)

    # check whether a valid value has been provided for the variable names.
    #   TRUE --> the value checks out
    #   otherwise, the invalid variable is highlighted and presented to the user
    validTrt      <- ifelse(covarInDataset(input$treatmentName, covarNames),
                            TRUE,
                            "<span style = \"color:red; font-weight:bold;\">treatment</span>")
    validOutcome  <- ifelse(covarInDataset(input$outcomeName, covarNames),
                            TRUE,
                            "<span style = \"color:red; font-weight:bold;\">outcome</span>")
    validSubgroup <- ifelse(covarInDataset(input$subgroupName, covarNames),
                            TRUE,
                            "<span style = \"color:red; font-weight:bold;\">subgroup</span>")
    validITE      <- ifelse(covarInDataset(input$ITEname, covarNames),
                            TRUE,
                           "<span style = \"color:red; font-weight:bold;\">estimated ITE</span>")

    # if any of the vars are not a logical (i.e., TRUE), there's a problem.
    # below we put together the error text to display to the user
    if ( !is.logical(validOutcome) | !is.logical(validSubgroup) |
         !is.logical(validTrt)     | !is.logical(validITE) ) {
      invalidVarNames <<- TRUE

      errorMsg.validVars <- paste("The name(s) provided for",
                                  "<span style = \"color:red; font-weight:bold;\">[</span>",
                                  if (!is.logical(validTrt)) validTrt else NULL,
                                  if (!is.logical(validOutcome)) validOutcome else NULL,
                                  if (!is.logical(validSubgroup)) validSubgroup else NULL,
                                  if (!is.logical(validITE)) validITE else NULL,
                                  "<span style = \"color:red; font-weight:bold;\">]</span>",
                                   "are not contained in the provided dataset.")
    } else {
      invalidVarNames <<- TRUE
      errorMsg.validVars <- "<span style = \"color:#51962e; font-weight:bold;\">
                              All provided variable names are valid.</span>"
    }

    # check whether the user has provided the requisite var names
    if ( nchar(input$treatmentName) & is.logical(validTrt) &
         nchar(input$outcomeName)   & is.logical(validOutcome) &
         nchar(input$subgroupName)  & is.logical(validSubgroup)  ) {
      invalidVarNames <<- FALSE
    }
    errorMsg.validVars
  })


  # output$start
  # - start button, but dynamically colored
  output$start <- renderUI({

    input$treatmentName
    input$outcomeName
    input$subgroupName

    # construct some dynamic formatting
    if ( abs(as.numeric(input$simData)) == 1 ) {
      # data is simulated so we're always ready
      style0 <- "float:right; background-color: #96f321;"
    } else if ( is.null(invalidVarNames)  ) {
      style0 <- "float:right;"
    } else if ( !invalidVarNames  ) {
      # we're also ready if the user has provided the requisite variable names
      style0 <- "float:right; background-color: #96f321;"
    } else {
      # otherwise, we're not ready
      style0 <- "float:right;"
    }

    div(actionButton("start", HTML("<a href=\"#\" class=\"button\">Generate vizualizations</a>"),
                     icon=icon("picture", lib='glyphicon'), style=style0),
        style=style0)
  }) # - - - end output$covarSelect



  # output$datasetSimData
  # - displays simulated data in tabular form
  output$datasetSimData <- DT::renderDataTable({

    input$start

    if (is.null(ds)) return(NULL)

    isolate({
      if (input$simData == 1 & input$simScenario == 1) {
        caption0 <- "Scroll to the left to see all covariates
                     (simulated under confounding with no effect modification)."
      } else if (input$simData == 1 & input$simScenario == 2) {
        caption0 <- "Scroll to the left to see all covariates
                     (simulated under effect modification with no confounding)."
      } else if (input$simData == 1 & input$simScenario == 3) {
        caption0 <- "Scroll to the left to see all covariates
                     (simulated under effect modification and confounding)."
      } else if (input$simData == 1 & input$simScenario == 4) {
        caption0 <- "Scroll to the left to see all covariates
                     (simulated under effect modification and
                     confounding by effect modifiers)."
      }
    })

    ds %>%
      DT::datatable(caption = caption0,
                    options = list( lengthMenu = c(5, 10, 25, 50, 100),
                                    pageLength = 5,
                                    style      = "bootstrap",
                                    searching  = FALSE) ) %>%
      DT::formatRound(columns = cols.cts, digits = 3)

  }) # - - - end output$datasetSimData


  output$covarNamesForUser <- renderText({

    input$userDataStrat

    # if (is.null(ds)) { print("dataset is empty!") }

    paste(
      paste0("<span class=\"covarBox\">", names(ds), "</span>") ,
      collapse = "  "
    )

  })


  # output$forestPlot
  # - generates the forest plot (assuming ITEs are provided)
  output$forestPlot <- plotly::renderPlotly({

  	input$start
    input$indivPlot

    progressBarMsg <- paste0("Generating forest plot: ")

    withProgress(message = progressBarMsg, value = 0, {
      p <- forestPlot( ds ,
                       displayMedian  = input$displayMedian,
                       displayError   = input$displayError,
                       displayLine    = input$displayLine,
                       displayMarMean = input$displayMarMean,
  	                   jitter         = input$jitter,
  	                   displayBoxplot = input$displayBoxplot,
                       col.outcome, col.trt, col.ITE, col.grp,
                       simData        = as.logical(as.numeric(input$simData)),
                       indivPlot      = as.logical(as.numeric(input$indivPlot)) )
      
      # updating the progress bar
      incProgress(0.10, detail = "Rendering the plot")
      
      return(p)
    })

  }) # - - - end output$forestPlot


  # output$subgroupProfiles
  # - generates one figure with all of the subgroup profiles
  output$subgroupProfiles <- plotly::renderPlotly({

    input$start
    
    progressBarMsg <- paste0("Generating subgroup profiles: ")
    
    withProgress(message = progressBarMsg, value = 0, {
      p <- subgroupProfiles( ds ,
                             SPplotPts   = input$SPplotPts ,
                             SPplotLines = input$SPplotLines,
                             SPcovarGuideLines = input$SPcovarGuideLines,
                             col.outcome, col.trt, col.ITE, col.grp, cols.covars,
                             cols.cts, cols.ctg,
                             simData     = as.logical(as.numeric(input$simData)) )
      
      # updating the progress bar
      incProgress(0.10, detail = "Rendering the plot")
      
      return(p)
    })
    
  }) # - - - end output$subgroupProfiles


  # output$covarProfiles
  # - generates one figure with all of the covariate profiles
  output$covarProfiles <- plotly::renderPlotly({

    input$start
    
    progressBarMsg <- paste0("Generating covariate profiles: ")
    
    withProgress(message = progressBarMsg, value = 0, {

    p <- covarProfiles( ds ,
                        CPplotPts   = input$CPplotPts ,
                        CPplotLines = input$CPplotLines ,
                        col.outcome, col.trt, col.ITE, col.grp, cols.covars,
                        cols.cts, cols.ctg,
                        simData     = as.logical(as.numeric(input$simData)) )
                   
      # updating the progress bar
      incProgress(0.10, detail = "Rendering the plot")
      
      return(p)
    
    })
                   
  }) # - - - end output$covarProfiles


  # output$covarSelect
  # - dropdown that allows the user to select the covariate
  #   they're interested in plotting
  output$covarSelect <- renderUI({
    # dynamically creating the list of options for the UI element;
    #   the list consists of the variable name (as a string)
    #   and the column number in the dataset
    varChoices <- paste0("list(",
                         paste0("\"", namesPlotVars, "\"", "=",
                                plotVarIdx, collapse=", "),
                         ")")
    varChoices <- eval(parse(text=varChoices))
    selectInput("covar", label = h5("Covariate of interest"),
                choices = varChoices,
                selected = plotVarIdx[1])
  }) # - - - end output$covarSelect


  # output$subgroupSelect
  # - numeric input that allows the user to select the subgroup
  #   they're interested in plotting
  output$subgroupSelect <- renderUI({
    numericInput("subgroup", label = h5("Subgroup of interest"), value = 1,
                 min = 1, max = length(unique(ds$estGrp)), step = 1)
  }) # - - - end output$subgroupSelect


  # output$subgroupSelect.covars
  # - a collection of checkboxes that allows the user to select what covariates
  #   to plot (in a particular subgroup)
  output$subgroupSelect.covars <- renderUI({
    # dynamically creating the list of options for the UI element;
    #   the list consists of the variable name (as a string)
    #   and the column number in the dataset

    varCovarChoices <- paste0("list(",
                              paste0("\"", namesPlotVars, "\"", "=",
                                     plotVarIdx, collapse=", "),
                              ")")
    varCovarChoices <- eval(parse(text=varCovarChoices))
    checkboxGroupInput("covarGroup", label = h5("Select covariates to see their empirical distribution
                                                (conditional on the selected subgroup)."),
                       choices = varCovarChoices,
                       selected = plotVarIdx)
  }) # - - - end output$subgroupSelect.covars


  output$subgroupSelectInfo <- renderText({
    paste0("(Select an integer between 1 and ",length(unique(ds$estGrp)),".)")
  })


  # output$vizByCovar
  # - generates one figure corresponding to the distribution
  #   (across subgroups) of one covariate
  output$vizByCovar <- plotly::renderPlotly({

    input$start
    
    progressBarMsg <- paste0("Generating plot for covariate ", 
                             namesPlotVars[as.integer(input$covar)], "..." )

    withProgress(message = progressBarMsg, value = 0, {

      p <- vizByCovar( ds ,
                       covar0       = as.integer(input$covar) ,
                       VCPplotPts   = input$VCPplotPts ,
                       VCPplotLines = input$VCPplotLines,
                       col.outcome, col.trt, col.ITE, col.grp, cols.covars,
                       cols.cts, cols.ctg,
                       simData     = as.logical(as.numeric(input$simData)) )
                       
    })
    
  }) # - - - end output$vizByCovar


  # -- the block of code within observeEvent() only executes when
  #    input$subgroupSelect changes value
  #    (i.e., the user selects a different subset of data to look at).
  #    the block of code is used to subset the data,
  #    then generate plots (of each covar in that stratum).
  observeEvent(input$subgroup, {

    if(is.null(ds)) return()

    if(input$start == 0) return()

    subgrpData <- dplyr::filter(ds, ds$estGrp == input$subgroup)
    
    # if there is no one in the subgroup, skip the plotting procedures
    if (nrow(subgrpData) == 0) {
      output$vizBySubgrpErr <- renderText("The selected subgroup contains no
                                           observations.")
      return()
    } 

    progressBarMsg <- paste0("Generating plots for Subgroup ", input$subgroup, "..." )

    withProgress(message = progressBarMsg, value = 0, {

      # cycle through all the covariates
      for(k in seq_along(plotVarIdx)) {

        # NOTE from author: because of the way I think shiny is working in the
        #                   background, it's necessary to save data in a
        #                   separate variable before passing it to the
        #                   function.

        plotName   <- paste0("covar", plotVarIdx[k])

        incProgress(1/length(plotVarIdx), detail = paste("Covariate '", namesPlotVars[k], "'"))

        # these plots are generated by ggvis, not plotly, because I don't yet understand
        # the asyncronicity of ggplotly. hopefully in the future I or someone else
        # will develop an analog to bind_shiny().
        # (note this in documentation)
        
        # select out covariate data (depending on whether data is 
        #   contained in a data.frame or a tibble)...
        #   subgrpData may be a tibble (if user-provided),
        #   so the syntax is different
        if( as.logical(as.numeric(input$simData)) ) {
          covarData0 <- subgrpData[,k]
          trt0       <- subgrpData[,col.trt]
        } else {
          covarData0 <- subgrpData[[k]]
          trt0       <- subgrpData[[col.trt]]        
        }
        
        vizBySubgroup(covarData  = covarData0,
                      trt        = trt0,
                      covarName  = namesPlotVars[k],
                      continuous = k %in% cols.cts) %>% 
                      bind_shiny(plotName)

        updateCheckboxGroupInput(session,
                                 inputId  = "covarGroup",
                                 selected = plotVarIdx)
      }

    }) # - - - end withProgress()

  }) # - - - end observeEvent(input$subgroup, ...)


  # the block of code is used to decide whether to show all covariates.
  observeEvent(input$selectAll, {

    updateCheckboxGroupInput(session,
                             inputId  = "covarGroup",
                             selected = plotVarIdx)

  }) # - - - end observeEvent(input$selectAll, ...)


  # the block of code is used to decide whether to show no covariates.
  observeEvent(input$selectNone, {

    updateCheckboxGroupInput(session,
                             inputId  = "covarGroup",
                             selected = character(0))

    # figure out which plots to hide
    plotsToHide <- paste0("covar", plotVarIdx, "-container")
    sapply( plotsToHide , shinyjs::hide , anim = TRUE, animType = "fade")

  }) # - - - end observeEvent(input$selectNone, ...)


  #    the block of code is used to decide which covariates to show or display.
  observeEvent(input$covarGroup, {

    dsColumnIDX <- seq_along(names(ds))

    # figure out which plots to hide and which plots to show
    plotsToShow <-   dsColumnIDX %in% as.numeric(input$covarGroup)
    plotsToShow <- paste0("covar", dsColumnIDX[plotsToShow], "-container")

    plotsToHide <- !(dsColumnIDX %in% as.numeric(input$covarGroup))
    plotsToHide <- paste0("covar", dsColumnIDX[plotsToHide], "-container")

    sapply( plotsToShow , shinyjs::show , anim = TRUE, animType = "fade")
    sapply( plotsToHide , shinyjs::hide , anim = TRUE, animType = "fade")

  }) # - - - end observeEvent(input$covarGroup, ...)

}
