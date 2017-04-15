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

# - - - - - - - - - - - - - - - - - - -

function(input, output, session) {

  # initialization
  ds       <<- NULL  # analytic dataset
  cols.cts <<- NULL  # column indices of cts vars
  cols.ctg <<- NULL  # column indices of categorical vars

  # hide visualization tabs (to start)
  shinyjs::hide(selector = "#viz li a[data-value=forestPlot],
                            #viz li a[data-value=subgroupProfiles],
                            #viz li a[data-value=vizSubgroup],
                            #viz li a[data-value=covarProfiles],
                            #viz li a[data-value=vizCovar]")
  # hide data preview (to start)
  shinyjs::hide(id = "datasetSimData")
  #shinyjs::hide(id = "datasetUserData")

  # output$datasetUserData
  # - displays user-provided data in tabular form

  userData <- eventReactive(input$userDataStrat, {

    print("test1")

    inFile <- input$userDataStrat

    print(input$userDataStrat)

    if (is.null(inFile)) { print("NULL inFile"); return(NULL) }

    print("test3")

    print(inFile$datapath)

    print("test4")

    ds <<- readr::read_delim(inFile$datapath,
                             delim = ',',
                             col_names = TRUE)

    print("test5")

    print(head(ds))

    print("test6")

    # display data preview
    #shinyjs::show(id="datasetUserData")

    print("test7")

  })


  output$datasetUserData <- DT::renderDataTable({

    userData()
    
    print(head(ds))
    
    ds %>%
      DT::datatable(caption = "caption0",
                    options = list( lengthMenu = c(5, 10, 25, 50, 100),
                                    pageLength = 5,
                                    style      = "bootstrap",
                                    searching  = FALSE) ) %>%
      DT::formatRound(columns = cols.cts, digits = 3)
    
  }) # - - - end output$datasetUserData

}
