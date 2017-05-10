# # # # # # # # # # # # # # # # # # #
# # ui.R                            #
#   User-interface script that      #
#   controls the layout and         #
#   appearance of the app.          #
# # # # # # # # # # # # # # # # # # #

library(hetviz)

source("ui_utils.R", local = TRUE)

# - - - - - - - - - - - - - - - - - - -

tagList(
  tags$noscript(
    style = "color: red; font-size: 30px; text-align: center;",
    "Please enable JavaScript to use hetviz."
  ),

  shinyjs::useShinyjs(), # use traditional JavaScript tools

  withMathJax(), # load MathJax JavaScript library

fluidPage(

  #theme = shinythemes::shinytheme("paper"),

  includeCSS("css/global.css"),

  titlePanel("hetviz: Treatment Effect Heterogeneity visualization"),

  fluidRow(
    column(10, offset = 1,
      "Instructions for how to use this application can be found in the ",
      a( href   = "https://github.com/sanoke/hetviz/wiki",
         "User Manual",
         target = "_blank"),
      span(".")
    )

  ), # - - - end fluidRow() w/ intro paragraph

  br(),

  br(),

  sidebarLayout(

  	# # - - sidebar contents - - # #
    sidebarPanel(

      radioButtons("simData", label = h4("Data Source"),
        choices = list("Simple simulated data"  =  1,
                       "Complex simulated data" = -1,
                       "User-provided data"     =  0),
        selected = 1), # - - - end radioButtons() (for data source)

      hr(),

      h4("Data Structure"),

      conditionalPanel(
        condition = "input.simData == 1",
        p("A total of 11 variables are generated. The distribution of
           the treatment indicator \\(T\\) and continuous outcome \\( Y \\)
           depend on the scenario selected."),

        radioButtons("simScenario", label = p("Scenario"),
        choices = list("Confounding and no effect modification"     = 1,
                       "Effect modification and no confounding"     = 2,
                       "Effect modification and confounding"        = 3,
                       "Effect modification and confounding by EMs" = 4),
        selected = 2)

      ), # - - - end conditionalPanel() (for simple simulated data)

      conditionalPanel(
        condition = "input.simData == -1",
        p("Variables are simulated from the estimated correlation
           structure of a real data set.")
      ), # - - - end conditionalPanel() (for complex simulated data)

      conditionalPanel(
        condition = "input.simData == 0",
        radioButtons("userDataType", label = h5("Source"),
                     choices = list("Upload from local file" = 1, "Download from URL" = 2,
                                    "Connect to PostgreSQL database" = 3),
                     selected = 1)
      ), # - - - end conditionalPanel() (for user-provided data)
      
      conditionalPanel(
        condition = "input.simData == 0 && input.userDataType == 1",
        fileInput("userDataStrat",
                  label = p("Load pre-grouped data below."),
                  accept = c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv'))
      ), # - - - end conditionalPanel() (for user-uploaded data) 
      
      conditionalPanel(
        condition = "input.simData == 0 && input.userDataType == 2",
        helpText("URL must begin with http://, https://, ftp://, or ftps://"),
        textInput("userDataURL", label = p("URL of dataset"), 
                  value = "Enter URL...")
        
      ), # - - - end conditionalPanel() (for user data via URL) 
      
      conditionalPanel(
        condition = "input.simData == 0 && input.userDataType == 3",
        helpText("Provide connection settings for your database."),
        textInput("userDataDBNAME", 
                  label = "Name of database", 
                  value = ""),
        textInput("userDataHOST", 
                  label = "Host name", 
                  value = ""),
        textInput("userDataPORT", 
                  label = "Port number", 
                  value = ""),
        textInput("userDataUSER", 
                  label = "User name (if needed)", 
                  value = ""),
        textInput("userDataPASSWORD", 
                  label = "Password (if needed)", 
                  value = "")        
      ), # - - - end conditionalPanel() (for user data via PostgreSQL)       

      hr(),

      helpText("Once all options have been determined,
                the appropriate button will become green, indicating that it
                is ready to be pressed."),

      # start button
      uiOutput("start")

      # div(actionButton("reset", HTML("<a href=\"#\" class=\"button\">Reset</a>"),
      #                  icon=icon("remove-sign", lib='glyphicon')),
      #    style="float:right;"
      # ) # - - - end div(actionButton())

    ), # - - - end sidebarPanel()

  	# # - - mainPanel contents - - # #
    mainPanel(

       tabsetPanel(
         tabPanel("Data Preview", value="dataPrev", br(),

         conditionalPanel(
           condition = "input.simData == 0",
           span("Specify the location of the data at left. 
                 If provided locally or via URL, these data should be
                 provided within a flat CSV file; more detail can be found
                 in the "),
           a( href = "https://github.com/sanoke/hetviz/wiki/Data-Provision",
              "User Manual",
              target = "_blank"),
           span(". Upon specification,
              a preview of the data will appear below."),
           p("Use this preview to
              provide the names (case sensitive) of the covariates corresponding to:"),

           htmlOutput("providedVarsNotFound", inline = FALSE),
           br(),

           textInput("treatmentName", label = "the (binary) treatment", value = ""),
           textInput("outcomeName", label = "the outcome", value = ""),
           textInput("subgroupName", label = "the subgroup indicator", value = ""),

           p("If your dataset contains a variable corresponding to each observation's
              estimated ITE, please specify the name of the covariate below."),

           textInput("ITEname", label = "the estimated ITE (if available)", value = ""),

           hr(),

           p("For your convenience, the names of the covariates in the
              dataset are provided below, followed by a preview
              of the dataset contents."),

           htmlOutput("covarNamesForUser", inline=FALSE),

           br(),

           DT::dataTableOutput("datasetUserData")
         ),

         conditionalPanel(
           condition = "input.simData == 1",
           tags$ul(
             tags$li("Confounders \\( ( X_1 , X_2 , X_3, X_4 ) \\overset{i.i.d.}{\\sim} N(0,1)\\)"),
             tags$li("Instrument \\( X_5 \\sim N(0,1) \\)"),
             tags$li("Prognostic variable \\( X_6 \\sim N(0,1)\\)"),
             tags$li("Effect modifiers \\( ( E_1 , E_2 , E_3 ) \\overset{i.i.d.}{\\sim} \\mathrm{Bern}(0.5)\\)")
           )
         ),

         conditionalPanel(
           condition = "input.simData == 1 && input.simScenario == 1",
           tags$ul(
             tags$li("Treatment  \\(T \\sim \\mathrm{Bern}(\\mathrm{expit}( 0.1X_1 - 0.1X_2 + 1.1X_3 - 1.1X_4 + 0.4X_5 ))\\)"),
             tags$li("Outcome \\(Y \\sim N(-3.85 + 5T + 0.5X_1 - 2X_2 - 0.5X_3 + 2X_4 + X_6 , 1)\\)")
           )
         ),

         conditionalPanel(
           condition = "input.simData == 1 && input.simScenario == 2",
           tags$ul(
             tags$li("Treatment  \\(T \\sim \\mathrm{Bern}(\\mathrm{expit}(0.4X_5))\\)"),
             tags$li("Outcome \\(Y \\sim N(-3.85 + 5T + X_6 - E_1 - 2E_3 + TE_1 + 4TE_2 - 4TE_3 , 1)\\)")
           )
         ),

         conditionalPanel(
           condition = "input.simData == 1 && input.simScenario == 3",
           tags$ul(
             tags$li("Treatment  \\(T \\sim \\mathrm{Bern}(\\mathrm{expit}( 0.1X_1 - 0.1X_2 + 1.1X_3 - 1.1X_4 + 0.4X_5 ))\\)"),
             tags$li("Outcome \\(Y \\sim N(-3.85 + 5T + 0.5X_1 - 2X_2 - 0.5X_3 + 2X_4 + X_6 - E_1 - 2E_3 + TE_1 + 4TE_2 - 4TE_3 , 1)\\)")
           )
         ),

         conditionalPanel(
           condition = "input.simData == 1 && input.simScenario == 4",
           tags$ul(
             tags$li("Treatment  \\(T \\sim \\mathrm{Bern}(\\mathrm{expit}(0.1X_1 - 0.1X_2 + 1.1X_3 - 1.1X_4 + 0.4X_5 - 0.1E_1 + 1.1 E_2 - 4E_3))\\)"),
             tags$li("Outcome \\(Y \\sim N(-3.85 + 5T + 0.5X_1 - 2X_2 - 0.5X_3 + 2X_4 + X_6 - E_1 - 2E_3 + TE_1 + 4TE_2 - 4TE_3 , 1)\\)")
           )
         ),
         br(),

         # text to show if user selects effect modification
         conditionalPanel(
           condition = "input.simData == 1 && input.simScenario != 1",
           span("As described above, there are three binary effect modifiers.
                 These three covariates define "),
           strong(" eight subgroups "),
           span("(Group 1, ..., Group 8), with six unique treatment effects
                 among them. As determined by the mean of \\(Y\\) and the eight unique
                 values of \\( ( E_1 , E_2 , E_3 ) \\), "),
           span(" the subgroup-specific average
                 treatment effects (ATEs) are 1, 2, 5, 5, 6, 9, and 10 units respectively
                 (on the risk difference scale).")
         ),

         # text to show if user selects no effect modification
         conditionalPanel(
           condition = "input.simData == 1 && input.simScenario == 1",
           span("As described above, there is no effect modificiation in this scenario. "),
           span("As determined by the mean of \\(Y\\), the average
                 treatment effect (ATE) is 5 units (on the risk difference scale).")
         ),

         conditionalPanel(
           condition = "input.simData == 1",
           br(),
           span("Regardless of the data generation mechanism, the subgroups were estimated using
                 Bayesian Additive Regression Trees (BART) as provided in the "),
           span("bayesTree", style = "font-family: monospace;"),
           span(" package in "),
           span("R", style = "font-family: monospace;"),
           span("(procedure described in Anoke "), em("et al."), span(" (2017))."),
           span("Briefly, a dataset with 1500 independent observations has been generated
                 according to the parameters above, and partitioned into deciles based on
                 the empirical distribution of individual treatment effects (ITEs)."),
           br(),
           br(),
           h5("Data Generation Notes"),
           tags$ol(
             tags$li(
               span("A "),
               strong("prognostic variable"),
               span(" is a covariate associated with the outcome \\(Y\\) only.")
             ),
             tags$li(
               span("An "),
               strong("instrument"),
               span(" is a covariate associated with the treatment \\(T\\) only.")
             ),
             tags$li(
               span("To fill in: Where the treatment and outcome mean coefficients came from.")
             )
           ),
           br(),
           DT::dataTableOutput("datasetSimData"),
           br(),
           br(),
           span("*This data generation mechanism is from:", style = "color: #778899; font-size:0.8em;"),
           br(),
           span("Anoke SC, Normand S-L, Zigler CM (2017). Approaches to treatment effect heterogeneity in the presence of confounding (submitted).",
                style = "color: #778899; font-size:0.8em;")
         )), # - - - end "Data Preview" tab

         tabPanel("Forest Plot", value="forestPlot", br(),
                  
           HTML('This <strong>forest plot</strong> visualization allows 
                you to compare the subgroup-specific 
                treatment effects and identify any 
                general patterns that would suggest heterogeneity.
                <p />
                A description of what you should expect to see can be
                found in the 
                <a href="https://github.com/sanoke/hetviz/wiki/Forest-Plot" target="_blank">
                User Manual</a>.'),

           conditionalPanel(
              condition = "input.indivPlot == 0 && !input.displayMedian && !input.displayError && !input.displayLine",
              id = "errorMsg-forestPlot-grp",
              "Please select valid plotting options."
           ),

           conditionalPanel(
             condition = "input.indivPlot && !input.jitter && !input.displayBoxplot",
             id = "errorMsg-forestPlot-indiv",
             "Please select valid plotting options."
           ),

            plotly::plotlyOutput("forestPlot"),
            br(),

            p("Customize your plot:"),

            radioButtons("indivPlot",
                         label = NULL,
                         choices = c("Individual-level plotting" = 1,
                                     "Subgroup-level plotting"   = 0),
                         selected = 0),
            helpText("Note: Individual-level plotting is only available when
                      estimated individual treatment effects (ITEs) have been
                      provided, for small datasets
                      (<5000) and for a small number of subgroups (<30). It is available
                      for simple simulated data."),

            conditionalPanel(
              condition = "input.indivPlot == 1",
              checkboxInput("jitter", label = "Add jittered ITEs", value = FALSE),
              checkboxInput("displayBoxplot", label = "Display boxplot,
                             summarizing empirical distribution of subgroup ITEs",
                             value = TRUE)
            ),
            conditionalPanel(
              condition = "input.indivPlot == 0",
              checkboxInput("displayMedian",
                            label = "Display subgroup medians",
                            value = TRUE),
              checkboxInput("displayError",
                            label = "Display error bars",
                            value = TRUE),
              checkboxInput("displayLine",
                            label = "Display line, connecting medians", value = FALSE),
              checkboxInput("displayMarMean",
                            label = "Display line indicating the marginal mean",
                            value = TRUE)
            )

         ), # - - - end "Forest Plot" tab

         tabPanel("Subgroup Profiles", value="subgroupProfiles", br(),

            p("Display of profile for each subgroup, allowing for the comparison
               of profiles across subgroups."),

            conditionalPanel(
              condition = "!input.SPplotPts && !input.SPplotLines",
              id = "errorMsg-subgroupProfiles",
              "Please select valid plotting options."
            ),

            plotly::plotlyOutput("subgroupProfiles"),

            br(),
            p("Note that the plot may take several seconds to complete its rendering."),
            
            p("Notes about how color is associated with increasing treatment effect. 
               Maybe add image w/ color scale."),
            
            br(),
            helpText("Customize your plot:"),
            checkboxInput("SPplotPts", label = "Plot points", value = TRUE),
            checkboxInput("SPplotLines", label = "Plot connecting lines",
                          value = TRUE),
            checkboxInput("SPcovarGuideLines", label = "Plot covariate guide lines",
                          value = TRUE)

         ), # - - - end "Subgroup Profiles" tab

         eval(
           parse(text = flexPlot.byStratum(paste0("covar", 1:MAXPLOTS)))
         ), # - - - end "Viz by Subgroup" tab
            # - - - (see vizBySubgroup-plotFcn.R)

         tabPanel("Covariate Profiles", value="covarProfiles", br(),

            p("Display of covariate profiles across subgroups."),

            p("Allows for user to see how the mean of a particular covariate
               changes across subgroups."),

            conditionalPanel(
              condition = "!input.CPplotPts && !input.CPplotLines",
              id = "errorMsg-covarProfiles",
              "Please select valid plotting options."
            ),

            plotly::plotlyOutput("covarProfiles"),

            br(),
            helpText("Customize your plot:"),
            checkboxInput("CPplotPts", label = "Plot points", value = TRUE),
            checkboxInput("CPplotLines", label = "Plot connecting lines",
                          value = TRUE)

         ), # - - - end "Covariate Profiles" tab

         tabPanel("Viz by Covariate", value="vizCovar", br(),

            p("Display subgroup-specific averages of a specific covariate, 
               as a function of the subgroup-specific estimated treatment effect."),

            # conditionalPanel(
            #   condition = "!input.VCPplotPts && !input.VCPplotLines",
            #   id = "errorMsg-vizByCovar",
            #   "Please select valid plotting options."
            # ),

            fluidRow(column(8, uiOutput("covarSelect"), offset=3)),

            plotly::plotlyOutput("vizByCovar"),

            br(),
            helpText("Customize your plot:"),
            checkboxInput("VCPplotPts", label = "Plot points", value = TRUE),
            checkboxInput("VCPplotLines", label = "Plot connecting line",
                          value = FALSE)

         ), # - - - end "Viz by Covar" tab

         id="viz"
       ), # - - - end tabsetPanel()

      br(),

      hr(),

      br()

    ) # - - - end mainPanel()


  ) # - - - end sidebarLayout()
)  # - - - end fluidPage()
)  # - - - end tagList()
