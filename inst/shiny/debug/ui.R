# # # # # # # # # # # # # # # # # # #
# # ui.R                            #
#   User-interface script that      #
#   controls the layout and         #
#   appearance of the app.          #
# # # # # # # # # # # # # # # # # # #

source("ui_utils.R", local = TRUE)

# - - - - - - - - - - - - - - - - - - -

tagList(
  tags$noscript(
    style = "color: red; font-size: 30px; text-align: center;",
    "Please enable JavaScript to use ShinyStan."
  ),

  #shinyjs::useShinyjs(), # use traditional JavaScript tools

  withMathJax(), # load MathJax JavaScript library

fluidPage(

  #theme = shinythemes::shinytheme("paper"),

  titlePanel("hetviz: Treatment Effect Heterogeneity visualization"),

  fluidRow(
    column(10, offset = 1,
      "Introductory text, including information on the intended
       audience and how to use the application (including structure
       of user-provided data).",
       br(), br(),
       "Required packages are shiny, shinyjs, ggplot2, and plotly."
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
        condition = "input.simData == -1",
        p("Variables are simulated from the estimated correlation
           structure of a real data set.")
      ), # - - - end conditionalPanel() (for complex simulated data)

      conditionalPanel(
        condition = "input.simData == 0",
        fileInput("userDataStrat",
                  label = p("Load pre-grouped data below."),
                  accept = c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv'))
      ), # - - - end conditionalPanel() (for user-provided data)

      hr(),

      helpText("Once options have been selected above, press the
                button below."),

      div(actionButton("start", HTML("<a href=\"#\" class=\"button\">Generate vizualizations</a>"),
                       icon=icon("picture", lib='glyphicon')),
          style="float:right;"
      ), # - - - end div(actionButton())

      div(actionButton("reset", HTML("<a href=\"#\" class=\"button\">Reset</a>"),
                       icon=icon("remove-sign", lib='glyphicon')),
          style="float:right;"
      ) # - - - end div(actionButton())

    ), # - - - end sidebarPanel()

  	# # - - mainPanel contents - - # #
    mainPanel(

       tabsetPanel(
         tabPanel("Data Preview", value="dataPrev", br(),

         "test about user data + requirements... space to enter names of outcome,
         treatment, etc",
         
         DT::dataTableOutput("datasetUserData")

         #conditionalPanel(
         #  condition = "input.simData == 0",
         #  dataTableOutput("datasetUserData")
         #)

         ), # - - - end "Data Preview" tab

         tabPanel("Forest Plot", value="forestPlot", br(),

           conditionalPanel(
              condition = "!input.jitter && !input.displayBoxplot",
              id = "errorMsg-forestPlot",
              "Please select valid plotting options."
              ),

            plotly::plotlyOutput("forestPlot"),
            br(),
            helpText("Customize your plot:"),
            checkboxInput("jitter", label = "Add jittered ITEs", value = TRUE),
            checkboxInput("displayBoxplot", label = "Display boxplot,
                           summarizing empirical distribution of subgroup ITEs",
                           value = TRUE)

         ), # - - - end "Forest Plot" tab

         tabPanel("Subgroup Profiles", value="subgroupProfiles", br(),

            p("Display of the covariate profiles, by subgroup."), br(),

            p("Allows for user to compare profiles across subgroups."),

            conditionalPanel(
              condition = "!input.SPplotPts && !input.SPplotLines",
              id = "errorMsg-subgroupProfiles",
              "Please select valid plotting options."
            ),

            plotly::plotlyOutput("subgroupProfiles"),

            br(),
            helpText("Customize your plot:"),
            checkboxInput("SPplotPts", label = "Plot points", value = TRUE),
            checkboxInput("SPplotLines", label = "Plot connecting lines",
                          value = TRUE)

         ), # - - - end "Subgroup Profiles" tab

         eval(
           parse(text = flexPlot.byStratum(paste0("covar", 1:MAXPLOTS)))
         ), # - - - end "Viz by Subgroup" tab
            # - - - (see vizBySubgroup-plotFcn.R)

         tabPanel("Covariate Profiles", value="covarProfiles", br(),

            p("Display of the covariate profile, by subgroup."),

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

            p("Display of the distribution of individual covariates,
               in a particular subgroup."),

            p("Allows for user to see individual figures for each covariate."),

            conditionalPanel(
              condition = "!input.VCPplotPts && !input.VCPplotLines",
              id = "errorMsg-vizByCovar",
              "Please select valid plotting options."
            ),

            fluidRow(column(8, uiOutput("covarSelect"), offset=3)),

            plotly::plotlyOutput("vizByCovar"),

            br(),
            helpText("Customize your plot:"),
            checkboxInput("VCPplotPts", label = "Plot points", value = TRUE),
            checkboxInput("VCPplotLines", label = "Plot connecting line",
                          value = FALSE)

         ), # - - - end "Viz by Covar" tab

         tabPanel("FAQ", value="FAQ", br(),

            p("More details instructions, tutorials, and FAQ."), br(),

            p("Contents (with links to the specific section)."), br(),

            p("(More in-depth explanations will link out to an external page.)")

         ), # - - - end "FAQ" tab

         id="viz"
       ), # - - - end tabsetPanel()

      br(),

      hr(),

      br()

    ) # - - - end mainPanel()


  ) # - - - end sidebarLayout()
)  # - - - end fluidPage()
)  # - - - end tagList()
