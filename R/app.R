
# Set up Layout for Application
#' @import shiny
#' @importFrom shinyjs show
#' @importFrom shinyjs hide
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs hidden
#' @importFrom shinyjs html
#' @import plotly
#' @importFrom fields image.plot
#' @importFrom viridis inferno
#' @importFrom grid viewport
#' @importFrom grid grid.polygon
#' @importFrom grid textGrob
#' @importFrom grid grid.legend
#' @importFrom grid gpar
#' @importFrom gridExtra tableGrob
#' @importFrom gridExtra grid.table
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
ui <- fluidPage(
  #theme = bslib::bs_theme(bootswatch = "pulse"),
  shinyjs::useShinyjs(),
  tags$h1("Empirical Frequency Band Analysis"),
  #tags$a(tags$strong(tags$em("Source: Empirical Frequency Band Analysis of Nonstationary Time Series")),
  #       href = "https://www.tandfonline.com/doi/full/10.1080/01621459.2019.1671199"),
  tags$p(tags$em("Research reported in this publication was supported by the National Institute of General Medical Sciences
         of the National Institutes of Health under Award Number R01GM140476. The content is solely the responsibility
         of the authors and does not necessarily represent the official views of the National Institutes of Health")),
  tags$p(tags$strong("Authors: Dylan Ward, Mohit Chhaparia, Kevin Gunalan Antony Michael Raj")), (tags$em(tags$u("Under the guidance of Professor Scott Bruce"))),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==2",
                       radioButtons("type", "Time Series Type", # Selects the type of Simulated Data to look at
                                    choices=c("Univariate","Multivariate", "Functional"),
                                    selected="Univariate"),

                       # Inputs for Simulated, Univariate Data
                       conditionalPanel(condition = "input.type == 'Univariate'",
                       selectInput("Simsetting", "Simulated Example", # Selects the type of Univariate Data to look at
                                   c("White Noise" = "W",
                                     "Linear" = "L",
                                     "Sinusoidal" = "S"), selected="S"),
                       selectInput(inputId="Time", label = "Choose total length of time series (T)",
                                   choices = as.numeric(c(500,1000,5000,10000, 20000, 50000)), selected=1000), # Sets value for T
                       selectInput(inputId="Num", label = "Choose number of observations per approximately stationary block* (N)",
                                   choices=100, selected=100), # Sets value for N
                       selectInput(inputId="Tapers", label="Choose number of tapers to use in multitaper spectral estimator** (K)",
                                  choices=10, selected=10), # Sets value for K
                      selectInput(inputId = "Signi", label="Choose significance level",
                                  choices=as.numeric(seq(0.01,0.05, by=0.01)), selected = 0.05), # Sets value for alpha
                       radioButtons(inputId = "TF", label = "Standardize",
                                    c("True" = TRUE, "False" = FALSE), selected = FALSE), # Lets us Standardize the Variance in each block
                       actionButton("go", label = "Run"), # Runs the Algorithm based on our selected parameters
                      htmlOutput("Res"),
                      htmlOutput("Res1"),
                      htmlOutput("Res2")
                      ),

                      # Inputs for Simulated, Functional Data
                      conditionalPanel(condition = "input.type== 'Functional'",
                                       # Conditionally Displays 3-D Plots
                                       radioButtons("Plot3D", "3D Plots",
                                                    choices=c("Include","Exclude"),
                                                    selected="Include"),
                      selectInput("SimF1", "Simulation Setting",
                                  c("White Noise" = "W",
                                  "Linear" = "L",
                                  "Sinusoidal" = "S"), selected="S"), # Selects the type of Functional Data to look at
                      selectInput(inputId = "TsF1", label="Choose total length of time series (T)",
                                  choices = c( 500, 1000, 2000, 5000), selected=2000), # Sets value for T
                      selectInput(inputId = "RF1", label = "Choose number of points in functional domain (R)",
                                  choices=seq(from=5, to=50, by=5), selected=5), # Sets value for R
                      selectInput(inputId = "NF1", label = "Choose number of observations per approximately stationary block* (N)",
                                  choices = 30, selected = 30), # Sets value for N
                      selectInput(inputId = "KF1", label = "Choose number of tapers to use in multitaper spectral estimator** (K)",
                                  choices = 5, selected = 5), # Sets value for K
                      selectInput(inputId = "RselF1", label = "Choose number of points in the functional domain to use in computing test statistics*** (Rsel)",
                                  choices = c(5,10), selected = 5), # Sets value for RSel
                      selectInput(inputId = "AlphaF1", label="Choose significance level",
                                  choices=as.numeric(seq(0.01,0.05, by=0.01)), selected = 0.05), # Sets value for alpha
                      radioButtons(inputId = "TF_F1", label = "Standardize",
                                   c("True" = TRUE, "False" = FALSE), selected = FALSE), # Lets us Standardize the Variance in each block
                      actionButton("goF1", label = HTML("Run (Warning: <br/> The Algorithm will take a while to run)")), # Runs the Algorithm based on our selected parameters
                      htmlOutput("F1_1"),
                      htmlOutput("F1_2"),
                      htmlOutput("F1_3"),
                      htmlOutput("F1_4")
                      ),

                      # Inputs for Simulated, Multivariate Data
                      conditionalPanel(condition="input.type == 'Multivariate'",
                                       # Conditionally Displays 3-D Plots
                                       radioButtons("Plot3DM", "3D Plots",
                                                    choices=c("Include","Exclude"),
                                                    selected="Include"),
                      selectInput("SimSettingM", "Simulation Setting",
                                  c("White Noise" = "W",
                                    "Linear" = "L",
                                    "Sinusoidal" = "S",
                                    "Linear and Sinusoidal (Mixture)" = "LASM",
                                    "Linear and Sinusoidal (Differing Proportions)" = "LASDP"), selected="S"), # Selects the type of Multivariate Data to look at
                      selectInput(inputId = "TsMv", label="Choose total length of time series (T)",
                                  choices = c( 200, 500, 1000, 2000, 5000), selected=200), # Sets value for T
                      selectInput(inputId = "RMv", label = "Choose the number of components (R)",
                                  choices=seq(from=5, to=50, by=5), selected=10), # Sets value for R
                      selectInput(inputId = "nrepMv", label="Choose the number of repetitions (nrep)",
                                  choices=c(100,500,1000,2000), selected=1000),# Sets value for nrep
                      selectInput(inputId = "WselMv", label="Choose the number of different neighborhood sizes for multiscale approach (Wsel)",
                                  choices=c(1,2,3,4,5), selected=3), # Sets value for Wsel
                      actionButton("goMv", label = "Run"), # Runs the Algorithm based on our selected parameters
                      htmlOutput("Check111")
                      )),

      conditionalPanel(condition = "input.tabselected==1",
                       htmlOutput("Numeric_File"),
                       fileInput("file_csv", "Choose CSV File",
                                 multiple = TRUE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       checkboxInput("header", "Header", TRUE),
                       htmlOutput("Warnings"),
                       htmlOutput("UniVarDis"),
                       htmlOutput("NotUniVar"),
                       htmlOutput("BlankSpace"),

                       # This will pop up if the data chosen has more than one component, letting the user choose what type
                       # of data is being inputted
                       hidden(radioButtons(inputId = "Data_Checker", label = NULL, choices = c("Multivariate", "Functional"), selected=character(0))),

                       # These below buttons will choose whether or not the 3D Plots are displayed, if we have either functional
                       # or multivariate data
                       hidden(radioButtons("Plot3D_File", "3D Plots",
                                    choices=c("Include","Exclude"),
                                    selected="Include")),
                       hidden(radioButtons("Plot3D_FileM", "3D Plots",
                                           choices=c("Include","Exclude"),
                                           selected="Include")),


                       # These 3 outputs correspond to each of the types of data we could analyze, and gives the length/dimensions
                       # of the inputted data
                       htmlOutput("T_len"),
                       hidden(htmlOutput("Ts_Fxn_Dim")),
                       hidden(htmlOutput("Ts_Mv_Dim")),

                       # Inputs for Observed, Univariate Data
                       numericInput(inputId = "Num2", label = "Choose number of observations per approximately stationary block* (N)",
                                   value = NULL, step = 1), # Sets value for N
                       numericInput(inputId = "Tapers2", label = "Choose number of tapers to use in multitaper spectral estimator** (K)",
                                   value = NULL, step = 1), # Sets value for K
                       selectInput(inputId = "Signi2", label="Choose significance level",
                                   choices=as.numeric(seq(0.01,0.05, by=0.01)), selected = 0.05), # Sets value for alpha
                       radioButtons(inputId = "TF2", label = "Standardize",
                                    c("True" = TRUE, "False" = FALSE), selected = FALSE), # Lets us Standardize the Variance in each block

                       # Inputs for Observed, Functional Data

                       # These are hidden, until it is selected that the inputted data is Functional. At that point, these will show, and the other
                       # chunks of inputs will be hidden
                       hidden(numericInput(inputId = "Num_Fxna", label = "Choose number of observations per approximately stationary block* (N)",
                                           value = NULL, step = 1)), # Sets value for N
                       hidden(numericInput(inputId = "Tapers_Fxna", label = "Choose number of tapers to use in multitaper spectral estimator** (K)",
                                           value = NULL, step = 1)), # Sets value for K
                       hidden(selectInput(inputId = "Rsel_Fxna", label = "Choose number of points in the functional domain to use in computing test statistics*** (Rsel)",
                                          choices = c(5,10), selected = 5)), # Sets value for Rsel
                       hidden(selectInput(inputId = "Signi_Fxna", label="Choose significance level",
                                   choices=as.numeric(seq(0.01,0.05, by=0.01)), selected = 0.05)), # Sets value for alpha
                       hidden(radioButtons(inputId = "TF_Fxna", label = "Standardize",
                                    c("True" = TRUE, "False" = FALSE), selected = FALSE)), # Lets us Standardize the Variance in each block
                       hidden(actionButton("go_Fxna", label = HTML("Run (Warning: <br/> The Algorithm will take a while to run)"))), # Runs the Functional Algorithm based on our selected parameters

                       # Inputs for Observed, Multivariate Data

                       # These are hidden, until it is selected that the inputted data is Multivariate At that point, these will show, and the other
                       # chunks of inputs will be hidden
                       hidden(selectInput(inputId = "nrepMv_file", label="Choose the number of repetitions (nrep)",
                                   choices=c(100,500,1000,2000), selected=1000)), # Sets value for nrep
                       hidden(selectInput(inputId = "WselMv_file", label="Choose the number of different neighborhood sizes for multiscale approach (Wsel)",
                                   choices=c(1,2,3,4,5), selected=3)), # Sets value for Wsel
                       hidden(actionButton("go_Mva", label= "Run")), # Runs the Multivariate Algorithm based on our selected parameters

                       actionButton("go2", label = "Run"), # Runs the Univariate Algorithm based on our selected parameters
                       htmlOutput("res9"),
                       htmlOutput("res10"),
                       htmlOutput("res11"),
                       hidden(htmlOutput("Fxn_AA")),
                       hidden(htmlOutput("Fxn_BB")),
                       hidden(htmlOutput("Fxn_CC")),
                       hidden(htmlOutput("Fxn_DD")),
                       hidden(htmlOutput("Mv_AA"))
                       ),


    ),
    mainPanel(
      tabsetPanel(type = "tabs", id = 'tabselected', selected = 1,
                  tabPanel("File Upload", value = 1),
                  tabPanel("Simulation Setting", value=2)), # Conditionally displays either the Simulated or File Upload sides of the app
      conditionalPanel(condition = "input.tabselected==2", # This corresponds to the Simulated Side of the app

                       # Outputs for Simulated, Univariate Data
                       conditionalPanel(condition = "input.type == 'Univariate'",
                       plotlyOutput("Image_Plotb", height=400, width = 1000), # Plots time series
                       plotOutput("Image_Plot", height=600, width=1000), # Plots Local periodogram
                       fluidRow(
                         splitLayout(cellWidths = c(500, 500),
                         plotOutput("summ_out_uni", height = 500), # Tables of Results for partition testing
                         plotOutput("summ_pval_uni", height = 500) # Scatterplot of p-values for partition testing
                         )
                       ),
                       hidden(downloadButton('downloadData','Download the Above Results')) , # Downloads the plots seen into a pdf
                       br(),
                       br(),
                       ),

                       # Outputs for Simulated, Functional Data
                       conditionalPanel(condition = "input.type == 'Functional'",

                                        ####
                        htmlOutput('SimFxn_text'),
                                        ####
                       plotlyOutput("Fxn_Plota", height=400, width=1000), # Plots Data

                       fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                       # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                splitLayout(cellWidths = c(500,333,167),
                                htmlOutput("FxnPlotaDesc") ,
                                hidden(htmlOutput("test12121")),
                                hidden(sliderInput(inputId = "x_F1", min=1, max=10, step=1, value=1,label=NULL, ticks = FALSE))
                                )
                       ),
                       tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),

                       plotOutput("Fxn_Plotb", height=600, width = 1000), # Plots Local periodogram

                       fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                       # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                  splitLayout(cellWidths = c(500,333,167),
                                  htmlOutput("FxnbPlotDesc") ,
                                htmlOutput("Blank2"),
                                hidden(sliderInput("plot1_FxnCheck",min=1,max=10,step=1,value=1,label=NULL, ticks = FALSE))
                                  )
                       ),
                       tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),

                       conditionalPanel(condition = "input.Plot3D == 'Include'",
                       plotlyOutput("Plotly_Fxna", height=600, width=1000), # 3-D Plot of entire data
                       plotlyOutput("Plotly_Fxnb", height=600, width=1000), # 3-D Plot of local periodogram

                       fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                splitLayout(cellWidths = c(500,333,167),
                                htmlOutput("FxnPlot22Desc"),
                                htmlOutput("Blank10100"),
                                hidden(sliderInput(inputId = "q11_F1", min=1, max=10, step=1, value=1, label=NULL, width="125%", ticks=FALSE))
                                )
                       )
                       ),
                       fluidRow(
                         splitLayout(cellWidths = c(500,500),
                         plotOutput("summ_out_fxn", height = 500), # Tables of Results for partition testing
                         plotOutput("summ_pval_fxn", height = 500) # Scatterplot of p-values for partition testing
                         )
                       ),
                       hidden(downloadButton('downloadDataFXN1','Download the Above Results')), # Downloads the plots seen into a pdf
                       br(),
                       br(),
                       ),

                       # Outputs for Simulated, Multivariate Data
                       conditionalPanel(condition = "input.type == 'Multivariate'",

                       ####
                       htmlOutput('SimMv_text'),
                       ####
                       plotlyOutput("Mv_Plota", height=400, width=1000), # Plots Data

                       fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                splitLayout(cellWidths = c(500,333,167),
                                htmlOutput("MvPlotaDesc"),
                                hidden(htmlOutput("mv_testa")),
                                hidden(sliderInput(inputId = "mvX_F1", min=1, max=10, step=1, value=1,label=NULL, ticks = FALSE))
                                )
                       ),
                       tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),

                       plotOutput("Mv_Plotb", height=600, width = 1000), # Plots Local periodogram
                       fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                splitLayout(cellWidths = c(500,333,167),
                                htmlOutput("MvbPlotDesc"),
                                hidden(htmlOutput("mv_testb")),
                                hidden(sliderInput("mvX_F2",min=1,max=10,step=1,value=1,label=NULL, ticks = FALSE))
                                )
                       ),
                       conditionalPanel(condition = "input.Plot3DM == 'Include'",
                       plotlyOutput("Plotly_Mvb", height=600, width=1000), # 3-D Plot of Local periodogram
                       fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                splitLayout(cellWidths = c(500,333,167),
                                htmlOutput("MvPlot22Desc"),
                                hidden(htmlOutput("Mv_10100")),
                                hidden(sliderInput(inputId = "q11_Mv1", min=1, max=10, step=1, value=1, label=NULL, width="125%", ticks=FALSE))
                                )
                       )
                       ),
                       fluidRow(
                         splitLayout(cellWidths = c(500,500),
                         plotOutput("summ_out_Mv", height = 500, width=500), # Tables of Results for partition testing
                         plotOutput("summ_pval_Mv", height = 500, width = 500) # Scatterplot of p-values for partition testing
                         )
                       ),

                       hidden(downloadButton('downloadDataMV1','Download the Above Results')), # Downloads the plots seen into a pdf
                       br(),
                       br(),
      )),
      conditionalPanel(condition = "input.tabselected==1", # This corresponds to the File Upload side of the app

                       # Outputs for Observed, Univariate Data
                       plotlyOutput("Image_Plota", height=400, width=1000), # Plots data
                       plotOutput("Image_Plot2", height=600, width=1000), # Plots local periodogram
                       fluidRow(
                         splitLayout(cellWidths = c(500,500),
                         plotOutput("summ_out_uni_file", height = 500, width=500), # Table of Results for partition testing
                         plotOutput("summ_pval_uni_file", height = 500, width = 500) # Scatterplot of p-values for partition testing
                         )
                       ),
                       hidden(downloadButton('downloadData1','Download the Above Results')), # Downloads the plots seen into a pdf
                       br(),
                       br(),

                       # Outputs for Observed, Functional Data
                       conditionalPanel(condition = "input.Data_Checker== 'Functional'",
                                                      ####
                                        htmlOutput('ObsFxn_text'),
                                                      ####
                                        plotlyOutput("Test_Fxna_Plot1", width=1000), # Plots the data

                                        fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                          # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                          splitLayout(cellWidths = c(500,333,167),
                                          htmlOutput("FxnPlotaDesc_AA"),
                                          htmlOutput("test12121_AA"),
                                          sliderInput(inputId = "x_F1_AA", min=1, max=10, step=1, value=1,label=NULL, ticks = FALSE)
                                          )
                                        ),
                                        tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),

                                        plotOutput("Fxn_Plotb_file", height=600, width=1000), # Plots Local periodogram
                                        fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                 # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                                 splitLayout(cellWidths = c(500,333,167),
                                                 htmlOutput("FxnbPlotDesc_file"),
                                                 htmlOutput("Blank2_file"),
                                                 sliderInput("plot1_FxnCheck_file",min=1,max=10,step=1,value=1,label=NULL, ticks = FALSE)
                                                 )
                                        ),
                                        tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),
                                        conditionalPanel(condition = "input.Plot3D_File == 'Include'",
                                        plotlyOutput("Plotly_Fxna_file", height=600, width=1000), # 3-D Plot of Entire Data
                                        plotlyOutput("Plotly_Fxnb_file", height=600, width=1000), # 3-D Plot of Local periodogram

                                        fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                 # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                                 splitLayout(cellWidths = c(500,333,167),
                                                 htmlOutput("FxnPlot22Desc_file"),
                                                 htmlOutput("Blank10100_file"),
                                                 sliderInput(inputId = "q11_F1_file", min=1, max=10, step=1, value=1, label=NULL,  ticks=FALSE)
                                                 )
                                        ),

                                        ),
                                        fluidRow(
                                          splitLayout(cellWidths = c(500,500),
                                          plotOutput("summ_out_fxn_file", height = 500), # Table of Results for Partition Testing
                                          plotOutput("summ_pval_fxn_file", height = 500) # Scatterplot of p-values for partition testing
                                          )
                                        ),
                                        hidden(downloadButton('downloadDataFXN1_File','Download the Above Results')), # Downloads the plots seen into a pdf
                                        br(),
                                        br()

                       ),

                       # Output for Observed, Multivariate Data
                       conditionalPanel(condition = "input.Data_Checker == 'Multivariate'",
                                                      ####
                                        htmlOutput('ObsMv_text'),
                                                      ####
                                        plotlyOutput("Test_Mva_Plot1", width=1000), # Plots the data
                                        fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                 # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                                 splitLayout(cellWidths = c(500,333,167),
                                                 htmlOutput("MvPlotaDesc_AA"),
                                                 htmlOutput("Mv12121_AA"),
                                                 sliderInput(inputId = "x_Mv_AA", min=1, max=10, step=1, value=1,label=NULL, ticks = FALSE)
                                                 )
                                        ),
                                        tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),
                                        plotOutput("Mv_Plotb_file", height=600, width=1000), # Plots Local periodogram
                                        fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                 # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                                 splitLayout(cellWidths = c(500,333,167),
                                                 htmlOutput("MvbPlotDesc_file"),
                                                 hidden(htmlOutput("Mv2_file")),
                                                 hidden(sliderInput("plot1_MvCheck_file",min=1,max=10,step=1,value=1,label=NULL, ticks = FALSE))
                                                 )
                                        ),
                                        tags$head(tags$style(HTML('.irs-from, .irs-min, .irs-to, .irs-max, .irs-single {
                                                 visibility: hidden !important;
                                                 }' ))),
                                        conditionalPanel(condition = "input.Plot3D_FileM == 'Include'",
                                                         plotlyOutput("Plotly_Mvb_file", height=600, width=1000), # 3-D Plot of Local Periodogram
                                                         fluidRow(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                                  # The 3 lines below give a description of above plot, with a slider to change data seen on plot
                                                                  splitLayout(cellWidths = c(500,333,167),
                                                                  htmlOutput("MvPlot22Desc_file"),
                                                                  hidden(htmlOutput("BlankMv_file")),
                                                                  hidden(sliderInput(inputId = "q11_Mv_file", min=1, max=10, step=1, value=1, label=NULL,  ticks=FALSE))
                                                                  )
                                                         ),

                                        ),
                                        fluidRow(
                                          splitLayout(cellWidths = c(500,500),
                                          plotOutput("summ_out_Mv_file", height = 500), # Table of Results for Partition Testing
                                          plotOutput("summ_pval_Mv_file", height = 500) # Scatterplot of p-values for partition testing
                                          )
                                        ),
                                        hidden(downloadButton('downloadDataMv1_File','Download the Above Results')), # Downloads the plots seen into a pdf
                                        br(),
                                        br()
                                        )
      ),
      )
  ))

# This houses all of the code that will produce the above plots and results
server <- function(input,output, session) {
  output$BlankSpace <- renderText({
    paste("\n")
  })
  # Ensures that Rsel will never be greater than R in the Simulated, Functional portion
  observe({
    RF1 <- as.numeric(input$RF1)
    if(RF1 == 5){
      updateSelectInput(session, "RselF1", choices=c(5), selected=5)
    } else {
      updateSelectInput(session, "RselF1", choices=c(5, 10), selected=5)
    }
  })

  # Generates possible values for N, based on T in the Simulated, Functional Portion
  observe({
    t2 <- as.numeric(input$TsF1)
    if(t2 == 500) {
      choices = c(62, 105)
    } else if(t2 == 1000) {
      choices = c(31, 100, 177)
    } else if(t2 == 5000) {
      choices = c(70, 292, 594 )
    } else if(t2 == 10000) {
      choices = c(100, 464, 1000)
    } else if(t2==2000) {
      choices = c(44, 158, 299)
    } else {
      choices=c(223, 1357, 3343)
    }
    updateSelectInput(session, "NF1", choices = choices, selected = choices[length(choices) - 1])
  })

  # Generates possible values for K, based on N in the Simulated, Functional Portion
  observe({
    t2 <- as.numeric(input$TsF1)
    sc_val <- (as.numeric(input$NF1))
    if(t2 == 500) {
      if(sc_val == 105){
        tap=10
      } else {
        tap=7
      }
    } else if(t2 == 1000) {
      if(sc_val == 31){
        tap=5
      } else if(sc_val == 100) {
        tap=10
      } else {
        tap=13
      }
    } else if(t2 == 5000) {
      if(sc_val == 70){
        tap=8
      } else if(sc_val == 292) {
        tap = c(17, 70)
      } else {
        tap=c(24, 120)
      }
    } else if(t2 == 10000) {
      if(sc_val == 100) {
        tap=10
      } else if(sc_val == 464){
        tap=c(21, 99)
      } else {
        tap=c(31, 177)
      }
    } else if(t2 == 2000){
      if(sc_val == 44) {
        tap=6
      } else if(sc_val == 158) {
        tap=c(12, 44)
      } else {
        tap=c(17, 71)
      }
    } else {
      if(sc_val == 223){
        tap=14
      } else if(sc_val == 1357){
        tap=c(36,223)
      } else {
        tap=c(57, 439)
      }
    }
    updateSelectInput(session, "KF1",choices=tap, selected = tap[1])
  })

  # Generates possible values for N, based on T in the Simulated, Univariate Portion
  observe({
    t2 <- as.numeric(input$Time)
    if(t2 == 500) {
      choices = c(62, 105)
    } else if(t2 == 1000) {
      choices = c(31, 100, 177)
    } else if(t2 == 5000) {
      choices = c(70, 292, 594 )
    } else if(t2 == 10000) {
      choices = c(100, 464, 1000)
    } else if(t2==20000) {
      choices = c(141, 736, 1681)
    } else {
      choices=c(223, 1357, 3343)
    }
    updateSelectInput(session, "Num", choices = choices, selected = choices[length(choices) - 1])
  })

  # Generates possible values for K, based on N in the Simulated, Univariate Portion
  observe({
    t2 <- as.numeric(input$Time)
    sc_val <- (as.numeric(input$Num))
    if(t2 == 500) {
      if(sc_val == 105){
        tap=10
      } else {
        tap=7
      }
    } else if(t2 == 1000) {
      if(sc_val == 31){
        tap=5
      } else if(sc_val == 100) {
        tap=10
        } else {
        tap=13
      }
    } else if(t2 == 5000) {
      if(sc_val == 70){
        tap=8
      } else if(sc_val == 292) {
        tap = c(17, 70)
      } else {
        tap=c(24, 120)
      }
    } else if(t2 == 10000) {
      if(sc_val == 100) {
        tap=10
      } else if(sc_val == 464){
        tap=c(21, 99)
      } else {
        tap=c(31, 177)
      }
    } else if(t2 == 20000){
      if(sc_val == 141) {
        tap=11
      } else if(sc_val == 736) {
        tap=c(27, 141)
      } else {
        tap=c(41, 262)
      }
    } else {
      if(sc_val == 223){
        tap=14
      } else if(sc_val == 1357){
        tap=c(36,223)
      } else {
        tap=c(57, 439)
      }
    }
    updateSelectInput(session, "Tapers",choices=tap, selected = tap[1])
  })

  # The below renderText blocks serve to indicate the possible valid values that can be selected on the Simulated Side
  # of the application
  output$Res <- renderText({
    paste(h6("*Choices are T", HTML(paste(tags$sup("1/2"))), ", T", HTML(paste(tags$sup("2/3"),
                  ", or T", HTML(paste(tags$sup("3/4"), ", provided it satisfies 30", HTML("&le;"),
                                    "N", HTML("&le;"), HTML(paste(tags$sup("T"))), "/", HTML(paste(tags$sub(2)))))))))
    })
  output$Res1 <- renderText({
    paste(h6("**Choices are N", HTML(paste(tags$sup("1/2"))), ", or N", HTML(paste(tags$sup("3/4"),
                   ", provided it satisfies ", HTML(paste(tags$sup("floor(N/2)"))),
                                     "/", HTML(paste(tags$sub(2))), " - 1> floor((K+1)(", HTML(paste(tags$sup("N"))),
                                     "/", HTML(paste(tags$sub("N+1"))), "))"))))
  })
  output$Res2 <- renderText({
    paste(h6("For more explanation of the above terms, and the algorithm that is run, consult:",HTML("<br>"),"Scott A. Bruce, Cheng Yong Tang, Martica H. Hall & Robert T. Krafty (2020) Empirical Frequency Band Analysis of Nonstationary Time Series, Journal of the American Statistical Association, 115:532, 1933-1945,", HTML(paste(tags$a("doi.org/10.1080/01621459.2019.1671199", href = "https://doi.org/10.1080/01621459.2019.1671199")))))
  })
  output$F1_1 <- renderText({
    paste(h6("*Choices are T", HTML(paste(tags$sup("1/2"))), ", T", HTML(paste(tags$sup("2/3"))),
             ", or T",HTML(paste(tags$sup("3/4"))),", provided it satisfies 30 ", HTML("&le;"), "N", HTML("&le;"), "T"))
  })
  output$F1_2 <- renderText({
    paste(h6("**Choices are N", HTML(paste(tags$sup("1/2"))),", or N",HTML(paste(tags$sup("3/4"))),
             ", provided it satisfies 1 ", HTML("&le;"), "K", "<", "floor(N/4 - 1)"))
  })
  output$F1_3 <- renderText({
    paste(h6("***Choices are 5, or 10, provided it satisfies 1 ", HTML("&le;"), "Rsel", HTML("&le;"), "R"))
  })
  output$F1_4 <- renderText({
    paste(h6("For more explanation of the above terms, and
             the algorithm that is run, consult:",HTML("<br>"), "Efficient Algorithm for Frequency-Domain Dimension Reduction of Functional Time Series via Adaptive Frequency Band Learning by Bruce and Bagchi (2020+)", HTML(paste(tags$a("doi.org/10.48550/arXiv.2102.01784", ref="https://doi.org/10.48550/arXiv.2102.01784")))))
  })
  output$Check111 <- renderText({
    paste(h6("For more explanation of the above terms, and
             the algorithm that is run, consult:",HTML("<br>"),"Frequency Band Analysis of Nonstationary Multivariate Time Series by Raanju R. Sundararajan and Scott A. Bruce (2023)", HTML(paste(tags$a("doi.org/10.48550/arXiv.2301.03664", href="https://doi.org/10.48550/arXiv.2301.03664")))))
  })
  output$res9 <- renderText({
    paste("*Please enter a dataframe")
  })


  #######################################################
  ## Event Reactive Block Start point for the Simulated
  ## Multivariate algorithm
  #######################################################


  plot.listMv <- eventReactive(input$goMv, ignoreNULL = TRUE, {
    hide("downloadDataMV1")
    hide("MvPlotaDesc")
    hide("mv_testa")
    hide("mvX_F1")
    hide("MvbPlotDesc")
    hide("mv_testb")
    hide("mvX_F2")
    hide("MvPlot22Desc")
    hide("Mv_10100")
    hide("q11_Mv1")
    t = as.numeric(input$TsMv); #Length of Time Series
    R = as.numeric(input$RMv); #Number of Components
    nrep = as.numeric(input$nrepMv)
    Wsel = as.numeric(input$WselMv)
    seed=234; #seed for reproducibility

    ##################################################################
    ## Start - Algorithm Execution if Algorithm Type is White Noise ##
    ##################################################################

    if(input$SimSettingM == 'W'){
      #simulate data
      X.wn <- matrix(NA,nrow=t,ncol=R);
      for (m in 1:R){
        X.wn[,m] <- meba.simdata(t)$wn;
      }


      #compute and plot local periodogram
      N <- 2*floor(t^0.7)-floor(t^0.7/2)*2; #neighborhood for local periodogram
      freq <- seq(0,floor(N/2),by=1)/N
      pse <- fhat_lp(X.wn,N,stdz=FALSE);

      X = X.wn
    }

    ################################################################
    ## End - Algorithm Execution if Algorithm Type is White Noise ##
    ################################################################

    #################################################################
    ## Start - Algorithm Execution if Algorithm Type is Sinusoidal ##
    #################################################################

    else if(input$SimSettingM == 'S'){

      #simulate data
      X.s3b <- matrix(NA,nrow=t,ncol=R);
      df <- meba.simdata(t+200);
      ll <- seq(from=0,by=-1,length.out=R); #ll as c(0,-1,-2,-3,..)
      cf <- rep(1,R); #same for all cf as 1

      for (m in 1:R){
        X.s3b[,m] <- cf[m]*df$bS[(101+ll[m]):(t+100+ll[m])]
      }

      #compute and plot local periodogram and demeaned local periodogram
      N <- 2*floor(t^0.7)-floor(t^0.7/2)*2; #neighborhood for local periodogram
      freq <- seq(0,floor(N/2),by=1)/N
      pse <- fhat_lp(X.s3b,N,stdz=FALSE);

      X = X.s3b;
    }

    ###############################################################
    ## End - Algorithm Execution if Algorithm Type is Sinusoidal ##
    ###############################################################

    #############################################################
    ## Start - Algorithm Execution if Algorithm Type is Linear ##
    #############################################################

    else if(input$SimSettingM == 'L'){
      #simulate data
      X.l3b <- matrix(NA,nrow=t,ncol=R);
      df <- meba.simdata(t+200);
      ll <- seq(from=0,by=-1,length.out=R); #ll as c(0,-1,-2,-3,..)
      cf <- rep(1,R); #same for all cf as 1

      for (m in 1:R){
        X.l3b[,m] <- cf[m]*df$bL[(101+ll[m]):(t+100+ll[m])]
      }

      #compute and plot local periodogram and demeaned local periodogram
      N <- 2*floor(t^0.7)-floor(t^0.7/2)*2; #neighborhood for local periodogram
      freq <- seq(0,floor(N/2),by=1)/N
      pse <- fhat_lp(X.l3b,N,stdz=FALSE);

      X = X.l3b;
    }

    ###########################################################
    ## End - Algorithm Execution if Algorithm Type is Linear ##
    ###########################################################

    ################################################
    ## Start - Algorithm Execution if Algorithm Type
    ## is Linear & Sinusoidal - Mixture
    ################################################

    else if(input$SimSettingM == 'LASM'){
      #simulate data
      X.m3b1 <- matrix(NA,nrow=t,ncol=R);
      df <- meba.simdata(t+200);
      ll <- seq(from=0,by=-1,length.out=R); #ll as c(0,-1,-2,-3,..)
      cf <- rep(1,R); #same for all cf as 1

      for (m in 1:floor(R/2)){
        X.m3b1[,m] <- cf[m]*df$bL[(101+ll[m]):(t+100+ll[m])]
      }

      for (m in (floor(R/2)+1):R){
        X.m3b1[,m] <- cf[m]*df$bS[(101+ll[m]):(t+100+ll[m])]
      }
      #compute and plot local periodogram and demeaned local periodogram
      N <- 2*floor(t^0.7)-floor(t^0.7/2)*2; #neighborhood for local periodogram
      freq <- seq(0,floor(N/2),by=1)/N
      pse <- fhat_lp(X.m3b1,N,stdz=FALSE);

      X = X.m3b1;

    }

    ##############################################
    ## End - Algorithm Execution if Algorithm Type
    ## is Linear & Sinusoidal - Mixture
    ##############################################

    #################################################
    ## Start - Algorithm Execution if Algorithm Type
    ## is Linear & Sinusoidal - Differing Proportions
    #################################################

    else if(input$SimSettingM == "LASDP"){
      #simulate data
      X.m3b2 <- matrix(NA,nrow=t,ncol=R);
      df <- meba.simdata(t+200);
      ll <- seq(from=0,by=-1,length.out=R); #ll as c(0,-1,-2,-3,..)
      cf <- rep(1,R); #same for all cf as 1

      for (m in 1:floor(R*0.2)){
        X.m3b2[,m] <- cf[m]*df$bL2f15[(101+ll[m]):(t+100+ll[m])]
      }

      for (m in (floor(R*0.2)+1):R){
        X.m3b2[,m] <- cf[m]*df$bS2f35[(101+ll[m]):(t+100+ll[m])]
      }

      #compute and plot local periodogram and demeaned local periodogram
      N <- 2*floor(t^0.7)-floor(t^0.7/2)*2; #neighborhood for local periodogram
      freq <- seq(0,floor(N/2),by=1)/N
      pse <- fhat_lp(X.m3b2,N,stdz=FALSE);

      X = X.m3b2;
    }

    #################################################
    ## End - Algorithm Execution if Algorithm Type
    ## is Linear & Sinusoidal - Differing Proportions
    #################################################

    else{
      print('Error in Multivariate')
    }

    #################################################
    ## Start - Gather Results from Algorithm, to display in
    ## aforementioned plots
    #################################################
    #output$SimMv_text <- renderText({
    #  paste(h4(strong("You can check the progress of the algorithm in the file 'sim-mEBA.txt', in the directory this
    #                  application is located in. Rerunning the algorithm for Simulated, Multivariate data will overwrite
    #                  the contents of the file, and closing the application will delete the file.")))
    #})
    shinyjs::html(id='SimMv_text', html = "<h4> <strong> You can check the progress of the algorithm in the file 'sim-mEBA.txt', in your working directory.
                      Rerunning the algorithm for Simulated, Multivariate data will overwrite
                      the contents of the file, and closing the application will delete the file. If you wish to preserve the file, you can
                      change its name, to ensure it won't be overwritten or deleted.<strong> <h4>")
    show("SimMv_text")
    sink("sim-mEBA.txt")
    method <- msboot(nrep=nrep, X, Wsel=Wsel, stdz=FALSE, ncore=1)
    sink()
    vals <- method[[4]][which(method[[4]][,2]==1),1]
    indexes <- method[[4]][,2][method[[4]][,1]%in% method[[3]][[1]][,1]]
    thresh <- numeric(0)
    for(i in 1:nrow(method[[3]])){
      thresh[i] <- 0.05 / nrow(method[[3]][[i]])
    }
    thresh_bounds <- numeric(0)
    num_W <- dim(method[[3]])[1]
    pvals <- numeric(0)
    fre <- numeric(0)
    for(i in 1:num_W){
      curr <- method[[3]][[i]]
      pvals <- c(pvals, curr[,2])
      fre <- c(fre, curr[,1])
    }
    min_val <- numeric(0)
    for(i in 1:length(unique(fre))){
      cur_freq <- unique(fre)[i]
      all_pvals <- pvals[which(fre == cur_freq)]
      min_val <- c(min_val, min(all_pvals))
      thresh_bounds[i] <- thresh[[which.min(all_pvals)]]
    }
    mod_val <- min_val
    for(i in 1:length(min_val)){
      if(min_val[i] == 0.000){
        mod_val[i] <- paste("<", 1/nrep, sep="")
      }
    }
    uni_fre <- unique(fre)
    sim_type <- input$SimSettingM
    dimnames(pse) <- list(freq,apply(expand.grid(1:R,1:R),1,paste,collapse = "-"),1:t);
    comp_names <- apply(expand.grid(1:R,1:R),1,paste,collapse = "-")

    #################################################
    ## End - Gather Results from Algorithm, to display in
    ## aforementioned plots
    #################################################


    #################################################
    ## Start - Show and Update Sliders and Descriptors,
    ## and store important values for future use
    #################################################
    hide("SimMv_text")

    show("MvPlotaDesc")
    show("MvbPlotDesc")
    show("MvPlot22Desc")
    show("downloadDataMV1")

    show("mv_testa")
    show("mvX_F1")
    show("mv_testb")
    show("mvX_F2")
    show("Mv_10100")
    show("q11_Mv1")
    updateSliderInput(session, "mvX_F1", min = 1, max=as.numeric(R), value=1, step=1)
    updateSliderInput(session, "mvX_F2", min = 1, max=as.numeric(R), value=1, step=1)
    updateSliderInput(session, "q11_Mv1", min = 1, max=as.numeric(R), value=1, step=1)
    output$MvPlotaDesc <- renderText({
      paste(h4("Currently viewing component "))
    })
    output$mv_testa <- renderText({
      paste(h4(strong((paste("1")))))
    })
    output$MvbPlotDesc <- renderText({
      paste(h4("Currently viewing component "))
    })
    output$MvPlot22Desc <- renderText({
      paste(h4("Currently viewing component "))
    })
    output$mv_testb <- renderText({
      paste(h4(strong((paste("1")))))
    })
    output$Mv_10100 <- renderText({
      paste(h4(strong((paste("1")))))
    })
    list(X = X, pse = pse, freq = freq, vals = vals, min_val = min_val, mod_val = mod_val,
         sim_type = sim_type, comp_names = comp_names, thresh_bounds = thresh_bounds,
         tested_freq = uni_fre, indexes=indexes)
    #################################################
    ## End - Show and Update Sliders and Descriptors,
    ## and store important values for future use
    #################################################
  })

  #################################################
  ## Start - Initialize a plot, on the first component
  ## of the simulated, multivariate data
  #################################################

  output$Mv_Plota <- renderPlotly({
    a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listMv()[[1]][,1])), y=plot.listMv()[[1]][,1])) +
      xlab("Time") + ylab("") + ggtitle("Simulated Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
      scale_x_continuous(limits=c(0,1), expand=c(0,0))
    plotly <- ggplotly(a)
    fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                       mode = 'lines',
                                       line= list(color='black'),
                                       x = seq(from=0, to=1, length.out=length(plot.listMv()[[1]][,1])),
                                       y = plot.listMv()[[1]][,1],
                                       hovertemplate = paste('<i>Time</i>: %{x:.5f}',
                                                             '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
  })

  #################################################
  ## End - Initialize a plot, on the first component
  ## of the simulated, multivariate data
  #################################################

  #################################################
  ## Start - Initialize a plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################

  output$Mv_Plotb <- renderPlot({
    image.plot(x=(1:as.numeric(dim(plot.listMv()[[1]])[1])) / (as.numeric(dim(plot.listMv()[[1]])[1])),y=plot.listMv()[[3]][-1],z=t(Re(plot.listMv()[[2]][-1,1,])),
               axes = TRUE, col = inferno(256),
               main = "Local Periodogram",xlab='Time',ylab='Hz',xaxs="i",
               bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
    abline(h=plot.listMv()[[4]], col="skyblue", lwd=3);
    if(plot.listMv()[[7]] != 'W'){
      abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
    }
    vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                      just=c("left", "top"), y=0.55, x=0.65)
    if(plot.listMv()[[7]] == 'W'){
      act <- c("(0, 0.5)")
    } else {
      act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
    }
    len <- length(plot.listMv()[[4]])
    vals <- plot.listMv()[[4]]
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    max_len <- max(length(act), length(spp))
    if(length(act) == length(spp)){

    } else if(length(act) > length(spp)){
      sp_l <- length(spp) + 1
      for(i in sp_l: length(act)){
        spp[i] <- ""
      }
    } else {
      ac_l <- length(act) + 1
      for(i in ac_l: length(spp)){
        act[i] <- ""
      }
    }
    pp <- data.frame(
      "Actual Frequency Bands" = act,
      "Predicted Frequency Bands" = spp)
    colnames(pp) <- c(
      "Actual \n Frequency Bands",
      "Predicted \n Frequency Bands")
    grid.table(pp, vp=vp.br, rows=NULL)

    vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                     just=c("left", "top"), y=0.95, x=0.65)
    grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
    jj <- grid.legend(c("Predicted Partition Points"
                        , "Actual Partition Points"
                        ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
                                                       , "lawngreen"
                                                       )), vp=vp.r,
                      draw=TRUE)
  })

  #################################################
  ## End - Initialize a plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################

  #################################################
  ## Start - Update plot on Simulated Multivariate
  ## data, to display the component selected via the slider
  #################################################

  observeEvent(input$mvX_F1, ignoreNULL = FALSE, {
    curr_col <- as.numeric(input$mvX_F1)
    output$mv_testa <- renderText({
      paste(h4(strong(paste(curr_col))))
    })
    if(is.na(curr_col)){

    } else {
      output$Mv_Plota <- renderPlotly({
        a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listMv()[[1]][,curr_col])), y=plot.listMv()[[1]][,curr_col])) +
          xlab("Time") + ylab("") + ggtitle("Simulated Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          scale_x_continuous(limits=c(0,1), expand=c(0,0))
        plotly <- ggplotly(a)
        fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                           mode = 'lines',
                                           line= list(color='black'),
                                           x = seq(from=0, to=1, length.out=length(plot.listMv()[[1]][,curr_col])),
                                           y = plot.listMv()[[1]][,curr_col],
                                           hovertemplate = paste('<i>Time</i>: %{x:.5f}',
                                                                 '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
      })
    }

  })

  #################################################
  ## End - Update plot on Simulated Multivariate
  ## data, to display the component selected via the slider
  #################################################

  #################################################
  ## Start - Update plot on local Periodogram of
  ## Simulated Multivariate data, to selected component
  #################################################

  observeEvent(input$mvX_F2, ignoreNULL = TRUE, {
    curr_num <- as.numeric(input$mvX_F2)
    #if(is.na(plot.listF1()[[4]])){
    #
    #} else {
      curr_comp <- paste(curr_num)
      output$mv_testb <- renderText({
        paste(h4(strong((paste(curr_comp)))))
      })
      #if(strsplit(curr_comp, "-")[[1]][1] == strsplit(curr_comp, "-")[[1]][2]){
        output$Mv_Plotb <- renderPlot({
          image.plot(x=(1:as.numeric(dim(plot.listMv()[[1]])[1])) / (as.numeric(dim(plot.listMv()[[1]])[1])),y=plot.listMv()[[3]][-1],z=t(Re(plot.listMv()[[2]][-1,curr_num+(curr_num-1)*dim(plot.listMv()[[1]])[2],])),
                     axes = TRUE, col = inferno(256),
                     main = "Local Periodogram",xlab='Time',ylab='Hz',xaxs="i",
                     bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
          abline(h=plot.listMv()[[4]], col="skyblue", lwd=3);
          if(plot.listMv()[[7]] != 'W'){
            abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
          }
          vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                            just=c("left", "top"), y=0.55, x=0.65)
          if(plot.listMv()[[7]] == 'W'){
            act <- c("(0, 0.5)")
          } else {
            act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
          }
          len <- length(plot.listMv()[[4]])
          vals <- plot.listMv()[[4]]
          if(len == 0){
            str <- "(0, 0.5),"
          } else if (len == 1) {
            str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
          } else {
            str <- paste("(0", sep="")
            for(i in 1:len){
              str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
            }
            str <- paste(str, ",", "0.5),", sep="")
          }
          spp <- strsplit(str, "),")[[1]]
          for(a in 1:length(spp)){
            spp[a] <- paste(spp[a], ")", sep="")
          }
          max_len <- max(length(act), length(spp))
          if(length(act) == length(spp)){

          } else if(length(act) > length(spp)){
            sp_l <- length(spp) + 1
            for(i in sp_l: length(act)){
              spp[i] <- ""
            }
          } else {
            ac_l <- length(act) + 1
            for(i in ac_l: length(spp)){
              act[i] <- ""
            }
          }
          pp <- data.frame(
            "Actual Frequency Bands" = act,
            "Predicted Frequency Bands" = spp)
          colnames(pp) <- c(
            "Actual \n Frequency Bands",
            "Predicted \n Frequency Bands")
          grid.table(pp, vp=vp.br, rows=NULL)

          vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                           just=c("left", "top"), y=0.95, x=0.65)
          grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
          jj <- grid.legend(c("Predicted Partition Points"
                              , "Actual Partition Points"
          ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
                                         , "lawngreen"
          )), vp=vp.r,
          draw=TRUE)
        })

      #  } else {
      #    output$Mv_Plotb <- renderPlot({
      #      image.plot(x=(1:as.numeric(input$TsMv)) / (as.numeric(input$TsMv)),y=plot.listMv()[[3]][-1],z=t(Re(plot.listMv()[[2]][-1,as.numeric(input$RMv),])),
      #                 axes = TRUE, col = inferno(256),
      #                 main = "Estimated Coherence",xlab='Time',ylab='Hz',xaxs="i",
      #                 bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
      #      abline(h=plot.listMv()[[4]], col="skyblue", lwd=3);
      #      if(plot.listMv()[[7]] != 'W'){
      #        abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
      #      }
      #      vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
      #                        just=c("left", "top"), y=0.55, x=0.65)
      #      if(plot.listMv()[[7]] == 'W'){
      #        act <- c("(0, 0.5)")
      #      } else {
      #        act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
      #      }
      #      len <- length(plot.listMv()[[4]])
      #      vals <- plot.listMv()[[4]]
      #      if(len == 0){
      #        str <- "(0, 0.5),"
      #      } else if (len == 1) {
      #        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      #      } else {
      #        str <- paste("(0", sep="")
      #        for(i in 1:len){
      #          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      #        }
      #        str <- paste(str, ",", "0.5),", sep="")
      #      }
      #      spp <- strsplit(str, "),")[[1]]
      #      for(a in 1:length(spp)){
      #        spp[a] <- paste(spp[a], ")", sep="")
      #      }
      #      max_len <- max(length(act), length(spp))
      #      if(length(act) == length(spp)){
      #
      #      } else if(length(act) > length(spp)){
      #        sp_l <- length(spp) + 1
      #        for(i in sp_l: length(act)){
      #          spp[i] <- ""
      #        }
      #      } else {
      #        ac_l <- length(act) + 1
      #        for(i in ac_l: length(spp)){
      #          act[i] <- ""
      #        }
      #      }
      #      pp <- data.frame(
      #        "Actual Frequency Bands" = act,
      #        "Predicted Frequency Bands" = spp)
      #      colnames(pp) <- c(
      #        "Actual \n Frequency Bands",
      #        "Predicted \n Frequency Bands")
      #      grid.table(pp, vp=vp.br, rows=NULL)
      #
      #      vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
      #                       just=c("left", "top"), y=0.95, x=0.65)
      #      grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
      #      jj <- grid.legend(c("Predicted Partition Points"
      #                          , "Actual Partition Points"
      #      ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
      #                                     , "lawngreen"
      #      )), vp=vp.r,
      #      draw=TRUE)
      #    })
      #
      # }


   # }

  })

  #################################################
  ## End - Update plot on local Periodogram of
  ## Simulated Multivariate data, to selected component
  #################################################

  #################################################
  ## Start - Initialize a 3-D plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################
  observeEvent(plot.listMv()[[7]], {
    output$Plotly_Mvb <- renderPlotly({
      plot_ly(y=~seq(from=0, to=1, length.out=nrow(plot.listMv()[[1]])),
              x=~plot.listMv()[[3]][-1],
              z=~t(Re(plot.listMv()[[2]][-1,1,])))  %>%layout(title="3D Representation of Local Periodogram",scene = list( xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                           yaxis = list(title="Timepoint", range=c(0,1)),
                                                                                                                           zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
    })
  })

  #################################################
  ## End - Initialize a 3-D plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################

  #################################################
  ## Start - Update our 3-D plot of the local periodogram
  ## of our selected Component in our simulated multivariate data
  #################################################

  observeEvent(input$q11_Mv1, ignoreNULL = TRUE, {
    if(is.na(plot.listMv()[[7]])){

    } else {
      curr_num <- as.numeric(input$q11_Mv1)
      curr_comp <- paste(curr_num)
      output$Mv_10100 <- renderText({
        paste(h4(strong((paste(curr_comp)))))
      })
      output$Plotly_Mvb <- renderPlotly({
        plot_ly(y=~seq(from=0, to=1, length.out=nrow(plot.listMv()[[1]])),
                x=~plot.listMv()[[3]][-1],
                z=~t(Re(plot.listMv()[[2]][-1,curr_num + (curr_num-1)*ncol(plot.listMv()[[1]]),])))  %>%layout(title="3D Representation of Local Periodogram",scene = list( xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                                                                            yaxis = list(title="Timepoint", range=c(0,1)),
                                                                                                                                                                            zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
      })
    }

  })

  #################################################
  ## End - Update our 3-D plot of the local periodogram
  ## of our selected Component in our simulated multivariate data
  #################################################

  #################################################
  ## Start - Display Table containing results on
  ## partition testing for Simulated, Multivariate Data
  #################################################

  output$summ_out_Mv <- renderPlot({
    freq <- round(plot.listMv()[[10]], 3)
    mod_pval <- plot.listMv()[[6]]
    min_pval <- round(plot.listMv()[[5]], 5)
    thresh <- round(plot.listMv()[[9]], 5)
    Sig <- character(length(min_pval))
    for(i in 1:length(Sig)){
      if(min_pval[i] < thresh[i]){
        Sig[i] <- "TRUE"
      } else {
        Sig[i] <- "FALSE"
      }
    }
    res <- data.frame("Freq" = freq, "val" = mod_pval, "t"=thresh, "s" = as.character(Sig))
    colnames(res) <- c("Frequency", "Minimum \n P-Value", "P-Value \n Threshold", "Significant")
    sig <- which(plot.listMv()[[10]] %in% plot.listMv()[[4]])
    if(length(sig) == 0){
      sig = which.min(min_pval)
    }
    res <- res[sig, ]
    res1 <- tableGrob(res, rows = NULL)
    title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
    blank9090 <- textGrob(""); blank0909 <- textGrob("")
    grid.arrange(title, res1, blank0909, ncol = 1)
  })

  #################################################
  ## End - Display Table containing results on
  ## partition testing for Simulated, Multivariate Data
  #################################################

  #################################################
  ## Start - Create Scatterplot of p-values for Partition
  ## testing for Simulated, Multivariate data at each frequency
  #################################################

  output$summ_pval_Mv <- renderPlot({
    ggplot() + geom_point(aes(x = as.numeric(plot.listMv()[[10]]), y = as.numeric(plot.listMv()[[5]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
      xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
      geom_vline(xintercept = plot.listMv()[[4]], linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01))
  })

  #################################################
  ## End - Create Scatterplot of p-values for Partition
  ## testing for Simulated, Multivariate data at each frequency
  #################################################

  #######################################################
  ## Event Reactive Block End point for the Simulated
  ## Multivariate algorithm
  #######################################################


  #######################################################
  ## Event Reactive Block Start point for the Simulated
  ## Functional algorithm
  #######################################################

  plot.listF1 <- eventReactive(input$goF1, ignoreNULL = TRUE, {
    output$Fxn_Plota <- renderPlotly({

    })
    output$Plotly_Fxna <- renderPlotly({

    })
    hide("downloadDataFXN1")
    hide("FxnPlotaDesc")
    hide("test12121")
    hide("x_F1")
    hide("FxnbPlotDesc")
    hide("Blank2")
    hide("plot1_FxnCheck")
    hide("FxnPlot22Desc")
    hide("Blank10100")
    hide("q11_F1")
    ##get sim data
    nb=15; #number of basis functions used to generate white noise
    R=as.numeric(input$RF1); #number of points in functional domain
    Ts=as.numeric(input$TsF1); #length of time series
    seed=234; #seed for reproducibility
    B=floor(as.numeric(input$TsF1)/as.numeric(input$NF1)); #number of time blocks
    N=as.numeric(input$NF1); #number of observations per time block
    bw=floor((as.numeric(input$KF1) + 1) / (as.numeric(input$NF1) + 1)); #bandwidth for multitaper spectral estimator
    K=as.numeric(input$KF1); #number of tapers for multitaper spectral estimator
    std=as.logical(input$TF_F1); #standardize variance for points in functional domain (TRUE) or not (FALSE)
    freq=seq(from=0,by=1/N,length.out=floor(N/2)+1); #Fourier frequencies
    Rsel=as.numeric(input$RselF1); #number of points in functional domain used for test statistics

    #################################################################
    ## Start - Data Processing for White Noise Data ##
    #################################################################

    if (input$SimF1 == "W"){
      X=fws.sim(nb=nb,gsz=R,Ts=Ts,seed=seed);
      pse=fhat_pmt(X,N,K,Rsel,std);

       cmpnt="1-1"; #select component to view
       dimnames(pse) <- list(freq,apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-"),1:B);
       plot.x <- ((1:B) * (1/B)); plot.y <- as.numeric(rownames(pse)); plot.z <- pse
       plot.cmp <- apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-")
       plot.main <- "Multitaper Autospectrum"; plot.data = X

       #################################################################
       ## End - Data Processing for White Noise Data ##
       #################################################################

       #################################################################
       ## Start - Data Processing for Linear Data ##
       #################################################################

    } else if (input$SimF1 == "L") {
      X=f3bL.sim(nb=nb,gsz=R,Ts=Ts,seed=seed);
      pse=fhat_pmt(X,N,K,Rsel,std);
       cmpnt="1-1"; #select component to view
       dimnames(pse) <- list(freq,apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-"),1:B);
       plot.x <- ((1:B) * (1/B)); plot.y <-as.numeric( rownames(pse)); plot.z <- pse
       plot.cmp <- apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-")
       plot.main <- "Multitaper Autospectrum"; plot.data = X

       #################################################################
       ## End - Data Processing for Linear Data ##
       #################################################################

       #################################################################
       ## Start - Data Processing for Sinusoidal Data ##
       #################################################################

    } else if (input$SimF1 == "S") {
      X=f3bS.sim(nb=nb,gsz=R,Ts=Ts,seed=seed);
       pse=fhat_pmt(X,N,K,Rsel,std);
       cmpnt="1-1"; #select component to view
       dimnames(pse) <- list(freq,apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-"),1:B);
       plot.x <- ((1:B) * (1/B)); plot.y <- as.numeric(rownames(pse)); plot.z <- pse
       plot.cmp <- apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-")
       plot.main <- "Multitaper Autospectrum"; plot.data = X

       #################################################################
       ## End - Data Processing for Sinusoidal Data ##
       #################################################################
    }

    #################################################################
    ## Start - Algorithm Execution based on aforementioned data ##
    #################################################################

    set.seed(47)
    ndraw=100000; #number of draws from Gaussian process for approximating p-values
    blockdiag=TRUE; #use block diagonal covariance matrix approximation
    dcap=40; #max number of frequencies tested in a given pass
    alpha=as.numeric(input$AlphaF1)/ceiling((1-2*bw/0.5)*(floor(N/2)+1)/dcap); #alpha with Bonferroni correction
    shinyjs::html(id='SimFxn_text', html = "<h4> <strong> You can check the progress of the algorithm in the file 'sim-fEBA.txt', in your working directory.
                      Rerunning the algorithm for Simulated, Functional data will overwrite
                      the contents of the file, and closing the application will delete the file. If you wish to preserve the file, you can
                      change its name, to ensure it won't be overwritten or deleted.<strong> <h4>")
    show("SimFxn_text")
    sink("sim-fEBA.txt")
    res <- fEBA.wrapper(X,Rsel,K,N,ndraw,alpha,std,blockdiag,dcap);
    sink()
    ##View test statistics and p-values over frequencies
    tmp=cbind(as.numeric(unlist(lapply(res$log, function(x) rownames(x$Qint)))),
              unlist(lapply(res$log, function(x) x$Qint)),
              unlist(lapply(res$log, function(x) x$Qpv[,'Qint'])));
    tmp=tmp[!duplicated(tmp[,1]),];
    hide("SimFxn_text")
    #################################################################
    ## End - Algorithm Execution based on aforementioned data ##
    #################################################################

    #################################################################
    ## Start - Initialize Plot for first timepoint of our simulated
    ## Functional data
    #################################################################
    output$Fxn_Plota <- renderPlotly({
         a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(X[1,])), y=X[1,])) +
           xlab("Functional Domain") + ylab("") + ggtitle("Simulated Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
           scale_x_continuous(limits=c(0,1), expand=c(0,0))
         plotly <- ggplotly(a)
         fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                            mode = 'lines',
                                            line= list(color='black'),
                                            x = seq(from=0, to=1, length.out=length(X[1,])),
                                            y = X[1,],
                                            hovertemplate = paste('<i>Functional Domain</i>: %{x:.5f}',
                                                                  '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
       })
    #################################################################
    ## End - Initialize Plot for first timepoint of our simulated
    ## Functional data
    #################################################################

    #################################################
    ## Start - Find Signal Coherence Based on Power
    ## Spectrum Estimate
    #################################################
    conf <- numeric(length(pse))
    dim(conf) <- dim(pse)
    dimnames(conf) <- dimnames(pse)
    for(k in 1:dim(pse)[3]){
      for(j in 1:dim(pse)[2]){
        first_col <- ((j - 1) %% (as.numeric(input$RselF1))) + 1
        second_col <- ((j - 1) %/% (as.numeric(input$RselF1))) + 1
        cmpt_1 <- paste(first_col, "-", first_col, sep="")
        cmpt_2 <- paste(second_col, "-", second_col, sep="")
        for(i in 1:dim(pse)[1]){
          if(first_col == second_col){
            conf[i,j,k] <- Re(pse[i,j,k])
          } else {
            conf[i,j,k] <- Re((Mod(pse[i,j,k])**2) / (pse[i,cmpt_1,k] * pse[i,cmpt_2,k]))
          }
        }
      }
    }
    plot.z <- conf

    #################################################
    ## End - Find Signal Coherence Based on Power
    ## Spectrum Estimate
    #################################################

    #################################################
    ## Start - Show and Update Sliders and Descriptors,
    ## and store important values for future use
    #################################################

    output$FxnPlotaDesc <- renderText({
      paste(h4("Currently viewing timepoint "))
    })

    output$FxnbPlotDesc <- renderText({
      paste(h4("Currently viewing cross-component "))
    })
    output$FxnPlot22Desc <- renderText({
      paste(h4("Currently viewing cross-component "))
    })
    show("downloadDataFXN1")
    show("FxnPlotaDesc")
    show("FxnbPlotDesc")
    show("FxnPlot22Desc")
    show("x_F1")
    show("q_F1")
    show("q11_F1")
    show("Plotly_Fxna")
    show("plot1_FxnCheck")
    show("test12121")
    show("Blank2")
    show("Blank10100")
    updateSliderInput(session, "x_F1", min = 1, max=as.numeric(input$TsF1), value=1, step=1)
    updateSelectInput(session, "q_F1", choices=plot.cmp, selected = plot.cmp[1])
    updateSliderInput(session, "q11_F1", min=1, max=length(plot.cmp), value=1)
    updateSliderInput(session, "plot1_FxnCheck", min=1, max=length(plot.cmp), value=1)
    output$Blank2 <- renderText({
      paste(h4(strong((paste("1-1")))))
    })
    output$Blank10100 <- renderText({
      paste(h4(strong((paste("1-1")))))
    })
    output$test12121 <- renderText({
      paste(h4(strong((paste("1")))))
    })

    #################################################
    ## Start - Create 3D Plot of entire dataset
    #################################################
    output$Plotly_Fxna <- renderPlotly({
      plot_ly(x = ~seq(from=0, to=1, length.out = ncol(X)),
              y = ~seq(from=0, to=1, length.out=nrow(X)),
              z = ~X) %>% add_surface() %>% layout(
        title = "3D Representation of Simulated Data",
        scene = list(
        xaxis = list(title="Functional Domain"),
        yaxis = list(title = "Timepoint"),
        zaxis = list(title="Value")
      )) %>% colorbar(title = "Value", len=1)
    })
    #################################################
    ## End - Create 3D Plot of entire dataset
    #################################################

    show("Fxn_Row")
    plot.main_2 <- "Estimated Coherence"
    plot.main_3D <- "3D Representation of Autospectrogram"
    plot.main_3D.2 <- "3D Representation of Coherence"
    dat_type <- as.character(input$SimF1)
    list(plot.x = plot.x, plot.y = plot.y, plot.z = plot.z,
        plot.main = plot.main, plot.cmp = plot.cmp, plot.data = plot.data,
        plot.main_2 = plot.main_2, plot.main_3D = plot.main_3D, plot.main_3D.2 = plot.main_3D.2,
        plot.log = res$summary, plot.freq = unname(tmp[,1]), plot.pvals = unname(tmp[,3]),
        dat_type = dat_type)
    #################################################
    ## End - Show and Update Sliders and Descriptors,
    ## and store important values for future use
    #################################################
  });



  #################################################################
  ## Start - Update Plot based on selected timepoint of our simulated
  ## Functional data
  #################################################################

  observeEvent(input$x_F1, ignoreNULL = FALSE, {
    curr_row <- as.numeric(input$x_F1)
    output$test12121 <- renderText({
      paste(h4(strong(paste(curr_row))))
    })
    if(is.na(curr_row)){

    } else {
      output$Fxn_Plota <- renderPlotly({
        a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listF1()[[6]][curr_row,])), y=plot.listF1()[[6]][curr_row,])) +
          xlab("Functional Domain") + ylab("") + ggtitle("Simulated Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          scale_x_continuous(limits = c(0,1), expand=c(0,0))
        plotly <- ggplotly(a)
        fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                           mode = 'lines',
                                           line= list(color='black'),
                                           x = seq(from=0, to=1, length.out=length(plot.listF1()[[6]][curr_row,])),
                                           y = plot.listF1()[[6]][curr_row,],
                                           hovertemplate = paste('<i>Functional Domain</i>: %{x:.5f}',
                                                                 '<br><i>Value</i>: %{y:.5f}<extra></extra>'))

      })
    }

  })

  #################################################################
  ## End - Update Plot based on selected timepoint of our simulated
  ## Functional data
  #################################################################

  # observe({
  #   if(exists("example.txt")){
  #     fileData <- reactiveFileReader(1000, session, "example.txt", read.delim, header=F)
  #
  #     output$text <- renderTable({
  #       fileData()
  #     })
  #   } else{
  #     print("Yes")
  #   }
  # })

  # observe({
  #   ## read the text file once every 50 ms
  #   invalidateLater(50, session)
  #   req(file.exists("example.txt"))
  #   txt <- paste(readLines("example.txt"), collapse = "\n")
  #   output$text <- renderText(txt)
  # })


  #################################################
  ## Start - Display Table containing results on
  ## partition testing for Simulated, Functional Data
  #################################################

  output$summ_out_fxn <- renderPlot({
    freq <- round(plot.listF1()[[10]][,1], 3)
    pval <- round(plot.listF1()[[10]][,2], 5)
    thresh <- round(plot.listF1()[[10]][,3], 5)
    Sig <- character(length(pval))
    for(i in 1:length(Sig)){
      if(pval[i] < thresh[i]){
        Sig[i] <- "TRUE"
      } else {
        Sig[i] <- "FALSE"
      }
    }
    res <- data.frame("Freq" = freq, "val" = pval, "t"=thresh, "s" = as.character(Sig))
    colnames(res) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
    res1 <- tableGrob(res, rows = NULL)
    title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
    blank9090 <- textGrob(""); blank0909 <- textGrob("")
    grid.arrange(blank9090, title, res1, blank0909, ncol = 1)
  })

  #################################################
  ## End - Display Table containing results on
  ## partition testing for Simulated, Functional Data
  #################################################

  #################################################
  ## Start - Create Scatterplot of p-values for Partition
  ## testing for Simulated, Functional data at each frequency
  #################################################

  output$summ_pval_fxn <- renderPlot({
    ggplot() + geom_point(aes(x = as.numeric(plot.listF1()[[11]]), y = as.numeric(plot.listF1()[[12]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
      xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
      geom_vline(xintercept = (plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]), linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01))
  })

  #################################################
  ## End - Create Scatterplot of p-values for Partition
  ## testing for Simulated, Functional data at each frequency
  #################################################

  #################################################
  ## Start - Initialize a plot, on the local periodogram
  ## of the 1-1 Component in our simulated functional data
  #################################################

  output$Fxn_Plotb <- renderPlot({
    image.plot(x=plot.listF1()[[1]],y=plot.listF1()[[2]],z=suppressWarnings(t(Re(plot.listF1()[[3]][,"1-1",]))),
               axes = TRUE, col = inferno(256),
               main = plot.listF1()[[4]],xlab='Time',ylab='Hz',xaxs="i",
               bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
    abline(h=unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]), col="skyblue", lwd=3);
    if(plot.listF1()[[13]] == "W"){
      act <- c("(0,0.5)")
    } else {
      abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
      act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
    }
    vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                      just=c("left", "top"), y=0.55, x=0.65)
    len <- length(unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]))
    vals <- unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1])
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    max_len <- max(length(act), length(spp))
    if(length(act) == length(spp)){

    } else if(length(act) > length(spp)){
      sp_l <- length(spp) + 1
      for(i in sp_l: length(act)){
        spp[i] <- ""
      }
    } else {
      ac_l <- length(act) + 1
      for(i in ac_l: length(spp)){
        act[i] <- ""
      }
    }
    pp <- data.frame("Actual Frequency Bands" = act, "Predicted Frequency Bands" = spp)
    colnames(pp) <- c("Actual \n Frequency Bands", "Predicted \n Frequency Bands")
    grid.table(pp, vp=vp.br, rows=NULL)

    vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                     just=c("left", "top"), y=0.95, x=0.65)
    grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
    jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                      draw=TRUE)
  })

  #################################################
  ## End - Initialize a plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################

  #################################################
  ## Start - Initialize a 3-D plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################
  output$Plotly_Fxnb <- renderPlotly({
    plot_ly(y=~plot.listF1()[[1]], x=~plot.listF1()[[2]], z=~t(Re(plot.listF1()[[3]][,"1-1",])))  %>%layout(title=plot.listF1()[[8]],
                                                                                                            scene = list(
           xaxis = list(title='Frequency',range = c(0.5, 0)),
           yaxis = list(title="Timepoint"),
           zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
  })

  #################################################
  ## End - Initialize a 3-D plot, on the local periodogram
  ## of the 1-1 Component in our simulated multivariate data
  #################################################

  #################################################
  ## Start - Update a plot on the local periodogram of the
  ## component selected via the slider in our simulated functional data
  #################################################

  observeEvent(input$plot1_FxnCheck, ignoreNULL = TRUE, {
    curr_num <- as.numeric(input$plot1_FxnCheck)
    if(is.na(plot.listF1()[[4]])){

    } else {
      curr_comp <- plot.listF1()[[5]][curr_num]
      output$Blank2 <- renderText({
        paste(h4(strong((paste(curr_comp)))))
      })
      if(strsplit(curr_comp, "-")[[1]][1] == strsplit(curr_comp, "-")[[1]][2]){
        output$Fxn_Plotb <- renderPlot({
          image.plot(x=plot.listF1()[[1]],y=plot.listF1()[[2]],z=suppressWarnings(t(Re(plot.listF1()[[3]][,curr_comp,]))),
                     axes = TRUE, col = inferno(256),
                     main = plot.listF1()[[4]],xlab='Time',ylab='Hz',xaxs="i",
                     bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
          abline(h=unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]), col="skyblue", lwd=3);
          if(plot.listF1()[[13]] == "W"){
            act <- c("(0,0.5)")
          } else {
            abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
            act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
          }
          vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                            just=c("left", "top"), y=0.55, x=0.65)
          len <- length(unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]))
          vals <- unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1])
          if(len == 0){
            str <- "(0, 0.5),"
          } else if (len == 1) {
            str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
          } else {
            str <- paste("(0", sep="")
            for(i in 1:len){
              str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
            }
            str <- paste(str, ",", "0.5),", sep="")
          }
          spp <- strsplit(str, "),")[[1]]
          for(a in 1:length(spp)){
            spp[a] <- paste(spp[a], ")", sep="")
          }
          max_len <- max(length(act), length(spp))
          if(length(act) == length(spp)){

          } else if(length(act) > length(spp)){
            sp_l <- length(spp) + 1
            for(i in sp_l: length(act)){
              spp[i] <- ""
            }
          } else {
            ac_l <- length(act) + 1
            for(i in ac_l: length(spp)){
              act[i] <- ""
            }
          }
          pp <- data.frame("Actual Frequency Bands" = act, "Predicted Frequency Bands" = spp)
          colnames(pp) <- c("Actual \n Frequency Bands", "Predicted \n Frequency Bands")
          grid.table(pp, vp=vp.br, rows=NULL)

          vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                           just=c("left", "top"), y=0.95, x=0.65)
          grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
          jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                            draw=TRUE)
        })

      } else {
        output$Fxn_Plotb <- renderPlot({
          image.plot(x=plot.listF1()[[1]],y=plot.listF1()[[2]],z=suppressWarnings(t(Re(plot.listF1()[[3]][,curr_comp,]))),
                     axes = TRUE, col = inferno(256),
                     main = plot.listF1()[[7]],xlab='Time',ylab='Hz',xaxs="i",
                     bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
          abline(h=unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]), col="skyblue", lwd=3);
          if(plot.listF1()[[13]] == "W"){
            act <- c("(0,0.5)")
          } else {
            abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
            act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
          }
          vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                            just=c("left", "top"), y=0.55, x=0.65)
          len <- length(unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]))
          vals <- unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1])
          if(len == 0){
            str <- "(0, 0.5),"
          } else if (len == 1) {
            str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
          } else {
            str <- paste("(0", sep="")
            for(i in 1:len){
              str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
            }
            str <- paste(str, ",", "0.5),", sep="")
          }
          spp <- strsplit(str, "),")[[1]]
          for(a in 1:length(spp)){
            spp[a] <- paste(spp[a], ")", sep="")
          }
          max_len <- max(length(act), length(spp))
          if(length(act) == length(spp)){

          } else if(length(act) > length(spp)){
            sp_l <- length(spp) + 1
            for(i in sp_l: length(act)){
              spp[i] <- ""
            }
          } else {
            ac_l <- length(act) + 1
            for(i in ac_l: length(spp)){
              act[i] <- ""
            }
          }
          pp <- data.frame("Actual Frequency Bands" = act, "Predicted Frequency Bands" = spp)
          colnames(pp) <- c("Actual \n Frequency Bands", "Predicted \n Frequency Bands")
          grid.table(pp, vp=vp.br, rows=NULL)

          vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                           just=c("left", "top"), y=0.95, x=0.65)
          grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
          jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                            draw=TRUE)
        })

      }
    }
  })

  #################################################
  ## End - Update a plot on the local periodogram of the
  ## component selected via the slider in our simulated functional data
  #################################################

  #################################################
  ## Start - Update a 3-D plot on the local periodogram of the
  ## component selected via the slider in our simulated functional data
  #################################################

  observeEvent(input$q11_F1, ignoreNULL = TRUE, {
    curr_num <- as.numeric(input$q11_F1)
    if(is.na(plot.listF1()[[4]])){

    } else {
      curr_comp <- plot.listF1()[[5]][curr_num]
      output$Blank10100 <- renderText({
        paste(h4(strong((paste(curr_comp)))))
      })
      vals <- as.numeric(strsplit(curr_comp, "-")[[1]])
      if(vals[1] == vals[2]){
        output$Plotly_Fxnb <- renderPlotly({
          plot_ly(y=~plot.listF1()[[1]], x=~plot.listF1()[[2]], z=~t(Re(plot.listF1()[[3]][,curr_comp,])))  %>%layout(title=plot.listF1()[[8]],
                                                                                                                      scene = list(
                                                                                                                        xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                        yaxis = list(title="Timepoint"),
                                                                                                                        zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)


        })
      } else {
        output$Plotly_Fxnb <- renderPlotly({
          plot_ly(y=~plot.listF1()[[1]], x=~plot.listF1()[[2]], z=~t(Re(plot.listF1()[[3]][,curr_comp,])))  %>%layout(title=plot.listF1()[[9]],
                                                                                                                      scene = list(
                                                                                                                        xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                        yaxis = list(title="Timepoint"),
                                                                                                                        zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)


        })
      }



    }

  })

  #################################################
  ## End - Update a 3-D plot on the local periodogram of the
  ## component selected via the slider in our simulated functional data
  #################################################

  #######################################################
  ## Event Reactive Block End point for the Simulated
  ## Functional algorithm
  #######################################################




  #######################################################
  ## Event Reactive Block Start point for the Simulated
  ## Univariate algorithm
  #######################################################

  plot.list <- eventReactive(input$go, ignoreNULL = FALSE, {
    hide('downloadData')
    set.seed(823819)
    X = eba.simdata(T=as.numeric(input$Time))

    if (input$Simsetting == "W"){
      ################################################
      ## Start - Algorithm Execution if Data is White Noise
      ################################################
      ebaout.wn <- eba.search(X=X$wn,N= as.numeric(input$Num),K=as.numeric(input$Tapers),std=input$TF,alpha=as.numeric(input$Signi))
      plot.x = ebaout.wn$mtspec$t
      plot.y = ebaout.wn$mtspec$f
      plot.z = t(ebaout.wn$mtspec$mtspec)
      plot.main = "Multitaper Spectrogram for White Noise Setting"
      plot.h = as.numeric(ebaout.wn$part.final[c(-1,-length(ebaout.wn$part.final))])
      plot.data = X$wn
      plot.log = ebaout.wn$log
      plot.pvals = ebaout.wn$pvals
      plot.flat = ebaout.wn$flat
      ################################################
      ## End - Algorithm Execution if Data is White Noise
      ################################################

    } else if (input$Simsetting == "L") {
      ################################################
      ## Start - Algorithm Execution if Data is Linear
      ################################################
      ebaout.bL <- eba.search(X=X$bL,N= as.numeric(input$Num),K=as.numeric(input$Tapers),std=input$TF,alpha=as.numeric(input$Signi))
      plot.x = ebaout.bL$mtspec$t
      plot.y = ebaout.bL$mtspec$f
      plot.z = t(ebaout.bL$mtspec$mtspec)
      plot.main = "Multitaper Spectrogram for Linear Setting"
      plot.h = as.numeric(ebaout.bL$part.final[c(-1,-length(ebaout.bL$part.final))])
      plot.data = X$bL
      plot.log = ebaout.bL$log
      plot.pvals = ebaout.bL$pvals
      plot.flat = ebaout.bL$flat
      ################################################
      ## End - Algorithm Execution if Data is Linear
      ################################################

    } else if (input$Simsetting == "S") {
      ################################################
      ## Start - Algorithm Execution if Data is Sinusoidal
      ################################################
      ebaout.bS <- eba.search(X=X$bS,N= as.numeric(input$Num),K=as.numeric(input$Tapers),std=input$TF,alpha=as.numeric(input$Signi))
      plot.x = ebaout.bS$mtspec$t
      plot.y = ebaout.bS$mtspec$f
      plot.z = t(ebaout.bS$mtspec$mtspec)
      plot.main = "Multitaper Spectrogram for Sinusoidal Setting"
      plot.h = as.numeric(ebaout.bS$part.final[c(-1,-length(ebaout.bS$part.final))])
      plot.data = X$bS
      plot.log = ebaout.bS$log
      plot.pvals = ebaout.bS$pvals
      plot.flat = ebaout.bS$flat
      ################################################
      ## End - Algorithm Execution if Data is Sinusoidal
      ################################################
    }
    show('downloadData')
    dat_type <- as.character(input$Simsetting)
    list(plot.x = plot.x, plot.y = plot.y, plot.z = plot.z,
         plot.main = plot.main, plot.h = plot.h, plot.data = plot.data,
         plot.log = plot.log, plot.pvals = plot.pvals, plot.flat = plot.flat,
         dat_type = dat_type)

  });

  ################################################
  ## Start - Create both a plot of the data, and a
  ## plot of the Local Periodogram
  ################################################
  output$Image_Plotb <- renderPlotly({
    plot <- ggplot() + geom_line(aes(x=seq(0,1,length.out = length(plot.list()[[6]])), y= plot.list()[[6]])) + xlab("Time") +
             ylab("") + ggtitle("Simulated Time Series Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
             scale_x_continuous(limits=c(0,1), expand=c(0,0))
    plotly <- ggplotly(plot)
    fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                       mode = 'lines',
                                       line= list(color='black'),
                                      x = seq(0,1,length.out = length(plot.list()[[6]])),
                                      y = plot.list()[[6]],
                                      hovertemplate = paste('<i>Time</i>: %{x:.5f}',
                                                            '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
  })
  output$Image_Plot <- renderPlot({
    image.plot(x=plot.list()[[1]], y=plot.list()[[2]], z=plot.list()[[3]],
               axes = TRUE, col = inferno(256),
               xlab='Time',ylab='Hz',xaxs="i", main=plot.list()[[4]],
               bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85))
    abline(h=plot.list()[[5]], col = "skyblue", lwd=3)
    if(plot.list()[[10]] == "W"){
      act <- c("(0,0.5)")
    } else {
      abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
      act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
    }
    vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                      just=c("left", "top"), y=0.55, x=0.65)
    len <- length(plot.list()[[5]])
    vals <- plot.list()[[5]]
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    max_len <- max(length(act), length(spp))
    if(length(act) == length(spp)){

    } else if(length(act) > length(spp)){
      sp_l <- length(spp) + 1
      for(i in sp_l: length(act)){
        spp[i] <- ""
      }
    } else {
      ac_l <- length(act) + 1
      for(i in ac_l: length(spp)){
        act[i] <- ""
      }
    }
    pp <- data.frame("Actual Frequency Bands" = act, "Predicted Frequency Bands" = spp)
    colnames(pp) <- c("Actual \n Frequency Bands", "Predicted \n Frequency Bands")
    grid.table(pp, vp=vp.br, rows=NULL)

    vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                     just=c("left", "top"), y=0.95, x=0.65)
    grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
    jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                      draw=TRUE)
  });

  ################################################
  ## End - Create both a plot of the data, and a
  ## plot of the Local Periodogram
  ################################################

  #################################################
  ## Start - Display Table containing results on
  ## partition testing for Simulated, Univariate Data
  #################################################

  output$summ_out_uni <- renderPlot({
    pvals <- round(plot.list()[[7]][,4], 5)
    pval.th <- round(plot.list()[[7]][,5], 5)
    Sig <- character(length(pvals))
    for(i in 1:length(Sig)){
      if(pvals[i] < pval.th[i]){
        Sig[i] <- "TRUE"
      } else {
        Sig[i] <- "FALSE"
      }
    }
    pp <- data.frame("Frequency" = round(plot.list()[[7]][,2], 3), "P-Value" = round(plot.list()[[7]][,4], 5),
                     "P-Value\nThreshold" = round(plot.list()[[7]][,5], 5), "Significance" = as.character(Sig))
    colnames(pp) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
    table <- tableGrob(pp, rows=NULL)
    title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
    blank1 <- textGrob("")

    len <- length(plot.list()[[5]])
    vals <- plot.list()[[5]]
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    pvals <- plot.list()[[9]][,2]
    Res <- character(length(pvals))
    Sig2 <- numeric(length(pvals))
    for(i in 1:length(Res)){
      if(pvals[i] < 0.05){
        Res[i] = "Segment has \n nonflat spectrum"
        Sig2[i] = "TRUE"
      } else {
        Res[i] = "Segment has \n flat spectrum"
        Sig2[i] = "FALSE"
      }
    }
    blank1 <- textGrob(""); blank2 <- textGrob("")
    new_tab <- data.frame("Frequency Bands" = spp, "P-Values" = round(as.numeric(pvals), 5),"Significant" = Sig2 ,"Results" = Res)
    colnames(new_tab) <- c("Frequency \n Bands", "P-Value", "Significant", "Results")
    test1 <- tableGrob(new_tab, rows = NULL);
    title2 <- textGrob(expression(bold("Summary of Testing for Flat \n Spectrum in Each Segment")))
    grid.arrange(title, table, title2, test1,blank2, heights = c(0.75,0.75,0.85,0.75, 1) ,nrow = 5)
  })

  #################################################
  ## End - Display Table containing results on
  ## partition testing for Simulated, Univariate Data
  #################################################

  #################################################
  ## Start - Create Scatterplot of p-values for Partition
  ## testing for Simulated, Univariate data at each frequency
  #################################################

  output$summ_pval_uni <- renderPlot({
    ggplot() + geom_point(aes(x = as.numeric(plot.list()[[8]][,1]), y = as.numeric(plot.list()[[8]][,2]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
      xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
      geom_vline(xintercept = plot.list()[[5]], linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01))
  })

  #################################################
  ## End - Create Scatterplot of p-values for Partition
  ## testing for Simulated, Univariate data at each frequency
  #################################################

  #######################################################
  ## Event Reactive Block End point for the Simulated
  ## Univariate algorithm
  #######################################################


  #######################################################
  ## Start - Read in and store the chosen inputted data
  #######################################################

  plot.listaa <- eventReactive(input$file_csv, ignoreNULL = FALSE,  {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    le <- length(file[[1]])
    if(le == 0){

    } else {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    dataf <- read.csv(file$datapath, header = input$header)
    dataf <- dataf[[1]]
    list(dataf=dataf)}}
    )
  plot.listbb <- eventReactive(input$file_csv, ignoreNULL = FALSE,  {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    le <- length(file[[1]])
    if(le == 0){

    } else {
      file <- input$file_csv
      ext <- tools::file_ext(file$datapath)
      dataf <- read.csv(file$datapath, header = input$header)
      updateNumericInput(session, "Num_Fxna", value  = floor(sqrt(dim(dataf)[1])))
      updateNumericInput(session, "Tapers_Fxna", value = floor(dim(dataf)[1] ** 0.25))
      if(dim(dataf)[2] == 5){
        updateSelectInput(session, "Rsel_Fxna", choices = 5, selected = 5)
      }
      main = c("check")
      show("x_Mv_AA"); show("x_F1_AA")
      show("MvPlotaDesc_AA"); show("Mv12121_AA")
      updateSliderInput(session, "x_F1_AA", max=dim(dataf)[1], min=1, value=1, step=1)
      updateSliderInput(session, "x_Mv_AA", min = 1, max=dim(dataf)[2], value=1, step=1)
      colnam <- colnames(dataf)
      if(all(colnam == paste("V", 1:dim(dataf)[2], sep=""))){
        names = F
      } else {
        names = T
      }

      #######################################################
      ## Start - Display possible warnings, if uploaded data
      ## isn't the correct type
      #######################################################
      if(!input$header){
        row <- dataf[1,]
        if(!(sum(!is.na(as.numeric(as.matrix(dataf[-1,])))) == length(as.matrix(dataf[-1,])))){
          show("Numeric_File")
          output$Numeric_File <- renderText({
            paste(h3("X must contain only numeric values. Please upload a different dataset."), br())
          })

        } else {
          show("Numeric_File")
          output$Numeric_File <- renderText({
            paste(h3("The first row in your data is the only row that isn't numeric. Did you mean to check the box for the header?"), br())
          })
        }
      } else if(names){
        if(!(sum(!is.na(as.numeric(as.matrix(dataf[-1,])))) == length(as.matrix(dataf[-1,])))){
          show("Numeric_File")
          output$Numeric_File <- renderText({
            paste(h3("X must contain only numeric values. Please upload a different dataset."), br())
          })
        } else {
          hide("Numeric_File")
        }
      } else {
        if(!(sum(!is.na(as.numeric(as.matrix(dataf)))) == length(as.matrix(dataf)))){
          show("Numeric_File")
          output$Numeric_File <- renderText({
            paste(h3("X must contain only numeric values. Please upload a different dataset."), br())
          })
        } else {
          hide("Numeric_File")
        }
      }
      #######################################################
      ## End - Display possible warnings, if uploaded data
      ## isn't the correct type
      #######################################################

      output$FxnPlotaDesc_AA <- renderText({
        paste(h4("Currently viewing timepoint "))
      })
      output$MvPlotaDesc_AA <- renderText({
        paste(h4("Currently viewing component "))
      })

      list(dataf=dataf, main = main, colnames = colnam, names = names)}}
  )
  #######################################################
  ## End - Read in and store the chosen inputted data
  #######################################################


  observeEvent(input$file_csv, {
    if(is.na(plot.listbb()[[2]])){

    } else {

      #######################################################
      ## Start - Initialize Plot for First timepoint in
      ## Observed, Functional Data
      #######################################################

      output$Test_Fxna_Plot1<- renderPlotly({
        a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[2]), y=as.numeric(plot.listbb()[[1]][1,]))) +
          xlab("Functional Domain") + ylab("") + ggtitle("Observed Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          scale_x_continuous(limits=c(0,1), expand=c(0,0))
        plotly <- ggplotly(a)
        fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                           mode = 'lines',
                                           line= list(color='black'),
                                           x = seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[2]),
                                           y = as.numeric(plot.listbb()[[1]][1,]),
                                           hovertemplate = paste('<i>Functional Domain</i>: %{x:.5f}',
                                                                 '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
      })
      #######################################################
      ## End - Initialize Plot for First timepoint in
      ## Observed, Functional Data
      #######################################################

      #######################################################
      ## Start - Initialize Plot for First Component in
      ## Observed, Multivariate Data
      #######################################################

      output$Test_Mva_Plot1 <- renderPlotly({
        a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[1]), y=as.numeric(plot.listbb()[[1]][,1]))) +
          xlab("Time") + ylab("") + ggtitle("Observed Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          scale_x_continuous(limits=c(0,1), expand=c(0,0))
        plotly <- ggplotly(a)
        fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                           mode = 'lines',
                                           line= list(color='black'),
                                           x = seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[1]),
                                           y = as.numeric(plot.listbb()[[1]][,1]),
                                           hovertemplate = paste('<i>Time</i>: %{x:.5f}',
                                                                 '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
      })

      #######################################################
      ## End - Initialize Plot for First Component in
      ## Observed, Multivariate Data
      #######################################################

      #######################################################
      ## Start - Hide and Reset Plots and Descriptors, to have a "blank
      ## slate" whenever a new file is uploaded, before any code is run
      #######################################################


      output$test12121_AA <- renderText({
        paste(h4(strong("1")))
      })
      if(plot.listbb()[[4]]){
        output$Mv12121_AA <- renderText({
          paste(h4(strong((paste(plot.listbb()[[3]][1])))))
        })
      } else {
        output$Mv12121_AA <- renderText({
          paste(h4(strong((paste("1")))))
        })
      }
      hide("MvbPlotDesc_file")
      hide("Mv1_file")
      hide("MvPlot22Desc_file")
      hide("BlankMv_file")
      hide("q11_Mv_file")
      hide("plot1_MvCheck_file")
      hide("Blank2_file")
      hide("FxnbPlotDesc_file")
      hide("plot1_FxnCheck_file")
      hide("Fxn_Plotb_file")
      hide("downloadDataFXN1_File")
      hide("summ_out_fxn_file")
      hide("summ_pval_fxn_file")
      hide("summ_out_Mv_file")
      hide("summ_pval_Mv_file")
      hide("Mv_Plotb_file")
      hide("Mv2_file")
      hide("plot1_MvCheck_file")
      hide("BlankMv_file")
      hide("q11_Mv_file")
      hide("MvbPlotDesc_file")
      hide("MvPlot22Desc_file")
      output$Mv_Plotb_file <- renderPlot({

      })
      output$Plotly_Mvb_file <- renderPlotly({

      })
      output$Fxn_Plotb_file <- renderPlot({

      })
      hide("FxnPlot22Desc_file")
      hide("Blank10100_file")
      hide("q11_F1_file")
      output$Plotly_Fxna_file <-renderPlotly({

      })
      output$Plotly_Fxnb_file <- renderPlotly({

        })

      #######################################################
      ## End - Hide and Reset Plots and Descriptors, to have a "blank
      ## slate" whenever a new file is uploaded, before any code is run
      #######################################################

    }
  })

  #######################################################
  ## Start - Show the necessary descriptors and sliders
  ## for the Observed, Functional algorithm
  #######################################################

  observeEvent(input$go_Fxna, {
    show("Blank2_file")
    show("FxnbPlotDesc_file")
    show("plot1_FxnCheck_file")
    show("Fxn_Plotb_file")
    show("FxnPlot22Desc_file")
    show("Blank10100_file")
    show("q11_F1_file")
  })

  #######################################################
  ## End - Show the necessary descriptors and sliders
  ## for the Observed, Functional algorithm
  #######################################################


  #######################################################
  ## Start - Conditionally shows or hides descriptors and
  ## sliders on the file upload side, depending on if the
  ## inputted data is univariate or not
  #######################################################

  observeEvent(input$file_csv, {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    dataf <- read.csv(file$datapath, header = input$header)
    dims <- dim(dataf)[2]
    if(dims == 1){
      hide("NotUniVar")
      show("UniVarDis")
      output$UniVarDis <- renderText({
        paste(strong("This is a Univariate Time Series"))
      })
      hide("Data_Checker")
      updateRadioButtons(session, "Data_Checker", selected=character(0))
      hide("Plot3D_File")
      hide("Plot3D_FileM")
      hide("Num_Fxna")
      hide("Tapers_Fxna")
      hide("Ts_Fxn_Dim")
      hide("Rsel_Fxna")
      hide("Warnings")
      hide("Fxn_AA")
      hide("Fxn_BB")
      hide("Fxn_CC")
      hide("Fxn_DD")
      hide("Mv_AA")
      hide("Signi_Fxna")
      hide("TF_Fxna")
      hide("go_Fxna")
      hide("Ts_Mv_Dim")
      hide("nrepMv_file")
      hide("WselMv_file")
      hide("go_Mva")
      show("T_len")
      show("Num2")
      show("Tapers2")
      show("Signi2")
      show("TF2")
      show("go2")
      show("res9")
      show("res10")
      show("res11")
      show("Image_Plota")
      show("Image_Plot2")
      show("downloadData1")
      show("downloadData1_A1")
      show("summ_out_uni_file")
      show("summ_pval_uni_file")
    } else {
      dataf <- dataf
      hide("UniVarDis")
      show("NotUniVar")
      output$NotUniVar <- renderText({
        paste(strong("This time series has multiple components. Choose what type of
                     time series this is. "))
      })
      output$Ts_Fxn_Dim <- renderText({
        paste(strong(paste("Dimensions of Time Series (T x R): ", dim(dataf)[1], "x", dim(dataf)[2] )))
      })
      output$Ts_Mv_Dim <- renderText({
        paste(strong(paste("Dimensions of Time Series (T x R): ", dim(dataf)[1], "x", dim(dataf)[2] )))
      })
      if(dim(dataf)[2] < 10){
        output$Fxn_CC <- renderText({
          paste(h6("***Valid choices are 5 as we need to satisfy 1", HTML("&le;"),
                   "Rsel", HTML("&le;")," R"))
        })
      } else {
        output$Fxn_CC <- renderText({
          paste(h6("***Valid choices are 5, and 10 as we need to satisfy 1", HTML("&le;"),
                   "Rsel", HTML("&le;")," R"))
        })
      }

      show("Data_Checker")
      #show("Plot3D_File")
      hide("T_len")
      hide("Num2")
      hide("Tapers2")
      hide("Signi2")
      hide("TF2")
      hide("go2")
      hide("res9")
      hide("res10")
      hide("res11")
      hide("Image_Plota")
      hide("Image_Plot2")
      hide("downloadData1")
      hide("downloadData1_A1")
      hide("summ_out_uni_file")
      hide("summ_pval_uni_file")
    }
  })

  #######################################################
  ## Hide - Conditionally shows or hides descriptors and
  ## sliders on the file upload side, depending on if the
  ## inputted data is univariate or not
  #######################################################

  #######################################################
  ## Start - Conditionally shows or hides descriptors and
  ## sliders on the file upload side, depending on if the
  ## inputted data is functional or multivariate- as this
  ## wouldn't trigger if the data is univariate
  #######################################################

  observeEvent(input$Data_Checker, {
    if(input$Data_Checker == "Functional"){
      show("Plot3D_File")
      show("Num_Fxna")
      show("Tapers_Fxna")
      show("Rsel_Fxna")
      show("Signi_Fxna")
      show("TF_Fxna")
      show("go_Fxna")
      show("Ts_Fxn_Dim")
      show("Warnings")
      output$Warnings <- renderText({
        paste(h5(strong("NOTE: Selecting Values for N and K that are outside the range of the valid choices outlined below will crash the application")))
      })
      show("Fxn_AA")
      output$Fxn_AA <- renderText({
        paste(h6("*Valid choices range from 30 to ", dim(plot.listbb()[[1]])[1] , "as we need to satisfy 30", HTML("&le;"),
                 "N", HTML("&le;"), "T"))
      })
      updateNumericInput(session, "Num_Fxna", min=30, max=dim(plot.listbb()[[1]])[1])
      show("Fxn_BB")
      output$Fxn_BB <- renderText({
        paste(h6("**Valid choices range from 1 to ", floor(sqrt(dim(plot.listbb()[[1]])[1]) / 4 - 1) - 1, "as we need to satisfy 1", HTML("&le;"),
                 "K < floor(N/4 - 1)"))
      })
      output$Fxn_DD <- renderText({
        paste(h6("For more explanation of the above terms, and
             the algorithm that is run, consult:",HTML("<br>"), "Efficient Algorithm for Frequency-Domain Dimension Reduction of Functional Time Series via Adaptive Frequency Band Learning by Bruce and Bagchi (2020+)", HTML(paste(tags$a("doi.org/10.48550/arXiv.2102.01784", ref="https://doi.org/10.48550/arXiv.2102.01784")))))
      })
      show("Fxn_DD")
      show("Fxn_CC")
      hide("Ts_Mv_Dim")
      hide("nrepMv_file")
      hide("WselMv_file")
      hide("go_Mva")
      hide("Plot3D_FileM")
      hide("Mv_AA")
    } else {
      output$Mv_AA <- renderText({
        paste(h6("For more explanation of the above terms, and
             the algorithm that is run, consult:", HTML("<br>"),"Frequency Band Analysis of Nonstationary Multivariate Time Series by Raanju R. Sundararajan and Scott A. Bruce (2023)", HTML(paste(tags$a("doi.org/10.48550/arXiv.2301.03664", href="https://doi.org/10.48550/arXiv.2301.03664")))))
      })
      hide("Warnings")
      show("Mv_AA")
      hide("Plot3D_File")
      hide("Num_Fxna")
      hide("Tapers_Fxna")
      hide("Rsel_Fxna")
      hide("Signi_Fxna")
      hide("TF_Fxna")
      hide("go_Fxna")
      hide("Ts_Fxn_Dim")
      hide("Fxn_AA")
      hide("Fxn_BB")
      hide("Fxn_CC")
      hide("Fxn_DD")
      show("Ts_Mv_Dim")
      show("nrepMv_file")
      show("WselMv_file")
      show("go_Mva")
      show("Plot3D_FileM")
    }
  })

  #######################################################
  ## End - Conditionally shows or hides descriptors and
  ## sliders on the file upload side, depending on if the
  ## inputted data is functional or multivariate- as this
  ## wouldn't trigger if the data is univariate
  #######################################################

  observeEvent(input$file_csv, {

   output$T_len <- renderText({
     paste(strong("Total Length of Time Series (T): ", length(plot.listaa()[[1]])))
   }) # Displays length of uploaded, univariate data

   output$Blank <- renderText({
     paste("")
   })

   #######################################################
   ## Start - Create a plot of Uploaded, Univariate data
   #######################################################
    output$Image_Plota <- renderPlotly({
      a <- ggplot() + geom_line(aes(x=seq(0,1,length.out = length(plot.listaa()[[1]])), y= plot.listaa()[[1]])) + xlab("Time") +
               ylab("") + ggtitle("Observed Time Series Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
               scale_x_continuous(limits=c(0,1), expand=c(0,0))
      plotly <- ggplotly(a)
      fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                         mode = 'lines',
                                         line= list(color='black'),
                                         x = seq(from=0, to=1, length.out=length(plot.listaa()[[1]])),
                                         y = plot.listaa()[[1]],
                                         hovertemplate = paste('<i>Time</i>: %{x:.5f}',
                                                               '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
    })
   #######################################################
   ## End - Create a plot of Uploaded, Univariate data
   #######################################################

    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    le <- length(file[[1]])
    if(le == 0){

    } else {
      file <- input$file_csv
      ext <- tools::file_ext(file$datapath)
      dataf <- read.csv(file$datapath, header = input$header)
      dataf <- dataf[[1]]

      #######################################################
      ## Start - Get Initial Values for parameters in Univariate
      ## algorithm, based on uploaded data
      #######################################################
    updateNumericInput(session, "Num2", value=0)
    updateNumericInput(session, "Num2", value=floor(sqrt(length(dataf))), min=30, max=floor(length(dataf)/2))
    updateNumericInput(session, "Tapers2", value=floor(0.15 * sqrt(length(dataf))))
    output$res9 <- renderText({
        paste(h6("*Valid choices range from 30 to ", floor(length(dataf)/2), "as we need to satisfy 30", HTML("&le;"),
                                                                                                   "N", HTML("&le;"), HTML(paste(tags$sup("T"))), "/", HTML(paste(tags$sub(2)))))
    })

    output$res10 <- renderText({
          paste("**Valid choices range from 1 to ", floor(sqrt(length(dataf))*0.24))
    }) }
    #######################################################
    ## End - Get Initial Values for parameters in Univariate
    ## algorithm, based on uploaded data
    #######################################################
  })

  #######################################################
  ## Start - Get Values for K based on N, for Uploaded
  ## Univariate data
  #######################################################

  observeEvent(input$Num2, ignoreNULL = FALSE, {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    le <- length(file[[1]])
    if(le == 0){
      output$res10 <- renderText({
          paste()
       })
    } else {
    T_B <- input$Num2
    K <- input$Tapers2
    if(!is.na(T_B) & !is.na(K)){
      f_part <- c(1, floor(T_B/2 + 1))
      diff <- abs(diff(f_part)) / 2
      for(i in 1:T_B){
        temp_k <- i
        bw <- floor((temp_k+1)*(T_B/(T_B+1))) + 1
        if(bw < diff) {

        } else {
          break
        }
      }
      bw <- floor((K+1)*(T_B/(T_B+1))) + 1
      f_part <- c(1, floor(T_B/2 + 1))
      flo_mea <- floor(mean(f_part))
      max_tap <- ((flo_mea - 1) * ((T_B+1)/T_B)) - 1
      mmm <- abs(diff(f_part))
      output$res10 <- renderText({
        paste(h6("**Valid choices range from 1 to ", (i-1), "as we need to satisfy ", HTML(paste(tags$sup("floor(N/2)"))),
                 "/", HTML(paste(tags$sub(2))), " - 1> floor((K+1)(", HTML(paste(tags$sup("N"))),
                 "/", HTML(paste(tags$sub("N+1"))), "))"))
      })
      updateNumericInput(session, "Tapers2", min=1, max=i-1)

      output$res11 <- renderText({
        paste(h6("For more explanation of the above terms, and the algorithm that is run, consult:",HTML("<br>"),"Scott A. Bruce, Cheng Yong Tang, Martica H. Hall & Robert T. Krafty (2020) Empirical Frequency Band Analysis of Nonstationary Time Series, Journal of the American Statistical Association, 115:532, 1933-1945,", HTML(paste(tags$a("doi.org/10.1080/01621459.2019.1671199", href = "https://doi.org/10.1080/01621459.2019.1671199")))))
      })
    }
    }

  })

  #######################################################
  ## End - Get Values for K based on N, for Uploaded
  ## Univariate data
  #######################################################

  #######################################################
  ## Start - Set Reminder text on File Upload, Functional
  ## Side, as to valid choices for K
  #######################################################

  observeEvent(input$Num_Fxna, {
    curr_num <- as.numeric(input$Num_Fxna)
    output$Fxn_BB <- renderText({
      paste(h6("**Valid choices range from 1 to ", floor((curr_num/ 4 - 1)) - 1 , "as we need to satisfy 1", HTML("&le;"),
               "K < floor(N/4 - 1)"))
    })
    updateNumericInput(session, "Tapers_Fxna", min=1, max=floor((curr_num/ 4 - 1)) - 1)
  })

  #######################################################
  ## End - Set Reminder text on File Upload, Functional
  ## Side, as to valid choices for K
  #######################################################

  #######################################################
  ## Start - Update plot of Uploaded Functional Data,
  ## to display timepoint chosen via slider
  #######################################################

  observeEvent(input$x_F1_AA, ignoreNULL = FALSE, {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    le <- length(file[[1]])
    if(le == 0){

    } else if(is.na(plot.listbb()[[2]])){

    } else {
      get_num <- as.numeric(input$x_F1_AA)

      output$test12121_AA <- renderText({
        paste(h4(strong((paste(get_num)))))
      })
      output$Test_Fxna_Plot1 <- renderPlotly({
        a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listbb()[[1]][get_num,])), y=as.numeric(plot.listbb()[[1]][get_num,]))) +
          xlab("Functional Domain") + ylab("") + ggtitle("Observed Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          scale_x_continuous(limits=c(0,1), expand=c(0,0))
        plotly <- ggplotly(a)
        fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                           mode = 'lines',
                                           line= list(color='black'),
                                           x = seq(from=0, to=1, length.out=length(plot.listbb()[[1]][get_num,])),
                                           y = as.numeric(plot.listbb()[[1]][get_num,]),
                                           hovertemplate = paste('<i>Functional Domain</i>: %{x:.5f}',
                                                                 '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
      })
    }

  })

  #######################################################
  ## End - Update plot of Uploaded Functional Data,
  ## to display timepoint chosen via slider
  #######################################################

  #######################################################
  ## Start - Update plot of Uploaded Multivariate Data,
  ## to display component chosen via slider
  #######################################################

  observeEvent(input$x_Mv_AA, ignoreNULL = FALSE, {
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    le <- length(file[[1]])
    if(le == 0){

    } else if(is.na(plot.listbb()[[2]])){

    } else {
      get_num <- as.numeric(input$x_Mv_AA)
      if(plot.listbb()[[4]]){
        output$Mv12121_AA <- renderText({
          paste(h4(strong((paste(plot.listbb()[[3]][get_num])))))
        })
      } else {
        output$Mv12121_AA <- renderText({
          paste(h4(strong((paste(get_num)))))
        })
      }
      output$Test_Mva_Plot1 <- renderPlotly({
        a <- ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[1]), y=as.numeric(plot.listbb()[[1]][,get_num]))) +
          xlab("Time") + ylab("") + ggtitle("Observed Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          scale_x_continuous(limits=c(0,1), expand=c(0,0))
        plotly <- ggplotly(a)
        fin_plot <- plotly %>% add_trace(  type = 'scatter',
                                           mode = 'lines',
                                           line= list(color='black'),
                                           x = seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[1]),
                                           y = as.numeric(plot.listbb()[[1]][,get_num]),
                                           hovertemplate = paste('<i>Time</i>: %{x:.5f}',
                                                                 '<br><i>Value</i>: %{y:.5f}<extra></extra>'))
      })
    }

  })

  #######################################################
  ## End - Update plot of Uploaded Multivariate Data,
  ## to display component chosen via slider
  #######################################################

  output$res10 <- renderText({

  })

  #######################################################
  ## Event Reactive Block Start point for the Observed
  ## Univariate algorithm
  #######################################################

  plot.list2 <- eventReactive(input$go2, ignoreNULL = FALSE, {
    hide('downloadData1')
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    dataf <- read.csv(file$datapath, header = input$header)
    dataf <- as.vector(dataf[[1]], mode = "numeric")
    ebaoutfu <- eba.search(X=dataf,N= as.numeric(input$Num2),K=as.numeric(input$Tapers2),std=input$TF2,alpha=as.numeric(input$Signi2))
    plot.x = ebaoutfu$mtspec$t
    plot.y = ebaoutfu$mtspec$f
    plot.z = t(ebaoutfu$mtspec$mtspec)
    plot.main = "Multitaper Spectrogram"
    plot.h = ebaoutfu$part.final[c(-1,-length(ebaoutfu$part.final))]
    plot.data = dataf
    plot.log <- ebaoutfu$log
    plot.pvals <- ebaoutfu$pvals
    plot.flat <- ebaoutfu$flat
    show('downloadData1')
    list(plot.x = plot.x, plot.y = plot.y, plot.z = plot.z,
         plot.main = plot.main, plot.h = plot.h, plot.data = plot.data,
         plot.log = plot.log, plot.pvals = plot.pvals, plot.flat = plot.flat)})

  #######################################################
  ## Event Reactive Block End point for the Observed
  ## Univariate algorithm
  #######################################################

  #######################################################
  ## Event Reactive Block Start point for the Observed
  ## Multivariate algorithm
  #######################################################

  plot.listMv2 <- eventReactive(input$go_Mva, ignoreNULL = FALSE, {
    hide("downloadDataMv1_File")
    hide("MvbPlotDesc_file")
    hide("Mv2_file")
    hide("plot1_MvCheck_file")
    hide("MvPlot22Desc_file")
    hide("BlankMv_file")
    hide("q11_Mv_file")
    #################################################
    ## Start - Read in the Data
    #################################################
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    dataf <- read.csv(file$datapath, header = input$header)
    component_names <- colnames(dataf)
    if(all(component_names == paste("V", 1:dim(dataf)[2], sep=""))){
      names = FALSE
    } else {
      names = TRUE
    }
    dataf <- as.matrix(dataf)
    #################################################
    ## End - Read in the Data
    #################################################

    #################################################
    ## Start - Initialize values based on data
    #################################################
    colnames(dataf) <- NULL
    nrow = nrow(dataf); ncol = ncol(dataf)
    t <- nrow; R <- ncol
    nrep = as.numeric(input$nrepMv_file)
    Wsel = as.numeric(input$WselMv_file)
    seed=234; #seed for reproducibility
    N <- 2*floor(t^0.7)-floor(t^0.7/2)*2; #neighborhood for local periodogram
    freq <- seq(0,floor(N/2),by=1)/N
    #################################################
    ## End - Initialize values based on data
    #################################################

    #################################################
    ## Start - Run the algorithm
    #################################################
    pse <- fhat_lp(dataf,N,stdz=FALSE);
    gpse <- ghat(pse);
    shinyjs::html(id='ObsMv_text', html = "<h4> <strong> You can check the progress of the algorithm in the file 'obs-mEBA.txt', in your working directory.
                      Rerunning the algorithm for Observed, Multivariate data will overwrite
                      the contents of the file, and closing the application will delete the file. If you wish to preserve the file, you can
                      change its name, to ensure it won't be overwritten or deleted.<strong> <h4>")
    show("ObsMv_text")
    sink("obs-mEBA.txt")
    b.out=msboot(nrep=nrep, dataf, Wsel=Wsel, stdz=FALSE, ncore=1)
    sink()
    method <- b.out
    #################################################
    ## End - Run the algorithm
    #################################################

    #################################################
    ## Start - Save and store values from algorithm
    #################################################
    vals <- b.out[[4]][which(b.out[[4]][,2]==1),1]
    indexes <- method[[4]][,2][method[[4]][,1]%in% method[[3]][[1]][,1]]
    thresh <- numeric(0)
    for(i in 1:nrow(method[[3]])){
      thresh[i] <- 0.05 / nrow(method[[3]][[i]])
    }
    thresh_bounds <- numeric(0)
    num_W <- dim(method[[3]])[1]
    pvals <- numeric(0)
    fre <- numeric(0)
    for(i in 1:num_W){
      curr <- method[[3]][[i]]
      pvals <- c(pvals, curr[,2])
      fre <- c(fre, curr[,1])
    }
    min_val <- numeric(0)
    for(i in 1:length(unique(fre))){
      cur_freq <- unique(fre)[i]
      all_pvals <- pvals[which(fre == cur_freq)]
      min_val <- c(min_val, min(all_pvals))
      thresh_bounds[i] <- thresh[[which.min(all_pvals)]]
    }
    mod_val <- min_val
    for(i in 1:length(min_val)){
      if(min_val[i] == 0.000){
        mod_val[i] <- paste("<", 1/nrep, sep="")
      }
    }
    uni_fre <- unique(fre)
    dimnames(pse) <- list(freq,apply(expand.grid(1:R,1:R),1,paste,collapse = "-"),1:t);
    #################################################
    ## End - Save and store values from algorithm
    #################################################

    comp_names <- apply(expand.grid(1:R,1:R),1,paste,collapse = "-")


    #################################################
    ## Start - Update Descriptors and Sliders
    #################################################
    hide("ObsMv_text")

    show("MvPlotaDesc_AA")
    show("Mv12121_AA")
    show("x_Mv_AA")
    show("Mv2_file")
    show("Mv_Plotb_file")
    show("plot1_MvCheck_file")
    show("q11_Mv_file")
    show("BlankMv_file")
    show("summ_out_Mv_file")
    show("summ_pval_Mv_file")
    show("MvbPlotDesc_file")
    show("MvPlot22Desc_file")
    show("downloadDataMv1_File")
    # show("q11_Mv1")
    updateSliderInput(session, "x_Mv_AA", min = 1, max=as.numeric(R), value=1, step=1)
    updateSliderInput(session, "plot1_MvCheck_file", min = 1, max=as.numeric(R), value=1, step=1)
    updateSliderInput(session, "q11_Mv_file", min = 1, max=as.numeric(R), value=1, step=1)
    # output$MvPlotaDesc <- renderText({
    #   paste(h4("Currently viewing component "))
    # })
    if(names){
      first = component_names[1]
      output$Mv12121_AA <- renderText({
        paste(h4(strong((paste(first)))))
      })
      output$Mv2_file <- renderText({
        paste(h4(strong((paste(first)))))
      })
      output$BlankMv_file <- renderText({
        paste(h4(strong((paste(first)))))
      })
    } else {
      output$Mv12121_AA <- renderText({
        paste(h4(strong((paste("1")))))
      })
      output$Mv2_file <- renderText({
        paste(h4(strong((paste("1")))))
      })
      output$BlankMv_file <- renderText({
        paste(h4(strong((paste("1")))))
      })
    }
    output$MvbPlotDesc_file <- renderText({
       paste(h4("Currently viewing component "))
    })
    output$MvPlot22Desc_file <- renderText({
      paste(h4("Currently viewing component "))
    })
    output$MvPlotaDesc_AA <- renderText({
      paste(h4("Currently viewing component "))
    })
    check = "Test"
    #################################################
    ## End - Update Descriptors and Sliders
    #################################################
    list(X = dataf, pse = pse, freq = freq, vals = vals, min_val = min_val, mod_val = mod_val,
         comp_names = comp_names, thresh_bounds = thresh_bounds,
         tested_freq = uni_fre, indexes=indexes, check = check, component_names = component_names,
         names=names)
  })

  #######################################################
  ## Event Reactive Block End point for the Observed
  ## Multivariate algorithm
  #######################################################

  observeEvent(plot.listMv2()[[11]], {

  })

  #################################################
  ## Start - Initalize the Periodogram for the 1-1
  ## Component in the Observed, Multivariate data
  #################################################
  output$Mv_Plotb_file <- renderPlot({
    image.plot(x=(1:as.numeric(dim(plot.listMv2()[[1]])[1])) / (as.numeric(dim(plot.listMv2()[[1]])[1])),y=plot.listMv2()[[3]][-1],z=t(Re(plot.listMv2()[[2]][-1,1,])),
               axes = TRUE, col = inferno(256),
               main = "Local Periodogram",xlab='Time',ylab='Hz',xaxs="i",
               bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
    abline(h=plot.listMv2()[[4]], col="skyblue", lwd=3);
    vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                      just=c("left", "top"), y=0.55, x=0.65)
    len <- length(plot.listMv2()[[4]])
    vals <- plot.listMv2()[[4]]
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    pp <- data.frame(
      "Predicted Frequency Bands" = spp)
    colnames(pp) <- c(
      "Predicted \n Frequency Bands")
    grid.table(pp, vp=vp.br, rows=NULL)

    vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                     just=c("left", "top"), y=0.95, x=0.65)
    grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
    jj <- grid.legend(c("Predicted Partition Points"
    ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
    )), vp=vp.r,
    draw=TRUE)
  })
  #################################################
  ## End - Initalize the Periodogram for the 1-1
  ## Component in the Observed, Multivariate data
  #################################################

  #################################################
  ## Start - Update the Periodogram for the
  ## Observed, Multivariate data, based on selected component
  #################################################
  observeEvent(input$plot1_MvCheck_file, {
    curr_num <- as.numeric(input$plot1_MvCheck_file)
    if(is.na(plot.listMv2()[[11]])){

    } else {
      if(plot.listMv2()[[13]]){
      curr = plot.listMv2()[[12]][curr_num]
      curr_comp <- paste(curr)
    } else {
      curr_comp <- paste(curr_num)
    }
    output$Mv2_file <- renderText({
      paste(h4(strong((paste(curr_comp)))))
    })
    output$Mv_Plotb_file <- renderPlot({
      image.plot(x=(1:as.numeric(dim(plot.listMv2()[[1]])[1])) / (as.numeric(dim(plot.listMv2()[[1]])[1])),y=plot.listMv2()[[3]][-1],z=t(Re(plot.listMv2()[[2]][-1,curr_num+(curr_num-1)*dim(plot.listMv2()[[1]])[2],])),
                 axes = TRUE, col = inferno(256),
                 main = "Local Periodogram",xlab='Time',ylab='Hz',xaxs="i",
                 bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
      abline(h=plot.listMv2()[[4]], col="skyblue", lwd=3);
      vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                        just=c("left", "top"), y=0.55, x=0.65)
      len <- length(plot.listMv2()[[4]])
      vals <- plot.listMv2()[[4]]
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pp <- data.frame(
        "Predicted Frequency Bands" = spp)
      colnames(pp) <- c(
        "Predicted \n Frequency Bands")
      grid.table(pp, vp=vp.br, rows=NULL)

      vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                       just=c("left", "top"), y=0.95, x=0.65)
      grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
      jj <- grid.legend(c("Predicted Partition Points"
      ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
      )), vp=vp.r,
      draw=TRUE)
    })
    }
  })
  #################################################
  ## End - Update the Periodogram for the
  ## Observed, Multivariate data, based on selected component
  #################################################

  #################################################
  ## Start - Initalize the 3-D Plot for the 1-1 component
  ## of the periodogram for observed, multivariate data
  #################################################
  observeEvent(plot.listMv2()[[11]], {
    output$Plotly_Mvb_file <- renderPlotly({
      plot_ly(y=~seq(from=0, to=1, length.out=nrow(plot.listMv2()[[1]])),
              x=~plot.listMv2()[[3]][-1],
              z=~t(Re(plot.listMv2()[[2]][-1,1,])))  %>%layout(title="3D Representation of Local Periodogram",scene = list( xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                            yaxis = list(title="Timepoint", range=c(0,1)),
                                                                                                                            zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
    })
  })
  #################################################
  ## End - Initalize the 3-D Plot for the 1-1 component
  ## of the periodogram for observed, multivariate data
  #################################################

  #################################################
  ## Start - Update the Periodogram for the
  ## Observed, Multivariate data, based on selected component
  #################################################
  observeEvent(plot.listMv2()[[11]], {
    output$Mv_Plotb_file <- renderPlot({
      image.plot(x=(1:as.numeric(dim(plot.listMv2()[[1]])[1])) / (as.numeric(dim(plot.listMv2()[[1]])[1])),y=plot.listMv2()[[3]][-1],z=t(Re(plot.listMv2()[[2]][-1,1,])),
                 axes = TRUE, col = inferno(256),
                 main = "Local Periodogram",xlab='Time',ylab='Hz',xaxs="i",
                 bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
      abline(h=plot.listMv2()[[4]], col="skyblue", lwd=3);
      vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                        just=c("left", "top"), y=0.55, x=0.65)
      len <- length(plot.listMv2()[[4]])
      vals <- plot.listMv2()[[4]]
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pp <- data.frame(
        "Predicted Frequency Bands" = spp)
      colnames(pp) <- c(
        "Predicted \n Frequency Bands")
      grid.table(pp, vp=vp.br, rows=NULL)

      vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                       just=c("left", "top"), y=0.95, x=0.65)
      grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
      jj <- grid.legend(c("Predicted Partition Points"
      ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
      )), vp=vp.r,
      draw=TRUE)
    })
    #################################################
    ## End - Update the Periodogram for the
    ## Observed, Multivariate data, based on selected component
    #################################################

    #################################################
    ## Start - Initalize the 3-D Plot for the 1-1 component
    ## of the periodogram for observed, multivariate data
    #################################################
    output$Plotly_Mvb_file <- renderPlotly({
      plot_ly(y=~seq(from=0, to=1, length.out=nrow(plot.listMv2()[[1]])),
              x=~plot.listMv2()[[3]][-1],
              z=~t(Re(plot.listMv2()[[2]][-1,1,])))  %>%layout(title="3D Representation of Local Periodogram",scene = list( xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                      yaxis = list(title="Timepoint", range=c(0,1)),
                                                                                                                      zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
    })
    #################################################
    ## End - Initialize the 3-D Plot for the 1-1 component
    ## of the periodogram for observed, multivariate data
    #################################################

    #################################################
    ## Start - Display Table containing results on
    ## partition testing for Observed, Multivariate Data
    #################################################
    output$summ_out_Mv_file <- renderPlot({
      freq <- round(plot.listMv2()[[9]], 3)
      mod_pval <- plot.listMv2()[[6]]
      min_pval <- round(plot.listMv2()[[5]], 5)
      thresh <- round(plot.listMv2()[[8]], 5)
      Sig <- character(length(min_pval))
      for(i in 1:length(Sig)){
        if(min_pval[i] < thresh[i]){
          Sig[i] <- "TRUE"
        } else {
          Sig[i] <- "FALSE"
        }
      }
      res <- data.frame("Freq" = freq, "val" = mod_pval, "t"=thresh, "s" = as.character(Sig))
      colnames(res) <- c("Frequency", "Minimum \n P-Value", "P-Value \n Threshold", "Significant")
      sig <- which(plot.listMv2()[[9]] %in% plot.listMv2()[[4]])
      if(length(sig) == 0){
        sig = which.min(min_pval)
      }
      res <- res[sig, ]
      res1 <- tableGrob(res, rows = NULL)
      title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(title, res1, blank0909, ncol = 1)
    })
    #################################################
    ## End - Display Table containing results on
    ## partition testing for Observed, Multivariate Data
    #################################################

    #################################################
    ## Start - Create Scatterplot of p-values for Partition
    ## testing for Observed, Multivariate data at each frequency
    #################################################
    output$summ_pval_Mv_file <- renderPlot({
      ggplot() + geom_point(aes(x = as.numeric(plot.listMv2()[[9]]), y = as.numeric(plot.listMv2()[[5]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
        xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
        geom_vline(xintercept = plot.listMv2()[[4]], linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01))
    })
    #################################################
    ## End - Create Scatterplot of p-values for Partition
    ## testing for Observed, Multivariate data at each frequency
    #################################################
  })

  #################################################
  ## Start - Update the 3-D Plot for the selected component
  ## of the periodogram for observed, multivariate data
  #################################################
  observeEvent(input$q11_Mv_file, ignoreNULL = TRUE, {
    curr_num <- as.numeric(input$q11_Mv_file)
    if(is.na(plot.listMv2()[[11]])){

    } else {
      if(plot.listMv2()[[13]]){
      curr = plot.listMv2()[[12]][curr_num]
      curr_comp <- paste(curr)
    } else {
      curr_comp <- paste(curr_num)
    }
    output$BlankMv_file <- renderText({
      paste(h4(strong((paste(curr_comp)))))
    })
    output$Plotly_Mvb_file <- renderPlotly({
      plot_ly(y=~seq(from=0, to=1, length.out=nrow(plot.listMv2()[[1]])),
              x=~plot.listMv2()[[3]][-1],
              z=~t(Re(plot.listMv2()[[2]][-1,curr_num + (curr_num-1)*ncol(plot.listMv2()[[1]]),])))  %>%layout(title="3D Representation of Periodogram",scene = list( xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                                                                    yaxis = list(title="Timepoint", range=c(0,1)),
                                                                                                                                                                    zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
    })
    }
  })
  #################################################
  ## End - Update the 3-D Plot for the selected component
  ## of the periodogram for observed, multivariate data
  #################################################

  #######################################################
  ## Event Reactive Block Start point for the Observed
  ## Functional algorithm
  #######################################################
  plot.listFxn2 <- eventReactive(input$go_Fxna,  {
    hide("FxnbPlotDesc_file")
    hide("Blank2_file")
    hide("plot1_FxnCheck_file")
    hide("FxnPlot22Desc_file")
    hide("Blank10100_file")
    hide("q11_F1_file")
    hide("downloadDataFXN1_File")
    #################################################
    ## Start - Read in the Data
    #################################################
    file <- input$file_csv
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    dataf <- read.csv(file$datapath, header = input$header)
    dataf <- as.matrix(dataf)
    #################################################
    ## End - Read in the Data
    #################################################

    #################################################
    ## Start - Initialize values based on data
    #################################################
    colnames(dataf) <- NULL
    nrow = nrow(dataf); ncol = ncol(dataf)
    nb=15; #number of basis functions used to generate white noise
    R=dim(dataf)[2]; #number of points in functional domain
    Ts=dim(dataf)[1]; #length of time series
    seed=234; #seed for reproducibility
    B=floor(dim(dataf)[1]/as.numeric(input$Num_Fxna)); #number of time blocks
    N=as.numeric(input$Num_Fxna); #number of observations per time block
    bw=floor((as.numeric(input$Tapers_Fxna) + 1) / (as.numeric(input$Num_Fxna) + 1)); #bandwidth for multitaper spectral estimator
    K=as.numeric(input$Tapers_Fxna); #number of tapers for multitaper spectral estimator
    std=as.logical(input$TF_Fxna); #standardize variance for points in functional domain (TRUE) or not (FALSE)
    freq=seq(from=0,by=1/N,length.out=floor(N/2)+1); #Fourier frequencies
    #################################################
    ## End - Initialize values based on data
    #################################################

    #################################################
    ## Start - Run the algorithm
    #################################################
    Rsel=as.numeric(input$Rsel_Fxna); #number of points in functional domain used for test statistics
    set.seed(47)
    ndraw=100000; #number of draws from Gaussian process for approximating p-values
    blockdiag=TRUE; #use block diagonal covariance matrix approximation
    dcap=40; #max number of frequencies tested in a given pass
    alpha=as.numeric(input$Signi_Fxna)/ceiling((1-2*bw/0.5)*(floor(N/2)+1)/dcap); #alpha with Bonferroni correction
    shinyjs::html(id='ObsFxn_text', html = "<h4> <strong> You can check the progress of the algorithm in the file 'obs-fEBA.txt', in your working directory.
                      Rerunning the algorithm for Observed, Functional data will overwrite
                      the contents of the file, and closing the application will delete the file. If you wish to preserve the file, you can
                      change its name, to ensure it won't be overwritten or deleted.<strong> <h4>")
    show("ObsFxn_text")
    sink("obs-fEBA.txt")
    res <- fEBA.wrapper(dataf,Rsel,K,N,ndraw,alpha,std,blockdiag,dcap);
    sink()

    pse=fhat_pmt(dataf,N,K,Rsel,std);
    cmpnt="1-1"; #select component to view
    dimnames(pse) <- list(freq,apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-"),1:B);
    plot.x <- ((1:B) * (1/B)); plot.y <- as.numeric(rownames(pse)); plot.z <- pse
    plot.cmp <- apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-")
    plot.main <- "Multitaper Autospectrum"; plot.data = dataf

    #################################################
    ## Start - Find Signal Coherence Based on Power
    ## Spectrum Estimate
    #################################################
    conf <- numeric(length(pse))
    dim(conf) <- dim(pse)
    dimnames(conf) <- dimnames(pse)
    for(k in 1:dim(pse)[3]){
      for(j in 1:dim(pse)[2]){
        first_col <- ((j - 1) %% (as.numeric(Rsel))) + 1
        second_col <- ((j - 1) %/% (as.numeric(Rsel))) + 1
        cmpt_1 <- paste(first_col, "-", first_col, sep="")
        cmpt_2 <- paste(second_col, "-", second_col, sep="")
        for(i in 1:dim(pse)[1]){
          if(first_col == second_col){
            conf[i,j,k] <- Re(pse[i,j,k])
          } else {
            conf[i,j,k] <- Re((Mod(pse[i,j,k])**2) / (pse[i,cmpt_1,k] * pse[i,cmpt_2,k]))
          }
        }
      }
    }
    #################################################
    ## End - Find Signal Coherence Based on Power
    ## Spectrum Estimate
    #################################################

    ##View test statistics and p-values over frequencies
    tmp=cbind(as.numeric(unlist(lapply(res$log, function(x) rownames(x$Qint)))),
              unlist(lapply(res$log, function(x) x$Qint)),
              unlist(lapply(res$log, function(x) x$Qpv[,'Qint'])));
    tmp=tmp[!duplicated(tmp[,1]),];
    #################################################
    ## End - Run the algorithm
    #################################################
    plot.z <- conf

    plot.data = dataf

    #################################################
    ## Start - Update Descriptors and Sliders
    #################################################
    output$FxnPlotaDesc_AA <- renderText({
      paste(h4("Currently viewing timepoint "))
    })
    output$FxnbPlotDesc_file <- renderText({
      paste(h4("Currently viewing cross-component "))
    })
    output$FxnPlot22Desc_file <- renderText({
      paste(h4("Currently viewing cross-component "))
    })
    hide("ObsFxn_text")

    show("FxnPlotaDesc_AA")
    show("FxnbPlotDesc_file")
    show("FxnPlot22Desc_file")
    show("Blank10100_file")
    show("x_F1_AA")
    #show("q_F1")
    show("q11_F1_file")
    show("Plotly_Fxna_file")
    show("Plotly_Fxnb_file")
    show("plot1_FxnCheck_file")
    show("test12121_AA")
    show("Blank2_file")
    show("downloadDataFXN1_File")
    show("summ_out_fxn_file")
    show("summ_pval_fxn_file")
    #show("Blank10100")
    updateSliderInput(session, "x_F1_AA", min = 1, max=Ts, value=1, step=1)
    #updateSelectInput(session, "q_F1", choices=plot.cmp, selected = plot.cmp[1])
    updateSliderInput(session, "q11_F1_file", min=1, max=length(plot.cmp), value=1)
    updateSliderInput(session, "plot1_FxnCheck_file", min=1, max=length(plot.cmp), value=1)
    output$Blank2_file <- renderText({
      paste(h4(strong((paste("1-1")))))
    })
    output$Blank10100_file <- renderText({
      paste(h4(strong((paste("1-1")))))
    })
    output$test12121_AA <- renderText({
      paste(h4(strong((paste("1")))))
    })

    #################################################
    ## Start - Initialize a 3-D Plot of the entire
    ## Observed, Functional Data
    #################################################
    output$Plotly_Fxna_file <- renderPlotly({
      plot_ly(x = ~seq(from=0, to = 1, length.out=ncol(dataf)),
              y = ~seq(from=0, to=1, , length.out=nrow(dataf)),
              z = ~dataf) %>% add_surface() %>% layout(
        title = "3D Representation of Simulated Data",
        scene = list(
          xaxis = list(title="Functional Domain"),
           yaxis = list(title = "Timepoint"),
           zaxis = list(title="Value")
         )) %>% colorbar(title = "Value", len=1)
     })
    #################################################
    ## End - Initialize a 3-D Plot of the entire
    ## Observed, Functional Data
    #################################################

    plot.main_2 <- "Estimated Coherence"
    plot.main_3D <- "3D Representation of Autospectrogram"
    plot.main_3D.2 <- "3D Representation of Coherence"
    #################################################
    ## End - Update Descriptors and Sliders
    #################################################
    list(plot.x = plot.x, plot.y = plot.y, plot.z = plot.z,
         plot.main = plot.main, plot.cmp = plot.cmp, plot.data = plot.data,
         nrow = nrow, ncol = ncol, plot.main_2 = plot.main_2,
         plot.main_3D = plot.main_3D, plot.main_3D.2 = plot.main_3D.2,
         plot.log = res$summary, plot.freq = unname(tmp[,1]), plot.pvals = unname(tmp[,3]))})

  #######################################################
  ## Event Reactive Block End point for the Observed
  ## Functional algorithm
  #######################################################


  #################################################
  ## Start - Initialize the Periodogram for the 1-1
  ## Component of the Observed, Functional data
  #################################################
  output$Fxn_Plotb_file <- renderPlot({
    image.plot(x=plot.listFxn2()[[1]],y=plot.listFxn2()[[2]],z=suppressWarnings(t(Re(plot.listFxn2()[[3]][,"1-1",]))),
               axes = TRUE, col = inferno(256),
               main = plot.listFxn2()[[4]],xlab='Time',ylab='Hz',xaxs="i",
               bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
    abline(h=unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), col="skyblue", lwd=3);
    vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                      just=c("left", "top"), y=0.55, x=0.65)
    len <- length(unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]))
    vals <- unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1])
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    pp <- data.frame("Predicted Frequency Bands" = spp)
    colnames(pp) <- c("Predicted \n Frequency Bands")
    grid.table(pp, vp=vp.br, rows=NULL)

    vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                     just=c("left", "top"), y=0.95, x=0.65)
    grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
    jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                      draw=TRUE)
  })
  #################################################
  ## End - Initialize the Periodogram for the 1-1
  ## Component of the Observed, Functional data
  #################################################

  #################################################
  ## Start - Initialize the 3-D Periodogram for the 1-1
  ## Component of the Observed, Functional data
  #################################################
  output$Plotly_Fxnb_file <- renderPlotly({
    plot_ly(y=~plot.listFxn2()[[1]], x=~plot.listFxn2()[[2]], z=~t(Re(plot.listFxn2()[[3]][,"1-1",])))  %>%layout(title=plot.listFxn2()[[10]],
                                                                                                                      scene = list(
                                                                                                                        xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                        yaxis = list(title="Timepoint"),
                                                                                                                        zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
  })
  #################################################
  ## End - Initialize the Periodogram for the 1-1
  ## Component of the Observed, Functional data
  #################################################

  #################################################
  ## Start - Display Table containing results on
  ## partition testing for Observed, Functional data
  #################################################
  output$summ_out_fxn_file <- renderPlot({
    freq <- round(plot.listFxn2()[[12]][,1], 3)
    pval <- round(plot.listFxn2()[[12]][,2], 5)
    thresh <- round(plot.listFxn2()[[12]][,3], 5)
    Sig <- character(length(pval))
    for(i in 1:length(Sig)){
      if(pval[i] < thresh[i]){
        Sig[i] <- "TRUE"
      } else {
        Sig[i] <- "FALSE"
      }
    }
    res <- data.frame("Freq" = freq, "val" = pval, "t"=thresh, "s" = as.character(Sig))
    colnames(res) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
    res1 <- tableGrob(res, rows = NULL)
    title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
    blank9090 <- textGrob(""); blank0909 <- textGrob("")
    grid.arrange(blank9090, title, res1, blank0909, ncol = 1)
  })
  #################################################
  ## End - Display Table containing results on
  ## partition testing for Observed, Functional data
  #################################################

  #################################################
  ## Start - Create Scatterplot of p-values for Partition
  ## testiing for observed, functional data at each frequency
  #################################################
  output$summ_pval_fxn_file <- renderPlot({
    ggplot() + geom_point(aes(x = as.numeric(plot.listFxn2()[[13]]), y = as.numeric(plot.listFxn2()[[14]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
      xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
      geom_vline(xintercept = (plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01))
  })
  #################################################
  ## End - Create Scatterplot of p-values for Partition
  ## testiing for observed, functional data at each frequency
  #################################################

  observeEvent(plot.listFxn2()[[6]],{

    #################################################
    ## Start - Initialize the Periodogram for the 1-1
    ## Component of the Observed, Functional data
    #################################################
    output$Fxn_Plotb_file <- renderPlot({
      image.plot(x=plot.listFxn2()[[1]],y=plot.listFxn2()[[2]],z=suppressWarnings(t(Re(plot.listFxn2()[[3]][,"1-1",]))),
                 axes = TRUE, col = inferno(256),
                 main = plot.listFxn2()[[4]],xlab='Time',ylab='Hz',xaxs="i",
                 bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
      abline(h=unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), col="skyblue", lwd=3);
      vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                        just=c("left", "top"), y=0.55, x=0.65)
      len <- length(unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]))
      vals <- unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1])
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pp <- data.frame("Predicted Frequency Bands" = spp)
      colnames(pp) <- c("Predicted \n Frequency Bands")
      grid.table(pp, vp=vp.br, rows=NULL)

      vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                       just=c("left", "top"), y=0.95, x=0.65)
      grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
      jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                        draw=TRUE)
    })
    #################################################
    ## End - Initialize the Periodogram for the 1-1
    ## Component of the Observed, Functional data
    #################################################

    #################################################
    ## Start - Initialize the 3-D Periodogram for the 1-1
    ## Component of the Observed, Functional data
    #################################################
    output$Plotly_Fxnb_file <- renderPlotly({
      plot_ly(y=~plot.listFxn2()[[1]], x=~plot.listFxn2()[[2]], z=~t(Re(plot.listFxn2()[[3]][,"1-1",])))  %>%layout(title=plot.listFxn2()[[10]],
                                                                                                                    scene = list(
                                                                                                                      xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                      yaxis = list(title="Timepoint"),
                                                                                                                      zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)
    })
    #################################################
    ## End - Initialize the 3-D Periodogram for the 1-1
    ## Component of the Observed, Functional data
    #################################################

    })

  #################################################
  ## Start - Update the Periodogram for the chosen
  ## Component of the Observed, Functional data
  #################################################
  observeEvent(input$plot1_FxnCheck_file, ignoreNULL = TRUE, {
    curr_num <- as.numeric(input$plot1_FxnCheck_file)
    if(is.na(plot.listFxn2()[[4]])){

    } else {
      curr_comp <- plot.listFxn2()[[5]][curr_num]
      output$Blank2_file <- renderText({
        paste(h4(strong((paste(curr_comp)))))
      })
      if(strsplit(curr_comp, "-")[[1]][1] == strsplit(curr_comp, "-")[[1]][2]){
        output$Fxn_Plotb_file <- renderPlot({
          image.plot(x=plot.listFxn2()[[1]],y=plot.listFxn2()[[2]],z=suppressWarnings(t(Re(plot.listFxn2()[[3]][,curr_comp,]))),
                     axes = TRUE, col = inferno(256),
                     main = plot.listFxn2()[[4]],xlab='Time',ylab='Hz',xaxs="i",
                     bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
          abline(h=unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), col="skyblue", lwd=3);
          vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                            just=c("left", "top"), y=0.55, x=0.65)
          len <- length(unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]))
          vals <- unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1])
          if(len == 0){
            str <- "(0, 0.5),"
          } else if (len == 1) {
            str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
          } else {
            str <- paste("(0", sep="")
            for(i in 1:len){
              str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
            }
            str <- paste(str, ",", "0.5),", sep="")
          }
          spp <- strsplit(str, "),")[[1]]
          for(a in 1:length(spp)){
            spp[a] <- paste(spp[a], ")", sep="")
          }
          pp <- data.frame("Predicted Frequency Bands" = spp)
          colnames(pp) <- c("Predicted \n Frequency Bands")
          grid.table(pp, vp=vp.br, rows=NULL)

          vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                           just=c("left", "top"), y=0.95, x=0.65)
          grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
          jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                            draw=TRUE)
        })

      } else {
        output$Fxn_Plotb_file <- renderPlot({
          image.plot(x=plot.listFxn2()[[1]],y=plot.listFxn2()[[2]],z=suppressWarnings(t(Re(plot.listFxn2()[[3]][,curr_comp,]))),
                     axes = TRUE, col = inferno(256),
                     main = plot.listFxn2()[[9]],xlab='Time',ylab='Hz',xaxs="i",
                     bigplot = c(.1, .55, .15, .85), smallplot = c(.6, .65, .15, .85));
          abline(h=unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), col="skyblue", lwd=3);
          vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                            just=c("left", "top"), y=0.55, x=0.65)
          len <- length(unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]))
          vals <- unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1])
          if(len == 0){
            str <- "(0, 0.5),"
          } else if (len == 1) {
            str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
          } else {
            str <- paste("(0", sep="")
            for(i in 1:len){
              str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
            }
            str <- paste(str, ",", "0.5),", sep="")
          }
          spp <- strsplit(str, "),")[[1]]
          for(a in 1:length(spp)){
            spp[a] <- paste(spp[a], ")", sep="")
          }
          pp <- data.frame("Predicted Frequency Bands" = spp)
          colnames(pp) <- c("Predicted \n Frequency Bands")
          grid.table(pp, vp=vp.br, rows=NULL)

          vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.325, "npc"),
                           just=c("left", "top"), y=0.95, x=0.65)
          grid.polygon(x=c(0.25, 0.25,0.75, 0.75), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
          jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                            draw=TRUE)
        })
      }
    }
  })
  #################################################
  ## End - Update the Periodogram for the chosen
  ## Component of the Observed, Functional data
  #################################################

  #################################################
  ## Start - Update the 3-D Periodogram for the chosen
  ## Component of the Observed, Functional data
  #################################################
  observeEvent(input$q11_F1_file, ignoreNULL = TRUE, {
    curr_num <- as.numeric(input$q11_F1_file)
    if(is.na(plot.listFxn2()[[4]])){

    } else {
      curr_comp <- plot.listFxn2()[[5]][curr_num]
      output$Blank10100_file <- renderText({
        paste(h4(strong((paste(curr_comp)))))
      })
      vals <- as.numeric(strsplit(curr_comp, "-")[[1]])
      if(vals[1] == vals[2]){
        output$Plotly_Fxnb_file <- renderPlotly({
          plot_ly(y=~plot.listFxn2()[[1]], x=~plot.listFxn2()[[2]], z=~t(Re(plot.listFxn2()[[3]][,curr_comp,])))  %>%layout(title=plot.listFxn2()[[10]],
                                                                                                                            scene = list(
                                                                                                                              xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                              yaxis = list(title="Timepoint"),
                                                                                                                              zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)


        })
      } else {
        output$Plotly_Fxnb_file <- renderPlotly({
          plot_ly(y=~plot.listFxn2()[[1]], x=~plot.listFxn2()[[2]], z=~t(Re(plot.listFxn2()[[3]][,curr_comp,])))  %>%layout(title=plot.listFxn2()[[11]],
                                                                                                                            scene = list(
                                                                                                                              xaxis = list(title='Frequency',range = c(0.5, 0)),
                                                                                                                              yaxis = list(title="Timepoint"),
                                                                                                                              zaxis = list(title="Value"))) %>% add_surface() %>% colorbar(title="Value", len=1)


        })
      }



    }

  })
  #################################################
  ## End - Update the 3-D Periodogram for the chosen
  ## Component of the Observed, Functional data
  #################################################

  #################################################
  ## Start - Initialize the Periodogram for the Observed
  ## Univariate data
  #################################################
  output$Image_Plot2 <- renderPlot({
    par(mar=c(4,4,12,12))
    vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                       just=c( "bottom"), y=0.6, x=0.475)
    plot.new()
    image.plot(x=plot.list2()[[1]], y=plot.list2()[[2]], z=plot.list2()[[3]],
               axes = TRUE, col = inferno(256),
               xlab='Time',ylab='Hz',xaxs="i",
               bigplot = c(0.075, .675, .125, .925), smallplot = c(.7, .75, .125, .925));title(plot.list2()[[4]], line=0.75);
    abline(h=plot.list2()[[5]], col = "skyblue", lwd=3);

    vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.4, "npc"),
                      just=c("left", "top"), y=0.625, x=0.7)
    len <- length(plot.list2()[[5]])
    vals <- plot.list2()[[5]]
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    pp <- data.frame("Predicted Frequency Bands" = spp)
    colnames(pp) <- c("Predicted \n Frequency Bands")
    grid.table(pp, vp=vp.br, rows=NULL)

    vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.4, "npc"),
                     just=c("left", "top"), y=0.8, x=0.7)
    grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
    jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                      draw=TRUE)
  })
  #################################################
  ## End - Initialize the Periodogram for the Observed
  ## Univariate data
  #################################################

  #################################################
  ## Start - Display Table containing results on
  ## partiton testing for Observed, Univariate data
  #################################################
  output$summ_out_uni_file <- renderPlot({
    pvals <- round(plot.list2()[[7]][,4], 5)
    pval.th <- round(plot.list2()[[7]][,5], 5)
    Sig <- character(length(pvals))
    for(i in 1:length(Sig)){
      if(pvals[i] < pval.th[i]){
        Sig[i] <- "TRUE"
      } else {
        Sig[i] <- "FALSE"
      }
    }
    pp <- data.frame("Frequency" = round(plot.list2()[[7]][,2], 3), "P-Value" = round(plot.list2()[[7]][,4], 5),
                     "P-Value\nThreshold" = round(plot.list2()[[7]][,5], 5), "Significance" = as.character(Sig))
    colnames(pp) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
    table <- tableGrob(pp, rows=NULL)
    title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
    blank1 <- textGrob("")

    len <- length(plot.list2()[[5]])
    vals <- plot.list2()[[5]]
    if(len == 0){
      str <- "(0, 0.5),"
    } else if (len == 1) {
      str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
    } else {
      str <- paste("(0", sep="")
      for(i in 1:len){
        str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
      }
      str <- paste(str, ",", "0.5),", sep="")
    }
    spp <- strsplit(str, "),")[[1]]
    for(a in 1:length(spp)){
      spp[a] <- paste(spp[a], ")", sep="")
    }
    pvals <- plot.list2()[[9]][,2]
    Res <- character(length(pvals))
    Sig2 <- numeric(length(pvals))
    for(i in 1:length(Res)){
      if(pvals[i] < 0.05){
        Res[i] = "Segment has \n nonflat spectrum"
        Sig2[i] = "TRUE"
      } else {
        Res[i] = "Segment has \n flat spectrum"
        Sig2[i] = "FALSE"
      }
    }
    blank1 <- textGrob(""); blank2 <- textGrob("")
    new_tab <- data.frame("Frequency Bands" = spp, "P-Values" = round(as.numeric(pvals), 5),"Significant" = Sig2 ,"Results" = Res)
    colnames(new_tab) <- c("Frequency \n Bands", "P-Value", "Significant", "Results")
    test1 <- tableGrob(new_tab, rows = NULL);
    title2 <- textGrob(expression(bold("Summary of Testing for Flat \n Spectrum in Each Segment")))
    #grid.arrange(test1)
    grid.arrange(title, table, title2, test1,blank2, heights = c(0.75,0.75,0.85,0.75, 1) ,nrow = 5)
  })
  #################################################
  ## End - Display Table containing results on
  ## partiton testing for Observed, Univariate data
  #################################################

  #################################################
  ## Start - Create Scatterplot of p-values for Partition
  ## testing for Observed, Univariate data at each frequency
  #################################################
  output$summ_pval_uni_file <- renderPlot({
    ggplot() + geom_point(aes(x = as.numeric(plot.list2()[[8]][,1]), y = as.numeric(plot.list2()[[8]][,2]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
      xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
      geom_vline(xintercept = plot.list2()[[5]], linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01))
  })
  #################################################
  ## End - Create Scatterplot of p-values for Partition
  ## testing for Observed, Univariate data at each frequency
  #################################################

  #################################################
  ## Start - Recreate the above Plots for Simulated,
  ## Univariate Data, to make them downloadable
  #################################################
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Simulated_Univariate_Output_Results","pdf",sep = ".")
    },
    content = function(file){
      pdf(file, paper = "USr", width = 1100, height=600, onefile = TRUE)
      par(mar=c(4,4,12,12))
      vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                         just=c( "bottom"), y=0.6, x=0.475)
      #plot.new()
      image.plot(x=plot.list()[[1]], y=plot.list()[[2]], z=plot.list()[[3]],
                 axes = TRUE, col = inferno(256),
                 xlab='Time',ylab='Hz',xaxs="i",
                 bigplot = c(.1, .55, .1, .5), smallplot = c(.6, .65, .1, .5));title(plot.list()[[4]], line=0.75);
      abline(h=plot.list()[[5]], col = "skyblue", lwd=3)
      if(plot.list()[[10]] == "W"){
        act <- c("(0,0.5)")
      } else {
        abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
        act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
      }
      vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                        just=c("left", "top"), y=0.5, x=0.65)
      len <- length(plot.list()[[5]])
      vals <- plot.list()[[5]]
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "),[", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      max_len <- max(length(act), length(spp))
      if(length(act) == length(spp)){

      } else if(length(act) > length(spp)){
        sp_l <- length(spp) + 1
        for(i in sp_l: length(act)){
          spp[i] <- ""
        }
      } else {
        ac_l <- length(act) + 1
        for(i in ac_l: length(spp)){
          act[i] <- ""
        }
      }
      pp <- data.frame("Actual Frequency Bands" = act, "Predicted Frequency Bands" = spp)
      colnames(pp) <- c("Actual \n Frequency Bands", "Predicted \n Frequency Bands")

        vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                          just=c("left", "top"), y=0.5, x=0.63)
        grid.table(pp, vp=vp.br, rows=NULL)
        vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.5, "npc"),
                         just=c("left", "top"), y=0.65, x=0.58)
        grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)

       jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                        draw=TRUE)

      print( ggplot() + geom_line(aes(x=seq(0,1,length.out = length(plot.list()[[6]])), y= plot.list()[[6]])) + xlab("Time") +
               ylab("") + ggtitle("Simulated Time Series Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
               scale_x_continuous(limits=c(0,1), expand=c(0,0)), vp=vp.top)
      #plot.new()
      vp.r <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                      y=0.5, x=0.75)
      vp.l <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.25)
      pvals <- round(plot.list()[[7]][,4], 5)
      pval.th <- round(plot.list()[[7]][,5], 5)
      Sig <- character(length(pvals))
      for(i in 1:length(Sig)){
        if(pvals[i] < pval.th[i]){
          Sig[i] <- "TRUE"
        } else {
          Sig[i] <- "FALSE"
        }
      }
      pp <- data.frame("Frequency" = round(plot.list()[[7]][,2], 3), "P-Value" = round(plot.list()[[7]][,4], 5),
                       "P-Value\nThreshold" = round(plot.list()[[7]][,5], 5), "Significance" = as.character(Sig))
      colnames(pp) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
      table <- tableGrob(pp, rows=NULL)
      title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
      blank1 <- textGrob("")

      len <- length(plot.list()[[5]])
      vals <- plot.list()[[5]]
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pvals <- plot.list()[[9]][,2]
      Res <- character(length(pvals))
      Sig2 <- numeric(length(pvals))
      for(i in 1:length(Res)){
        if(pvals[i] < 0.05){
          Res[i] = "Segment has \n nonflat spectrum"
          Sig2[i] = "TRUE"
        } else {
          Res[i] = "Segment has \n flat spectrum"
          Sig2[i] = "FALSE"
        }
      }
      blank1 <- textGrob(""); blank2 <- textGrob("")
      new_tab <- data.frame("Frequency Bands" = spp, "P-Values" = round(as.numeric(pvals), 5),"Significant" = Sig2 ,"Results" = Res)
      colnames(new_tab) <- c("Frequency \n Bands", "P-Value", "Significant", "Results")
      test1 <- tableGrob(new_tab, rows = NULL);
      title2 <- textGrob(expression(bold("Summary of Testing for Flat \n Spectrum in Each Segment")))
      #grid.arrange(test1)
      grid.arrange(title, table, title2, test1,blank2, heights = c(0.75,0.75,0.85,0.75, 1) ,nrow = 5, vp = vp.l)
      print(ggplot() + geom_point(aes(x = as.numeric(plot.list()[[8]][,1]), y = as.numeric(plot.list()[[8]][,2]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
              xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
              geom_vline(xintercept = plot.list()[[5]], linetype = "dashed") , vp=vp.r)

      dev.off()
    }
  )
  #################################################
  ## End - Recreate the above Plots for Simulated,
  ## Univariate Data, to make them downloadable
  #################################################

  #################################################
  ## Start - Recreate the above Plots for Observed,
  ## Multivariate Data, to make them downloadable
  #################################################
  output$downloadDataMv1_File <- downloadHandler(
    filename = function(){
      paste("Observed_Multivariate_Output_Results","pdf",sep = ".")
    },
    content = function(file){
      pdf(file, paper = "USr", width = 1100, height=600, onefile = TRUE)
      par(mar=c(4,4,12,12))
      vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                         just=c( "bottom"), y=0.6, x=0.475)
      curr_num <- as.numeric(input$plot1_MvCheck_file)
      if(plot.listMv2()[[13]]){
        curr <- plot.listMv2()[[12]][curr_num]
        curr_comp <- paste(curr)
      } else {
        curr_comp <- paste(curr_num)
      }
      image.plot(x=(1:as.numeric(dim(plot.listMv2()[[1]])[1])) / (as.numeric(dim(plot.listMv2()[[1]])[1])),y=plot.listMv2()[[3]][-1],z=t(Re(plot.listMv2()[[2]][-1,curr_num+(curr_num-1)*dim(plot.listMv2()[[1]])[2],])),
                   axes = TRUE, col = inferno(256),
                   xlab='Time',ylab='Hz',xaxs="i",
                 bigplot = c(.1, .55, .1, .5), smallplot = c(.6, .65, .1, .5)); title(paste("Local Periodogram of Component", curr_comp), line=0.75)
        abline(h=plot.listMv2()[[4]], col="skyblue", lwd=3);
        vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                          just=c("left", "top"), y=0.475, x=0.65)
        len <- length(plot.listMv2()[[4]])
        vals <- plot.listMv2()[[4]]
        if(len == 0){
          str <- "(0, 0.5),"
        } else if (len == 1) {
          str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
        } else {
          str <- paste("(0", sep="")
          for(i in 1:len){
            str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
          }
          str <- paste(str, ",", "0.5),", sep="")
        }
        spp <- strsplit(str, "),")[[1]]
        for(a in 1:length(spp)){
          spp[a] <- paste(spp[a], ")", sep="")
        }
        pp <- data.frame(
          "Predicted Frequency Bands" = spp)
        colnames(pp) <- c(
          "Predicted \n Frequency Bands")
        grid.table(pp, vp=vp.br, rows=NULL)

        vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.5, "npc"),
                         just=c("left", "top"), y=0.65, x=0.58)
        grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)
        jj <- grid.legend(c("Predicted Partition Points"
        ), gp=gpar(lty=1, lwd=3, col=c("skyblue"
        )), vp=vp.r,
        draw=TRUE)
        get_num <- as.numeric(input$x_Mv_AA)
        if(plot.listMv2()[[13]]){
          comp_name <- plot.listMv2()[[12]][get_num]
        } else {
          comp_name = get_num
        }
        print(ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=dim(plot.listbb()[[1]])[1]), y=as.numeric(plot.listbb()[[1]][,get_num]))) +
            xlab("Time") + ylab("") + ggtitle(paste("Observed Data- Component", comp_name)) + theme(plot.title = element_text(face="bold", hjust=0.5)) +
            scale_x_continuous(limits=c(0,1), expand=c(0,0)), vp=vp.top)
        vp.r <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                         y=0.5, x=0.75)
        vp.l <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                         y=0.5, x=0.25)

        freq <- round(plot.listMv2()[[9]], 3)
        mod_pval <- plot.listMv2()[[6]]
        min_pval <- round(plot.listMv2()[[5]], 5)
        thresh <- round(plot.listMv2()[[8]], 5)
        Sig <- character(length(min_pval))
        for(i in 1:length(Sig)){
          if(min_pval[i] < thresh[i]){
            Sig[i] <- "TRUE"
          } else {
            Sig[i] <- "FALSE"
          }
        }
        res <- data.frame("Freq" = freq, "val" = mod_pval, "t"=thresh, "s" = as.character(Sig))
        colnames(res) <- c("Frequency", "Minimum \n P-Value", "P-Value \n Threshold", "Significant")
        sig <- which(plot.listMv2()[[9]] %in% plot.listMv2()[[4]])
        if(length(sig) == 0){
          sig = which.min(min_pval)
        }
        res <- res[sig, ]
        res1 <- tableGrob(res, rows = NULL)
        title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
        blank9090 <- textGrob(""); blank0909 <- textGrob("")
        grid.arrange(title, res1, blank0909, ncol = 1, vp=vp.l)

        print( ggplot() + geom_point(aes(x = as.numeric(plot.listMv2()[[9]]), y = as.numeric(plot.listMv2()[[5]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
                 xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
                 geom_vline(xintercept = plot.listMv2()[[4]], linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01)), vp=vp.r)
      dev.off()
    }
  )
  #################################################
  ## End - Recreate the above Plots for Observed,
  ## Multivariate Data, to make them downloadable
  #################################################

  #################################################
  ## Start - Recreate the above Plots for Observed,
  ## Functional Data, to make them downloadable
  #################################################
  output$downloadDataFXN1_File <- downloadHandler(
    filename = function(){
      paste("Observed_Functional_Output_Results","pdf",sep = ".")
    },
    content = function(file){
      pdf(file, paper = "USr", width = 1100, height=600, onefile = TRUE)
      par(mar=c(4,4,12,12))
      vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                         just=c( "bottom"), y=0.6, x=0.475)
      #plot.new()
      curr_num <- as.numeric(input$plot1_FxnCheck_file)
      curr_comp <-plot.listFxn2()[[5]][curr_num]
      if(strsplit(curr_comp, "-")[[1]][1] == strsplit(curr_comp,"-")[[1]][2]){
        image.plot(x=plot.listFxn2()[[1]], y=plot.listFxn2()[[2]], z=t(Re(plot.listFxn2()[[3]][,curr_comp,])),
                   axes = TRUE, col = inferno(256),
                   xlab='Time',ylab='Hz',xaxs="i",
                   bigplot = c(.1, .55, .1, .5), smallplot = c(.6, .65, .1, .5)); title(paste("Multitaper Autospectrum of Cross-Component", curr_comp), line=0.75)
      } else {
        image.plot(x=plot.listFxn2()[[1]], y=plot.listFxn2()[[2]], z=t(Re(plot.listFxn2()[[3]][,curr_comp,])),
                   axes = TRUE, col = inferno(256),
                   xlab='Time',ylab='Hz',xaxs="i",
                   bigplot = c(.1, .55, .1, .5), smallplot = c(.6, .65, .1, .5)); title(paste("Estimated Coherence of Cross-Component", curr_comp), line = 0.75)
      }
      abline(h=unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), col = "skyblue", lwd=3);

      vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                        just=c("left", "top"), y=0.5, x=0.65)
      len <- length(unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]))
      vals <- unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1])
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "),[", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pp <- data.frame("Predicted Frequency Bands" = spp)
      colnames(pp) <- c("Predicted \n Frequency Bands")

      vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                        just=c("left", "top"), y=0.5, x=0.63)
      grid.table(pp, vp=vp.br, rows=NULL)
      vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.5, "npc"),
                       just=c("left", "top"), y=0.65, x=0.58)
      grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)

      jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                        draw=TRUE)
      curr_row <- as.numeric(input$x_F1_AA)
      print( ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listFxn2()[[6]][curr_row,])), y=plot.listFxn2()[[6]][curr_row,])) +
               xlab("Functional Domain") + ylab("") + ggtitle(paste("Observed Data- Timepoint", curr_row)) + theme(plot.title = element_text(face="bold", hjust=0.5)) +
               scale_x_continuous(limits = c(0,1), expand=c(0,0)), vp=vp.top)
      #plot.new()
      vp.r <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.75)
      vp.l <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.25)
      ###

      freq <- round(plot.listFxn2()[[12]][,1], 3)
      pval <- round(plot.listFxn2()[[12]][,2], 5)
      thresh <- round(plot.listFxn2()[[12]][,3], 5)
      Sig <- character(length(pval))
      for(i in 1:length(Sig)){
        if(pval[i] < thresh[i]){
          Sig[i] <- "TRUE"
        } else {
          Sig[i] <- "FALSE"
        }
      }
      res <- data.frame("Freq" = freq, "val" = pval, "t"=thresh, "s" = as.character(Sig))
      colnames(res) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
      res1 <- tableGrob(res, rows = NULL)
      title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(blank9090, title, res1, blank0909, ncol = 1, vp = vp.l)

      ###
      print(ggplot() + geom_point(aes(x = as.numeric(plot.listFxn2()[[13]]), y = as.numeric(plot.listFxn2()[[14]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
              xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
              geom_vline(xintercept = unname(plot.listFxn2()[[12]][which(plot.listFxn2()[[12]][,4] == 1), 1]), linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01)), vp=vp.r)

      dev.off()
    }
  )
  #################################################
  ## End - Recreate the above Plots for Observed,
  ## Functional Data, to make them downloadable
  #################################################

  #################################################
  ## Start - Recreate the above Plots for Simulated,
  ## Functional Data, to make them downloadable
  #################################################
  output$downloadDataFXN1 <- downloadHandler(
    filename = function(){
      paste("Simulated_Functional_Output_Results","pdf",sep = ".")
    },
    content = function(file){
      pdf(file, paper = "USr", width = 1100, height=600, onefile = TRUE)
      par(mar=c(4,4,12,12))
      vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                         just=c( "bottom"), y=0.6, x=0.475)
      #plot.new()
      curr_num <- as.numeric(input$plot1_FxnCheck)
      curr_comp <-plot.listF1()[[5]][curr_num]
      if(strsplit(curr_comp, "-")[[1]][1] == strsplit(curr_comp,"-")[[1]][2]){
        image.plot(x=plot.listF1()[[1]], y=plot.listF1()[[2]], z=t(Re(plot.listF1()[[3]][,curr_comp,])),
                   axes = TRUE, col = inferno(256),
                   xlab='Time',ylab='Hz',xaxs="i",
                   bigplot = c(.1, .55, .1, .5), smallplot = c(.6, .65, .1, .5)); title(paste("Multitaper Autospectrum of Cross-Component", curr_comp), line=0.75)
      } else {
        image.plot(x=plot.listF1()[[1]], y=plot.listF1()[[2]], z=t(Re(plot.listF1()[[3]][,curr_comp,])),
                   axes = TRUE, col = inferno(256),
                   xlab='Time',ylab='Hz',xaxs="i",
                   bigplot = c(.1, .55, .1, .5), smallplot = c(.6, .65, .1, .5)); title(paste("Estimated Coherence of Cross-Component", curr_comp), line = 0.75)
      }
      abline(h=unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]), col = "skyblue", lwd=3);
      if(plot.listF1()[[13]] == "W"){
        act <- c("(0,0.5)")
      } else {
        abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
        act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
      }
      vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                        just=c("left", "top"), y=0.5, x=0.65)
      len <- length(unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]))
      vals <- unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1])
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "),[", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      max_len <- max(length(act), length(spp))
      if(length(act) == length(spp)){

      } else if(length(act) > length(spp)){
        sp_l <- length(spp) + 1
        for(i in sp_l: length(act)){
          spp[i] <- ""
        }
      } else {
        ac_l <- length(act) + 1
        for(i in ac_l: length(spp)){
          act[i] <- ""
        }
      }
      pp <- data.frame("Actual Frequency Bands" = act, "Predicted Frequency Bands" = spp)
      colnames(pp) <- c("Actual \n Frequency Bands", "Predicted \n Frequency Bands")

      vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                        just=c("left", "top"), y=0.5, x=0.63)
      grid.table(pp, vp=vp.br, rows=NULL)
      vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.5, "npc"),
                       just=c("left", "top"), y=0.65, x=0.58)
      grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)

      jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                        draw=TRUE)
      curr_row <- as.numeric(input$x_F1)
      print( ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listF1()[[6]][curr_row,])), y=plot.listF1()[[6]][curr_row,])) +
               xlab("Functional Domain") + ylab("") + ggtitle(paste("Simulated Data- Timepoint", curr_row)) + theme(plot.title = element_text(face="bold", hjust=0.5)) +
               scale_x_continuous(limits = c(0,1), expand=c(0,0)), vp=vp.top)
      #plot.new()
      vp.r <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.75)
      vp.l <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.25)
      ###

      freq <- round(plot.listF1()[[10]][,1], 3)
      pval <- round(plot.listF1()[[10]][,2], 5)
      thresh <- round(plot.listF1()[[10]][,3], 5)
      Sig <- character(length(pval))
      for(i in 1:length(Sig)){
        if(pval[i] < thresh[i]){
          Sig[i] <- "TRUE"
        } else {
          Sig[i] <- "FALSE"
        }
      }
      res <- data.frame("Freq" = freq, "val" = pval, "t"=thresh, "s" = as.character(Sig))
      colnames(res) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
      res1 <- tableGrob(res, rows = NULL)
      title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(blank9090, title, res1, blank0909, ncol = 1, vp = vp.l)

      ###
      print(ggplot() + geom_point(aes(x = as.numeric(plot.listF1()[[11]]), y = as.numeric(plot.listF1()[[12]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
              xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
              geom_vline(xintercept = unname(plot.listF1()[[10]][which(plot.listF1()[[10]][,4] == 1), 1]), linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01)), vp=vp.r)

      dev.off()
    }
  )
  #################################################
  ## End - Recreate the above Plots for Simulated,
  ## Functional Data, to make them downloadable
  #################################################

  #################################################
  ## Start - Recreate the above Plots for Simulated,
  ## Multivariate Data, to make them downloadable
  #################################################
  output$downloadDataMV1 <- downloadHandler(
    filename = function(){
      paste("Simulated_Multivariate_Output_Results","pdf",sep = ".")
    },
    content = function(file){
      pdf(file, paper = "USr", width = 1100, height=600, onefile = TRUE)
      par(mar=c(4,4,12,12))
      vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                         just=c( "bottom"), y=0.6, x=0.475)
      curr_num <- as.numeric(input$mvX_F2)
      curr_comp <- paste(curr_num)

        image.plot(x=(1:as.numeric(dim(plot.listMv()[[1]])[1])) / (as.numeric(dim(plot.listMv()[[1]])[1])),y=plot.listMv()[[3]][-1],z=t(Re(plot.listMv()[[2]][-1,curr_num+(curr_num-1)*dim(plot.listMv()[[1]])[2],])),
                   axes = TRUE, col = inferno(256),
                   xlab='Time',ylab='Hz',xaxs="i",
                   bigplot = c(.125, .575, .125, .525), smallplot = c(.6, .65, .125, .525)); title(paste("Multitaper Autospectrum of Component", curr_comp), line=0.75)
        abline(h=plot.listMv()[[4]], col="skyblue", lwd=3);
        if(plot.listMv()[[7]] != 'W'){
          abline(h=c(0.15, 0.35), col="lawngreen", lwd=3)
        }
        vp.br <- viewport(height=unit(0.55, "npc"), width=unit(0.35, "npc"),
                          just=c("left", "top"), y=0.55, x=0.65)
        if(plot.listMv()[[7]] == 'W'){
          act <- c("(0, 0.5)")
        } else {
          act <- c("(0, 0.15)", "[0.15, 0.35)", "[0.35, 0.5)")
        }
        len <- length(plot.listMv()[[4]])
        vals <- plot.listMv()[[4]]
        if(len == 0){
          str <- "(0, 0.5),"
        } else if (len == 1) {
          str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
        } else {
          str <- paste("(0", sep="")
          for(i in 1:len){
            str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
          }
          str <- paste(str, ",", "0.5),", sep="")
        }
        spp <- strsplit(str, "),")[[1]]
        for(a in 1:length(spp)){
          spp[a] <- paste(spp[a], ")", sep="")
        }
        max_len <- max(length(act), length(spp))
        if(length(act) == length(spp)){

        } else if(length(act) > length(spp)){
          sp_l <- length(spp) + 1
          for(i in sp_l: length(act)){
            spp[i] <- ""
          }
        } else {
          ac_l <- length(act) + 1
          for(i in ac_l: length(spp)){
            act[i] <- ""
          }
        }
        pp <- data.frame(
          "Actual Frequency Bands" = act,
          "Predicted Frequency Bands" = spp)
        colnames(pp) <- c(
          "Actual \n Frequency Bands",
          "Predicted \n Frequency Bands")


        vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                          just=c("left", "top"), y=0.5, x=0.63)
        grid.table(pp, vp=vp.br, rows=NULL)
        vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.5, "npc"),
                         just=c("left", "top"), y=0.65, x=0.58)
        grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)

        jj <- grid.legend(c("Predicted Partition Points", "Actual Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue", "lawngreen")), vp=vp.r,
                          draw=TRUE)

        curr_col <- as.numeric(input$mvX_F1)
        print(ggplot() + geom_line(aes(x=seq(from=0, to=1, length.out=length(plot.listMv()[[1]][,curr_col])), y=plot.listMv()[[1]][,curr_col])) +
              xlab("Time") + ylab("") + ggtitle(paste("Simulated Data of Component ", curr_col)) + theme(plot.title = element_text(face="bold", hjust=0.5)) +
              scale_x_continuous(limits=c(0,1), expand=c(0,0)), vp=vp.top )


      #plot.new()
      vp.r <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.75)
      vp.l <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"),
                       y=0.5, x=0.25)
      ###

        freq <- round(plot.listMv()[[10]], 3)
        mod_pval <- plot.listMv()[[6]]
        min_pval <- round(plot.listMv()[[5]], 5)
        thresh <- round(plot.listMv()[[9]], 5)
        Sig <- character(length(min_pval))
        for(i in 1:length(Sig)){
          if(min_pval[i] < thresh[i]){
            Sig[i] <- "TRUE"
          } else {
            Sig[i] <- "FALSE"
          }
        }
        res <- data.frame("Freq" = freq, "val" = mod_pval, "t"=thresh, "s" = as.character(Sig))
        colnames(res) <- c("Frequency", "Minimum \n P-Value", "P-Value \n Threshold", "Significant")
        sig <- which(plot.listMv()[[10]] %in% plot.listMv()[[4]])
        if(length(sig) == 0){
          sig = which.min(min_pval)
        }
        res <- res[sig, ]
        res1 <- tableGrob(res, rows = NULL)
        title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
        blank9090 <- textGrob(""); blank0909 <- textGrob("")
        grid.arrange(title, res1, blank0909, ncol = 1, vp=vp.l)
        print(ggplot() + geom_point(aes(x = as.numeric(plot.listMv()[[10]]), y = as.numeric(plot.listMv()[[5]]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
          xlab("Frequency") + ylab("P-Value") + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
          geom_vline(xintercept = plot.listMv()[[4]], linetype = "dashed") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01)), vp=vp.r)
      dev.off()
    }
  )
  #################################################
  ## End - Recreate the above Plots for Simulated,
  ## Multivariate Data, to make them downloadable
  #################################################

  #################################################
  ## Start - Recreate the above Plots for Observed,
  ## Univariate Data, to make them downloadable
  #################################################
  output$downloadData1 <- downloadHandler(
    filename = function(){
      paste("Observed_Univariate_Output_Results","pdf",sep = ".")
    },
    content = function(file){
      pdf(file, paper = "USr", width = 1100, height=600, onefile = TRUE)
      par(mar=c(4,4,12,12))
      vp.top <- viewport(height=unit(0.4, "npc"), width=unit(0.8, "npc"),
                         just=c( "bottom"), y=0.6, x=0.475)
      image.plot(x=plot.list2()[[1]], y=plot.list2()[[2]], z=plot.list2()[[3]],
                 axes = TRUE, col = inferno(256),
                 xlab='Time',ylab='Hz',xaxs="i",
                 bigplot = c(.125, .575, .125, .525), smallplot = c(.6, .65, .125, .525));title(plot.list2()[[4]], line=0.75);
      abline(h=plot.list2()[[5]], col = "skyblue", lwd=3);

      len <- length(plot.list2()[[5]])
      vals <- plot.list2()[[5]]
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pp <- data.frame("Predicted Frequency Bands" = spp)
      colnames(pp) <- c("Predicted \n Frequency Bands")

        vp.br <- viewport(height=unit(0.5, "npc"), width=unit(0.43, "npc"),
                          just=c("left", "top"), y=0.5, x=0.63)
        grid.table(pp, vp=vp.br, rows=NULL)
        vp.r <- viewport(height=unit(0.5, "npc"), width=unit(0.5, "npc"),
                         just=c("left", "top"), y=0.65, x=0.58)
        grid.polygon(x=c(0.29, 0.29,0.71, 0.71), y=c(0.6,0.4, 0.4,0.6 ), vp=vp.r)



      jj <- grid.legend(c("Predicted Partition Points"), gp=gpar(lty=1, lwd=3, col=c("skyblue")), vp=vp.r,
                        draw=TRUE)

      print( ggplot() + geom_line(aes(x=seq(0,1,length.out = length(plot.list2()[[6]])), y= plot.list2()[[6]])) + xlab("Time") +
               ylab("") + ggtitle("Observed Time Series Data") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
               scale_x_continuous(limits=c(0,1), expand=c(0,0)), vp=vp.top)
      vp.r <- viewport(height=unit(1, "npc"), width = unit(0.5, "npc"),
                       x=0.75, y=0.5)
      vp.l <- viewport(height=unit(1, "npc"), width = unit(0.5, "npc"),
                       x=0.25, y=0.5)
      pvals <- round(plot.list2()[[7]][,4], 5)
      pval.th <- round(plot.list2()[[7]][,5], 5)
      Sig <- character(length(pvals))
      for(i in 1:length(Sig)){
        if(pvals[i] < pval.th[i]){
          Sig[i] <- "TRUE"
        } else {
          Sig[i] <- "FALSE"
        }
      }
      pp <- data.frame("Frequency" = round(plot.list2()[[7]][,2], 3), "P-Value" = round(plot.list2()[[7]][,4], 5),
                       "P-Value\nThreshold" = round(plot.list2()[[7]][,5], 5), "Significance" = as.character(Sig))

      colnames(pp) <- c("Frequency", "P-Value", "P-Value \n Threshold", "Significant")
      table <- tableGrob(pp, rows=NULL)
      title <- textGrob(expression(bold("Summary of Partition \n      Point Tests")))
      blank1 <- textGrob("")
      #grid.arrange(title, table, heights = c(2, 1, 5))

      len <- length(plot.list2()[[5]])
      vals <- plot.list2()[[5]]
      if(len == 0){
        str <- "(0, 0.5),"
      } else if (len == 1) {
        str <- paste("(0, ", round(vals, 3), "), [", round(vals, 3), ", 0.5),", sep="")
      } else {
        str <- paste("(0", sep="")
        for(i in 1:len){
          str <- paste(str, ", ",round(vals[i], 3),"),[", round(vals[i], 3), sep="")
        }
        str <- paste(str, ",", "0.5),", sep="")
      }
      spp <- strsplit(str, "),")[[1]]
      for(a in 1:length(spp)){
        spp[a] <- paste(spp[a], ")", sep="")
      }
      pvals <- plot.list2()[[9]][,2]
      Res <- character(length(pvals))
      Sig2 <- numeric(length(pvals))
      for(i in 1:length(Res)){
        if(pvals[i] < 0.05){
          Res[i] = "Segment has \n nonflat spectrum"
          Sig2[i] = "TRUE"
        } else {
          Res[i] = "Segment has \n flat spectrum"
          Sig2[i] = "FALSE"
        }
      }
      blank1 <- textGrob(""); blank2 <- textGrob("")
      new_tab <- data.frame("Frequency Bands" = spp, "P-Values" = round(as.numeric(pvals), 5),"Significant" = Sig2 ,"Results" = Res)
      colnames(new_tab) <- c("Frequency \n Bands", "P-Value", "Significant", "Results")
      test1 <- tableGrob(new_tab, rows = NULL);
      title2 <- textGrob(expression(bold("Summary of Testing for Flat \n Spectrum in Each Segment")), gp=gpar(fontface = "bold"))
      grid.arrange(title, table, title2, test1,blank2, heights = c(0.75,0.75,0.85,0.75, 1) ,nrow = 5,
                   vp = vp.l)
      print (ggplot() + geom_point(aes(x = as.numeric(plot.list2()[[8]][,1]), y = as.numeric(plot.list2()[[8]][,2]))) + xlim(c(0,0.5)) + ylim(c(0,1)) +
               xlab("Frequency") + ylab("P-Value") + scale_x_continuous(expand=c(0,0), limits=c(0,0.5)) + scale_y_continuous(expand = c(0,0), limits=c(-0.01,1.01)) + ggtitle("P-Values for Testing Partition Points") + theme(plot.title = element_text(face="bold", hjust=0.5)) +
               geom_vline(xintercept = plot.list2()[[5]], linetype = "dashed"), vp = vp.r)
      dev.off()
    }
  )
  #################################################
  ## End - Recreate the above Plots for Observed,
  ## Univariate Data, to make them downloadable
  #################################################

  #################################################
  ## Start - Delete Sink Files created as app ran
  #################################################
  session$onSessionEnded(function() {
    if(file.exists("sim-mEBA.txt")){
      file.remove("sim-mEBA.txt")
    }
    if(file.exists("sim-fEBA.txt")){
      file.remove("sim-fEBA.txt")
    }
    if(file.exists("obs-mEBA.txt")){
      file.remove("obs-mEBA.txt")
    }
    if(file.exists("obs-fEBA.txt")){
      file.remove("obs-fEBA.txt")
    }
  })
  #################################################
  ## End - Delete Sink Files created as app ran
  #################################################
}


