library(shiny)
library(shinycssloaders)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel("PKPD Optimization"),
  fluidRow(align = "center",
          sliderInput("ratio", "ratio: mAbA / total dose", 
                      min = 0, max = 1, step = 0.05, value = .5),#),
           textOutput("ratio_print")
  ),
  tabsetPanel(
  tabPanel("PK",
         sidebarLayout(
           sidebarPanel(
             numericInput("dose", "total dose (mg)", value = 300, min = 100, max = 3000, step = 50),
             numericInput("finaltime", "time (days)", min = 0, value = 75),
             sliderInput("hlA", "mAb A HL (days):", min = 5, max = 100, step = 5, value = 30),
             sliderInput("hlB", "mAb B HL (days):", min = 5, max = 100, step = 5, value = 15),
             fluidRow(column(10, strong("distribution volume (L)")), align = "center",
                      fluidRow(
                        column(5, numericInput("VA", "VA", min = 0, max = 20, value = 3, step = 0.25)),
                        column(5, numericInput("VB", "VB", min = 0, max = 20, value = 3, step = 0.25))
                      )),
             fluidRow(column(10, strong("Absorption (SC/IM admin)")), align = "center",
                      fluidRow(
                        column(5, checkboxInput("SC_A", "A", value = FALSE)),
                        column(5, checkboxInput("SC_B", "B", value = FALSE)),
                        column(5, numericInput("kaA", "kaA", min = 0, value = 0.4, step = 0.025)),
                        column(5, numericInput("kaB", "kaB", min = 0, value = 0.4, step = 0.025)),
                        column(5, numericInput("FbioA", "Bioavail. A", min = 0, max = 1,
                                               value = 0.7, step = 0.025)),
                        column(5, numericInput("FbioB", "Bioavail. B", min = 0, max = 1,
                                               value = 0.7, step = 0.025))
                      )),
             fluidRow(column(10, strong("Two-compartment")), align = "center",
                      fluidRow(
                        column(5, checkboxInput("A_twocmpt", "A", value = FALSE)),
                        column(5, checkboxInput("B_twocmpt", "B", value = FALSE)),
                        column(5, numericInput("QA", "QA", min = 0.01, value = 0.75, step = 0.025)),
                        column(5, numericInput("QB", "QB", min = 0.01, value = 0.75, step = 0.025)),
                        column(5, numericInput("VpA", "VpA", min = 0.01, max = 20, 
                                               value = 2, step = 0.25)),
                        column(5, numericInput("VpB", "VpB", min = 0.01, max = 20, 
                                               value = 2, step = 0.25))
                      ))
           ),
           mainPanel(plotOutput("PKplot"))
         )
  ),
  tabPanel("PD",
           sidebarLayout(
             sidebarPanel( 
               sliderInput("sim_n", "simulated viruses", min = 50, max = 1000, step = 50, value = 250),
               fluidRow(column(10, strong("mAb A log10(IC50)")), align = "center",
                 fluidRow(
                 column(5, numericInput("muA", "mean", min = -5, max = 2, value = -1, step = 0.25)),
                 column(5, numericInput("sdA", "sd", min = 0, max = 3, value = 2, step = 0.25))
               )), 
               sliderInput("phiA","% viral resistance", min = 0, max = 1, step = 0.1, value = 0.4),
               fluidRow(column(10, strong("mAb B log10(IC50)")), align = "center",
                        fluidRow(
                          column(5, numericInput("muB", "mean", min = -5, max = 2, value = -3, step = 0.25)),
                          column(5, numericInput("sdB", "sd", min = 0, max = 3, value = 1, step = 0.25))
                        )),               
               sliderInput("phiB", "% viral resistance", min = 0, max = 1, step = 0.1, value = 0.1),
               numericInput("seed", "sim seed", min = 1, value = sample(1e5, 1))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Marginal PD", plotOutput("margPDplot")),
                 tabPanel("Combined PD", plotOutput("PDplot", width = "auto"))
               )
             ))
  ),
  tabPanel("PKPD",
           sidebarLayout(sidebarPanel(
             #sliderInput("ratio", "ratio: mAbA / total dose", min = 0, max = 1, step = 0.1, value = .5),
             checkboxGroupInput("interaction", "mAb interaction model", 
                                choiceNames = c("Bliss-Hill", "Additive", "Maximum", "Minimum"), 
                                choiceValues = c("BH", "additivity", "maxNeut", "minNeut"),
                                selected = c("BH", "additivity", "maxNeut", "minNeut")),
             selectInput("endpoint", "Endpoint", 
                         choices = c("IIP" = "IIP",
                                     "Neutralization" = "neut"),
                         selected = "iip"),
             checkboxInput("threshout", "IIP threshold coverage"),
             conditionalPanel(
               condition = "input.threshout == true",
               numericInput("threshold", "IIP Threshold", value = 3, min = 0, step = 0.25)
             ),
             actionButton("run_optim", "Run ratio optimization")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("PKPD over time", plotOutput("PKPDpl")),
               tabPanel("Optimized PKPD", withSpinner(plotOutput("optratio", height = "600", width = "600"), 
                                                      type = 8, color = "#3D85B8", hide.ui = FALSE, 
                                                      size = 2.5))
             )                             
           ))
  )
  )
)
)
