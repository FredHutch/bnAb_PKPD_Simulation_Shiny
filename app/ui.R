library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel("PKPD Optimization"),
  tabsetPanel(tabPanel("PK",
                       sidebarLayout(
                         sidebarPanel(
                           numericInput("dose", "total dose (mg)", value = 300, min = 100, max = 3000),
                           sliderInput("ratio", "ratio: mAbA / total dose", min = 0, max = 1, step = 0.1, value = .5), 
                           numericInput("finaltime", "time (days)", min = 0, value = 84),
                           sliderInput( "hlA", "mAb A HL (days):", min = 5, max = 100, value = 45),
                           numericInput("VA", "distribution volume A (L)", min = 0, max = 20, value = 3),
                           sliderInput("hlB", "mAb B HL (days):", min = 5, max = 100, step = 5, value = 30),
                           numericInput("VB", "distribution volume B (L)", min = 0, max = 20, value = 3)
                         ),
                         mainPanel(plotOutput("PKplot"))
                       )
  ),
  tabPanel("PD",
           sidebarLayout(
             sidebarPanel( 
               sliderInput("sim_n", "simulated viruses", min = 50, max = 1000, step = 50, value = 1000),
               numericInput("seed", "sim seed", min = 1, value = sample(1e5, 1)),
               numericInput("muA", "mean log10 IC50 (mAb A)", min = -5, max = 2, value = -1),
               numericInput("sdA", "sd log10 IC50 (mAb A)", min = 0, max = 3, value = 1), 
               sliderInput("phiA","% viral resistance (mAb A)", min = 0, max = 1, step = 0.1, value = 0.3),
               numericInput("muB", "mean log10 IC50 (mAb B)", min = -5, max = 2, value = -1),
               numericInput("sdB", "sd log10 IC50 (mAb B)", min = 0, max = 3, value = 1), 
               sliderInput("phiB", "% viral resistance (mAb B)", min = 0, max = 1, step = 0.1, value = 0.3)
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
             checkboxGroupInput("interaction", "mAb interaction model", 
                                choiceNames = c("Bliss-Hill", "Additive", "Maximum", "Minimum"), 
                                choiceValues = c("BH", "additivity", "maxNeut", "minNeut"),
                                selected = c("BH", "additivity", "maxNeut", "minNeut")),
             selectInput("endpoint", "Endpoint", 
                         choices = c("IIP" = "IIP",
                                     "Neutralization" = "neut"),
                         selected = "iip"),
             checkboxInput("threshout", "Threshold coverage"),
             conditionalPanel(
               condition = "input.threshout == true",
               numericInput("threshold", "Threshold", value = 0.5, min = 0, step = 0.5)
             ),
             actionButton("run_optim", "Run ratio optimization")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("PKPD over time", plotOutput("PKPDpl")),
               tabPanel("Optimized PKPD", plotOutput("optratio"))
             )                             
           ))
  )
  )
)
)
