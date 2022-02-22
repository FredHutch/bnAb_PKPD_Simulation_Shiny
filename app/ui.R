library(shiny)
library(shinycssloaders)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel("BNAb PKPD Optimization"),
  fluidRow(column(12), 
           fluidRow(align = "center",
             column(6, numericInput("dose", "Total dose (mg)", 
                                    value = 600, min = 100, max = 3000, step = 50, width = "20%"), 
                    p("ex., 30 mg/kg x 70 kg = 2100 mg"),
                    align = "right"),
             column(6, sliderInput("ratio", "Ratio: mAb A / total dose", 
                                   min = 0, max = 1, step = 0.05, value = .5), 
                    textOutput("ratio_print"), align = "left")
           )),
  tabsetPanel(
  tabPanel("PK",
         sidebarLayout(
           sidebarPanel(
             numericInput("finaltime", "Time (days)", min = 0, value = 75),
             sliderInput("hlA", "mAb A HL (days):", min = 5, max = 100, step = 5, value = 30),
             sliderInput("hlB", "mAb B HL (days):", min = 5, max = 100, step = 5, value = 15),
             fluidRow(column(10, strong("Distribution volume (L)")), align = "center",
                      fluidRow(
                        column(5, numericInput("VA", p(HTML(paste0("V",tags$sub("A")))), min = 0, max = 20, value = 3, step = 0.25)),
                        column(5, numericInput("VB", p(HTML(paste0("V",tags$sub("B")))), min = 0, max = 20, value = 3, step = 0.25))
                      )),
             hr(style = "border-top: 1px solid #000000;"),
             fluidRow(column(10, strong("Absorption model (ie, SC/IM admin)")), align = "center",
                      fluidRow(
                        column(5, checkboxInput("SC_A", "mAb A", value = FALSE)),
                        column(5, checkboxInput("SC_B", "mAb B", value = FALSE)),
                        column(5, numericInput("kaA", p(HTML(paste0("ka",tags$sub("A")))), min = 0, value = 0.4, step = 0.025)),
                        column(5, numericInput("kaB", p(HTML(paste0("ka",tags$sub("B")))), min = 0, value = 0.4, step = 0.025)),
                        column(5, numericInput("FbioA", "mAb A Bioavail.", min = 0, max = 1,
                                               value = 0.7, step = 0.025)),
                        column(5, numericInput("FbioB", "mAb B Bioavail.", min = 0, max = 1,
                                               value = 0.7, step = 0.025))
                      )),
             hr(style = "border-top: 1px solid #000000;"),
             fluidRow(column(10, strong("Two-compartment model")), align = "center",
                      fluidRow(
                        column(10, checkboxInput("twocmpt_on", "Use two-compartment kinetics", value = FALSE)),
                        column(5, numericInput("QA", p(HTML(paste0("Q",tags$sub("A")))), min = 0.01, value = 0.75, step = 0.025)),
                        column(5, numericInput("QB", p(HTML(paste0("Q",tags$sub("B")))), min = 0.01, value = 0.75, step = 0.025)),
                        column(5, numericInput("VpA", p(HTML(paste0("Vp",tags$sub("A")))), min = 0.01, max = 20, 
                                               value = 2, step = 0.25)),
                        column(5, numericInput("VpB", p(HTML(paste0("Vp",tags$sub("B")))), min = 0.01, max = 20, 
                                               value = 2, step = 0.25))
                      )),
             hr(style = "border-top: 1px solid #000000;"),
             fluidRow(column(10, checkboxInput("log_time", "Log x-axis (time)", value = FALSE)),
                      column(10, checkboxInput("log_conc", "Log y-axis (concentration)", value = FALSE)))
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Simulated PK", plotOutput("PKplot"), tableOutput("pk_tab")),
               tabPanel("Example PK input", tableOutput("pk_ex_tab"), h6("Note: These are simplified un-adjusted parameters from these references. Inter-subject variation and covariate-adjustment (e.g., weight) should be assessed carefully in design."))
             ))
         )
  ),
  tabPanel("PD",
           sidebarLayout(
             sidebarPanel( 
               sliderInput("sim_n", "Simulated viruses", min = 50, max = 1000, step = 50, value = 250),
               fluidRow(column(10, strong("mAb A log10(IC50)")), align = "center",
                 fluidRow(
                 column(5, numericInput("muA", "mean", min = -4, max = 2, value = 0.5, step = 0.25)),
                 column(5, numericInput("sdA", "sd", min = 0, max = 2, value = 0.4, step = 0.25))
               )), 
               sliderInput("phiA","% viral resistance", min = 0, max = 1, step = 0.1, value = 0.5),
               fluidRow(column(10, strong("mAb B log10(IC50)")), align = "center",
                        fluidRow(
                          column(5, numericInput("muB", "mean", min = -4, max = 2, value = -2, step = 0.25)),
                          column(5, numericInput("sdB", "sd", min = 0, max = 2, value = 0.5, step = 0.25))
                        )),               
               sliderInput("phiB", "% viral resistance", min = 0, max = 1, step = 0.1, value = 0.1),
               numericInput("seed", "sim seed", min = 1, value = sample(1e5, 1))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Marginal PD", plotOutput("margPDplot", width = "600", height="500")),
                 tabPanel("Combined PD", plotOutput("PDplot", width = "auto")),
                 tabPanel("Example PD input", 
                          h6("IC50 denotes the estimated concentration that achieves 50% in vitro neutralization. These are simple approximations to illustrate the range of these values. Parameters may differ from references as the geometric means here are defined by the average IC50 among sensitive viruses."),
                          tableOutput("pd_ex_tab"),
                          tags$a(href="https://www.hiv.lanl.gov/components/sequence/HIV/neutralization/", "See CATNAP website", target="_blank")
                          )
               )
             ))
  ),
  tabPanel("PKPD",
           sidebarLayout(sidebarPanel(
             #sliderInput("ratio", "ratio: mAbA / total dose", min = 0, max = 1, step = 0.1, value = .5),
             checkboxGroupInput("interaction", "mAb interaction model", 
                                choiceNames = c("Bliss-Hill", "Additive", "Maximum", "Minimum"), 
                                choiceValues = c("BH", "additivity", "maxNeut", "minNeut"),
                                selected = c("BH", "additivity", "minNeut")),
             selectInput("endpoint", "Endpoint", 
                         choices = c("IIP" = "IIP",
                                     "Neutralization" = "neut"),
                         selected = "IIP"),
             actionButton("run_optim", "Run ratio optimization"),
             checkboxInput("threshout", "IIP threshold coverage"),
             conditionalPanel(
               condition = "input.threshout == true",
               numericInput("threshold", "IIP Threshold", value = 2, min = 0.3, step = 0.25),
               fluidRow(column(10, strong("Calculate IIP threshold from neutralization titer")), align = "center",
                        fluidRow(
                          column(5, numericInput("ID50", "ID50 titer threshold", value = 100, min = 1, step = 10)),
                          column(5, numericInput("hill", "Hill slope", value = 1, min = 0.3, max = 3, step = 0.05)),
                          tableOutput("titer_iip"),
                          align = "left"
                        ))
             )
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
