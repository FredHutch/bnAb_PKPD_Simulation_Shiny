library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(viridis)
library(ggplot2)


# Source required helper files

source("pk_fun.R")
source("processing_fun.R")
source("pkpd_fun.R")


# Define server logic required

shinyServer(function(input, output) {
              theme_set(theme_bw() + theme(legend.position = "top"))
              mtime = reactive({
                c(0.1, 0.5, 1, seq(2, input$finaltime, by = 1))
              })
              #mtime = times()
              
              ratio_txt = reactive({
                oddsB = round(1/input$ratio, 1) - 1
                oddsA = round(1/(1-input$ratio), 1) - 1
                ratio_txt = case_when(
                  oddsB == 0 ~ "mAbA only",
                  oddsA == 0 ~ "mAbB only",
                  oddsA > 0 & oddsA < 1 ~ paste0("dose ratio 1:", oddsB, "(mAbA:mAbB)"),
                  TRUE ~ paste0("dose ratio ", oddsA, ":1 (mAbA:mAbB)")
                )
              }) 
              
              coverage_opt = reactive({
                thresh = if(input$threshout) input$threshold else -1
                thresh
              })
              
              
              PKDat = reactive({
                tibble(
                  days = mtime(),
                  mAbA = one_cmpt_pk(mtime(), input$dose * input$ratio, input$VA, input$hlA),
                  mAbB = one_cmpt_pk(mtime(), input$dose * (1 - input$ratio), input$VB, input$hlB)
                ) 
              })
              
              PDdat_react = reactive({
                set.seed(input$seed)
                tibble(
                  mabA = ifelse(1 - rbinom(input$sim_n, 1, input$phiA), rnorm(input$sim_n, input$muA, input$sdA), 4),
                  mabA2 = 10^ifelse(mabA ==4, Inf, mabA),
                  mabB = ifelse(1 - rbinom(input$sim_n, 1, input$phiB), rnorm(input$sim_n, input$muB, input$sdB), 4),
                  mabB2 = 10^ifelse(mabB == 4, Inf, mabB)
                )
              })
              
              PDlims_react = reactive({
                c(min(c(PDdat_react()$mabA,PDdat_react()$mabB)), 4)
              })
              
              PKPDdat = reactive({
                make_pkpd_dat(PKDat(), PDdat_react(), endpoint_set = input$endpoint,
                              interaction_set = input$interaction, thresh = coverage_opt())
              })
              
              optRatio = eventReactive(input$run_optim, {
                map_df(seq(0, 1, by = 0.1), function(i) {
                  pk_dat = tibble(
                    days = mtime(),
                    mAbA = one_cmpt_pk(mtime(), input$dose * i, input$VA, input$hlA),
                    mAbB = one_cmpt_pk(mtime(), input$dose * (1 - i), input$VB, input$hlB
                    )
                  )
                  
                  make_pkpd_dat(pk_dat, PDdat_react(), endpoint_set = input$endpoint,
                                interaction_set = input$interaction, thresh = coverage_opt(), return_summary = T) %>%
                    mutate(ratio = i)
                  
                }) %>%
                  mutate(optim_value = max(value),
                         optimal_ratio = (value == optim_value),
                         user_ratio = ratio == input$ratio) %>%
                  ungroup()
              })
              
              output$PKplot <- renderPlot({
                
                PKDat() %>%
                  tidyr::gather(mAb, concentration, mAbA, mAbB) %>%
                  ggplot(aes(x = days, y = concentration, color = mAb)) +
                  geom_line() +
                  ggtitle(ratio_txt())
                
              })
              
              output$PDplot  <- renderPlot({
                
                (ggplot(PDdat_react(), aes(x = mabA, y = mabB)) + 
                   geom_point(alpha = 0.5) +
                   scale_x_continuous("Log10 IC50A", limits = PDlims_react()) +
                   scale_y_continuous("Log10 IC50B", limits = PDlims_react()) +
                   geom_abline())  %>%
                  ggExtra::ggMarginal(type = "histogram")
              })
              
              output$margPDplot  <- renderPlot({
                margpl = make_marg_pd_plot(PDdat_react())
                margpl
              })
              
              output$PKPDpl  <- renderPlot({
                if(unique(PKPDdat()$summary) == "mean") ylab = paste("mean", input$endpoint)
                if(unique(PKPDdat()$summary) == "coverage") ylab = paste(input$endpoint, ">", input$threshold)
                
                PKPDdat() %>%
                  ggplot(aes(x = days, y = value, color = interaction)) +
                  geom_line(size=0.75) +
                  scale_color_viridis_d() +
                  scale_y_continuous(ylab) +
                  ggtitle(ratio_txt())
                
              })
              
              ylab_opt = eventReactive(input$run_optim, {
                if(unique(optRatio()$summary) == "mean") x = paste("mean", input$endpoint)
                if(unique(optRatio()$summary) == "coverage") x = paste(input$endpoint, ">", input$threshold)
                x
              })
              
              output$optratio  <- renderPlot({
                optratio_pt = optRatio() %>%
                  gather(ratio_selection, selection, optimal_ratio, user_ratio) %>%
                  filter(selection) 
                
                optRatio() %>%
                  ggplot(aes(x = ratio, y = value, color = interaction, linetype = pk_summary)) +
                  geom_line() +
                  geom_point(data = optratio_pt, size = 4, aes(shape = ratio_selection)) +
                  scale_color_viridis_d(guide = F) +
                  scale_shape_manual(values = c(16, 4)) +
                  scale_y_continuous(ylab_opt()) +
                  facet_wrap(~interaction)
                
              })
            }
)
