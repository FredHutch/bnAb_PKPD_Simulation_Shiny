library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(pracma)
library(viridis)
library(ggplot2)

# Source required helper files

source("pk_fun.R")
source("processing_fun.R")
source("pkpd_fun.R")
source("load_example_parms.R")

# Define server logic required

shinyServer(function(input, output) {
              theme_set(theme_bw() + theme(legend.position = "top"))
              mtime = reactive({
                c(0.1, 0.5, 1, seq(2, input$finaltime, by = 1))
              })
              text_size = 16
              #mtime = times()

              ratio_txt = reactive({
                oddsB = round(1/input$ratio, 1) - 1
                oddsA = round(1/(1-input$ratio), 1) - 1
                ratio_txt = case_when(
                  oddsB == 0 ~ "mAbA only",
                  oddsA == 0 ~ "mAbB only",
                  oddsA > 0 & oddsA < 1 ~ paste0("1:", oddsB, "(mAbA:mAbB)"),
                  TRUE ~ paste0(oddsA, ":1 (mAbA:mAbB)")
                )
              })
              
              ClA = reactive({
                if(input$twocmpt_on) calc_CL(V = input$VA, Q = input$QA, 
                                            Vp = input$VpA, hl = input$hlA) else input$VA*log(2)/input$hlA
              })
              ClB = reactive({
                if(input$twocmpt_on) calc_CL(V = input$VB, Q = input$QB, 
                                            Vp = input$VpB, hl = input$hlB) else input$VB*log(2)/input$hlB
              })
              
              output$ratio_print = renderText({ratio_txt()})
              output$titer_iip = renderTable({tibble(`Predicted ID80` = input$ID50 * 4^(1/input$hill), 
                                                     `% neut.` = if_else(
                                                       titer2neut(input$ID50, hill = input$hill)>0.9999,
                                                       ">99.99",
                                                       as.character(round(100*titer2neut(input$ID50, hill = input$hill), 2))
                                                       ),
                                                     `IIP threshold`= -log10(1 - titer2neut(input$ID50, 
                                                                                            hill = input$hill)))
                })
              
              output$pk_tab = renderTable({
                 QA = if(input$twocmpt_on) input$QA else NA
                 VpA = if(input$twocmpt_on) input$VpA else NA
                 QB = if(input$twocmpt_on) input$QB else NA
                 VpB = if(input$twocmpt_on) input$VpB else NA                
                 kaA = if(input$SC_A)  input$kaA else NA
                 FbioA = if(input$SC_A)  input$FbioA else NA
                 kaB = if(input$SC_B)  input$kaB else NA
                 FbioB = if(input$SC_B)  input$FbioB else NA

                tribble(
                  ~bNAb, ~`Dose (mg)`, ~`Elim. HL (days)`, ~V, ~Cl, ~ka, ~Bioavailability, ~Q, ~Vp,
                  "mAb A", input$dose*input$ratio, input$hlA, input$VA, round(ClA(), 3), kaA, FbioA, QA, VpA, 
                  "mAb B", input$dose*(1-input$ratio), input$hlB, input$VB, round(ClB(), 3), kaB, FbioB, QB, VpB
                  ) %>%
                  mutate_all(as.character) %>%
                  replace(is.na(.), "")
                
              })
              
              output$pk_ex_tab = renderTable(pk_ex_tab)
              output$pd_ex_tab = renderTable(pd_ex_tab[1:10, ])
              
              coverage_opt = reactive({
                thresh = if(input$threshout) input$threshold else -1
                thresh
              })

              PKDat = reactive({
                tibble(
                  days = mtime(),
                  mAbA = make_pk_dat(mtime = mtime(), dose = input$dose * input$ratio,
                                     input = input, mAb = "A", Cl = ClA()),
                  mAbB = make_pk_dat(mtime = mtime(), dose = input$dose * (1 - input$ratio),
                                     input = input, mAb = "B", Cl = ClB())
                )
              })

              PDdat_react = reactive({
                set.seed(input$seed)
                tibble(
                  mabA = ifelse(1 - rbinom(input$sim_n, 1, input$phiA), 
                                rnorm(input$sim_n, input$muA, input$sdA), 4), #log10 scale
                  mabA2 = 10^ifelse(mabA == 4, Inf, mabA), 
                  mabB = ifelse(1 - rbinom(input$sim_n, 1, input$phiB), 
                                rnorm(input$sim_n, input$muB, input$sdB), 4), #log10 scale
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

              optRatio_tmp = eventReactive(input$run_optim, {
                map_df(seq(0, 1, by = 0.05), function(i) {
                  pk_dat = tibble(
                    days = mtime(),
                    mAbA = make_pk_dat(mtime = mtime(), dose = input$dose * i,
                                       input = input, mAb = "A", Cl = ClA()),
                    mAbB = make_pk_dat(mtime = mtime(), dose = input$dose * (1 - i),
                                       input = input, mAb = "B", Cl = ClB())
                  )

                  make_pkpd_dat(pk_dat, PDdat_react(), endpoint_set = input$endpoint,
                                interaction_set = input$interaction, thresh = coverage_opt(), return_summary = T) %>%
                    mutate(ratio = i)

                }) %>%
                  mutate(optim_value = max(value),
                         optimal_ratio = (value == optim_value),
                         user_ratio = abs(ratio - input$ratio) < 1e-3) %>%
                  ungroup()
              })
              optRatio = reactive({
                if(!is.null(optRatio_tmp())) {
                  optRatio_tmp() %>% mutate(user_ratio = abs(ratio - input$ratio) < 1e-3)
                } else optRatio_tmp()
              })
              
              # optRatio = observeEvent(input$dose, {
              #   if(exists('optRatio()')) optRatio() %>% mutate(user_ratio = ratio == input$ratio)
              # })

              output$PKplot <- renderPlot({

                pk_pl = PKDat() %>%
                  tidyr::gather(mAb, concentration, mAbA, mAbB) %>%
                  ggplot(aes(x = days, y = concentration, color = mAb)) +
                  geom_line(size = 1.5) +
                  theme(text = element_text(size = text_size),
                        legend.position = c(0.95, 0.9), legend.justification = "right")
                
                if(input$log_time) pk_pl = pk_pl + scale_x_log10()
                if(input$log_conc) pk_pl = pk_pl + scale_y_log10()
                
                pk_pl
              })

              output$PDplot  <- renderPlot({

                (ggplot(PDdat_react(), aes(x = 10^mabA, y = 10^mabB)) +
                   geom_point(alpha = 0.5) +
                   scale_x_log10("IC50A", limits = 10^PDlims_react()) +
                   scale_y_log10("IC50B", limits = 10^PDlims_react()) +
                   geom_abline() +
                   theme(text = element_text(size = text_size)))  %>%
                  ggExtra::ggMarginal(type = "histogram")
              })

              output$margPDplot  <- renderPlot({
                margpl = make_marg_pd_plot(PDdat_react())
                margpl
              })

              output$PKPDpl  <- renderPlot({
                if(unique(PKPDdat()$summary) == "mean") ylab = paste("mean", input$endpoint)
                if(unique(PKPDdat()$summary) == "coverage") ylab = paste("IIP", ">", input$threshold,
                                                                         "(% viruses)")

                PKPDdat() %>%
                  mutate(
                    interaction_lab = factor(interaction, levels = c("BH", "additivity", "maxNeut", "minNeut"),
                                             labels =  c("BH", "additivity", "maximum", "minimum"))
                  ) %>%
                  ggplot(aes(x = days, y = value, color = interaction_lab)) +
                  geom_line(size=1.5) +
                  scale_color_viridis_d("") +
                  scale_y_continuous(ylab) +
                  theme(text = element_text(size = text_size))

              })

              ylab_opt = eventReactive(input$run_optim, {
                if(unique(optRatio()$summary) == "mean") x = paste("mean", input$endpoint)
                if(unique(optRatio()$summary) == "coverage") x = paste("IIP", ">", input$threshold,
                                                                       "(% viruses)")
                x
              })

              output$optratio  <- renderPlot({
                optratio_pt = optRatio() %>%
                  gather(ratio_selection, selection, optimal_ratio, user_ratio) %>%
                  mutate(ratio_selection = gsub("_ratio", "", ratio_selection)) %>%
                  filter(selection) %>%
                  mutate(
                    interaction_lab = factor(interaction, levels = c("BH", "additivity", "maxNeut", "minNeut"),
                                             labels =  c("BH", "additivity", "maximum", "minimum"))
                  ) 

                optRatio() %>%
                  mutate(
                    interaction_lab = factor(interaction, levels = c("BH", "additivity", "maxNeut", "minNeut"),
                                             labels =  c("BH", "additivity", "maximum", "minimum"))
                  ) %>%
                  ggplot(aes(x = ratio, y = value, color = interaction_lab)) +
                  geom_line() +
                  geom_point(data = optratio_pt, size = 4, aes(shape = ratio_selection)) +
                  scale_color_viridis_d("") +
                  scale_shape_manual("ratio", values = c(16, 4)) +
                  scale_y_continuous(ylab_opt()) +
                  scale_x_continuous("ratio: mAbA / total dose", 
                                     breaks = 0:4/4,
                                     labels = c("All\nmAbB", 
                                                0.25, 0.5, 0.75,
                                                "All\nmAbA")) +
                  facet_grid(rows = vars(pk_summary), scales="free_y", switch = "y") +
                  theme(strip.placement = "outside", strip.background = element_blank(),
                        legend.box = "vertical", legend.position = "right",
                        panel.spacing.x = unit(1, "lines"),
                        strip.switch.pad.grid = unit(0, "lines"),
                        text = element_text(size = text_size))

              })
            }
)
