


make_marg_pd_plot = function(pddat, text_size = 16){
  resistant_dist = pddat %>%
    tidyr::gather(mab, ic50, mabA, mabB) %>%
    group_by(mab) %>%
    summarize(
      resistant = mean(ic50 == 4),
      resistant_frac = paste(n() * resistant, n(), sep = "/")
    )
  
  res_pl =  ggplot(resistant_dist, aes(x = mab, y = resistant)) +
    geom_bar(aes(fill = mab), stat = 'identity', width = 0.5) +
    geom_text(aes(label = resistant_frac), vjust = -.2, size = 6) +
    scale_y_continuous(expression(paste("Resistant Prop.")), limits = c(0,1), breaks = 0:4/4) +
    scale_x_discrete("") +
    theme_bw() + theme(
      text = element_text(size = text_size),
      axis.text.x = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(0.1, .1, -.5, .1), "lines")
    )
  
  ic50_pl = pddat %>%
    tidyr::gather(mab, ic50, mabA, mabB) %>%
    filter(ic50 < 4) %>%
    ggplot(aes(x = mab, y = 10^ic50, colour = mab)) +
    geom_boxplot() +
    scale_y_log10("IC50") +
    theme_bw() +
    theme(plot.margin = unit(c(0,0,0.1,0), "lines"), 
          text = element_text(size = text_size),
          strip.text = element_blank(),
          legend.position = "none")
  cowplot::plot_grid(res_pl, ic50_pl, nrow = 2, align = "v",
                     rel_heights = c(1.25,2))  
}


