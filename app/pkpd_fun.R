
titer2neut = function(id50) 1 - 1/(1+id50)

simple_IIP_interactions = function(dat) {
  dat %>%
    mutate(
      minID50 = pmin(ID50_1, ID50_2),
      minNeut = titer2neut(minID50),
      maxID50 = pmax(ID50_1, ID50_2),
      maxNeut = titer2neut(maxID50),
      additivity = 1 - 1 / (1 + (ID50_1 + ID50_2)),
      BH = 1 - (1 - titer2neut(ID50_1)) * (1 - titer2neut(ID50_2))
    ) %>%
    transmute_at(vars(BH, additivity, maxNeut, minNeut), list(neut = identity, IIP = ~(-log10(1 - .x))))
}

make_pkpd_dat = function(pkdat, pddat, endpoint_set, interaction_set, thresh = -1, return_summary = F) {
  pkpd_out = pkdat %>%
    group_by(days) %>%
    nest() %>%
    mutate(
      ID50_dat = map(data, ~ (
        tibble(
          ID50_1 = .x$mAbA / pddat$mabA2,
          ID50_2 = .x$mAbB / pddat$mabB2
        )
      )),
      neut_res = map(ID50_dat, simple_IIP_interactions),
      neut_summary = map(neut_res, summarize_all, list(mean = mean, coverage = ~100 * mean(.x > thresh)))
    ) %>%
    select(-data, -ID50_dat, -neut_res) %>% 
    unnest(neut_summary) %>%
    ungroup() %>%
    gather(outcome, value, -days) %>% 
    separate(outcome,
             into = c("interaction", "endpoint", "summary"),
             sep = "_") %>%
    filter(endpoint == endpoint_set &
            interaction %in% interaction_set)
  
  if (thresh >= 0) {
    pkpd_out = filter(pkpd_out, summary == "coverage")
  } else{
    pkpd_out = filter(pkpd_out, summary == "mean")
  }
  
  if(!return_summary) return(pkpd_out)
  
  pkpd_out %>%
    group_by(interaction, endpoint, summary) %>%
    arrange(days) %>%
    summarize_at(vars(-days), list(auc = ~pracma::trapz(days, .x)/max(days),
                                   trough = last)) %>%
    gather(pk_summary, value, auc, trough) %>%
    group_by(interaction, endpoint, summary, pk_summary) 
  
}
