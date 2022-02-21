pk_lit = tribble(
  ~bNAb, ~`Elim. HL (days)`, ~V, ~Q, ~Vp, ~ka, ~Bioavailability, ~Cl, ~Ref, 
  "IgG Meta-analysis", 23, 3.6, 0.75, 2.75, 0.3, 0.75, 0.2, "Davda et al. (MAbs, 2014)",
  "10-1074", 25, 4.5, 0.7, 3.5, NA, NA, 0.25, "Cohen et al. (Plos One, 2009)",
  "3BNC117", 17, 5.00, 2.25, 8.5, NA, NA, 0.65, "Cohen et al. (Plos One, 2009)",
  "PGT121", 22, 3.35, 1.04, 4.53, 0.37, 0.4, 0.27, "Stephenson et al. (Nature Med., 2021)",
  "VRC07-523LS", 35, 1.89, 0.4, 2.3, 0.4, 0.5, 0.09, "Gaudinski et al. (Lancet HIV, 2019)"
) %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")

# log(2)/convert_twocmpt_parms(Cl = 0.27, 3.35, 1.04, 4.53)$beta
