
# this doesnt work in deployment, annoying
# pk_ex_tab = read.csv("data/pk-lit-examples.csv") %>%
#   mutate_all(as.character)
# pd_ex_tab = read.csv("data/pd-lit-examples.csv") %>%
#   mutate_all(as.character)

pk_ex_tab = structure(
  list(
    bNAb = c(
      "IgG Meta-analysis",
      "10-1074",
      "3BNC117",
      "PGT121",
      "VRC07-523LS"
    ),
    Elim..HL..days. = c("23", "25", "17",
                        "22", "35"),
    V = c("3.6", "4.5", "5", "3.35", "1.89"),
    Q = c("0.75",
          "0.7", "2.25", "1.04", "0.4"),
    Vp = c("2.75", "3.5", "8.5", "4.53",
           "2.3"),
    ka = c("0.3", NA, NA, "0.37", "0.4"),
    Bioavailability = c("0.75",
                        NA, NA, "0.4", "0.5"),
    Cl = c("0.2", "0.25", "0.65", "0.27",
           "0.09"),
    Ref = c(
      "Davda et al. (MAbs, 2014)",
      "Cohen et al. (Plos One, 2009)",
      "Cohen et al. (Plos One, 2009)",
      "Stephenson et al. (Nature Med., 2021)",
      "Gaudinski et al. (Lancet HIV, 2019)"
    )
  ),
  class = "data.frame",
  row.names = c(NA,-5L)
)
pk_ex_tab = pk_ex_tab %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "") %>%
  as_tibble() %>%
  rename(`Elim. HL (days)` = Elim..HL..days.)

pd_ex_tab = structure(
  list(
    bNAb = c(
      "VRC01",
      "10E8.4/iMab",
      "PGT121",
      "3BNC117",
      "PGT121",
      "3BNC117",
      "10-1074",
      "VRC07-523LS",
      "VRC07-523LS",
      "CAP256-VRC25.26",
      "PDGM1400",
      "PGDM1400"
    ),
    Mean.log10.IC50 = c(
      "0.5",
      "-3",
      "-1.6",
      "-1.4",
      "-1.3",
      "-0.75",
      "-1.4",
      "-1.2",
      "0.05",
      "-2.7",
      "-0.25",
      "-1.7"
    ),
    Std..Dev..log10.IC50 = c(
      "0.5",
      "0.5",
      "1",
      "0.7",
      "1",
      "0.7",
      "0.9",
      "0.7",
      "0.6",
      "2.25",
      "1.2",
      "1.1"
    ),
    Resistant..pct. = c("50", "2.5", "35", "5", "30", "25", "35",
                        "5", "5", "50", "50", "25"),
    Clade = c(
      "Multiple (breakthrough)",
      "Tier 2 pseudovirus panel",
      "A1",
      "A1",
      "C",
      "C",
      "C",
      "C",
      "C (isolates)",
      "C (isolates)",
      "C (isolates)",
      "C"
    ),
    Ref = c(
      "Corey et al. (NEJM, 2021)",
      "Huang et al. (Cell, 2017)",
      "CATNAP Database",
      "CATNAP Database",
      "CATNAP Database",
      "CATNAP Database",
      "CATNAP Database",
      "CATNAP Database",
      "Lorenzi et al. (Virology, 2021)",
      "Lorenzi et al. (Virology, 2021)",
      "Lorenzi et al. (Virology, 2021)",
      "CATNAP Database"
    )
  ),
  class = "data.frame",
  row.names = c(NA,-12L)
)

pd_ex_tab = pd_ex_tab %>%
  as_tibble() %>%
  mutate_all(as.character) %>%
  rename(
    `Mean log10 IC50` = Mean.log10.IC50,
    `Std. Dev. log10 IC50` = Std..Dev..log10.IC50,
    `% resistant` = Resistant..pct.
  )
