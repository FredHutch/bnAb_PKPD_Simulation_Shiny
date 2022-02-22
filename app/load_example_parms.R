pk_ex_tab = read_csv("pk-lit-examples.csv", col_types = cols()) %>%
  mutate_all(as.character)
pd_ex_tab = read_csv("pd-lit-examples.csv", col_types = cols()) %>%
  mutate_all(as.character)