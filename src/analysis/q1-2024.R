source("src/pipeline/transform.R")
library(tidyverse)
bpconnect::vpn_start()

d <- load_sf() |> 
  load_surveys() |> 
  join_surveys()

d_root <- d |> 
  construct_root_df() |> 
  mutate(
    enrollment_quarter = lubridate::quarter(date_of_enrollment, fiscal_start = 4, type = "year.quarter")
  )

dq1 <- d_root |> 
  filter(enrollment_quarter == 2025.1) 

labs_lgl <- c(
  demo_born_in_canada = "Born in Canada", 
  demo_immigrant_lgl = "Immigrant", 
  demo_newcomer_lgl = "Newcomer (< 5 years in Canada)",
  demo_indigenous_lgl = "Indigenous", 
  demo_racialized_lgl = "Racialized", 
  demo_bipoc_lgl = "BIPOC",  
  demo_disability_lgl = "Disability", 
  demo_young_lgl = "Youth (< 30)",
  demo_old_lgl = "Older (> 64)", 
  demo_woman_plus_lgl = "Woman+"
)

dt_lgl <- dq1 |> 
  transmute(
    enrollment_id,
    assignment_label_fct, 
    demo_born_in_canada = !demo_immigrant_lgl, 
    demo_disability_lgl, 
    demo_immigrant_lgl, 
    demo_newcomer_lgl, 
    demo_indigenous_lgl,
    demo_racialized_lgl,
    demo_bipoc_lgl,
    demo_young_lgl = demo_age < 30, 
    demo_old_lgl = demo_age >= 65, 
    demo_woman_plus_lgl = demo_transgender_lgl | (demo_gender_fct != "Man"),
  ) |>
  pivot_longer(
    matches("^demo_"), 
    names_to = "var", 
    values_to = "val", 
    cols_vary = "slowest"
  ) |> 
  filter(!is.na(val)) |> 
  mutate(var = fct_inorder(var)) |> 
  group_by(assignment_label_fct, var) |>
  summarize(
    N = n(), 
    n = sum(val), 
    p = n / N, 
    pct_n = glue::glue("{scales::percent(p, accuracy = 1)} ({n}/{N})")
  ) |> 
  mutate(
    lab = labs_lgl[as.character(var)]
  ) |> 
  select(
    assignment_label_fct, 
    lab, 
    pct_n
  ) |> 
  pivot_wider(
    names_from = assignment_label_fct, 
    values_from = pct_n
  )

  
dt_prov <- dq1 |> 
  filter(!is.na(demo_province_fct)) |> 
  group_by(lab = demo_province_fct, assignment_label_fct) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |> 
  group_by(assignment_label_fct) |> 
  mutate(
    pct_n = glue::glue("{scales::percent(n / sum(n), accuracy = 1)} ({n}/{sum(n)})")
  ) |> 
  select(-n) |> 
  pivot_wider(
    names_from = assignment_label_fct, 
    values_from = pct_n, 
    values_fill = ""
  )

dt_lgl |> 
  bind_rows(dt_prov) |> 
  write_csv("out/q1-2025/demos_q1-2025.csv")

dt_lgl

