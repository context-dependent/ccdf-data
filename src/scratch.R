
bpconnect::vpn_start()




sf <- load_sf()
max(nchar(sf$pas_id))

qx <- load_surveys(sf)
qx |> 
  filter(survey_time == 1, survey_version == 2) |> 
  select(-sf) |> 
  unnest(qx) |> 
  filter(lubridate::quarter(StartDate, fiscal_start = 4, type = "year.quarter") == 2025.1) |>
  janitor::clean_names() |>  
  semi_join(sf, by = "response_id") |> 
  select(matches("date_of_birth")) |> 
  View()

d <- join_surveys(qx)

d_root <- d |> construct_root_df()
d_hope <- d |> construct_hope_df()
d_out <- d |> construct_outcomes_df()

d_hope
d_hope |> 
  tidyr::pivot_longer(
    matches("^ehs_"), 
    names_to = "ehs_domain", 
    values_to = "score"
  ) |> 
  dplyr::group_by(ehs_domain, survey_time) |> 
  dplyr::summarize(
    n = sum(!is.na(score)),
    score_mean = mean(score, na.rm = TRUE), 
    score_sd_t = sd(score, na.rm = TRUE)
  ) |> 
  dplyr::mutate(
    score_sd_p = weighted.mean(score_sd_t, n)
  ) |> 
  dplyr::mutate(
    score_delta = score_mean - lag(score_mean), 
    score_delta_cohen = score_delta / score_sd_p
  ) |> 
  View()


library(ggplot2)


d_hope |> 
  dplyr::mutate(survey_tag = forcats::fct_reorder(survey_tag, survey_time)) |> 
  dplyr::group_by(survey_time, survey_tag) |> 
  dplyr::summarize(
    across(
      matches("^ehs_"),
      ~ mean(.x, na.rm = TRUE)
    )
  ) |> 
  tidyr::pivot_longer(
    matches("^ehs_"),
    names_to = "ehs_domain", 
    values_to = "avg_score"
  ) |> 
  ggplot(aes(
    survey_tag, 
    avg_score, 
    color = ehs_domain, 
    group = ehs_domain
  )) + 
  geom_line()  

dr <- d |> 
  dplyr::filter(survey_time == 1, survey_version == 1) |> 
  dplyr::pull(data)

dr <- dr[[1]]

dr |> 
  dplyr::select(matches("^emp_hope")) |> 
  dplyr::glimpse()

dr |> 
  dplyr::filter(!is.na(pas_id_qx), nchar(pas_id_qx) > 0) |> 
  dplyr::summarize(mean(nchar(pas_id_qx)))

score_emp_hope(dr)



m <- matrix(sample(c(0, 1), 12, replace = TRUE), nrow = 4, ncol = 3)


x <- cbind(
  c(1, 1, 0), 
  c(0, 1, 1)
)

m %*% x / rbind(c(1, 2))

dr <- dr |> 
  dplyr::mutate(
        job_start_date = update(
          lubridate::today(), 
          year = readr::parse_integer(as.character(job_start_date_year)),
          month = as.integer(job_start_date_month), 
          day = 1
        ), 
        job_tenure = lubridate::interval(job_start_date, as.Date(start_date)) |> 
          lubridate::as.period()
      )

dr |> 
  dplyr::filter(!is.na(job_tenure)) |> 
  dplyr::mutate(job_tenure_weeks = lubridate::as.duration(job_tenure) / lubridate::dweeks(1)) |> 
  dplyr::select(job_tenure, job_tenure_weeks)



ehs_repair <- readr::read_csv(r"(Z:\FSC Pilot - CCDF\Data\RCT\Survey\Intake Survey\FSC_0.0.2_CCDF_RCT_IMMM_User_s01_Intake_March 15, 2022_07.01.csv)", guess_max = 20)

ehs_repair |> 
  select(matches("^Q115_"))

ehs_repaired <- ehs_repair |> 
  dplyr::slice(1:(n() - 2)) |> 
  tidyr::pivot_longer(
    matches("^Q115_"), 
    names_pattern = "Q115_(.*)_(.*)", 
    names_to = c("ehs_item", "option"), 
    values_to = "ehs_item_response"
  ) |> 
  janitor::clean_names() |> 
  mutate(ehs_item = as.integer(ehs_item)) |>  
  group_by(response_id, ehs_item) |> 
  summarize(
    ehs_item_value = case_when(
      all(is.na(ehs_item_response)) ~ NA_real_, 
      TRUE ~ mean(as.numeric(ehs_item_response), na.rm = TRUE)
    )
  ) |> 
  pivot_wider(
    names_from = ehs_item, 
    values_from = ehs_item_value
  )



ehs_repaired

intake_v2 <- d |> 
  filter(survey_time == 1, survey_version == 2) |> 
  unnest(data) |> 
  janitor::clean_names()

intake_v2 |>
  select(matches("^emp_hope"))


er <- load_ehs_repair(fetch = TRUE)


intake_v2 |> 
  filter(!is.na(response_id)) |> 
  mutate(across(matches("^emp_hope"), as.numeric)) |> 
  select(response_id, matches("^emp_hope")) |> 
  semi_join(er, by = "response_id") |> 
  rows_update(er, by = "response_id", unmatched = "ignore")



sf |> 
  group_by(cohort_name) |> 
  summarize(start_missing = mean(is.na(cohort_start_date))) |> 
  arrange(desc(start_missing))

intake_v2 <- d |> 
  filter(survey_time == 1, survey_version == 2) |> 
  unnest(data) 

intake_v2 |> 
  select(matches("job"))

dr <- intake_v2


dr |> 
  dplyr::select(-matches("a[2-9]")) |> 
  dplyr::rename_all(~stringr::str_remove(.x, "^a1_")) |> 
  dplyr::count(job_report_salary_9_text)

drp <- dr  |> 
  dplyr::select(-matches("a[2-9]")) |> 
  dplyr::rename_all(~stringr::str_remove(.x, "^a1_"))

dr |> 
  harmonize_employment(t = 1, v = 2)


intake_v1 |> 
  harmonize_employment(t = 1, v = 1)

drp1 <- intake_v1 |> 
  dplyr::count(job_benefits_17)
drp1

intake_v1 |> 
  dplyr::count(educ_type)

library(tidyverse)
source("src/pipeline/transform.R")
bpconnect::vpn_start()

d <- load_data() |> clean_data()
d |> 
  select(outcomes)

d0 <- load_data()

d0 |> 
  group_by(survey_time) |> 
  summarize(
    data = list(data) |> purrr::map(dplyr::bind_rows)
  )

d0 |> 
  construct_satis_df()
