library(tidyverse)
library(bpqx)

readRenviron(".env")

latest_cache_file <- function(obj_id) {
  cache_dir <- file.path(Sys.getenv("Z_HOME"), "Data", "cache", obj_id)

  if(!fs::dir_exists(cache_dir)) {
    return(NULL)
  }

  cache_files <- cache_dir |> 
    fs::dir_ls() |> 
    sort()

  if (length(cache_files) == 0) {
    return(NULL)
  }
  
  dplyr::last(cache_files)
} 

new_cache_file <- function(obj_id, extension = ".rds") {
  obj_dir <- file.path(Sys.getenv("Z_HOME"), "Data", "cache", obj_id)
  if(!fs::dir_exists(obj_dir)) {
    fs::dir_create(obj_dir)
  }
  
  file.path(
    obj_dir, 
    glue::glue("{strftime(lubridate::now(), format = '%Y%m%d%H%M%S')}_{extension}")
  )
}

save_survey <- function(qx_survey_id) {
  d <- bpqx::fetch_responses(qx_survey_id)
  cache_file <- new_cache_file(qx_survey_id)
  readr::write_rds(d, cache_file)
}

load_sf <- function(fetch = FALSE) {
  if (fetch) reticulate::py_run_file("src/pipeline/extract.py")

  readr::read_csv(latest_cache_file("sf"), show_col_types = FALSE) |> 
    dplyr::select(-dplyr::where(~all(is.na(.x)))) |> 
    dplyr::mutate(
      pas_id = stringr::str_sub(pas_id, 1, 15),
      .g = cohort_name |>
        stringr::str_trim() |>  
        stringr::str_extract("(Program|Comparison)$"), 
      assignment_label_fct = 
        dplyr::case_when(
          is.na(.g) ~ "Non-RCT", 
          TRUE ~ .g
        ) |>
        forcats::fct_relevel("Program", "Comparison", "Non-RCT"), 
      program_eligible = assignment_label_fct %in% c("Treatment", "Non-RCT") 
    )
}

load_ehs_repair <- function(fetch = FALSE) {
  if (fetch || is.null(latest_cache_file("ehs_repair"))) {
    f_in <- file.path(
      Sys.getenv("Z_HOME"), 
      "Data", "RCT", "Survey",
      "Intake Survey", 
      "FSC_0.0.2_CCDF_RCT_IMMM_User_s01_Intake_March 15, 2022_07.01.csv"
    )
    d <- readr::read_csv(f_in, show_col_types = FALSE)|> 
      dplyr::slice(1:(dplyr::n() - 2)) |> 
      tidyr::pivot_longer(
        matches("^Q115_"), 
        names_pattern = "Q115_(.*)_(.*)", 
        names_to = c("ehs_item", "option"), 
        values_to = "ehs_item_response"
      ) |> 
      janitor::clean_names() |> 
      dplyr::mutate(ehs_item = pmin(as.integer(ehs_item), 14)) |>  
      dplyr::group_by(response_id, ehs_item) |> 
      dplyr::summarize(
        ehs_item_value = case_when(
          all(is.na(ehs_item_response)) ~ NA_real_, 
          TRUE ~ mean(as.numeric(ehs_item_response), na.rm = TRUE)
        )
      ) |> 
      tidyr::pivot_wider(
        names_from = ehs_item, 
        values_from = ehs_item_value, 
        names_glue = "emp_hope_1_{ehs_item}"
      )

    f_out <- new_cache_file("ehs_repair")

    readr::write_rds(d, f_out)
  }

  readr::read_rds(latest_cache_file("ehs_repair"))
}

load_survey <- function(qx_survey_id, fetch = FALSE) {
  if (fetch || is.null(latest_cache_file(qx_survey_id))) {
    save_survey(qx_survey_id)
  } 
  readr::read_rds(latest_cache_file(qx_survey_id))
}

load_surveys <- function(sf, fetch = FALSE) {
  sf |> 
    dplyr::group_by(qx_survey_id) |> 
    dplyr::group_nest(.key = "sf") |> 
    dplyr::mutate(qx = purrr::map(qx_survey_id, ~ load_survey(.x, fetch = fetch))) |> 
    dplyr::left_join(survey_list(), by = "qx_survey_id") |> 
    dplyr::mutate(
      survey_time = survey_name |> 
        stringr::str_extract(r"((?<=_s)\d{2})") |> 
        readr::parse_integer(), 
      survey_version = survey_name |> 
        stringr::str_extract(r"((?<=^FSC_0\.0\.)\d)") |> 
        readr::parse_integer(), 
      survey_tag = survey_name |> 
        stringr::str_extract(r"((?<=s\d{2}_).*$)")
    ) |> 
    dplyr::select(-survey_name)
  
}

survey_list <- function(fetch = FALSE) {
  file <- "data/survey_list.rds"
  if(fetch | !fs::file_exists(file)) {
    readr::write_rds(bpqx::list_surveys(), file)
  }
  readr::read_rds(file) |> 
    dplyr::select(qx_survey_id = id, survey_name = name)
}

join_surveys <- function(d, by = c("response_id")) {
  d |> 
    dplyr::mutate(
      data = purrr::map2(
        sf, qx, ~dplyr::left_join(
          .x, janitor::clean_names(.y), 
          by = by, 
          suffix = c("_sf", "_qx")
        )
      )
    ) |> 
    dplyr::select(qx_survey_id, data, matches("^survey_"))
}

harmonize_baseline <- function(dr, t, v) {
  res <- dr |> 
    dplyr::filter(!is.na(response_id))
  
  if (t == 1) {
    res <- res |> 
      dplyr::mutate(
        # Education
        educ_int = as.integer(education_highest),  
        educ_max_fct = dplyr::case_when(
          educ_int == 1 ~ "Less than Highschool", 
          educ_int == 2 ~ "Highschool",
          educ_int <= 5 ~ "Apprenticeship or College", 
          educ_int == 6 ~ "Undergraduate Degree", 
          educ_int == 7 ~ "Graduate Degree", 
          TRUE ~ NA_character_
        ) |> forcats::fct_relevel(
          "Less than Highschool", 
          "Highschool", 
          "Apprenticeship or College", 
          "Undergraduate Degree", 
          "Graduate Degree"
        ), 
        educ_domestic_lgl = education_domestic == "In Canada",
        educ_hs_lgl = educ_max_fct == "Highschool", 
        educ_pse_lgl = dplyr::case_when(
          is.na(educ_max_fct) ~ NA, 
          educ_max_fct %in% c("Apprenticeship or College", "Undergraduate Degree", "Graduate Degree") ~ TRUE,
          TRUE ~ FALSE
        )
      )
  } else {
    res <- res |> 
      dplyr::filter(demos == "FALSE")
  }

  res <- res |> 
    dplyr::mutate(
      # Sex and Gender
      demo_gender_fct = suppressWarnings(dplyr::case_when(
        demo_sex == "Male" & demo_transgender == "No" ~ "Man", 
        demo_sex == "Female" & demo_transgender == "No" ~ "Woman", 
        demo_gender |> stringr::str_detect("Male|Man") ~ "Man", 
        demo_gender |> stringr::str_detect("Female|Woman") ~ "Woman", 
        is.na(demo_gender) ~ NA_character_, 
        TRUE ~ "Other"
      ) |> forcats::fct_relevel("Woman", "Man", "Other")), 
      demo_transgender_lgl = dplyr::case_when(
        is.na(demo_transgender) ~ NA,
        demo_transgender == "Prefer not to answer" ~ NA, 
        demo_transgender == "No" ~ FALSE, 
        TRUE ~ TRUE
      ), 

      # Age, Location
      demo_date_of_birth = update(
        lubridate::today(), 
        year = readr::parse_integer(as.character(demo_date_of_birth_3)),
        month = as.integer(demo_date_of_birth_1),
        day = 1
      ), 
      demo_age = lubridate::interval(demo_date_of_birth, date_of_enrollment) |> 
        lubridate::as.period() |> 
        lubridate::year(),
      demo_age_fct = cut(
        demo_age, 
        breaks = c(0, 20, 30, 40, 60, 100), 
        labels = c("20 or younger", "21 to 30", "31 to 40", "41 to 60", "61 or older")
      ),
      demo_province_fct = demo_province, 
      demo_in_sk_lgl = demo_province == "Saskatchewan", 
      demo_receiving_ei_lgl = income_source == "Employment Insurance", 
      demo_receiving_ia_lgl = income_source != "None of the above" & 
        income_source != "Employment Insurance",
      
      # Disability
      demo_disability_lgl = demo_disability == "Yes",
      
      # Immigration
      demo_immigrant_lgl = demo_born_in_canada == "No", 
      demo_newcomer_lgl = dplyr::case_when(
        is.na(demo_immigrant_lgl) ~ NA, 
        is.na(demo_year_of_arrival) ~ NA, 
        lubridate::year(date_of_enrollment) - demo_year_of_arrival <= 5 ~ TRUE, 
        TRUE ~ FALSE
      ), 
    ) 

    
  if (v == 1) {
    res <- res |> 
      dplyr::mutate(
        demo_indigenous_lgl = demo_indigenous == "Yes", 
        demo_racialized_lgl = demo_racialized == "Yes", 
        demo_parent_lgl = demo_children == "Yes" & demo_parent == "Yes",
      ) 
  } else {
    res <- res |>
      dplyr::mutate(
        demo_parent_lgl = demo_children == "Yes" & demo_caregiver == "Yes", 
      ) |> 
      dplyr::rowwise() |> 
      dplyr::mutate(
        nsel_indigenous = sum(!is.na(dplyr::c_across(matches("demo_indigenous_\\d")))), 
        demo_indigenous_lgl = dplyr::case_when(
          nsel_indigenous == 0 ~ NA, 
          !is.na(demo_indigenous_2) ~ FALSE, 
          TRUE ~ TRUE 
        ), 
        nsel_race = sum(!is.na(dplyr::c_across(demo_race_1:demo_race_11))), 
        demo_racialized_lgl = dplyr::case_when(
          nsel_race == 0 ~ NA, 
          nsel_race == 1 & !is.na(demo_race_10) ~ FALSE, 
          TRUE ~ TRUE
        )
      ) |> 
      dplyr::ungroup()
  }

  res |> 
    dplyr::mutate(
      demo_single_parent_lgl = demo_parent_lgl & 
        stringr::str_detect(demo_marital, "married|common-law", negate = TRUE),
      demo_bipoc_lgl = demo_racialized_lgl | demo_indigenous_lgl
    ) |> 
    dplyr::select(
      enrollment_id, 
      date_of_enrollment, 
      demo_age, 
      demo_date_of_birth, 
      matches("_fct$"), 
      matches("_lgl$")
    )
}

`%fill%` <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

first_non_missing <- function(x, a = NA) {
  if (length(x) == 0 || !is.na(a)) {
    return(a)
  } else {
    return(first_non_missing(x[-1], x[1]))
  }
}


construct_root_df <- function(d) {
  d |> 
    dplyr::filter(
      survey_time == 1 | 
      (survey_time <= 3 & survey_version == 2)
    ) |> 
    dplyr::mutate(
      demo = purrr::pmap(
        .l = list(dr = data, t = survey_time, v = survey_version), 
        .f = harmonize_baseline
      )
    ) |> 
    dplyr::arrange(survey_time, survey_version) |> 
    dplyr::select(-data) |> 
    tidyr::unnest(demo) |> 
    dplyr::select(-matches("survey")) |> 
    dplyr::group_by(enrollment_id) |> 
    dplyr::summarize(
      across(
        everything(), 
        first_non_missing
      )
    )
}

score_emp_hope <- function(dr, t, v, ehs_repair = load_ehs_repair()) {

  d <- dr |>
    dplyr::mutate(
      dplyr::across(
        matches("^emp_hope"), 
        function(x) {
          as.integer(x) - 1
        }
      )
    ) 
    
  if (t == 1 && v == 2) {
    d <- d |> 
      dplyr::rows_update(ehs_repair, by = "response_id", unmatched = "ignore")
  }
    
  hope_mat <- d |> 
    dplyr::select(matches("^emp_hope")) |> 
    as.matrix() |> 
    unname()


  factor_mat <- matrix(0L, nrow = 14, ncol = 4)
  factor_mat[1:4,   1] <- 1 / 4
  factor_mat[5:6,   2] <- 1 / 2
  factor_mat[7:10,  3] <- 1 / 4
  factor_mat[11:14, 4] <- 1 / 4

  score_mat <- hope_mat %*% factor_mat
  colnames(score_mat) <- c(
    "ehs_empowerment", 
    "ehs_motivation", 
    "ehs_utilization", 
    "ehs_goal_orientation"
  )

  dr |> 
    dplyr::select(enrollment_id) |> 
    dplyr::bind_cols(tibble::as_tibble(score_mat))

}

construct_hope_df <- function(d) {
  d |> 
    dplyr::filter(survey_time <= 3) |> 
    dplyr::mutate(
      emp_hope = purrr::pmap(
        .l = list(dr = data, t = survey_time, v = survey_version),
        .f = score_emp_hope
      )
    ) |> 
    dplyr::select(-data) |> 
    tidyr::unnest(emp_hope) |> 
    dplyr::group_by(enrollment_id, survey_time, survey_tag) |> 
    dplyr::summarize(
      dplyr::across(
        matches("^ehs_"),
        first_non_missing
      ), .groups = "drop"
    ) |> 
    dplyr::group_by(
      enrollment_id
    ) |> 
    dplyr::group_nest(.key = "ehs")
}

TODO <- function() {
  parent_scope <- deparse(sys.calls()[[sys.nframe() - 1]])  
  stop(glue::glue("{parent_scope} not yet implemented (#TODO)"), call. = FALSE)
}

harmonize_employment <- function(dr, t, v) {

  res <- dr

  if (v == 2) {
    # keep primary job only, to align with v1
    res <- res |> 
      dplyr::select(-matches("a[2-9]")) |> 
      dplyr::rename_all(~stringr::str_remove(.x, "^a1_"))
  }

  res <- res |> 
    dplyr::filter(!is.na(response_id)) |> 
    dplyr::mutate(
      job_employed = job_employed |> stringr::str_detect("Yes"),
      job_start_date = update(
        lubridate::today(), 
        year = readr::parse_integer(as.character(job_start_date_year)),
        month = as.integer(job_start_date_month), 
        day = 1
      ), 
      job_tenure = lubridate::interval(job_start_date, as.Date(start_date)) |> 
        lubridate::as.period(), 
      job_tenure_wks = lubridate::as.duration(job_tenure) / lubridate::dweeks(1), 
      job_seasonal = job_seasonal == "Yes", 
      job_satis_overall_pos = job_satisfaction_overall |> 
        stringr::str_detect("Agree|Strongly agree"),
      job_satis_advance_pos = job_satisfaction_advancement |> 
        stringr::str_detect("Agree|Strongly agree"),
      job_satis_precarity_pos = job_satisfaction_precarity |> 
        stringr::str_detect("Agree|Strongly agree"),
    )

  ben_cols <- res |> 
    colnames() |> 
    stringr::str_detect("^job_benefits_")
  
  colnames(res)[ben_cols] <- c(
    "job_benefits_health", 
    "job_benefits_dental",
    "job_benefits_disability", 
    "job_benefits_pension", 
    "job_benefits_vacation",
    "job_benefits_none"
  )[1:sum(ben_cols)]

  res <- res |> 
    dplyr::mutate(
      dplyr::across(
        matches("^job_benefits"), 
        function(x) !is.na(x)
      )
    ) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      job_benefits_n = dplyr::case_when(
        job_employed ~ sum(c_across(job_benefits_health:job_benefits_vacation)), 
        TRUE ~ NA_integer_
      ),
      job_benefits_any = job_benefits_n > 0, 
    ) |> 
    dplyr::ungroup()

  if (v == 1) {
    # VERSION 1
    res <- res |> 
      dplyr::mutate(
        job_annualized_salary = dplyr::case_when(
          nchar(job_salary) > 0 ~ readr::parse_number(job_salary), 
          job_has_hourly_wage %in% "Yes" ~ job_hourly_wage * job_hours * 52, 
          TRUE ~ NA_real_
        ), 
        job_temporary = job_permanent == "No", 

      )
  
  } else { 
    # VERSION 2
    res <- res |> 
      dplyr::mutate(
        job_annualized_salary = dplyr::case_when(
          job_has_hourly_wage %in% "Yes" ~ job_hourly_wage * job_hours * 52, 
          job_report_salary %in% "Yearly" ~ job_salary, 
          job_report_salary %in% "Monthly" ~ job_salary * 12,
          job_report_salary %in% "Weekly" ~ job_salary * 52,
          job_report_salary %in% "Semimonthly (twice per month)" ~ job_salary * 24,
          job_report_salary %in% "Biweekly (every two weeks)" ~ job_salary * 26,
          TRUE ~ NA_real_
        ), 
        job_temporary = job_temporary == "Yes", 
      )
  }
  
  res |> 
    dplyr::select(
      enrollment_id, 
      job_employed, 
      job_start_date, 
      job_tenure, 
      job_tenure_wks, 
      job_seasonal, 
      job_temporary, 
      job_casual, 
      matches("^job_satis_"), 
      matches("^job_benefits_"),
      job_annualized_salary
    )
}


harmonize_education <- function(dr, t, v) {

}

construct_outcomes_df <- function(d) {
  d |> 
    dplyr::mutate(
      emp = purrr::pmap(
        .l = list(dr = data, t = survey_time, v = survey_version), 
        .f = harmonize_employment
      ), 
      educ = purrr::pmap(
        .l = list(dr = data, t = survey_time, v = survey_version), 
        .f = harmonize_education
      )
    )
  
}