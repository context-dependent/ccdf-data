dl_button <- function(x, name) {
  x |> 
    downloadthis::download_this(
      output_name = glue::glue("sk-overview_{name}"), 
      output_extension = ".csv", 
      button_label = "Download data as CSV", 
      button_type = "default", 
      has_icon = TRUE, 
      icon = "fa fa-save"
    )
}

quarterly_table <- function(d, caption = NULL, file_name = NULL) {
  labs_lgl <- c(
    demo_immigrant_lgl = "Migration--Immigrant",
    demo_newcomer_lgl = "Migration--Newcomer (< 5 years in Canada)",
    demo_born_in_canada_lgl = "Migration--Born in Canada", 
    demo_receiving_ei_lgl = "Financial Assistance--Receiving EI",
    demo_receiving_ia_lgl = "Financial Assistance--Receiving Social Assistance", 
    demo_receiving_none_lgl = "Financial Assistance--None of the Above",
    demo_young_lgl = "Age--Youth (< 30)",
    demo_middle_age_lgl = "Age--Middle Age (30-64)",
    demo_old_lgl = "Age--Older (> 64)", 
    demo_racialized_lgl = "Marginalization Factors--Racialized", 
    demo_indigenous_lgl = "Marginalization Factors--Indigenous", 
    demo_bipoc_lgl = "Marginalization Factors--BIPOC",  
    demo_disability_lgl = "Marginalization Factors--Disability", 
    demo_woman_plus_lgl = "Marginalization Factors--Woman+",
    demo_mf_none_lgl = "Marginalization Factors--None of the Above"
  )

  t01 <- dplyr::bind_rows(d, d |> dplyr::mutate(assignment_label_fct = "Total")) |> 
    dplyr::mutate(
      demo_born_in_canada_lgl = !demo_immigrant_lgl,
      demo_woman_plus_lgl = demo_gender_fct != "Man" | demo_transgender_lgl, 
      demo_receiving_none_lgl = demo_receiving_ei_lgl + demo_receiving_ia_lgl == 0, 
      demo_young_lgl = demo_age < 30, 
      demo_old_lgl = demo_age > 64,
      demo_middle_age_lgl = demo_young_lgl + demo_old_lgl == 0, 
    ) |> 
    dplyr::select(enrollment_id, assignment_label_fct, matches('^demo_.*lgl$'))  |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      demo_mf_none_lgl = sum(
        c_across(
          matches("^demo_(racialized|indigenous|bipoc|disability|woman_plus)_lgl")
        )
      ) == 0
    ) |> 
    dplyr::ungroup() |> 
    dplyr::select(-matches("trans|in_sk|parent")) |> 
    tidyr::pivot_longer(cols = -c(enrollment_id, assignment_label_fct), names_to = "var", values_to = "val", cols_vary = "slowest") |>
    dplyr::mutate(var = fct_inorder(var)) |>  
    dplyr::filter(!is.na(val)) |> 
    dplyr::group_by(assignment_label_fct, var) |> 
    dplyr::summarize(
      d = glue::glue("{scales::percent(mean(val))} ({sum(val)}/{n()})"),
      .groups = "drop"
    ) |> 
    dplyr::mutate(lab = labs_lgl[as.character(var)]) |>
    tidyr::separate(lab, into = c("cat", "var"), sep = "--", extra = "merge") |> 
    tidyr::pivot_wider(
      names_from = assignment_label_fct, 
      values_from = d
    )

  tab <- t01 |> 
    dplyr::group_by(cat) |> 
    gt::gt(rowname_col = "var") |>
    gt::tab_stub_indent(rows = everything()) |> 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups(groups = everything())
    ) |> 
    gt::cols_align(align = "right", columns = everything())

  if (!is.null(caption)) {
    tab <- tab |> 
      gt::tab_source_note(source_note = caption) |> 
      gt::tab_style(
        style = gt::cell_text(
          color = "lightgray", 
          size = "small"
        ), 
        locations = gt::cells_source_notes()
      )
  } 

  btn <- dl_button(t01, file_name)

  list(tab = tab, btn = btn)
}
