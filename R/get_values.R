##' @title Get static branching values
##'
##' @return
##' @author Shir Dekel
##' @export
get_values <- function() {

  tibble(
    experiment_number = c(2, 3, 8),
    iv = list(
      ## Experiment 1 (alignment2)
    c(
      "alignment",
      "reliability_amount",
      "npv_amount"
    ),
      ## Experiment 2 (alignment3)
    c(
      "alignment",
      "reliability_amount"
    ),
      ## Experiment 3 (alignment8)
    c(
      "alignment",
      "reliability_type",
      "reliability_amount",
      "npv_amount"
    )
    ),
    data = list(
      quote(alignment2::data),
      quote(alignment3::data),
      quote(alignment8::data)
    )
  ) %>%
    rowwise() %>%
    mutate_function_call(
      "plot",
      experiment_number
    ) %>%
    mutate_function_call(
      "results",
      experiment_number
    )
}
