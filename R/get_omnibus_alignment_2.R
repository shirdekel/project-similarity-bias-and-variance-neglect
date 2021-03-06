#' @title Omnibus results for alignment 2
##' @param data
#' @return
#' @author Shir Dekel
#' @export
get_omnibus_alignment_2 <- function(data, dv) {
  data %>%
    ## Makes sure that npv levels are in the correct order for the emmeans
    ## linear trend analysis
    arrange(npv_amount) %>%
    aov_ez(
      id = "id",
      dv = dv,
      between = c("alignment", "reliability_amount"),
      within = "npv_amount",
      data = .,
      type = 2
    )
}
