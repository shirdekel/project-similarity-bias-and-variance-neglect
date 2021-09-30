##' @title Get simple effects for alignment 8

##' @return
##' @author Shir Dekel
##' @export
get_simple_effects_alignment_8 <- function(omnibus = allocation_omnibus) {
  ## High alignment: NPV x reliability level x reliability type

  alignment_high_three_way <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_type", "reliability_amount"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Implicit_explicit_Low_high_High")

  ## Explicit reliability: NPV x reliability level x alignment

  reliability_type_explicit_three_way <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment", "reliability_amount"),
      by = "reliability_type",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Low_high_Low_high_Explicit")

  ## Low alignment: NPV x reliability type

  alignment_low_two_way <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_type"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Implicit_explicit_Low")

  ## High and low alignment explicit reliability: NPV x reliability level

  alignment_reliability_type_explicit_reliability_amount <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_amount"),
      by = c("alignment", "reliability_type"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print()

  alignment_high_reliability_type_explicit_reliability_amount <-
    alignment_reliability_type_explicit_reliability_amount %>%
    pluck("full_result", "Linear_Low_high_High_Explicit")

  alignment_low_reliability_type_explicit_reliability_amount <-
    alignment_reliability_type_explicit_reliability_amount %>%
    pluck("full_result", "Linear_Low_high_Low_Explicit")

  alignment_high_reliability_type_explicit_reliability_amount_npv_amount <-
    omnibus %>%
    emmeans(
      "npv_amount",
      by = c("alignment", "reliability_type", "reliability_amount"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = "poly"
    ) %>%
    apa_print()

  alignment_high_reliability_type_explicit_reliability_amount_high_npv_amount <-
    alignment_high_reliability_type_explicit_reliability_amount_npv_amount %>%
    pluck("full_result", "Linear_High_Explicit_High")

  alignment_high_reliability_type_explicit_reliability_amount_low_npv_amount <-
    alignment_high_reliability_type_explicit_reliability_amount_npv_amount %>%
    pluck("full_result", "Linear_High_Explicit_Low")

  ## Low explicit reliability: low vs high alignment

  reliability_type_explicit_reliability_amount_low_alignment <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment"),
      by = c("reliability_amount", "reliability_type"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Low_high_Low_Explicit")

  ## Low alignment: implicit vs explicit reliability (averaged over reliability level)

  alignment_low_reliability_type <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_type"),
      by = "alignment",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Implicit_explicit_Low")

  ## Implicit reliability: low vs high alignment (averaged over reliability
  ## amount) - null

  reliability_type_implicit_alignment_null <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment"),
      by = "reliability_type",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec"),
      delta = 0.022
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Low_high_Implicit")

  ## Implicit reliability: low vs high alignment (averaged over reliability
  ## amount)

  reliability_type_implicit_alignment <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment"),
      by = "reliability_type",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec"),
      adjust = "bonferroni"
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Low_high_Implicit")

  ## Low and high alignment implicit reliability: low vs high reliability level
  ## - null

  alignment_reliability_type_implicit_reliability_amount_null <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_amount"),
      by = c("alignment", "reliability_type"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec"),
      delta = 0.022
    ) %>%
    apa_print()

  alignment_low_reliability_type_implicit_reliability_amount_null <-
    alignment_reliability_type_implicit_reliability_amount_null %>%
    pluck("full_result", "Linear_Low_high_Low_Implicit")

  alignment_high_reliability_type_implicit_reliability_amount_null <-
    alignment_reliability_type_implicit_reliability_amount_null %>%
    pluck("full_result", "Linear_Low_high_High_Implicit")

  ## Low and high alignment implicit reliability: low vs high reliability level

  alignment_reliability_type_implicit_reliability_amount <-
    omnibus %>%
    emmeans(
      c("npv_amount", "reliability_amount"),
      by = c("alignment", "reliability_type"),
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec"),
      adjust = "bonferroni"
    ) %>%
    apa_print()

  alignment_low_reliability_type_implicit_reliability_amount <-
    alignment_reliability_type_implicit_reliability_amount %>%
    pluck("full_result", "Linear_Low_high_Low_Implicit")

  alignment_high_reliability_type_implicit_reliability_amount <-
    alignment_reliability_type_implicit_reliability_amount %>%
    pluck("full_result", "Linear_Low_high_High_Implicit")

  ## Explicit reliability: low vs high alignment (averaged over reliability level)

  reliability_type_explicit_alignment <-
    omnibus %>%
    emmeans(
      c("npv_amount", "alignment"),
      by = "reliability_type",
      model = "multivariate"
    ) %>%
    contrast(
      interaction = c("poly", "consec")
    ) %>%
    apa_print() %>%
    pluck("full_result", "Linear_Low_high_Explicit")

  lst(
    alignment_high_three_way,
    reliability_type_explicit_three_way,
    alignment_low_two_way,
    alignment_high_reliability_type_explicit_reliability_amount,
    alignment_high_reliability_type_explicit_reliability_amount_high_npv_amount,
    alignment_high_reliability_type_explicit_reliability_amount_low_npv_amount,
    alignment_low_reliability_type_explicit_reliability_amount,
    reliability_type_explicit_reliability_amount_low_alignment,
    alignment_low_reliability_type,
    reliability_type_implicit_alignment_null,
    reliability_type_implicit_alignment,
    alignment_low_reliability_type_implicit_reliability_amount_null,
    alignment_high_reliability_type_implicit_reliability_amount_null,
    alignment_low_reliability_type_implicit_reliability_amount,
    alignment_high_reliability_type_implicit_reliability_amount,
    reliability_type_explicit_alignment
  )
}
