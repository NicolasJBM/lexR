#' @name eval_stm
#' @title Evaluate Structural Topic Models.
#' @author Nicolas Mangin
#' @description Return various metrics to evaluate topic model performance for different topic numbers.
#' @param dtm         dtm. Document to term matrix used for Structural Topic Model.
#' @param topic_model stm. Structural Topic Model based on the document to term matrix.
#' @param keywords    Tibble.
#' @return A tibble with various metrics of tm quality for each possible combination of the topic number and seed.
#' @seealso \code{\link{create_stm}}
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_all
#' @importFrom dplyr summarise
#' @importFrom dplyr sample_n
#' @importFrom dplyr case_when
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr top_n
#' @importFrom dplyr bind_cols
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tm stemDocument
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_int
#' @importFrom purrr map_dbl
#' @importFrom purrr pmap
#' @importFrom tibble as_tibble
#' @importFrom stm stm
#' @importFrom stm exclusivity
#' @importFrom stm checkResiduals
#' @importFrom stm semanticCoherence
#' @importFrom stm make.heldout
#' @importFrom stm eval.heldout
#' @importFrom tidytext tidy
#' @importFrom stats runif
#' @export

eval_stm <- function(dtm = NA,
                     topic_model = NA,
                     keywords = NULL) {
  stopifnot(
    !is.na(dtm),
    !is.na(topic_model)
  )

  # Bind variables for dplyr
  bound <- NULL
  lfact <- NULL
  document <- NULL
  topic <- NULL
  common <- NULL
  nbkw <- NULL
  prop_kw <- NULL
  prop_trm <- NULL
  jaccard <- NULL

  heldout <- stm::make.heldout(dtm)

  tm <- topic_model$topic_model
  td <- topic_model$topic_document

  # Compute usual indicators of stm quality
  stm_quality <- tibble::tibble(
    topics =
      length(unique(td$topic)),
    exclusivity =
      mean(stm::exclusivity(tm), na.rm = TRUE),
    semantic_coherence =
      mean(stm::semanticCoherence(tm, documents = dtm), na.rm = TRUE),
    residuals =
      stm::checkResiduals(tm, dtm)$dispersion,
    held_out_likelihood =
      stm::eval.heldout(tm, heldout$missing)$expected.heldout,
    bound =
      unlist(max(tm$convergence$bound)),
    lfact =
      unlist(factorial(tm$settings$dim$K)),
    iter =
      length(tm$convergence$bound)
  ) %>%
    dplyr::mutate(lbound = bound + lfact)

  rm(heldout)

  # Assess the overlap between documents' topical terms and documents' keywords
  if (!is.null(keywords)) {
    td <- td %>%
      select(-nbkw) %>%
      unique() %>%
      group_by(document) %>%
      summarise(
        topic = n(),
        gamma = max(gamma, na.rm = TRUE),
        common = max(common, na.rm = TRUE),
        prop_kw = max(prop_kw, na.rm = TRUE),
        prop_trm = max(prop_trm, na.rm = TRUE),
        jaccard = max(jaccard, na.rm = TRUE)
      ) %>%
      select(-document) %>%
      summarise(
        document = n(),
        tpd_min = min(topic, na.rm = TRUE),
        tpd_avg = mean(topic, na.rm = TRUE),
        tpd_med = median(topic, na.rm = TRUE),
        tpd_max = max(topic, na.rm = TRUE),
        gamma_avg = mean(gamma, na.rm = TRUE),
        common_avg = mean(common, na.rm = TRUE),
        prop_kw_avg = mean(prop_kw, na.rm = TRUE),
        prop_trm_avg = mean(prop_trm, na.rm = TRUE),
        jaccard_avg = mean(jaccard, na.rm = TRUE)
      )

    stm_quality <- dplyr::bind_cols(stm_quality, td)
  }

  return(stm_quality)
}
