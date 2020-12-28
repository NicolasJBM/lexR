#' @name create_stm
#' @title Create a Structural Topic Model
#' @author Nicolas Mangin
#' @description Create a topic model from a document-to-term matrix.
#' @param dtm             Document-to-Term matrix.
#' @param topic_nbr       Numeric. Number of topics to model.
#' @param prevalence      Formula. See the stm function for detailed instructions.
#' @param content         Formula. See the stm function for detailed instructions.
#' @param init_type       Character string. See the stm function for detailed instructions.
#' @param seed            Numeric. Seed for initialization.
#' @param iterations      Integer. Maximum number of iteration.
#' @param tolerance       Numeric. See the stm function for detailed instructions.
#' @param keywords        Tibble. Two variables: one for the "document" and one of the related "keywords".
#' @param baselab         Character string. Whether labels should be based on "prob", "frex", "lift", or "score".
#' @param frexweight      Numeric. The weight on discriminance in term selection.
#' @param min_gamma       Numeric. Minimum strength of the relationship between document and topic.
#' @param min_beta        Numeric. Minimum strength of the relationship between term and topic.
#' @param min_common      Integer. Maximum number of topics allowed per document.
#' @param verbose         Logical. Should messages be printed.
#' @return A list with the "topic_model", the "topic_term", the "topic_label", and the "topic_document".
#' @seealso \code{\link[stm]{stm}}
#' @seealso \code{\link{create_dtm}}
#' @importFrom stm stm
#' @importFrom stm labelTopics
#' @importFrom tidyr unite
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @importFrom purrr map2_dbl
#' @importFrom dplyr select
#' @importFrom tidytext tidy
#' @export


create_stm <- function(dtm,
                       topic_nbr = 0,
                       prevalence = NULL,
                       content = NULL,
                       init_type = "Spectral",
                       seed = 1234,
                       iterations = 50,
                       tolerance = 0,
                       keywords = NULL,
                       baselab = "prob",
                       frexweight = 0.6,
                       min_gamma = 0.1,
                       min_beta = 0.01,
                       min_common = 1,
                       verbose = TRUE) {


  # Bind variables for dplyr
  label <- NULL
  topic <- NULL
  document <- NULL
  nbkw <- NULL
  nbtrm <- NULL
  common <- NULL
  term <- NULL

  # Create the topic model
  topic_model <- stm::stm(
    dtm,
    K = topic_nbr,
    prevalence = prevalence,
    content = content,
    init.type = init_type,
    seed = seed,
    max.em.its = iterations,
    emtol = tolerance,
    verbose = verbose
  )

  # Identify the terms most closely related to each topic.
  topic_term <- tidytext::tidy(topic_model, matrix = "beta") %>%
    dplyr::group_by(term) %>%
    dplyr::top_n(1, beta) %>%
    dplyr::ungroup() %>%
    dplyr::filter(beta >= min_beta) %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(term = list(term)) %>%
    dplyr::ungroup() %>%
    dplyr::select(topic, term) %>%
    dplyr::mutate(topic = as.integer(topic))

  # Create a label for the topic based on most closely related terms.
  topic_label <- stm::labelTopics(
    topic_model,
    n = 4,
    frexweight = frexweight
  )[[baselab]] %>%
    as.data.frame() %>%
    tidyr::unite(label, sep = " ") %>%
    tibble::rownames_to_column("topic") %>%
    dplyr::mutate(topic = as.integer(topic))

  # Identify the documents most closely related to each topic.
  topic_document <- topic_model$theta %>%
    as.data.frame() %>%
    dplyr::mutate(document = dtm@Dimnames$docs) %>%
    tidyr::gather(topic, gamma, -document) %>%
    dplyr::mutate(topic = as.integer(gsub("V", "", topic))) %>%
    dplyr::filter(gamma >= min_gamma)

  # filter relations based on the overlap between
  # documents' topical terms and documents' keywords
  if (!is.null(keywords)) {
    topic_document <- topic_document %>%
      dplyr::left_join(topic_term, by = "topic") %>%
      dplyr::left_join(keywords, by = "document") %>%
      dplyr::mutate(common = purrr::map2_dbl(
        keywords,
        term,
        function(x, y) length(intersect(x, y))
      )) %>%
      dplyr::mutate(nbkw = purrr::map_dbl(
        keywords,
        function(x) length(unique(x))
      )) %>%
      dplyr::mutate(
        nbkw = case_when(is.finite(nbkw) & nbkw > 0 ~ nbkw, TRUE ~ 1)
      ) %>%
      dplyr::mutate(nbtrm = purrr::map_dbl(
        term,
        function(x) length(unique(x))
      )) %>%
      dplyr::mutate(
        prop_kw = common / nbkw,
        prop_trm = common / nbtrm,
        jaccard = common / (nbkw + nbtrm - common)
      ) %>%
      dplyr::select(-keywords, -term) %>%
      dplyr::filter(nbkw <= 2 | common >= min_common)
  }

  # Gather and return the results.
  result <- list(
    topic_model = topic_model,
    topic_term = topic_term,
    topic_label = topic_label,
    topic_document = topic_document
  )

  return(result)
}
