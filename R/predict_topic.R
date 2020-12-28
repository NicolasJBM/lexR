#' @name predict_topic
#' @title Apply a STM to a new corpus
#' @author Nicolas Mangin
#' @description Apply a topic model to a new corpus of documents.
#' @param bow         Tibble. Bag of words as returned by the function create_bow
#' @param topic_model List. Structural topic model as returned by the function create_stm.
#' @param keywords    Tibble. One variable specifying the "document", one variable containing the "keywords" as a character vector.
#' @param min_gamma   Numeric. Minimum strength of the relationship between topic and document.
#' @param min_common  Integer. Maximum number of topics allowed per document.
#' @return A tibble linking topics and documents for a new corpus.
#' @seealso \code{\link{create_bow}}
#' @seealso \code{\link{create_stm}}
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom stm textProcessor
#' @importFrom stm prepDocuments
#' @importFrom stm alignCorpus
#' @importFrom stm fitNewDocuments
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom stringr str_remove_all
#' @export


predict_topic <- function(bow,
                          topic_model,
                          keywords = NULL,
                          min_gamma = 0.1,
                          min_common = 1) {

  # Bind variables for manipulation with dplyr
  term <- NULL
  document <- NULL
  topic <- NULL
  nbkw <- NULL
  common <- NULL


  # From the number of times a term appear to
  # a number of appearances of the term
  doc <- bow %>%
    dplyr::mutate(
      term = purrr::map2(
        term,
        count,
        function(x, y) paste(rep(x, y), collapse = " ")
      )
    ) %>%
    dplyr::select(document, term) %>%
    dplyr::group_by(document) %>%
    dplyr::summarise(term = paste(term, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(document)


  # Gather and format data to apply the structural topic model
  newdoc <- stm::textProcessor(
    doc$term,
    removestopwords = FALSE,
    removenumbers = FALSE,
    removepunctuation = FALSE,
    stem = FALSE
  )
  prepdoc <- stm::prepDocuments(newdoc$documents, newdoc$vocab, newdoc$meta)
  old_vocab <- topic_model$topic_model$vocab
  aligneddoc <- stm::alignCorpus(prepdoc, old.vocab = old_vocab)

  # Apply the structural topic model to the new corpus
  apply_tm <- stm::fitNewDocuments(
    model = topic_model$topic_model,
    documents = aligneddoc$documents,
    newData = aligneddoc$meta
  )
  prediction <- as.data.frame(apply_tm$theta)


  # Ensure that kept documents match made predictions
  if (length(row.names(prediction)) == length(doc$document)) {
    row.names(prediction) <- doc$document
  } else {
    row.names(prediction) <- doc$document[-aligneddoc$docs.removed]
  }
  prediction <- prediction %>%
    tibble::rownames_to_column("document") %>%
    tidyr::gather(topic, gamma, -document) %>%
    dplyr::mutate(topic = as.integer(stringr::str_remove_all(topic, "V"))) %>%
    dplyr::filter(gamma > min_gamma)


  # Evaluate the quality of the keywords-based match between documents and
  # predicted topic
  if (!is.null(keywords)) {
    prediction <- prediction %>%
      dplyr::left_join(
        dplyr::select(topic_model$topic_term, topic, term),
        by = "topic"
      ) %>%
      dplyr::left_join(keywords, by = "document") %>%
      dplyr::mutate(common = purrr::map2_dbl(
        keywords, term, function(x, y) length(intersect(x, y))
      )) %>%
      dplyr::mutate(nbkw = purrr::map(
        keywords,
        function(x) length(unique(x))
      )) %>%
      dplyr::select(-term, -keywords) %>%
      dplyr::filter(nbkw <= 2 | common >= min_common) %>%
      dplyr::select(-nbkw)
  }

  return(prediction)
}
