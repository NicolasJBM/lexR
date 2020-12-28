#' @name eval_bow
#' @title Compute bag of words metrics
#' @author Nicolas Mangin
#' @description Compute basic metrics for a bag of words.
#' @param bow      Tibble. Bag of words produced with create_bow.
#' @param document Character string. Name of the variable indicating the document id.
#' @param term     Character string. Name of the variable indicating the term.
#' @param count    Character string. Name of the variable indicating the frequence.
#' @return A tibble 9 variables: document, term, count, doc_word_count (number of words in the document), term_count (frequency of the term), term_document_count (number of documents in which the term appears), tf, idf, tf_idf.
#' @seealso \code{\link{create_bow}}
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_all
#' @importFrom dplyr %>%
#' @importFrom tidytext bind_tf_idf
#' @importFrom stats median
#' @export

eval_bow <- function(bow,
                     document = "document",
                     term = "term",
                     count = "count") {
  stopifnot(
    tibble::is_tibble(bow),
    is.character(document),
    is.character(term),
    is.character(count)
  )

  bow <- bow %>%
    select(document = document, term = term, count = count) %>%
    group_by(document, term) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  # Count in how many documents the term appears
  documentcount <- bow %>%
    select(document, doc_word_count = count) %>%
    group_by(document) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  # Count how many times the term appears
  termcount <- bow %>%
    select(term, term_count = count) %>%
    mutate(term_doc_count = 1) %>%
    group_by(term) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()

  # Append all metrics and add tf-idf
  bow <- bow %>%
    left_join(documentcount, by = "document") %>%
    left_join(termcount, by = "term") %>%
    bind_tf_idf(term, document, count)

  return(bow)
}
