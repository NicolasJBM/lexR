#' @name create_dtm
#' @title Create a Document-to-Term Matrix
#' @author Nicolas Mangin
#' @description Transform bags of words into a document to term matrix after applying some filters.
#' @param bow        Tibble. Output of the function eval_bow. Document ids must be in a variable called "document".
#' @param min_term   Integer. Remove terms appearing less than this number of times.
#' @param max_term   Integer. Remove terms appearing more than this number of times.
#' @param min_doc    Integer, Remove terms appearing in less than this number of documents.
#' @param max_doc    Integer, Remove terms appearing in more than this number of documents.
#' @param nbterm     Integer. Select this number of terms based on tf-idf.
#' @param keep_terms Character vector. List of words which should be included even if they do not meet the other criteria.
#' @param docvar     Tibble. Additional information about documents to be appended to the docvar of the dtm. Document ids must be in a variable called "document".
#' @return A document to term matrix.
#' @seealso \code{\link{eval_bow}}
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr top_n
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map_dbl
#' @export


create_dtm <- function(bow,
                       min_term = 0,
                       max_term = Inf,
                       min_doc = 0,
                       max_doc = Inf,
                       nbterm = 1000,
                       keep_terms = NULL,
                       docvar = NULL) {
  stopifnot(
    names(bow) == c(
      "document", "term", "count", "doc_word_count",
      "term_count", "term_doc_count", "tf", "idf", "tf_idf"
    )
  )

  # Bind variables for dplyr
  term_count <- NULL
  term_doc_count <- NULL
  term <- NULL
  data <- NULL
  maxtfidf <- NULL
  doc_word_count <- NULL
  count <- NULL
  document <- NULL

  # Apply filters
  dtm <- bow %>%
    dplyr::filter(
      term_count >= min_term,
      term_count <= max_term,
      term_doc_count >= min_doc,
      term_doc_count <= max_doc
    )

  dtm <- dtm %>%
    dplyr::group_by(term) %>%
    tidyr::nest() %>%
    dplyr::mutate(maxtfidf = purrr::map_dbl(
      data, function(x) max(x$tf_idf)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::top_n(nbterm, maxtfidf) %>%
    dplyr::select(-maxtfidf) %>%
    tidyr::unnest(data)

  # Re-append terms to be kept
  if (!is.null(keep_terms)) {
    add2dtm <- bow %>%
      dplyr::filter(term %in% keep_terms, !(term %in% (unique(dtm$term))))
    dtm <- dtm %>%
      dplyr::bind_rows(add2dtm) %>%
      unique()
  }

  # Create table with document characteristics (word count + docvar)
  doc <- unique(dplyr::select(dtm, document, word_count = doc_word_count))
  if (!is.null(docvar)) {
    doc <- dplyr::left_join(doc, docvar, by = "document")
  }

  # Create the document to term matrix
  dtm <- dtm %>%
    dplyr::select(document, term, count) %>%
    tidytext::cast_dfm(document = document, term = term, value = count)

  # Add the document characteristics to the document to term matrix
  dtm@docvars <- doc

  return(dtm)
}
