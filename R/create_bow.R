#' @name create_bow
#' @title Create a bag of words
#' @author Nicolas Mangin
#' @description Create bags of words from either an analysis of syntactic relationships or a character string.
#' @param text    Tibble or character string. Either syntactic relationships for one document, as returned by create_syntrel after grouping per document, or one document as a single simplified (see clean_letters) and ideally lemmatized string (see clean_replace and dat_en_lemmas).
#' @param basis    Character string. Whether a bag of "word", a bag of "lemma", or a bag of "stem" should be returned (only for create_syntrel output).
#' @param keep_pos Character vector. Parts of speech which should be kept to build the bags of words (only for create_syntrel output)
#' @return A tibble simplified into a bag of words.
#' @seealso \code{\link{create_syntrel}}
#' @seealso \code{\link{clean_letters}}
#' @seealso \code{\link{clean_replace}}
#' @seealso \code{\link{dat_en_lemmas}}
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom tibble is_tibble
#' @importFrom tibble as_tibble
#' @export

create_bow <- function(text = NULL,
                       basis = "lemma",
                       keep_pos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV")) {
  stopifnot(
    tibble::is_tibble(text) | length(text) == 1,
    basis %in% c("word", "lemma", "stem"),
    is.character(keep_pos)
  )

  # Bind variables for dplyr
  src_word <- NULL
  src_lemma <- NULL
  src_stem <- NULL
  src_pos <- NULL
  src_token <- NULL
  term <- NULL
  word <- NULL
  lemma <- NULL
  stem <- NULL

  if (tibble::is_tibble(text)) {
    bow <- text %>%
      dplyr::rename(word = src_word, lemma = src_lemma, stem = src_stem) %>%
      dplyr::mutate(
        word = tolower(word),
        lemma = tolower(lemma),
        stem = tolower(stem)
      ) %>%
      dplyr::filter(src_pos %in% keep_pos) %>%
      dplyr::select(src_token, term = basis) %>%
      unique() %>%
      dplyr::select(-src_token) %>%
      dplyr::group_by(term) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::ungroup()
  } else {
    bow <- text %>%
      as.character() %>%
      strsplit(split = " ") %>%
      table() %>%
      tibble::as_tibble()
    names(bow) <- c("term", "count")
  }

  return(bow)
}
