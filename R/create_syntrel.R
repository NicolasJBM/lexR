#' @name create_syntrel
#' @title Create Syntactic Relationships
#' @author Nicolas Mangin
#' @description Apply a udpipe model for part-of-speech tagging and dependency parsing to identify syntactic relationships.
#' @param text  Character string. String to be parsed.
#' @param model Character string. Path from the working directory to the downloaded udpipe model.
#' @return A tibble where each observation is a syntactic relationship between two words from the initial text.
#' @seealso \code{\link[udpipe]{udpipe}}
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#' @importFrom udpipe udpipe_load_model
#' @importFrom udpipe udpipe
#' @importFrom tm stemDocument
#' @export


create_syntrel <- function(text,
                           model) {
  stopifnot(
    is.character(text),
    length(text) == 1,
    is.character(model),
    length(model) == 1
  )

  # Bind variables for dplyr
  doc_id <- NULL
  paragraph_id <- NULL
  sentence_id <- NULL
  token_id <- NULL
  token <- NULL
  src_token <- NULL
  tgt_token <- NULL
  lemma <- NULL
  stem <- NULL
  upos <- NULL
  dep_rel <- NULL
  head_token_id <- NULL
  src_token <- NULL
  src_word <- NULL
  src_lemma <- NULL
  src_stem <- NULL
  src_pos <- NULL

  # Load the model, conduct the analysis, and stem
  ud_model <- udpipe::udpipe_load_model(model)
  y <- tibble(doc_id = "document", text = text) %>%
    udpipe::udpipe(ud_model, parallel.cores = 1) %>%
    tidyr::unite(
      src_token,
      doc_id,
      paragraph_id,
      sentence_id,
      token_id,
      sep = "_",
      remove = FALSE
    ) %>%
    tidyr::unite(
      tgt_token,
      doc_id,
      paragraph_id,
      sentence_id,
      head_token_id,
      sep = "_",
      remove = TRUE
    ) %>%
    dplyr::mutate(stem = tm::stemDocument(lemma))

  # Select source tokens
  y <- y %>%
    dplyr::select(
      src_token,
      src_word = token,
      src_lemma = lemma,
      src_stem = stem,
      src_pos = upos,
      relation = dep_rel,
      tgt_token
    )

  # Select target tokens
  z <- y %>%
    dplyr::select(
      tgt_token = src_token,
      tgt_word = src_word,
      tgt_lemma = src_lemma,
      tgt_stem = src_stem,
      tgt_pos = src_pos
    ) %>%
    unique()

  # Join target to sources
  y <- y %>%
    dplyr::left_join(z, by = "tgt_token")

  return(y)
}
