#' @name create_syntnet
#' @title Create a Syntactic Network
#' @author Nicolas Mangin
#' @description Make an tidygraph from a list of syntactic relationships as returned by create_syntrel.
#' @param syntrel   Tibble. Output of the function create_syntrel.
#' @param basis     Character string. Whether the network should be based on "word" or "lemma".
#' @param keep_pos  Character vector. Parts of speech which should be kept to build the network.
#' @param multiplex Logical. Whether the kind of relationship should be kept.
#' @param min_count Numeric. Minimum number of occurrences for a relationship to be kept.
#' @return A tidygraph of syntactic relationships between words of lemmas
#' @seealso \code{\link{create_syntrel}}
#' @importFrom tibble is_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr everything
#' @importFrom dplyr n
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @importFrom stats na.omit
#' @export

create_syntnet <- function(syntrel,
                           basis = "lemma",
                           keep_pos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV"),
                           multiplex = FALSE,
                           min_count = 1) {
  stopifnot(
    tibble::is_tibble(syntrel),
    basis %in% c("word", "lemma", "stem"),
    is.character(keep_pos),
    is.logical(multiplex),
    is.numeric(min_count)
  )

  # Bind variables for dplyr
  src_word <- NULL
  src_lemma <- NULL
  src_stem <- NULL
  src_pos <- NULL
  src_token <- NULL
  src_pos <- NULL
  tgt_pos <- NULL
  relation <- NULL
  rel <- NULL
  tgt_token <- NULL
  tgt_word <- NULL
  tgt_lemma <- NULL
  tgt_stem <- NULL
  label <- NULL
  weight <- NULL
  name <- NULL
  occurrences <- NULL

  # Define which level should be used as source
  src <- switch(basis,
    word = src_word,
    lemma = src_lemma,
    stem = src_stem
  )

  # Define which level should be used as target
  tgt <- switch(basis,
    word = tgt_word,
    lemma = tgt_lemma,
    stem = tgt_stem
  )

  # Filter the requested parts of speech
  edges <- syntrel %>%
    dplyr::filter(src_pos %in% keep_pos, tgt_pos %in% keep_pos)

  # Create edges
  if (multiplex) {
    edges <- edges %>%
      dplyr::select(src = src, src_pos, tgt = tgt, tgt_pos, rel = relation) %>%
      dplyr::group_by(src, src_pos, tgt, tgt_pos, rel) %>%
      dplyr::summarise(weight = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(weight >= min_count) %>%
      na.omit()
  } else {
    edges <- edges %>%
      dplyr::select(src = src, tgt = tgt) %>%
      dplyr::group_by(src, tgt) %>%
      dplyr::summarise(weight = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(weight >= min_count) %>%
      na.omit()
  }

  # Create nodes
  nodes <- dplyr::bind_rows(
    dplyr::select(
      syntrel,
      token = src_token,
      word = src_word,
      lemma = src_lemma
    ),
    dplyr::select(
      syntrel,
      token = tgt_token,
      word = tgt_word,
      emma = tgt_lemma
    )
  ) %>%
    unique() %>%
    dplyr::select(name = basis) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(occurrences = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(name %in% unique(union(edges$src, edges$tgt)))

  if (multiplex) {
    graph <- edges %>%
      dplyr::select(from = src, to = tgt, src_pos, tgt_pos, rel, weight)
  } else {
    graph <- edges %>%
      dplyr::select(from = src, to = tgt, weight)
  }

  # Create the graph
  if (nrow(edges) > 0) {
    graph <- graph %>%
      tidygraph::as_tbl_graph() %>%
      tidygraph::activate("nodes") %>%
      dplyr::left_join(nodes, by = "name") %>%
      dplyr::mutate(label = name) %>%
      dplyr::mutate(name = seq_len(length(label))) %>%
      dplyr::select(name, label, occurrences, dplyr::everything())
  } else {
    graph <- NA
  }

  return(graph)
}
