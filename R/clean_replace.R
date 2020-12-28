#' @name clean_replace
#' @title Replace words in string.
#' @author Nicolas Mangin
#' @description Identify occurring patterns with a replacement and operate the replacement
#' @param text         Character. A string in which patterns have to be replaced.
#' @param replacements Tibble. A tibble with two variables: "pattern" and "replacement".
#' @return The initial string or vector in which replacements have been made.
#' @importFrom tibble is_tibble
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter
#' @export


clean_replace <- function(text, replacements) {
  stopifnot(
    is.character(text),
    tibble::is_tibble(replacements)
  )

  # Bind variables for dplyr
  pattern <- NULL

  # Identify occurring patterns
  y <- text
  z <- dplyr::filter(replacements, pattern %in% unlist(strsplit(y, split = " ")))

  # Replace
  for (i in seq_len(nrow(z))) {
    y <- stringr::str_replace_all(y, z$pattern[[i]], z$replacement[[i]])
  }

  return(y)
}
