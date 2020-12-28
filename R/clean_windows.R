#' @name clean_windows
#' @title Extract windows of words around a focal pattern.
#' @author Nicolas Mangin
#' @description Extract windows of words around a regex pattern.
#' @param text    Character string. String from which the windows should be extracted.
#' @param pattern Character string. Regex pattern around which the adjacent pattern has to be detected within the window.
#' @param before  Integer. Number of words to extract before the pattern.
#' @param after   Integer. Number of words to extract after the pattern.
#' @return A list of windows around each occurrences of the focal pattern.
#' @importFrom stringr str_extract_all
#' @export

clean_windows <- function(text,
                          pattern,
                          before = 3,
                          after = 3) {
  stopifnot(
    is.character(text),
    length(text) == 1,
    is.character(pattern),
    length(pattern) == 1,
    is.numeric(before),
    length(before) == 1,
    is.numeric(after),
    length(after) == 1
  )

  expression <- paste0(
    "([^\\s]+\\s){0,",
    before, "}", pattern,
    "?\\w+(\\s[^\\s]+){0,",
    after, "}"
  )

  extraction <- unlist(stringr::str_extract_all(text, expression))
  if (length(extraction) == 0) extraction <- ""

  return(extraction)
}
