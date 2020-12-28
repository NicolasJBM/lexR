#' @name clean_tags
#' @title Clean into pure ASCII text
#' @author Nicolas Mangin
#' @description Remove HTML tags.
#' @param text  Character. String with HTML tags.
#' @return String without HTML tags.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @export

clean_tags <- function(text) {
  stopifnot(
    is.character(text),
    length(text) == 1
  )

  # Remove html tags then
  # replace double spaces by single spaces
  text <- text %>%
    stringr::str_replace_all("<.*?>", " ") %>%
    stringr::str_replace_all("  ", " ")

  trimws(text)
}
