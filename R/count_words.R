#' @name count_words
#' @title Count words
#' @author Nicolas Mangin
#' @description Count the number of words in a text. Use what has been copied if text is empty.
#' @param text Character string. The sentence(s) in which the words have to be counted.
#' @return Numeric. Number of words in the text.
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom utils read.table
#' @importFrom clipr read_clip
#' @export


count_words <- function(text = "") {
  if (!is.null(text) & !is.na(text)){
    if (text == "") {
      text <- clipr::read_clip()
    } else {
      text <- text
    }
    text <- unlist(purrr::map(text, stringr::str_split, pattern = " "))
    text <- unlist(text[text != ""])
    length(text)
  } else 0
}
