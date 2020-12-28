#' @name clean_ascii
#' @title Convert or force into ASCII.
#' @author Nicolas Mangin
#' @description Replaces non-ASCII charactes by ASCII equivalent and force ASCII encoding.
#' @param text Character string. String to be forced into ASCII.
#' @return A clean ASCII character string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringi stri_trans_general
#' @importFrom textclean replace_non_ascii
#' @export

clean_ascii <- function(text) {
  stopifnot(
    is.character(text),
    length(text) == 1
  )

  # Break into words
  text <- text %>%
    stringr::str_split(" ") %>%
    unlist()

  # Break words into characters and replace
  split <- strsplit(text, split = "")

  # Replace each character by its ASCII equivalent
  # then recompose the words
  m <- lapply(split, match, lexR::dat_toascii$mapL)
  Map(
    function(split, m) {
      paste(
        ifelse(
          is.na(m),
          split,
          lexR::dat_toascii$mapA[m]
        ),
        collapse = ""
      )
    },
    split, m
  )

  # Ensure that words are ASCII, or remove them
  text <- text %>%
    stringi::stri_trans_general(id = "ascii") %>%
    textclean::replace_non_ascii(replacement = "", remove.nonconverted = TRUE)

  # Recompose the text
  text <- paste(text, collapse = " ")

  trimws(text)
}
