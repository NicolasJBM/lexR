#' @name clean_spaces
#' @title Clean spaces
#' @author Nicolas Mangin
#' @description Clean spaces around punctuation, dashes, and parentheses.
#' @param text Character string. String to be cleaned.
#' @return A clean string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tm stripWhitespace
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr fixed
#' @importFrom stringi stri_trans_general
#' @importFrom textclean replace_non_ascii
#' @export

clean_spaces <- function(text) {
  stopifnot(
    is.character(text),
    length(text) == 1
  )

  # Remove unnecessary spaces
  # Ensure proper punctuation (e.g. no space before, space after)
  clean <- text %>%
    stringr::str_replace_all("[\\s]{2,}", " ") %>%
    as.character() %>%
    trimws() %>%
    stringr::str_replace_all("\\.([A-Za-z])", ". \\1") %>%
    stringr::str_replace_all("\\;([A-Za-z])", "; \\1") %>%
    stringr::str_replace_all("\\,([A-Za-z])", ", \\1") %>%
    stringr::str_replace_all("\\:([A-Za-z])", ": \\1") %>%
    stringr::str_replace_all("\\!([A-Za-z])", "! \\1") %>%
    stringr::str_replace_all("\\?([A-Za-z])", "? \\1") %>%
    stringr::str_replace_all(stringr::fixed(" -"), "-") %>%
    stringr::str_replace_all("([A-Za-z])\\(", "\\1 \\(") %>%
    stringr::str_replace_all("\\)([A-Za-z])", "\\) \\1") %>%
    stringr::str_replace_all("([A-Za-z])\\(", "\\1 \\(") %>%
    stringr::str_replace_all("\\)([A-Za-z])", "\\) \\1") %>%
    stringr::str_replace_all("([A-Za-z])\\[", "\\1 \\[") %>%
    stringr::str_replace_all("\\]([A-Za-z])", "\\] \\1") %>%
    stringr::str_replace_all("([A-Za-z])\\{", "\\1 \\{") %>%
    stringr::str_replace_all("\\}([A-Za-z])", "\\} \\1") %>%
    stringr::str_replace_all(stringr::fixed(" ."), stringr::fixed(".")) %>%
    stringr::str_replace_all(stringr::fixed(" ;"), stringr::fixed(";")) %>%
    stringr::str_replace_all(stringr::fixed(" ,"), stringr::fixed(",")) %>%
    stringr::str_replace_all(stringr::fixed(" :"), stringr::fixed(":")) %>%
    stringr::str_replace_all(stringr::fixed(" !"), stringr::fixed("!")) %>%
    stringr::str_replace_all(stringr::fixed(" ?"), stringr::fixed("?")) %>%
    stringr::str_replace_all(stringr::fixed("- "), "-") %>%
    stringr::str_replace_all(stringr::fixed("( "), "(") %>%
    stringr::str_replace_all(stringr::fixed(" )"), ")") %>%
    stringr::str_replace_all(stringr::fixed("[ "), "[") %>%
    stringr::str_replace_all(stringr::fixed(" ]"), "]") %>%
    stringr::str_replace_all(stringr::fixed("{ "), "{") %>%
    stringr::str_replace_all(stringr::fixed(" }"), "}") %>%
    as.character()

  return(clean)
}
