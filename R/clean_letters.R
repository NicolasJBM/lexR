#' @name clean_letters
#' @title Simplify string
#' @author Nicolas Mangin
#' @description Simplify the text to only lower case letters
#' @param text  Character string. String to be simplified
#' @return A simplified string.
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove_all
#' @export

clean_letters <- function(text) {
  text %>%
    tolower() %>%
    stringr::str_remove_all("[,:!;?]") %>%
    stringr::str_remove_all("[0-9]") %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_remove_all(lexR::dat_symbols) %>%
    stringr::str_remove_all("'") %>%
    stringr::str_remove_all('"') %>%
    stringr::str_remove_all("\\(") %>%
    stringr::str_remove_all("\\)") %>%
    stringr::str_remove_all("\\[") %>%
    stringr::str_remove_all("\\]") %>%
    stringr::str_remove_all("\\{") %>%
    stringr::str_remove_all("\\}")
}
