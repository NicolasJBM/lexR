#' @name clean_paragraphs
#' @title Recompose paragraphs by identifying improper breaks.
#' @author Nicolas Mangin
#' @description Identify linebreaks which do not seem to match an end of paragraph and recompose the paragraph.
#' @param texts  Character vector. A vector of lines of text to be recomposed into paragraphs.
#' @return A character vector in which paragraphs (ending with "." and with an empty line after) are recomposed.
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom tidyr replace_na
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom purrr map
#' @export

clean_paragraphs <- function(texts){
  
  stopifnot(
    is.character(texts),
    length(texts > 1)
  )
  
  # Bind variables for dplyr
  texts2 <- NULL
  endparagraph <- NULL
  paragraph <- NULL
  
  # Concatenate lines which do not end with a full stop with the next ones
  y <- tibble::tibble(texts = texts) %>%
    dplyr::mutate(texts2 = dplyr::lead(texts)) %>%
    dplyr::mutate(endparagraph = endsWith(texts,".") & texts2 == "") %>%
    dplyr::mutate(paragraph = cumsum(endparagraph)) %>%
    dplyr::select(-texts2, -endparagraph) %>%
    dplyr::mutate(paragraph = dplyr::lag(paragraph)) %>%
    tidyr::replace_na(list(paragraph = 0)) %>%
    dplyr::group_by(paragraph) %>%
    dplyr::summarise(texts = paste(texts, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(texts) %>%
    dplyr::mutate(texts = purrr::map(texts, trimws))
  
  unlist(y$texts)
}
