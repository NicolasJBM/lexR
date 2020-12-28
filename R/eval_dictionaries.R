#' @name eval_dictionaries
#' @title Compute scores based on dictionaries' word counts
#' @author Nicolas Mangin
#' @description Count the occurrences of words associated with a dictionary.
#' @param bow        Tibble. Bag of words for one document with at least two variables: "term" and "count".
#' @param dictionary Tibble. Categorizations of words with at least three variables: "term", "category", and "value".
#' @param score      Character string. Whether the function should return a "total" or a "percentage".
#' @return A tibble with documents as rows, categories as variables, ad the required summary in cells
#' @seealso \code{\link{create_bow}}
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom tidyr spread
#' @importFrom tidyr replace_na
#' @importFrom stringr str_detect
#' @importFrom purrr map_dbl
#' @export


eval_dictionaries <- function(bow,
                              dictionary,
                              score = "total") {
  stopifnot(
    length(intersect(c("term", "count"), names(bow))) == 2,
    length(intersect(c("term", "category", "value"), names(dictionary))) == 3,
    score %in% c("total", "percentage")
  )

  # Bind variables for dplyr.
  term <- NULL
  category <- NULL
  value <- NULL
  scores <- NULL
  counts <- NULL

  # Select necessary variables and format them properly.
  bow <- dplyr::select(bow, term, count) %>%
    dplyr::mutate(term = as.character(term))

  # Compute the total number of words to compute percentages.
  nbr_words <- sum(bow$count)

  # Count the number of occurrence of each term of a dictionary and
  # sum this count at the requested dictionary level.
  dictionary <- dplyr::select(dictionary, term, category, value) %>%
    dplyr::mutate(term = as.character(term)) %>%
    dplyr::mutate(
      counts = purrr::map_dbl(
        term,
        function(x, y) {
          sum(dplyr::filter(y, stringr::str_detect(term, x))$count)
        },
        y = bow
      )
    ) %>%
    tidyr::replace_na(list(counts = 0)) %>%
    dplyr::mutate(scores = counts * value) %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(scores = sum(scores, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Transform into percentages id requested
  if (score == "percentage") {
    dictionary <- dplyr::mutate(dictionary, scores = scores / nbr_words)
  }

  result <- dictionary %>%
    tidyr::spread(category, scores, fill = 0)

  return(result)
}
