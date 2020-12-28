dat_en_lemmas <- readxl::read_excel("inst/script/dat_en_lemmas.xlsx")

dat_en_lemmas <- dat_en_lemmas %>%
  dplyr::mutate(
    ascii1 = stringi::stri_enc_mark(word),
    ascii2 = stringi::stri_enc_mark(lemma),
    ascii3 = stringi::stri_enc_mark(term)
  ) %>%
  dplyr::filter(ascii1 == "ASCII", ascii2 == "ASCII", ascii3 == "ASCII") %>%
  dplyr::select(word, lemma, term) %>%
  dplyr::mutate(word = iconv(word, to="ASCII//TRANSLIT"),
         lemma = iconv(lemma, to="ASCII//TRANSLIT"),
         term = iconv(term, to="ASCII//TRANSLIT")
  )

add_term <- tibble::tibble(
  word = unique(dat_en_lemmas$term)
) %>%
  dplyr::mutate(lemma = word, term = word)

dat_en_lemmas <- dat_en_lemmas %>%
  dplyr::bind_rows(add_term) %>%
  dplyr::mutate(
    difference = stringdist::stringdist(word, term),
    characters = nchar(term)
  ) %>%
  dplyr::group_by(word) %>%
  tidyr::nest() %>%
  dplyr::mutate(data = purrr::map(data, filter, difference == min(difference))) %>%
  dplyr::mutate(data = purrr::map(data, filter, characters == max(characters))) %>%
  dplyr::mutate(data = purrr::map(data, sample_n, 1)) %>%
  tidyr::unnest(data) %>%
  dplyr::ungroup() %>%
  dplyr::select(-characters) %>%
  unique()


save(dat_en_lemmas, file = "data/dat_en_lemmas.RData")
tools::resaveRdaFiles("data/dat_en_lemmas.RData")


dat_toascii <- data.frame(
  mapL = c("[á]","[é]","[í]","[ó]","[ú]","[Á]","[É]","[Í]","[Ó]","[Ú]","[ñ]","[Ñ]","[ü]","[Ü]","[ç]","[ä]","[Ä]","[ë]","[Ë]","[ï]","[Ï]","[ö]","[Ö]","[ü]","[Ü]","[ÿ]","[Ÿ]","[â]","[Â]","[ê]","[Ê]","[î]","[Î]","[ô]","[Ô]","[û]","[Û]","[à]","[À]","[è]","[È]","[ì]","[Ì]","[ò]","[Ò]","[ù]","[Ù]","[È]"),
  mapA = c("a","e","i","o","u","A","E","I","O","U","n","N","u","U","c","a","A","e","E","i","I","o","O","u","U","y","Y","a","A","e","E","i","I","o","O","u","U","a","A","e","E","i","I","o","O","u","U","E")
)

dat_toascii <- dplyr::mutate(dat_toascii, mapL = as.character(mapL), mapA = as.character(mapA))
dat_dictionaries <- read.csv("inst/script/dat_dictionaries.csv")
save(dat_dictionaries, file = "data/dat_dictionaries.RData")


dat_symbols <- "[~#\\(\\)\\\\@/%$£€&^]"
save(dat_toascii, file = "data/dat_toascii.RData")
save(dat_symbols, file = "data/dat_symbols.RData")
