library(readr)
library(tidyverse)

# asociaciones --------------------------------

estimulos_todos <- read_csv("E:/r/bigdata-rrss/data/estimulos_todos.csv")
terminos_todos <- read_csv("E:/r/bigdata-rrss/data/terminos_todos.csv")
terminos_todos <- terminos_todos %>% inner_join(estimulos_todos, by="id")
terminos <- terminos_todos %>% filter(
  estimulo == "Big data"
) %>% select(-tiempo,-estimulo)
rm(estimulos_todos, terminos_todos)

udmodel <- udpipe::udpipe_load_model(file = "e:/r/dix/spanish-gsd-ud-2.4-190531.udpipe")

terminos$id_orden <- paste(terminos$id,terminos$orden, sep = "$") # ponemos un id para las palabras
terminos$palabra <- tolower(trimws(terminos$palabra)) # minusculas
#terminos$palabra <- sub(pattern = "rse$", replacement = "r", x = terminos$palabra) # borramos la declinacion "rse"
terminos_tagged <- udpipe::udpipe_annotate(udmodel,
                                           x = terminos$palabra,
                                           doc_id = terminos$id_orden)
terminos_tagged <- as.data.frame(terminos_tagged)
terminos_tagged$lemma <- stringi::stri_trans_general(terminos_tagged$lemma, "Latin-ASCII") # sacamos acentos
terminos_tagged$lemma <- tolower(terminos_tagged$lemma)
terminos_tagged$lemma <- gsub(pattern = "gran_", replacement = "grande_",
                              x = terminos_tagged$lemma, fixed = TRUE)

lemmas <- terminos_tagged %>%
  filter(!is.na(upos), !upos %in% c("PUNCT","ADP") ) %>%
  group_by(doc_id) %>%
  summarize(lemma=paste(lemma, collapse = "_")) %>%
  rename(id_orden=doc_id)

terminos <- terminos %>% left_join( lemmas, by = "id_orden" )

glimpse(terminos)

terminos2 <- terminos %>%
  select(id,palabra,orden,valoracion) %>%
  mutate(valoracion=valoracion-5)


terminos2 %>% write.csv(file = "data/asociaciones.csv", row.names = FALSE, fileEncoding = "utf-8")


# biblio --------------------------------


articulos <- readr::read_csv(file = "./data/biblio.csv") %>%
  select(
    autores = Authors,
    titulo = Title,
    anio = year,
    claves = `Author Keywords`,

  )
