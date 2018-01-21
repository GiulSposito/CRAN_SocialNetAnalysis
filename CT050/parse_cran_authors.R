library(needs)
needs(data.table)
needs(stringr)   # limpeza e tratamento de strings
needs(tidyverse) # pipeline e manipulacao de dados
needs(igraph)    # manipulacao de grafos
needs(tidygraph) # visualizacoes de redes
needs(ggraph)    # visualizacoes de redes


# dados de publicacao de pacotes
pdb <- tools::CRAN_package_db()

# campo de autores dos pacotes
aut <- pdb$Author %>%
  iconv(to = "ASCII//TRANSLIT") %>%
  map(function(x) format(as.person(x),include = c("given", "family" ))) %>%
  set_names(pdb$Package)%>%
  map(function(v) keep(v,~.x!="")) %>%
  magrittr::extract(map_lgl(., function(x) length(x) > 1))

aut <- pdb_authors

x <- list(
  c("","Giul","JuVassallo", "JuSchober"),
  c("Giul","TaisDestro","","AliniPortela"),
  c("Giul", "Carol","Flavia","Michelle",""),
  c("")
)

str(x)

x %>% 
  map(keep(.x!=""))


1:10 %>%
  map(runif) %>%
  map(function(v) keep(v,~.x>0.5))

x 
  map(length)

?keep

runif(10) %>% keep(.,~.x>0.5)

rep(10, 10) %>%
  map(sample, 5) %>%
  keep(function(x) mean(x) > 6)
