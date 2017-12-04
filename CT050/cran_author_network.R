library(needs)
needs(data.table)
needs(tidyverse) # manipulacao de dados via dplyr, tibbles, magrittr, etc..,
needs(stringr)   # manipulacao de strings
needs(igraph)    # manipulacao de grafos
needs(tidygraph) # visualizacoes de redes
needs(ggraph)    # visualizacoes de redes

get_CRAN_network <- function(){
  
  # dados de publicacao de pacotes
  pdb <- tools::CRAN_package_db()
  
  # vamos obter os autors
  pbaut <- pdb$Author
  
  # limpando e tratando campo de autores
  aut <- pbaut %>%
    str_replace_all("\\(([^)]+)\\)", "") %>% # remocao 
    str_replace_all("\\[([^]]+)\\]", "") %>% # remocao
    str_replace_all("<([^>]+)>", "") %>% # remocao
    str_replace_all("\n", " ") %>% # remocao
    str_replace_all("[Cc]ontribution.* from|[Cc]ontribution.* by|[Cc]ontributors", " ") %>%
    str_replace_all("\\(|\\)|\\[|\\]", " ") %>% # remocao
    iconv(to = "ASCII//TRANSLIT") %>% # limpeza dos caracters especiais
    str_replace_all("'$|^'", "") %>%  # limpeza
    gsub("([A-Z])([A-Z]{1,})", "\\1\\L\\2", ., perl = TRUE) %>% 
    gsub("\\b([A-Z]{1}) \\b", "\\1\\. ", .) %>%
    map(str_split, ",|;|&| \\. |--|(?<=[a-z])\\.| [Aa]nd | [Ww]ith | [Bb]y ", simplify = TRUE) %>% 
    map(str_replace_all, "[[:space:]]+", " ") %>% 
    map(str_replace_all, " $|^ | \\.", "") %>% 
    map(function(x) x[str_length(x) != 0]) %>%
    set_names(pdb$Package) %>% 
    magrittr::extract(map_lgl(., function(x) length(x) > 1))
  
  # conta autores por pacote
  aut_list <- aut %>%
    unlist() %>%
    dplyr::as_data_frame() %>%
    count(value) %>%
    rename(name = value, packages = n)
  
  # transforma a lista "pacote" -> [autores] em uma edge list
  edge_list <- aut %>%
    map(combn, m = 2) %>%    # em cada pacote (map) gera uma combinacao do array de autores dois a dois
    do.call("cbind", .) %>%  
    t() %>%
    dplyr::as_data_frame() %>%
    arrange(V1, V2) %>%
    count(V1, V2) 
  
  g <- edge_list %>%
    select(V1, V2) %>%
    as.matrix() %>%
    graph.edgelist(directed = FALSE) %>%
    as_tbl_graph() %>%  # wrapper tidygraph para o objeto igraph
    activate("edges") %>% # tbl graph é duas linked table (edge e nodes) activate diz o que sera manipulado
    mutate(weight = edge_list$n)
    left_join(aut_list, by="name")
  
  return(g)
}
