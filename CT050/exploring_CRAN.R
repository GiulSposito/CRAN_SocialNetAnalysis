library(needs)
needs(stringr)
needs(tidyverse)
needs(igraph)    # manipulacao de grafos
needs(tidygraph) # visualizacoes de redes
needs(ggraph)    # visualizacoes de redes

pdb <- tools::CRAN_package_db()
saveRDS(pdb,"./CT050/pdb.rds")
pdb <- readRDS("./CT050/pdb.rds")

pbaut <- pdb$Author


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
  rename(Name = value, Package = n)

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
  mutate(Weight = edge_list$n) %>% # adiciona o peso nas arestas de acordo com o numero de vezes que os autores colabora
  activate("nodes") %>%    # manipulara os nos agora
  rename(Name = name) %>% # nomeia os nos conforme
  mutate(Component = group_components()) %>%
  filter(Component == names(table(Component))[which.max(table(Component))])

# ggraph(g, layout="lgl") +
#   geom_edge_fan(alpha=0.1)+
#   theme_graph()


g <- g %>%
  left_join(aut_list, by="Name") %>%
  filter(Package > 5) %>% # pelo menos 4 pacotes
  mutate(Component = group_components()) %>%
  filter(Component == names(table(Component))[which.max(table(Component))])

ggraph(g, layout = 'lgl') +
  geom_edge_fan(alpha = 0.1) +
  theme_graph()


g <- mutate(g, Community = group_edge_betweenness(),
            Degree = centrality_degree())

filter(g, Community == names(sort(table(Community), decr = TRUE))[1]) %>%
  select(Name, Package) %>%
  arrange(desc(Package)) %>%
  top_n(10, Package) %>%
  as_tibble() %>%
  knitr::kable(format = "html", caption = "Cluster 1")

filter(g, Community == names(sort(table(Community), decr = TRUE))[2]) %>%
  select(Name, Package) %>%
  arrange(desc(Package)) %>%
  top_n(10, Package) %>%
  as_tibble() %>%
  knitr::kable(format = "html", caption = "Cluster 2")

filter(g, Community == names(sort(table(Community), decr = TRUE))[3]) %>%
  select(Name, Package) %>%
  arrange(desc(Package)) %>%
  top_n(10, Package) %>%
  as_tibble() %>%
  knitr::kable(format = "html", caption = "Cluster 2")

g <- g %>%
  mutate(Community = case_when(Community == names(sort(table(Community),
                                                       decr = TRUE))[1] ~ "The Ancients",
                               Community == names(sort(table(Community),
                                                       decr = TRUE))[2] ~ "The Moderns",
                               Community == names(sort(table(Community),
                                                       decr = TRUE))[3] ~ "Suicide Squad",
                               Community == names(sort(table(Community),
                                                       decr = TRUE))[4] ~ "The Immortals",
                               Community %in% names(sort(table(Community),
                                                         decr = TRUE))[-1:-4] ~ "Unclassified")) %>%
  mutate(Community = factor(Community))


g <- g %>%
  filter(Degree > 5) %>%
  mutate(Degree = centrality_degree())


ggraph(g, layout = 'lgl') +
  geom_edge_fan(alpha = 0.1) +
  geom_node_point(aes(color = Community, size = Package)) +
  theme_graph() +
  scale_color_manual(breaks = c("The Ancients", "The Moderns", "Suicide Squad", "The Immortals"),
                     values=c("#F8766D", "#00BFC4", "#969696", "#FF0000", "#00FF00"))
