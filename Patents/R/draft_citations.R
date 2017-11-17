library(patentsview)
library(tidyverse)
library(igraph)    # manipulacao de grafos
library(tidygraph) # visualizacoes de redes
library(ggraph)    # visualizacoes de redes

# show field with "like" text in name
showFields <- function(like){
  fieldsdf %>%
    filter( grepl(like, field, fixed = TRUE) ) %>%
    select( -endpoint, -can_query, -common_name ) %>%
    distinct() %>%
    arrange( group, field)  %>%
    View()
}


keywords <- c("machine learning","deep learning", "neural network","artificial intelligence",
              "statistical learning", "data mining", "predictive model")

fields = c("patent_id","patent_title", "patent_abstract", "patent_num_us_application_citations",
           "inventor_id", "inventor_first_name", "inventor_total_num_patents",
           "assignee_id", "assignee_organization", "assignee_type",
           "uspc_mainclass_id", "uspc_mainclass_title", "uspc_subclass_id", "uspc_subclass_title")


qry <- qry_funs$and(
  qry_funs$gte(patent_year=2015),  # patents do ano 1990 para frente (Year patent was granted)
  qry_funs$or(
    qry_funs$text_phrase(patent_title=keywords),   # com (pelo menos) uma das keywords no titulo
    qry_funs$text_phrase(patent_abstract=keywords) # com (pelo menos) uma das keywords no abstract
  )
)


res <- search_pv(query = qry, fields = fields, all_pages = T)
res

# desaninha a estrutura de resposta

patents <- res$data$patents %>%
  select( patent_id, patent_title, patent_abstract, patent_num_us_application_citations) %>%
  distinct() %>%
  mutate( node_id = 1:nrow(.) )

inventors <- res$data$patents %>%
  select( inventors ) %>%
  tidyr::unnest(inventors) %>%
  distinct() %>%
  mutate( node_id = nrow(patents) + (1:nrow(.)) )
  
pat.inv <- res$data$patents %>%
  select( patent_id, inventors ) %>%
  tidyr::unnest(inventors) %>%
  select( patent_id, inventor_id ) %>%
  distinct()

assignees <- res$data$patents %>%
  select( assignees ) %>%
  tidyr::unnest(assignees) %>%
  distinct() %>%
  mutate( node_id = (nrow(patents)+nrow(inventors)) + (1:nrow(.)))

asg.pat <- res$data$patents %>%
  select( patent_id, assignees ) %>%
  tidyr::unnest(assignees) %>%
  select( patent_id, assignee_id ) %>%
  distinct() 

category <- res$data$patents %>% 
  select( patent_id, uspcs ) %>%
  tidyr::unnest(uspcs) %>%
  distinct()

## criacao dos vertices (nodes)

pat.nodes <- patents %>%
  select( node_id, patent_id, patent_title ) %>%
  rename( id = patent_id, label = patent_title ) %>%
  mutate( type = "p" ) %>%
  filter( !is.na(id) ) 

inv.nodes <- inventors %>%
  select( node_id, inventor_id, inventor_first_name ) %>%
  rename( id = inventor_id, label = inventor_first_name ) %>%
  mutate( type = "i" ) %>%
  filter( !is.na(id) ) %>%
  distinct()

asg.nodes <- assignees %>%
  select( node_id, assignee_id, assignee_organization ) %>%
  rename( id = assignee_id, label = assignee_organization ) %>%
  mutate( type = "a" ) %>%
  filter( !is.na(id) ) %>%
  distinct()

nodes <- bind_rows( pat.nodes, inv.nodes, asg.nodes )


## criacao da edgelist

pat.asg.edges <- asg.pat %>%
  inner_join(patents, by="patent_id") %>%
  select(node_id, patent_id, assignee_id) %>%
  rename(to_id=node_id) %>%
  inner_join(assignees, by="assignee_id" ) %>%
  select(node_id, to_id, assignee_id, patent_id ) %>%
  rename(from_id = node_id)

pat.inv.edges <- pat.inv %>%
  inner_join(patents, by="patent_id") %>%
  select(node_id, patent_id, inventor_id) %>%
  rename(to_id=node_id) %>%
  inner_join(inventors, by="inventor_id" ) %>%
  select(node_id, to_id, inventor_id, patent_id ) %>%
  rename(from_id = node_id)

edges <- bind_rows( pat.inv.edges[, 1:2], pat.asg.edges[, 1:2] )
edges <- edges[ !(edges$from_id==56 | edges$to_id==56), ] # descobrir onde foi o no 56!


## Criacao da rede

net <- edges %>%
  as.matrix() %>%
  graph.edgelist(directed = FALSE) %>% 
  as_tbl_graph() %>%  # wrapper tidygraph para o objeto igraph
  activate("nodes") %>%
  mutate( Name = c(nodes$label,"?"))

ggraph(net, layout="lgl") +
  geom_edge_fan(alpha=0.1)+
  theme_graph()


library(network)
net <- network(edges, vertex.attr = nodes, matrix.type = "edgelist")
net
plot(net, vertex.cex=3, mode = "circle")


detach(package:network)
rm(net)
library(igraph)

gnet <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot(net, edge.arrow.size = 0.2)
plot(net, layout = layout_with_graphopt, edge.arrow.size = 0.2)


library(tidygraph)
library(ggraph)

net <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
net <- as_tbl_graph(gnet)


net %>% activate("nodes") %>% View()

ggraph(net, layout = "graphopt") + geom_edge_link() + geom_node_point() + theme_graph()


nrow(nodes)
gorder(gr)
