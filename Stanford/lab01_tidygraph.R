###
# lab 01, versao tidygraph
###

library(igraph)
library(ggraph)
library(tidygraph)

# edge lists
advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
friendship_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Friendship.txt')
reports_to_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-ReportsTo.txt')

# node attributes
attributes <- read.csv('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-Attributes.csv', header=T)

# compose nodes
nodes <- cbind( data.frame(id = 1:nrow(attributes)),
                attributes)
# compose edges
edges <- data.frame(
  from = advice_data_frame$V1,
  to   = advice_data_frame$V2,
  advices_to     = advice_data_frame$V3,
  friendship_to = friendship_data_frame$V3,
  reports_to    = reports_to_data_frame$V3
)

g <- tbl_graph(nodes=nodes, edges = edges, directed = T) %>%
  activate("edges") %>%
  filter( advices_to != 0 | friendship_to != 0 | reports_to != 0 ) %>%
  activate("nodes") %>%
  mutate( DEPT = as.factor(DEPT) )

# plotting
arw <- arrow(type="closed", angle=10, length = unit(5, "mm"))

# all
ggraph(g) + 
  geom_node_point( aes(size=TENURE, color=DEPT) ) + 
  geom_edge_fan(alpha=0.2, arrow = arw) +
  theme_void()

# report
g %>%
  activate("edges") %>%
  filter(reports_to == 1) %>%
  ggraph() + 
  geom_node_point( aes(size=TENURE, color=DEPT) ) + 
  geom_edge_fan(alpha=0.2, arrow = arw) +
  theme_void()

saveRDS(g,"./Stanford/g_net.rds")
