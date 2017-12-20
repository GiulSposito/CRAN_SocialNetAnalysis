### 
# 1. SET UP SESSION
###

library(igraph)
library(tidygraph)
library(NetData)

### 
# 2. LOAD DATA
###

# We would ordinarily need to follow the same proceedure we did for the Krackhardt data
# as we did in lab 1; see that lab for detail.

data(kracknets, package = "NetData")

# compose nodes df
nodes <- cbind( data.frame(id = 1:nrow(attributes)),
                attributes)

krack_full <- tbl_graph(nodes=nodes, edges = krack_full_data_frame, directed = T) %>%
  activate("edges") %>%
  filter( advice_tie != 0 | friendship_tie != 0 | reports_to_tie != 0 ) %>%
  activate("nodes") %>%
  mutate( DEPT = as.factor(DEPT) ) %>%
  activate("edges") 

# Create sub-graphs based on edge attributes
krack_advice <- krack_full %>%
  filter( advice_tie == 1)

krack_friendship <- krack_full %>%
  filter( friendship_tie == 1)

krack_reports_to <- krack_full %>%
  filter( reports_to_tie == 1)

### 
# 3. NODE-LEVEL STATISTICS
###

calcMetrics <- function(g){
  g %>%
    activate("nodes") %>%
    mutate( degree.in  = centrality_degree(mode = "in"),
            degree.out = centrality_degree(mode = "out"),
            reach.in   = centrality_closeness(mode = "in"),
            reach.out  = centrality_closeness(mode = "in")) %>%
    return()
}

krack_full       <- calcMetrics(krack_full)
krack_advice     <- calcMetrics(krack_advice)
krack_friendship <- calcMetrics(krack_friendship)
krack_reports_to <- calcMetrics(krack_reports_to)



