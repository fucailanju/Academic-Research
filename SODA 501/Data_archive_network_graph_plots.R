rm(list = ls())
library(igraph)
library(ggraph)
library(tidygraph)

# ------------------- Load Files --------------------------------------
# Load edgelists 
follower_edges <- readRDS("processed_data/followers_edgelist_R1.Rds")
mentions_edges <- readRDS("processed_data/mentions_edgelist_R1.Rds")
rt_edges <- readRDS("processed_data/rt_edgelist_R1.Rds")

# Load Node list 
nodes <- readRDS("processed_data/cleaned_nodes_R1.Rds")
#----------------------------------------------------------------------

# Create networks using igraph for the follower, mentions and retweets network 
g_follower <- graph_from_data_frame(d = follower_edges, vertices = nodes, directed = TRUE)
g_mention <- graph_from_data_frame(d = mentions_edges, vertices = nodes, directed = TRUE)
g_rt <- graph_from_data_frame(d = rt_edges, vertices = nodes, directed = TRUE)

# Assign indegree/outdegree as a node attribute 
V(g_follower)$indegree <- degree(g_follower, mode = 'in', loops = FALSE)
V(g_mention)$indegree <- degree(g_mention, mode = 'in', loops = FALSE)
V(g_rt)$indegree <- degree(g_rt, mode = 'in', loops = FALSE)
#V(g_rt)$outdegree <- degree(g_rt, mode = 'out',  loops = FALSE)

# Network - In degree 
nodes$follower_indegree <- V(g_follower)$indegree
nodes$mention_indegree <- V(g_mention)$indegree
nodes$rt_indegree <- V(g_rt)$indegree

# Assign node's label  
# find the top 5 most central nodes within each state 
# & create labels of them
top_5_follower <- nodes %>% group_by(state) %>% top_n(5, wt=follower_indegree) %>% mutate(follower_labels = state.abb)
top_5_mention <- nodes %>% group_by(state) %>% top_n(5, wt=mention_indegree) %>% mutate(mention_labels = state.abb)
top_5_rt <- nodes %>% group_by(state) %>% top_n(5, wt=rt_indegree) %>% mutate(rt_labels = state.abb)

# Assign it as a node attributes in g
nodes <- nodes %>% 
  left_join(top_5_follower[c('str_id','follower_labels')], by='str_id')%>% 
  left_join(top_5_mention[c('str_id','mention_labels')], by='str_id')%>% 
  left_join(top_5_rt[c('str_id','rt_labels')], by='str_id')

V(g_rt)$labels <- as.character(nodes$labels)

# Covert to a tidy graph
# Create networks using igraph for the follower, mentions and retweets network 
g_follower <- graph_from_data_frame(d = follower_edges, vertices = nodes, directed = TRUE)
g_mention <- graph_from_data_frame(d = mentions_edges, vertices = nodes, directed = TRUE)
g_rt <- graph_from_data_frame(d = rt_edges, vertices = nodes, directed = TRUE)

g_follower_tidy <-  as_tbl_graph(g_follower)
g_mention_tidy <-  as_tbl_graph(g_mention)
g_rt_tidy <-  as_tbl_graph(g_rt)

# Plot the network 
# Convert to a tidy graph
# Choose nodes that have centrality greater than 10 
# Remove isolates
g_follower_tidy <- g_follower_tidy %>% activate(nodes) %>% filter(follower_indegree >2) %>% filter(!node_is_isolated())  
g_mention_tidy <- g_mention_tidy %>% activate(nodes) %>% filter(mention_indegree >2) %>% filter(!node_is_isolated())  
g_rt_tidy <- g_rt_tidy %>% activate(nodes) %>% filter(rt_indegree >1) %>% filter(!node_is_isolated())  

# Plot the network
#set.seed(1006)
set.seed(10)
# Assign vertical and horizontal position of the nodes 
follower_layout <- create_layout(g_follower_tidy, layout = 'igraph', algorithm = 'fr')
mention_layout <- create_layout(g_mention_tidy, layout = 'igraph', algorithm = 'fr')
rt_layout <- create_layout(g_rt_tidy, layout = 'igraph', algorithm = 'fr')

######################## Followers net ############################
p_follower <- ggraph(follower_layout) +
  geom_edge_link(alpha=0.009) +
  geom_node_point(aes(color = party,
                      shape = chamber,
                      alpha = follower_indegree/max(follower_indegree),
                      size = (follower_indegree/max(follower_indegree))-2)) +
  scale_color_manual("party",
                     values = c(D = "dodgerblue",
                                R = "firebrick2",
                                I = 'yellow'
                                ))+
  geom_node_text(aes(label = follower_labels,
                     size = (follower_indegree/max(follower_indegree))-3)) +
  theme_graph(base_family = 'Helvetica')

ggsave(p_follower, height = 10, width = 12, file = "plots/followers_net.pdf")


############### Mentions net #####################################
p_mention <- ggraph(mention_layout) +
  geom_edge_link(alpha=0.01,) +
  geom_node_point(aes(color = party3,
                      shape = chamber,
                      alpha = mention_indegree/max(mention_indegree)+.3,
                      size = (mention_indegree/max(mention_indegree)))) +
  scale_color_manual("party3",
                     values = c(D = "dodgerblue",
                                R = "firebrick2",
                                I = 'yellow'
                                ))+
  geom_node_text(aes(label = mention_labels,
                     size = (mention_indegree/max(mention_indegree))-.3)) +
  theme_graph(base_family = 'Helvetica')

ggsave(p_mention, height = 10, width = 12, file = "plots/mentions_net.pdf")

############## Retweets net ######################################
p_rt <- ggraph(rt_layout) +
  geom_edge_link(alpha=0.2,) +
  geom_node_point(aes(color = party3,
                      shape = chamber,
                      #alpha = 1.5,
                      size = (rt_indegree/max(rt_indegree))+.1)) +
  scale_color_manual("party3",
                     values = c(D = "dodgerblue",
                                R = "firebrick2",
                                I = 'yellow'
                     ))+
  geom_node_text(aes(label = rt_labels,
                     size = (rt_indegree/max(rt_indegree)))) +
  theme_graph(base_family = 'Helvetica')

ggsave(p_rt, height = 10, width = 12, file = "plots/retweets_net.pdf")





