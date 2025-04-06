rm(list = ls())
library(igraph)
library(tidygraph)
library(dplyr)
library(stargazer)


#################33 Function to load and prepare the network data ###################
load_and_prepare_network <- function(nodes_file, edgelist_file) {
  # Load nodes
  nodes <- readRDS(nodes_file)
  
  # Load the edgelist
  edges <- readRDS(edgelist_file)
  
  # Create network using igraph
  g <- graph_from_data_frame(d = edges, 
                             vertices = nodes, 
                             directed = TRUE)
  
  # Calculate in & out degree
  V(g)$indegree <- degree(g, mode = 'in', loops = FALSE)
  V(g)$outdegree <- degree(g, mode = 'out', loops = FALSE)
  
  # Convert to a tidy graph
  g_tidy <- as_tbl_graph(g)
  
  return(g_tidy)
}

########################## Function to get Top Legislators by Indegree Centrality ##########
get_top_indegree_legislators <- function(g_tidy, type, output_file) {
  nodes_tibble <- g_tidy %>% activate(nodes) %>% as_tibble()
  
  top_inDegree <- nodes_tibble %>%
    arrange(desc(indegree)) %>%
    select(name, state, party3, chamber) %>%
    head(10)
  
  stargazer::stargazer(top_inDegree, type = type, 
                       out = output_file, 
                       summary = FALSE,
                       row.names = FALSE)
}


#################### Function to get Top Legislators by Outdegree Centrality ################
get_top_outdegree_legislators <- function(g_tidy, type, output_file) {
  nodes_tibble <- g_tidy %>% activate(nodes) %>% as_tibble()
  
  top_outDegree <- nodes_tibble %>%
    arrange(desc(outdegree)) %>%
    select(name, state, party3, chamber) %>%
    head(10)
  
  stargazer::stargazer(top_outDegree, type = type, 
                       out = output_file, 
                       summary = FALSE,
                       row.names = FALSE)
}

###############################################################################################
################################   Apply the above functions  #################################
# --------------------------------------------------------------------------------------------#
# Get the top 10 legislators with the highest in and out degree in the follower network

nodes_file <- "processed_data/cleaned_nodes_R1.Rds"
edgelist_file <- "processed_data/followers_edgelist_R1.Rds"

# Apply the 1st function to prepare the data
g_tidy <- load_and_prepare_network(nodes_file, edgelist_file)

# Get top legislators by in degree centrality in the follower network
get_top_indegree_legislators(g_tidy, type = "latex", "tables/descriptive/followers_inDegree.tex")

# Get top legislators by outdegree centrality in the follower network
get_top_outdegree_legislators(g_tidy,  type = "latex", "tables/descriptive/followers_outDegree.tex")


# --------------------------------------------------------------------------------------------#
# Get the top 10 legislators with the highest in and out degree in the mentions network

nodes_file <- "processed_data/cleaned_nodes_R1.Rds"
edgelist_file <- "processed_data/mentions_edgelist_R1.Rds"

# Apply the 1st function to prepare the data
g_tidy <- load_and_prepare_network(nodes_file, edgelist_file)

# Get top legislators by in degree centrality in the mentions network
get_top_indegree_legislators(g_tidy, type="latex", output_file="tables/descriptive/mentions_inDegree.tex")

# Get top legislators by outdegree centrality in the mentions network
get_top_outdegree_legislators(g_tidy,  type="latex", output_file="tables/descriptive/mentions_outDegree.tex")
# --------------------------------------------------------------------------------------------#

# Get the top 10 legislators with the highest in and out degree in the retweets network

nodes_file <- "processed_data/cleaned_nodes_R1.Rds"
edgelist_file <- "processed_data/rt_edgelist_R1.Rds"

# Apply the 1st function to prepare the data
g_tidy <- load_and_prepare_network(nodes_file, edgelist_file)

# Get top legislators by in degree centrality in the retweets network
get_top_indegree_legislators(g_tidy, type="latex", output_file="tables/descriptive/retweets_inDegree.tex")

# Get top legislators by outdegree centrality in the retweets network
get_top_outdegree_legislators(g_tidy,  type="latex", output_file="tables/descriptive/retweets_outDegree.tex")

# --------------------------------------------------------------------------------------------#
###############################################################################################
