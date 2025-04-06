# Latest updated by Jack on 04062025
# Prepare the datasets for Google Colab and GitHub

setwd("/Users/fucabaturu/Downloads")

nodes <- readRDS("cleaned_nodes_R1.Rds")
edges <- readRDS("followers_edgelist_R1.Rds")


head(nodes)
str(nodes)

head(edges)
str(edges)

colnames(edges) <- c("source", "target")

colnames(nodes)[colnames(nodes) == "str_id"] <- "node_id"

table(nodes$party3)
table(nodes$party)


write.csv(edges, "follower_edges.csv", row.names = FALSE)
write.csv(nodes, "nodes.csv", row.names = FALSE)