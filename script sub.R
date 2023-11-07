# packages ----------------------------------------------------------------
library(tidyverse)
library(igraph)

# dataset loading ---------------------------------------------------------
data=read_csv2("bridgerton_network.csv")

#Cela vous donnera un aperçu visuel des interactions entre les personnes. De là, vous pouvez continuer à analyser le réseau (centralité, clusters, etc.), comme mentionné dans la réponse précédente.
data=data %>%
  filter(Perso1 %in% c("Anthony", "Kate", "Edwina","Benedicte"), 
         Perso2 %in% c("Anthony", "Kate", "Edwina","Benedicte")) 

colnames(data) <- c("source", "target", "weight")
(data=data %>% filter(weight!=0))

net <- graph.data.frame(data, directed=F)
plot(net)

net <- graph.data.frame(data, directed=T)
plot(net)

net <- graph.data.frame(data, directed=T)
E(net)$width <- data$weight
plot(net,edge.width=E(net)$width)

net <- graph.data.frame(data, directed=T)
E(net)$width <- data$weight
plot(net, vertex.color="deeppink", vertex.size=20, vertex.label.dist=3, edge.width=E(net)$width,edge.curved=0.2)

V(net)
# net <- set_vertex_attr(net, 
#                        "family", 
#                        value = c("Bridgerton","Bridgerton","Charma","Charma"))
(V(net)$color <- c("mediumspringgreen","mediumspringgreen","cyan3","cyan3"))
plot(net, vertex.size=20, vertex.label.dist=3, edge.width=E(net)$width,edge.curved=0.2,vertex.color=V(net)$color)


# degree ------------------------------------------------------------------
(degree_centrality <- degree(net))
(strength <- strength(net, mode="all", weights=E(net)$weight))
(in_s <- strength(net, mode="in", weights=E(net)$weight))
(out_s <- strength(net, mode="out", weights=E(net)$weight))

# closeness ---------------------------------------------------------------
# (closeness_centrality <- closeness(net))
# (closeness_centrality <- closeness(net,mod="all"))
# (closeness_centrality <- closeness(net,mod="in"))
# (closeness_centrality <- closeness(net,mod="out"))
#         #anthony
#         1/(1+1+2)
        
#Avec les poids, la proximité est généralement calculée en prenant en compte la distance inversée (c'est-à-dire que les arêtes à poids élevé ont une distance plus courte).
(closeness_centrality <- closeness(net, mode="all", weights=1/E(net)$weight))
        #anthony
        1/(0.25+0.5+0.75)#les plus courts chemin pour chaque noeud (entrant ou sortant)
        #edwina
        1/(0.25+1+1.25)
        
(closeness_centrality <- closeness(net, mode="out", weights=1/E(net)$weight))
        #anthony
        1/(0.3333+0.5+0.75)
        
(closeness_centrality <- closeness(net, mode="in", weights=1/E(net)$weight))
        #anthony
        1/(0.25+1+1.5)

# betweenness -------------------------------------------------------------
betweenness(net,directed=TRUE,weights=1/E(net)$weight)

# eigenvector -------------------------------------------------------------
(eigenvector_centrality <- eigen_centrality(net)$vector)
(eigenvector_centrality <- eigen_centrality(net, weights=E(net)$weight)$vector)

# pagerank ----------------------------------------------------------------
(pagerank_centrality <- page.rank(net)$vector)
(pagerank_centrality <- page.rank(net, directed=FALSE, weights=E(net)$weight)$vector)

# density -----------------------------------------------------------------
edge_density(net)
num_nodes=4
(num_links=nrow(data))
(possible_links <- num_nodes * (num_nodes - 1))
(density <- num_links / possible_links)

(num_links_weight <- sum(E(net)$weight))
#sum(data$weight)
num_nodes <- vcount(net)
possible_edges_weighted <- num_nodes * (num_nodes - 1)
(density_weighted <- total_weight / possible_edges_weighted)



# community detection -----------------------------------------------------
#set.seed(1234)
(cnet <- cluster_edge_betweenness(net))
plot(cnet,net)

# Détecter les communautés avec l'algorithme de Louvain
#sauf que fonctionne que si non  dirigé

#convertir le graphe dirigé en un graphe non dirigé  
net_undirected <- as.undirected(net, mode="collapse")

cnet <- cluster_louvain(net_undirected, weights=E(net_undirected)$weight)
(modularity_value <- modularity(cnet))

#Coloration des nœuds selon leur communauté
node_colors <- rainbow(max(membership(cnet)))
col_vector <- node_colors[membership(cnet)]

# Visualiser le graphe
plot(net_undirected, vertex.color=col_vector, main="Graph Network with Community Coloring")


# global ------------------------------------------------------------------
data=read_csv2("bridgerton_network.csv")
colnames(data) <- c("source", "target", "weight")
(data=data %>% filter(weight!=0))

net <- graph.data.frame(data, directed=T)
E(net)$width <- data$weight
plot(net, vertex.color="deeppink", vertex.size=20, vertex.label.dist=3, edge.width=E(net)$width,edge.curved=0.2)

