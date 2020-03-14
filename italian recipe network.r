#### Italian Recipe Network Graph ####

setwd("C:/Users/St. Cloud/Desktop/Portfolio")
## inspiration & help https://www.jessesadler.com/post/network-analysis-with-r/
## recipe & ingredient data from https://www.recipetineats.com/wp-content/uploads/2014/06/Authentic-Italian-Pasta-PDF21.pdf


library(igraph)
library(tidyverse)
library(tibble)

# import csv

data <- read.csv("Italian Recipe Ingredients List.csv")
data2 <- read.csv("Italian Recipe Network3.csv", stringsAsFactors = FALSE)
#data3 <- read.xlsx("Italian Recipe Network.xlsx")

## rename first column
colnames(data2)[1] <-"Recipe"

### transform from factors into characters
as.character(data2)


#### create a node & edge lists ####

#### NODE LIST ####
sources <- data2 %>%
  distinct(Recipe) %>%
  rename(label = Recipe)

destinations <- data2 %>%
  distinct(Ingredient) %>%
  rename(label = Ingredient)

#combine both sources and destinations
nodes <- full_join(sources, destinations, by = "label")

#add an id column to nodes

nodes <- nodes %>% rowid_to_column("id")

#### EDGE LIST ####

common_ingredients <- data2 %>%
  group_by(Recipe, Ingredient) %>%
  summarise(weight =n()) %>%
  ungroup()

# use left join to create 'from' and 'to' columns

edges <- common_ingredients %>%
  left_join(nodes, by = c("Recipe" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(nodes, by = c("Ingredient" = "label")) %>%
  rename(to = id)

## condense edges to only the columns we need (from, to, weight)

edges <- select(edges, from, to, weight)

##### PLOTTING NETWORKS ####

####plot networks using network() package ####
install.packages("network")
library(network)

ing.network <- network(edges, vertex.attrnames = nodes, matrix.type = "edgelist", ignore.eval=FALSE)

plot(ing.network, vertex.cex=3)

#### use igraph() for plotting ####
detach(package:network)
library(igraph)

ing.igraph <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)

# using the graphopt layout spreads out the points with more edges so they are more visible
plot(ing.igraph, layout = layout_with_graphopt)

#plot(ing.igraph, layout = layout_with_kk)

#### Using tidygraph() and ggraph() ####
library(tidygraph)
library(ggraph)

ingredients.tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# or convert out ing.igraph into at tbl_graph object

ing.igraph.tidy <- as_tbl_graph(ing.igraph)

# graph using ggraph
#basic graph
ggraph(ingredients.tidy) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

#more features graph - a bit messed up with the width in edge_link,
ggraph(ingredients.tidy, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width=weight), alpha=0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label=label), repel = TRUE) +
  labs(edge_width = "Ingredient Connections") +
  theme_graph()

#### add circularity to network graph ####
# still messes up due to width factor
ggraph(ingredients.tidy, layout = "linear") +
  geom_edge_arc(aes(width=weight), alpha=0.6) +
  scale_edge_width(range = c(0.2, 1)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Ingredient Connections") +
  theme_graph()

#### INTERACTIVE NETWORK GRAPHS using visNetwork & networkD3 ####
library(visNetwork)
library(networkD3)

#### visNetwork graph ####
ingredients.viz <- visNetwork(nodes, edges)

# create a width column to use 'weights' in visNetork
edges <- mutate(edges, width = weight/5 + 1)

htmlwidgets::saveWidget(ingredients.viz, "italyVisNet.html", selfcontained = TRUE, libdir = NULL)


#### networkD3 graph ####
# id lists in D3 must begin with a 0
nodes.D3 <- mutate(nodes, id = id - 1)
edges.D3 <- mutate(edges, from = from - 1, to = to - 1)



# use forceNetwork() function
italy.d3.net <- forceNetwork(Links = edges.D3, Nodes = nodes.D3, Nodesize = edges.D3$to, Source = "from", Target = "to",
             NodeID = "label", Group = "id", Value = "weight",
             opacity = 1, fontSize = 16, zoom = TRUE, opacityNoHover = TRUE, bounded = FALSE,
             radiusCalculation = JS("Math.sqrt(d.nodesize)+6", legend = TRUE))

htmlwidgets::saveWidget(italy.d3.net, "italyD3network.html", selfcontained = TRUE, libdir = NULL)

#### Sankey Diagram ####

italy.sankey.net <- sankeyNetwork(Links = edges.D3, Nodes = nodes.D3, Source = "from", Target = "to",
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Ingredient Connections")

htmlwidgets::saveWidget(italy.sankey.net, "italyD3sankey.html", selfcontained = TRUE, libdir = NULL)


#### Same graphs but now with reciprocity/double-links ####
# take the dataframe columns in *data2* into another df, flip them so that ingredients are in the first
# column and recipes are in the second - then row bind the new df w/ flipped columns to the original data2 df

#create new df
recip.data2 <- data2

#flip the columns 
recip.data2 <- recip.data2 %>%
  select(2:1, Recipe, everything())

#change column names so rowbind works
colnames(recip.data2)[1] <- "Recipe"
colnames(recip.data2)[2] <- "Ingredient"

#row bind
recip.data4 <- rbind(data2, recip.data2)

#### 2 create a node & edge lists ####

#### 2 NODE LIST ####
sources.recip <- recip.data4 %>%
  distinct(Recipe) %>%
  rename(label = Recipe)

destinations.recip <- recip.data4 %>%
  distinct(Ingredient) %>%
  rename(label = Ingredient)

#combine both sources and destinations
nodes.recip <- full_join(sources.recip, destinations.recip, by = "label")

#add an id column to nodes

nodes.recip <- nodes.recip %>% rowid_to_column("id")

#### 2 EDGE LIST ####

common_ingredients.recip <- recip.data4 %>%
  group_by(Recipe, Ingredient) %>%
  summarise(weight = n()) %>%
  ungroup() 

recip.data4 %>%
  group_by(Recipe, Ingredient) %>%
  mutate(weight = n()) %>%
  ungroup() 


# use left join to create 'from' and 'to' columns

edges.recip <- common_ingredients.recip %>%
  left_join(nodes.recip, by = c("Recipe" = "label")) %>%
  rename(from = id)

edges.recip <- edges.recip %>%
  left_join(nodes.recip, by = c("Ingredient" = "label")) %>%
  rename(to = id)

## condense edges to only the columns we need (from, to, weight)

edges.recip <- select(edges.recip, from, to, weight)


##### 2 PLOTTING NETWORKS ####

#### 2 plot networks using network() package ####
library(network)

ing.network.recip <- network(edges.recip, vertex.attrnames = nodes, matrix.type = "edgelist", ignore.eval=FALSE)

plot(ing.network.recip, vertex.cex=3)

#### 2 use igraph() for plotting ####
detach(package:network)
library(igraph)

ing.igraph.recip <- graph_from_data_frame(d=edges.recip, vertices=nodes.recip, directed=FALSE)

# using the graphopt layout spreads out the points with more edges so they are more visible
plot(ing.igraph.recip, layout = layout_with_graphopt)

#plot(ing.igraph, layout = layout_with_kk)

#### 2 Using tidygraph() and ggraph() ####
library(tidygraph)
library(ggraph)

ingredients.tidy.recip <- tbl_graph(nodes = nodes.recip, edges = edges.recip, directed = FALSE)

# or convert out ing.igraph into at tbl_graph object

ing.igraph.tidy.recip <- as_tbl_graph(ing.igraph.recip)

# graph using ggraph
#basic graph
ggraph(ingredients.tidy.recip) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

#more features graph - a bit messed up with the width in edge_link,
ggraph(ingredients.tidy.recip, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width=weight), alpha=0.8) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label=label), repel = TRUE) +
  labs(edge_width = "Ingredient Connections") +
  theme_graph()

#### 2 add circularity to network graph ####
# still messes up due to width factor
ggraph(ingredients.tidy.recip, layout = "linear") +
  geom_edge_arc(aes(width=weight), alpha=0.6) +
  scale_edge_width(range = c(0.2, 1)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Ingredient Connections") +
  theme_graph()


#### 2 INTERACTIVE NETWORK GRAPHS using visNetwork & networkD3 ####
library(visNetwork)
library(networkD3)

#### 2 visNetwork graph ####
ingredients.viz.recip <- visNetwork(nodes.recip, edges.recip)

# create a width column to use 'weights' in visNetork
edges.recip <- mutate(edges.recip, width = weight/5 + 1)

#### networkD3 graph ####
# id lists in D3 must begin with a 0
nodes.D3.recip <- mutate(nodes.recip, id = id - 1)
edges.D3.recip <- mutate(edges.recip, from = from - 1, to = to - 1)



# use forceNetwork() function
italy.d3.net.recip <- forceNetwork(Links = edges.D3.recip, Nodes = nodes.D3.recip, Source = "from", Target = "to",
                             NodeID = "label", Group = "id", Value = "weight",
                             opacity = 1, fontSize = 16, zoom = TRUE, opacityNoHover = TRUE, bounded = FALSE,
                             radiusCalculation = JS("Math.sqrt(d.nodesize)+6", legend = TRUE))

htmlwidgets::saveWidget(italy.d3.net, "italyD3network.html", selfcontained = TRUE, libdir = NULL)

#### Sankey Diagram ####

italy.sankey.net.recip <- sankeyNetwork(Links = edges.D3.recip, Nodes = nodes.D3.recip, Source = "from", Target = "to",
                                  NodeID = "label", Value = "weight", fontSize = 16, unit = "Ingredient Connections")

htmlwidgets::saveWidget(italy.sankey.net, "italyD3sankey.html", selfcontained = TRUE, libdir = NULL)



