library(tidyverse)
library(tidygraph)
library(RCurl)
library(plyr)
library(ggraph)
library(igraph)
library(stringdist)
library(xlsx) 
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre-9.0.4")


setwd("C:/Users/Fabio Ugolini/Google Drive/Projects/DRIVE/2 Deliverables/D7.6 36M patent literature study")
patents <- read.xlsx("Query_all.xlsx", na.string="", 1)

######################################################################### DATA CLEANING

#determine the size of the inventor matrix (n), needed for the number of columns in "inventors", and separate the strings of inventors at each ";;"
applicants.f <- patents[, "Applicants"]
n <- str_count(applicants.f, pattern = ";;")
n <- max(n, na.rm = TRUE)+1 
applicants <- applicants.f %>% 
  as.data.frame() %>%
    separate(1, into = c(paste0("app", seq(1:n))), sep = ";;")

#Remove rows with all NAs. #if there is no NA in a row, rowSums(is.na()) gives the row number, which is equal to the column number. Therefore, if those are different, there must be NAs in that Row
applicants <- applicants[rowSums(is.na(applicants))!=ncol(applicants), ] 

#Transpose to column (easier to make later operations this way). #need to remove factors to insert the "isolates". The "isolates" are needed until I perfect the counter
applicants <- t(applicants) 
applicants <- as.data.frame(applicants, stringsAsFactors = FALSE) 

#list with one elements are duplicated and puts them in an different list (they are the "isolates" in the network and mess up the caluclations). The duplication is needed to resolve an error in the other counter (later)
top <- ncol(applicants)
isolates <- c()

for (i in 1:top)
{
  if (sum(!is.na(applicants[,i])) == 1)
  {
    isolates <- append(isolates, (applicants[1,i]))
    applicants[2,i] <- "isolates" #does not work with factors
  }
  applicants[,i] <- as.factor(applicants[,i]) #need the factors back to make the list
}

isolates <- as.data.frame(isolates)
isolates <- cbind(isolates, patents = rep(1,nrow(isolates)))
isolates <- aggregate(isolates[,2], list(isolates[,-2]), sum)

#creates the list of nodes
nodeslist <- applicants %>% 
  sapply(levels)

#makes the links (from-to) dataframe fron the list of nodes
from <- NULL
to <- NULL
links.m <- cbind (from, to)

for (x in 1:length(nodeslist)) #this unlists all the lists in the list of nodes one by one
{
  a <- unlist(nodeslist[x])
  
  from <- NULL
  for (i in 1:length(a)) #this creates the first column of the source -- destination
  {
    from <- append(from, c(rep(a[i],length(a)-i)))
  }
  
  to <- NULL
  for (i in 2:length(a)) #this creates the second column of the source -- destinations
  {
    to <- append(to, c(a[i:length(a)]))
  }
  
  ab <- cbind (from,to)
  links.m <- rbind(links.m, ab)
  row.names(links.m) = NULL
}

links <- as.data.frame(links.m)
links <- cbind(links, weight = rep(1,nrow(links))) #insert a weight of 1 for each link
links <- aggregate(links[,3], links[,-3], sum)  #collapses repeated links and sums their weight
links <- links[order(links$from, links$to),]

#removes "isolates" links (edges) and nodes (vertex)
table(links$from)["isolates"]
table(links$to)["isolates"]
links <- links[links$from != "isolates" & links$to != "isolates", ]
links[ ,1] <- as.character(links[ ,1])
links[ ,2] <- as.character(links[ ,2])

#creates the nodes (vector) dataframe and assign unique IDs to Authors
nodes <- NULL
nodes <- as.data.frame(unlist(nodeslist))
nodes <- cbind(nodes, patents = rep(1,length(nodes))) #adds number of patents to the authors
nodes$patents <- as.numeric(nodes$patents)
nodes <- aggregate(nodes[,2], list(nodes[,1]), sum)
nodes<- nodes[nodes$Group.1 != "isolates", ]

#string matching in nodes automatic (corrects similar names, skip if the manual method is used) 
dist.name <- as.data.frame((adist(nodes[, 1], nodes[, 1], ignore.case = TRUE)))
levdistmin <- 1
levdistmax <- 1
match.coord <- as.data.frame(which(dist.name >= levdistmin & dist.name <= levdistmax, arr.ind = TRUE ))
match.coord <- arrange(match.coord, row)
match.record.nodes <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("original", "changed"))

for (i in 1:nrow(match.coord))
{
  a <- match.coord[i, 1]
  b <- match.coord[i, 2]
  match.record.nodes[i, 1] <- as.character(nodes[b, 1])
  match.record.nodes[i, 2] <- as.character(nodes[a, 1])
  nodes[b, 1] <- nodes[a, 1]
}

print(match.record.nodes)

#string matching in links (corrects similar names, does 14 iteration / min)
correct <- as.character(unique(match.record.nodes[,2]))
match.record.from <- NULL
match.record.to <- NULL

for (i in 1:length(correct))
{
  dist.name.from <- (adist(correct[i], (links[ ,1]), ignore.case = TRUE))
  rightname <- correct[i]
  for (x in 1:length(dist.name.from))
  {
    if (dist.name.from[1, x] >= levdistmin & dist.name.from[1, x] <= levdistmax)
    {
      match.record.from <- rbind(match.record.from, c(links[x, 1], rightname))
      links[x ,1] <- rightname
    }
  }  
}

for (i in 1:length(correct))
{
  dist.name.to <- (adist(correct[i], (links[ ,2]), ignore.case = TRUE))
  rightname <- correct[i]
  for (x in 1:length(dist.name.to))
  {
    if (dist.name.to[1, x] >= levdistmin & dist.name.to[1, x] <= levdistmax)
    {
      match.record.to <- rbind(match.record.to, c(links[x, 2], rightname))
      links[x ,2] <- rightname
    }
  }  
}


#Skip these lines if you do not want to do manual check
write.xlsx(nodes, "nodes_applicants.out.xlsx")
nodes <- read.xlsx("nodes_applicants.in.xlsx", 1, header = TRUE, stringsAsFactors = F) #allows for manual correction of nodes name. the corresponding link also needs to be corrected
write.xlsx(links, "links_applicants.out.xlsx")
links <- read.xlsx("links_applicants.in.xlsx", 1, header = TRUE, stringsAsFactors = F) #allows for manual correction of nodes name. the corresponding link also needs to be corrected
#end skip

nodes <- aggregate(nodes[,4], nodes[,-4], sum)
nodes <- nodes[order(nodes$label),]
row.names(nodes) <- NULL
nodes <- rowid_to_column(nodes)
colnames(nodes) <- c("id", "label", "group", "group_id", "patents")

#checks if groups were attributed univocally
n_occur <- data.frame(table(nodes$label))
n_occur[n_occur$Freq >1,]
###############

links[,3] <- as.numeric(links[,3])
links <- aggregate(links[,3], links[,-3], sum)  #collapses repeated links and sums their weight
links <- links[order(links$from, links$to),]
colnames(links)[3] <- "weight"
rownames(links) <- NULL

## inverts links with the same inverted edges
for (x in 1:nrow(links)){
  cell <- links[x,1:2]
  for (i in 1:nrow(links)){
    if (cell[1] == links[i,2] && cell[2] == links[i,1]){
      links[i,1:2] <- cell
      print(cell)
    }
  }
}

links <- aggregate(links[,3], links[,-3], sum)  #collapses repeated links and sums their weight
links <- links[order(links$from, links$to),]
colnames(links)[3] <- "weight"
rownames(links) <- NULL



#transfers the IDs from nodes to links
fromto <- links[, c(1,2)]
colnames(fromto) <- c("label", "placeholder")
idtransfer <- join(fromto, nodes, by = "label")
sum(is.na(idtransfer)) #must be 0
links$from <- idtransfer$id 
tocorrect <- idtransfer[is.na(idtransfer$id), ]

fromto <- links[, c(2,1)]
colnames(fromto) <- c("label", "placeholder")
idtransfer <- join(fromto, nodes, by = "label")
sum(is.na(idtransfer)) #must be 0
links$to <- idtransfer$id
tocorrect <- idtransfer[is.na(idtransfer$id), ]

# makes tibbles(makes it digestible for graphing libraries)
links <- as.tibble(links)
nodes <- as.tibble(nodes)

# removes unneeded variables
rm(list = setdiff(ls(), c("isolates", "links", "nodes")))

#variables used in plotting code
links.sub <- links
nodes.sub <- nodes

######################################################################### SUBSETTING

############ subset according to the weight of the connection 
minweight <- mean(links$weight)
links.sub <- links[links$weight >= minweight, ]
id.sub <- NULL
id.sub <- (unique(append(links.sub$from , links.sub$to)))
nodes.sub <- nodes[nodes$id %in% id.sub,]

############ subset according to the publication number ######
minpat <- 9
nodes.sub <- nodes[nodes$patents >= minpat, ]
id.sub <- nodes.sub$id 
links.sub <- links[links$from %in% id.sub | links$to %in% id.sub,  ]
id.sub <- NULL
id.sub <- (unique(append(links.sub$from , links.sub$to)))
nodes.sub <- nodes[nodes$id %in% id.sub,]


######################################################################### PLOTTING

#plotting with Visnetwork (https://datastorm-open.github.io/visNetwork/options.html)
library(visNetwork)
l <- "layout_with_fr" #Fruchterman-Reingold (force-directed)

nodes.vis <- mutate(nodes.sub, size = patents)
nodes.vis <- cbind(nodes.vis, 
                   title = paste0(nodes.sub$label, "<br>", nodes.sub$group, "<br>Inventions: ", nodes.sub$patents))
links.vis <- mutate(links.sub, width = weight)
links.vis <- cbind(links.vis, 
                   title = paste0("Collaborations: ", links.vis$weight))

visNetwork(nodes.vis, links.vis, height = "1000px", width = "100%") %>% 
  visIgraphLayout(layout = l) %>% 
  visEdges(color = list(color = "lightblue", highlight = "yellow")) %>%
  visNodes(color = list(background = "lightblue", border = "darkblue", highlight = "yellow")) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group")  %>% 
  visConfigure(enabled = TRUE)

l <- "layout_randomly" # Randomly placed vertices
l <- "layout_in_circle" # Circle layout
l <- "layout_on_sphere" # 3D sphere layout
l <- "layout_with_fr" #Fruchterman-Reingold (force-directed)
l <- "layout_with_kk" #Kamada Kawai (force-directed)
l <- "layout_with_lgl" #The LGL algorithm is meant for large, connected graphs. You can specify a root: a node that will be placed in the middle of the layout.

visNetwork(dot = 'dinetwork {1 -> 1 -> 2; 2 -> 3; 2 -- 4; 2 -> 1 }', width = "100%")

#refining options
net <- simplify(net, remove.multiple = T, remove.loops = T) 
net <- delete_edges(net, E(net)[weight < 60])
net <- subgraph.edges(net, eids = which(E(net)$weight > 59), delete.vertices = FALSE)

#layouts
l <- layout_randomly(net) # Randomly placed vertices
l <- layout_in_circle(net) # Circle layout
l <- layout_on_sphere(net) # 3D sphere layout
l <- layout_with_fr(net) #Fruchterman-Reingold (force-directed)
l <- layout_with_kk(net) #Kamada Kawai (force-directed)
l <- layout_with_lgl(net) #The LGL algorithm is meant for large, connected graphs. You can specify a root: a node that will be placed in the middle of the layout.

#Extract info from graph objects
class(net)
net

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$weight  # Edge attribute "weight"
V(net)$patents # Vertex attribute "patents"

as_edgelist(net, names=T) #extract and edgelist
as_adjacency_matrix(net,  attr="weight") #extracts a matrix

hist(links$weight)
mean(links$weight)
sd(links$weight)

#basic plot
net <- graph_from_data_frame(d=links.sub, vertices=nodes.sub, directed=F) 
l <- layout_with_fr(net)
plot(net,vertex.size = 4, layout = l, vertex.label = NA, vertex.label.cex = .5)


######################## TEST PLOTTING ############################
plot(net,vertex.size = 8, layout = l, vertex.color = "orange", 
     vertex.label.cex = .5, vertex.label.color = "black", edge.curved = .1)

V(net)$size <- V(net)$patents*0.2
E(net)$width <- 1+E(net)$weight/20
plot(net)

#plots legend
legend(x=-1.5, y=-1.1, c("Hello","world", "bye"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#plots labels only 
plot(net, vertex.shape="none", vertex.label.font=1, vertex.label.color="gray40",
     vertex.label.cex=.5, edge.color="gray85")

#rescaling
l <- layout_with_fr(net)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(net, rescale=F, layout=l*0.4)
plot(net, rescale=F, layout=l*0.6)
plot(net, rescale=F, layout=l*0.8)
plot(net, rescale=F, layout=l*1.0)

#plotting with igraph

net.tidy <- as_tbl_graph(net)
ggraph(net.tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(net.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Collaborations") +
  theme_graph()

ggraph(net.tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Collaborations") +
  theme_graph()

#plotting with Visnetwork (https://datastorm-open.github.io/visNetwork/options.html)
library(visNetwork)
nodes.vis <- mutate(nodes.sub, size = patents*2)
nodes.vis <- cbind(nodes.vis, 
                   title = paste0(nodes.sub$label,"<br>Inventions: ", nodes.sub$patents))
links.vis <- mutate(links.sub, width = weight)
links.vis <- cbind(links.vis, 
                   title = paste0("Collaborations: ", links.sub$weight*2))

visNetwork(nodes.vis, links.vis, height = "1000px", width = "100%") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(color = list(color = "lightblue", highlight = "yellow")) %>%
  visNodes(color = list(background = "lightblue", border = "darkblue", highlight = "yellow")) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)  %>% 
  visConfigure(enabled = FALSE)
  
visNetwork(dot = 'dinetwork {1 -> 1 -> 2; 2 -> 3; 2 -- 4; 2 -> 1 }', width = "100%")
 
#Plot with NetworkD3

library(networkD3)
nodes_d3 <- mutate(nodes.vis, id = id - min(nodes.vis$id))
links_d3 <- mutate(links.vis, from = from - min(nodes.vis$id), to = to - min(nodes.vis$id))
links_d3 <- as.data.frame(links_d3)
forceNetwork(Links = links_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = links_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

#NODES	 
#vertex.color	 Node color
#vertex.frame.color	 Node border color
#vertex.shape	 One of "none", "circle", "square", "csquare", "rectangle", "crectangle", "vrectangle", "pie", "raster", or "sphere"
#vertex.size	 Size of the node (default is 15)
#vertex.size2	 The second size of the node (e.g. for a rectangle)
#vertex.label	 Character vector used to label the nodes
#vertex.label.family	 Font family of the label (e.g."Times", "Helvetica")
#vertex.label.font	 Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#vertex.label.cex	 Font size (multiplication factor, device-dependent)
#vertex.label.dist	 Distance between the label and the vertex
#vertex.label.degree	 The position of the label in relation to the vertex, where 0 right, "pi" is left, "pi/2" is below, and "-pi/2" is above

#EDGES	 
#edge.color	 Edge color
#edge.width	 Edge width, defaults to 1
#edge.arrow.size	 Arrow size, defaults to 1
#edge.arrow.width	 Arrow width, defaults to 1
#edge.lty	 Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed", or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
#edge.label	 Character vector used to label edges
#edge.label.family	 Font family of the label (e.g."Times", "Helvetica")
#edge.label.font	 Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#edge.label.cex	 Font size for edge labels
#edge.curved	 Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
#arrow.mode	 Vector specifying whether edges should have arrows,
#possible values: 0 no arrow, 1 back, 2 forward, 3 both

#OTHER	 
#margin	 Empty space margins around the plot, vector with length 4
#frame	 if TRUE, the plot will be framed
#main	 If set, adds a title to the plot
#sub	 If set, adds a subtitle to the 

library(tidygraph)
library(ggraph)

net.plot.tidy2 <- tbl_graph(nodes = nodes, edges = links, directed = FALSE)
net.plot.tidy <- as_tbl_graph(net.plot)
ggraph(net.plot.tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(net.plot.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = rownames(labels)), repel = TRUE) +
  theme_graph()

ggraph(net.plot.tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = rownames(labels)) +
  theme_graph()

library(visNetwork)
visNetwork(nodes, links)
  
edges <- mutate(links, width = weight/5 + 1)
visNetwork(nodes, edges) %>% 
visIgraphLayout(layout = "layout_with_fr") %>% 
visEdges(arrows = "middle"
         
############ Routine to subset the links and nodes according to the weight ######
links.sub <- links[links$weight >50, ]
id.sub <- NULL
id.sub <- (unique(append(links.sub$from , links.sub$to)))
nodes.sub <- nodes[nodes$id %in% idsub,]

net.sub <- graph_from_data_frame(d = links.sub, vertices = nodes.sub, directed=F) 

l<- layout_as_star(net.sub)
l<- layout_with_fr(net.sub)
l<- layout_as_tree(net.sub)
l<- layout.auto(net.sub)
l<- layout_nicely(net.sub)
l<- layout_with_graphopt(net.sub)
plot(net.sub,vertex.size = 5, layout = l, vertex.label.color = "black", vertex.label.dist = 1, vertex.label.cex = .5)

#labels <- as.data.frame(as.factor(V(net.sub)))

net.sub.tidy2 <- tbl_graph(nodes = nodes.sub, edges = links.sub, directed = FALSE)
net.sub.tidy <- as_tbl_graph(net.sub)
ggraph(net.sub.tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(net.sub.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  theme_graph()

ggraph(net.sub.tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
                   theme_graph()

library(visNetwork)
nodes.sub <- as.tibble(nodes.sub)
links.sub <- as.tibble(links.sub)

visNetwork(nodes.sub, links.sub)
edges <- mutate(links.sub, width = weight/5 + 1)
visNetwork(nodes.sub, links.sub) %>% 
  visIgraphLayout(layout = "layout_with_fr")
 

library(networkD3)
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")


