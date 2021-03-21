##########
# PART 1 #
# Reading in Data

# read in data
class_mat=read.csv(file="https://raw.githubusercontent.com/JeffreyAlanSmith/Integrated_Network_Science/master/data/class555_matrix.csv")

# check object class: dataframe
class(class_mat)

# turn df into matrix
class_mat=as.matrix(class_mat)

# add row names and col names
rownames(class_mat)=1:nrow(class_mat)
colnames(class_mat)=1:ncol(class_mat)

# read in attribute data
class_attributes=read.csv(file="https://raw.githubusercontent.com/JeffreyAlanSmith/Integrated_Network_Science/master/data/class555_attributedata.csv", stringsAsFactors=T)

# getting columns
class_attributes$gender
class_attributes[,"gender"]

# checking column datatypes
class(class_attributes[,"grade"])      # integer
class(class_attributes[,"gender"])     # categorical
levels(class_attributes[,"gender"])

##########
# PART 2 #
# Constructing Networks w igraph

library(igraph)

# create network from matrix: graph_from_adjacency_matrix
class_netbymatrix=graph_from_adjacency_matrix(adjmatrix=class_mat, 
                                              mode="directed")
class_netbymatrix

# map node attributes: set_vertex_attr 
# gender
class_netbymatrix=set_vertex_attr(graph=class_netbymatrix, name="gender", 
                                  value=class_attributes$gender)
# grade
class_netbymatrix=set_vertex_attr(graph=class_netbymatrix, name="grade", 
                                  value=class_attributes$grade)
#race
class_netbymatrix=set_vertex_attr(graph=class_netbymatrix, name="race", 
                                  value=class_attributes$race)

class_netbymatrix

# edgelist: sender/receiver, no isolates
# read in edgelist
class_edges=read.csv(file="https://raw.githubusercontent.com/JeffreyAlanSmith/Integrated_Network_Science/master/data/class555_edgelist.csv")
head(class_edges)

# create igraph using edgelist
class_netbyedgelist=graph_from_data_frame(d=class_edges, directed=T)
class_netbyedgelist

# add class attributes as vertices
class_netbyedgelist=graph_from_data_frame(d=class_edges, directed=T, 
                                          vertices=class_attributes)
class_netbyedgelist

# check node order
V(class_netbyedgelist)$name

# show graph: can highlight isolates
net=graph_from_data_frame(d=class_edges, directed=T, vertices=(id=1:26))
plot(net)

# get attributes back out of igraph object
vertex_attr(graph=class_netbyedgelist, name="grade")

# get edge attribute (weight)
edge_attr(graph=class_netbyedgelist, name="weight")

# get edgelist
class_edges_temp=as_edgelist(graph=class_netbyedgelist, names=F)
head(class_edges_temp) 

# get matrix
as_adjacency_matrix(graph=class_netbyedgelist) 

# adjacency list
class_adjacency=read.csv(file="https://raw.githubusercontent.com/JeffreyAlanSmith/Integrated_Network_Science/master/data/class555_adjacency_list.csv")
class_adjacency

# convert adjacency list to edgelist bc easier to construct network
library(reshape)

# stack cols of interest to form receiver col of edgelist
# create vector of col names corresponding to nomination cols
nomination.columns=paste("Nomination", 1:5, sep="") 
nomination.columns

# reshape wide to long
class_edgelist_byadjacency=reshape(data=class_adjacency, 
                                   varying=nomination.columns, 
                                   v.names="receiver", idvar="id", 
                                   direction="long") 
head(class_edgelist_byadjacency)

# remove 2nd col and rename
class_edgelist_byadjacency=class_edgelist_byadjacency[,-2]
colnames(class_edgelist_byadjacency)=c("sender", "receiver")

# remove NAs
class_edgelist_byadjacency=
  class_edgelist_byadjacency[complete.cases(class_edgelist_byadjacency),]

# reorder to match edgelist
class_edgelist_byadjacency=
  class_edgelist_byadjacency[order(class_edgelist_byadjacency$sender),]
head(class_edgelist_byadjacency)

##########
# PART 3 #
# Constructing Networks w network

detach(package:igraph)
library(network)

# construct network
class_netbymatrix_example2=network(x=class_mat, directed=T) 

# convert attributes to characters
class_attributes$race=as.character(class_attributes$race)
class_attributes$gender=as.character(class_attributes$gender)

# df to list where each element is diff attr
attribute_list=do.call(list, class_attributes)
attribute_list

# create network w attr
class_netbymatrix_example2=network(x=class_mat, directed=T, 
                                   vertex.attr=attribute_list) 
class_netbymatrix_example2

# adding attr one at a time
set.vertex.attribute(x=class_netbymatrix_example2, attrname="gradenew", 
                     value=class_attributes$grade)

# use adgelist to construct network
# 24 is network size
attr(class_edges,"n")=24
class_netbyedgelist_example2=network(x=class_edges, directed=T, 
                                     vertex.attr=attribute_list) 
class_netbyedgelist_example2

# extract matrix
as.matrix(class_netbyedgelist_example2)

# extract vertex attributes
get.vertex.attribute(x=class_netbyedgelist_example2, attrname="grade")

# check node order
get.vertex.attribute(x=class_netbyedgelist_example2, attrname="vertex.names")

# adding edge attributes
set.edge.attribute(x=class_netbyedgelist_example2, 
                   attrname="weight", value=class_edges[,"weight"])
class_netbyedgelist_example2

# extract edge attr
get.edge.attribute(class_netbyedgelist_example2, attrname="weight")

##########
# PART 4 #
# Moving between igraph and network Objects

library(intergraph)

# asIgraph: network -> igraph
# asNetwork: igraph -> network
network_from_igraph=asNetwork(class_netbyedgelist)
network_from_igraph

##########
# PART 5 #
# Key Network Measures

# degree: outdegree (sent from), indegree (receive), network
outdeg=rowSums(class_mat) 
outdeg 

indeg=colSums(class_mat)
indeg

# degree w igraph
detach(package:network)
library(igraph)

outdeg_igraph=degree(graph=class_netbyedgelist, mode="out")
indeg_igraph=degree(graph=class_netbyedgelist, mode="in")

# check
table(outdeg==outdeg_igraph) 

# density: total num edges/total edges possible
# gsize: num edges
num_edges=gsize(class_netbyedgelist)
num_edges

# gorder: num nodes
num_nodes=gorder(class_netbyedgelist)
num_nodes

number_dyads=(num_nodes*(num_nodes-1))
den = num_edges/number_dyads
den

# density w igraph
edge_density(class_netbyedgelist)

# walks: sequence of nodes and edges connecting i to j
# num walks lenth 2: raise matrix to 2nd power
walks2=class_mat %*% class_mat
walks2 

walks3=class_mat %*% class_mat %*% class_mat
walks3

# paths: sequence of nodes and edges starting w one node and ending w another, cant visit same node twice
dist_mat=distances(graph=class_netbyedgelist, mode="out")
dist_mat[1:10, 1:10] 

# shortest path
all_shortest_paths(class_netbyedgelist, from=1, to=6) 
all_shortest_paths(class_netbyedgelist, from=1, to=16) 

# mean distance
diag(dist_mat)=NA                        # add na on diagonal
mean(dist_mat, na.rm=T)                  # remove na
mean(dist_mat[dist_mat!=Inf], na.rm=T)   # ignore unreachable (inf)

# closeness: inv distance (inf->0), 0 unreachable, 1 directly connected
# includes unreachable nodes
close_mat=1/dist_mat
close_mat[1:10, 1:10]
mean(close_mat, na.rm=T) 

# compare distance and closeness
median(dist_mat, na.rm=T)
median(close_mat, na.rm=T)

# reachability: i can reach j through any path, dist<inf
reach_mat=ifelse(dist_mat <Inf, yes=1, no=0) 
reach_mat

# diameter: longest distance between any 2 nodes
# take all shortest then calculate longest among that set
# max distance of nodes that can reach each other
max(dist_mat[dist_mat!=Inf], na.rm=T)
diameter(class_netbyedgelist)       # igraph










