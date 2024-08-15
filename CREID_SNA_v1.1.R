library("igraph")
library(statnet)
library(network)
library(sna)
library(ggraph)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(dplyr)

# UPDATE - MAY 30, 2024
# EID-SEARCH was cut from the network. Remake these plots with them removed.



setwd("C:/Users/eearley/OneDrive - Research Triangle Institute/Network Analysis/Social Network Analysis/")

#### Load data ####
# the Composite stakeholder  table has RC x RS links

# read.table will have issues with ' and special characters.
# 1. convert the text file to UTF-8 instead of ANSI
# 2. remove all double quotes from the file with sed
# 3. quote="" will stop treating ' as a special character
# 4. comment.char = "" will stop treating # as a comment character

master = read.table("data/Composite ORR_stakeholder_list_2023_10_19.txt",
                    sep="\t",
                    header=T, 
                    stringsAsFactors = F,
                    quote="",
                    comment.char = "",
                    fill=T)

dim(master)
colnames(master)[c(5,6,3,4,8)] = c("type","entityB","country","region","entityA")
master = master[,c("entityA","entityB","type","country","region")]
head(master)

# remove rows with no RC connection (entityA)
master = master[master$entityA != "",]
dim(master)
head(master)

# subset this table to just entityA and entityB for the network below
master.subset = master[,c("entityA","entityB")]


# add in survey 2 data
survey = read.table("data/survey2_table_clean_2023_10_05.txt", sep="\t", header=T, stringsAsFactors = F, quote="\"")
# ResearchSite = A
# collaborator = B
colnames(survey)[c(2,3,4,5)] = c("entityA","country","entityB","type")

for_net = rbind(survey[,c("entityA","entityB")],
                master.subset[,c("entityA","entityB")])
for_net = for_net[!duplicated(t(apply(for_net[c("entityA","entityB")], 1, sort))), ]
# get rid of empty cells -- this will be revisited once I learn how to plot edge-less nodes
for_net = for_net[for_net$entityA != "",]
for_net = for_net[for_net$entityB != "",]
dim(for_net)
# check this for errors manually with excel
write.table(for_net, file="data/merged_composite_survey_2023_10_19.txt",sep="\t", col.names=T, row.names=F, quote=F)
# corrections
for_net[58,1] = "Chulalongkorn University"
for_net = for_net[-c(70,336),] #RVF Field site is not connected to an RC
for_net[c(609),2] = "NMSU"

# Update May 30, 2024 - remove EID-SEARCH #
for_net = for_net[!for_net$entityA == "EID-SEARCH",]
# also remove "WARN-ID All Sites"
for_net = for_net[!for_net$entityA == "WARN-ID All Sites",]

#for_net[for_net$entityB == "EID-SEARCH",]

# create the basic network
n = network(for_net, directed=FALSE, loops=TRUE)

# create graph using igraph
net_igraph <- graph_from_data_frame(for_net, directed = FALSE)















#### Entity type table ####
# entity ; type
# start with composite data
type.tab = data.frame("entity" = master[,c("entityB")],
                      "type" = master[,c("type")])
# add on survey 2 data
add.me = data.frame("entity" = survey[,c("entityB")],
                    "type" = survey[,c("type")])
type.tab = data.frame(rbind(type.tab, add.me))
dim(type.tab)
table(type.tab$type)

# clean up the type categories
type.tab$type = ifelse(type.tab$type == "US & Foreign Government Entity", 
                       "Government Entity",
                       type.tab$type)
type.tab$type = ifelse(type.tab$type == "\"Professional Association, Stakeholder Fora, Network\"",
                       "Professional Association/Network",
                       type.tab$type)
type.tab$type = ifelse(type.tab$type == "CREID Field Site",
                       "Academic/Research Institution",
                       type.tab$type)
type.tab$type = ifelse(type.tab$type == "Non-governmental organization (NGO)",
                       "NGO",
                       type.tab$type)
type.tab$type = ifelse(type.tab$type == "Local Government",
                       "Government Entity",
                       type.tab$type)
type.tab$type = ifelse(type.tab$type == "Professional Association/Network",
                       "Professional Association",
                       type.tab$type)
table(type.tab$type)
type.tab = type.tab[!duplicated(type.tab$entity, type.tab$type),]
dim(type.tab)

# there are nodes that are not listed in the type.tab somehow?
add.me = data.frame("entity" = names(V(net_igraph))[!names(V(net_igraph)) %in% type.tab$entity],
                    "type" = "Academic/Research Institution")
type.tab = data.frame(rbind(type.tab, add.me))

#

# CREID entity list - remove EID-SEARCH
CREID.entities = c("CREID CC", "EEIDI", "CREID-ECA", "UWARN",
                   "CREATE-NEO", "WAC-EID", "WARN-ID", "CREID-ESP",
                   "A2CAREs", "PICREID")







#### Network statistics ####
length(V(net_igraph)) # 456 (452 without EID-SEARCH)
transitivity(net_igraph,
             type="global") # 0.016
#distance_table(net_igraph)
mean_distance(net_igraph) #4.5
centr_degree(net_igraph)





















#### Basic Plot - igraph ####
degrees = igraph::degree(net_igraph)
V(net_igraph)$size = degrees
V(net_igraph)$nodes = NA
V(net_igraph)$nodes = ifelse(names(V(net_igraph)) %in% c("CREID CC", "EEIDI", "CREID-ECA", "UWARN",
                                                         "CREATE-NEO", "WAC-EID", "WARN-ID", "CREID-ESP",
                                                         "A2CARES", "PICREID"),
                             names(V(net_igraph)),
                             V(net_igraph)$nodes)
#pdf(file="results/Network_graph_2023_10_20.pdf")
pdf(file="results/Network_graph_2024_05_30.pdf")
# layout algorithms
#l <- layout_with_fr(net_igraph) #classic, default; clustering in center with high degree nodes
#l <- layout_with_kk(net_igraph) # brings nodes closer
l <- layout_with_dh(net_igraph) # pushes nodes farther
plot(net_igraph,
     vertex.color="orange",
     vertex.label.color = "black",
     vertex.label = V(net_igraph)$nodes,
     vertex.size = degrees*0.5,
     layout=l)
dev.off()

# table for slide 
degrees[names(degrees) %in% CREID.entities]






#### Type plot - igraph ####

type.tab.subset = type.tab[type.tab$entity %in% names(V(net_igraph)),]
#order the type.tab.ubset to be colinear with the vertex names
type.tab.subset = type.tab.subset[match(names(V(net_igraph)),
                                        type.tab.subset$entity),]
#add the annotation
V(net_igraph)$type = type.tab.subset[type.tab.subset$entity %in% names(V(net_igraph)),
                                     "type"]
color.tab <- data.frame("color" = brewer.pal(6, "Spectral"),
                        "type" = unique(type.tab.subset$type))
colors = color.tab$color[match(V(net_igraph)$type,
                      color.tab$type)]



#set plot parameters
V(net_igraph)$size = igraph::degree(net_igraph) * 0.5
V(net_igraph)$color = colors
V(net_igraph)$nodes = NA
V(net_igraph)$nodes = ifelse(names(V(net_igraph)) %in% c("CREID CC", "EEIDI", "CREID-ECA", "UWARN",
                                                         "CREATE-NEO", "WAC-EID", "WARN-ID", "CREID-ESP",
                                                         "A2CARES",  "PICREID"),
                             names(V(net_igraph)),
                             V(net_igraph)$nodes)
V(net_igraph)$nodes = ifelse(degrees > 10,
                             names(V(net_igraph)),
                             V(net_igraph)$nodes)

# now plot
#pdf(file="results/Network_graph_types_2023_10_20.pdf")
pdf(file="results/Network_graph_types_2024_05_30.pdf")
# layout algorithms
#l <- layout_with_fr(net_igraph) #classic, default; clustering in center with high degree nodes
#l <- layout_with_kk(net_igraph) # brings nodes closer
set.seed(400)
l <- layout_with_dh(net_igraph) # pushes nodes farther
par(mar = c(3, 13, 3, 1), xpd = TRUE) # bottom, left, top, right
plot(net_igraph,
     layout=l,
     vertex.label = "",#V(net_igraph)$nodes,
     vertex.label.color = "black",
     vertex.label.font = 2) # 2=bold, 3=italic, 4=bold+italic
 legend(x="left",
      c(color.tab$type),
      pch=21,
      pt.bg = color.tab$color,
      cex=1,
      ncol=1,
      pt.cex=2,
      bty="y",
      inset=c(-0.6, 0))
dev.off()
#
set.seed(400)
l <- layout_with_dh(net_igraph) # pushes nodes farther
par(mar = c(3, 13, 3, 1), xpd = TRUE) # bottom, left, top, right
plot(net_igraph,
     layout=l,
     vertex.label = V(net_igraph)$nodes,#V(net_igraph)$nodes,
     vertex.label.color = "black",
     vertex.label.font = 2) # 2=bold, 3=italic, 4=bold+italic
#



#### Stats on network ####
# number of vertices
length(V(net_igraph))
vcount(net_igraph) #equivalent

# number of edges 
gsize(net_igraph)

# counts of different types of vertices
dim(type.tab.subset)
table(type.tab.subset$type)


#






##########################################################################################




#### Community plotting ####
# community measures
c1 = cluster_fast_greedy(net_igraph) #estimate community membership
length(c1) # number of communities
B = modularity_matrix(net_igraph)
round(B[1,],2)
membership(c1)
sizes(c1)
modularity(net_igraph,membership(c1))


# set plot parameters
V(net_igraph)$nodes = ifelse(names(V(net_igraph)) %in% c("CREID CC", "EEIDI", "CREID-ECA", "UWARN",
                                                         "CREATE-NEO", "WAC-EID", "WARN-ID", "CREID-ESP",
                                                         "A2CAREs", "EID-SEARCH", "PICREID"),
                             names(V(net_igraph)),
                             V(net_igraph)$nodes)
V(net_igraph)$label.cex = 1

l <- layout_with_fr(net_igraph)
pdf(file="results/Network_graph_communities_2023_10_20.pdf")
plot(c1,
     net_igraph,
     layout=l,
     vertex.label=V(net_igraph)$nodes,
     vertex.label.color="black")
dev.off()

# a version without borders around the communities
pdf(file="results/Network_graph_communities_noBorder_2023_10_20.pdf")
plot(net_igraph, 
     vertex.color=membership(c1), 
     layout=l)
dev.off()

# a dendrogram version
pdf(file="results/Network_dendrogram_communities_2023_10_20.pdf",
    height=5,
    width=10)
plot_dendrogram(c1,
                labels=FALSE)
dev.off()
