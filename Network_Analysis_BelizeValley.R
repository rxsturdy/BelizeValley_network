library(networkDynamic)
library(viridis)
library(ndtv)
library(tsna)

####### READ IN DATA #######

edges_dynamic <- read.csv("PHDynamicEdges-BelizeValley.csv")
nodes_dynamic <- as.data.frame(read.csv("PHDynamicNodes-BelizeValley.csv"))
vertex_attributes <- read.csv("PHVertexAttributes-BelizeValley.csv", stringsAsFactors = FALSE)

###############################################################################

#### Create static network edges data set

edges_static <- subset(edges_dynamic,select = c(tail,head))

### STATIC NETWORK (all connections at any time)

thenetwork <- network(
  edges_static,
  vertices = vertex_attributes,
  vertex.attrnames = c("vertex.id", "name", "region" ),
  directed = FALSE,
  bipartite = FALSE
)
network.vertex.names(thenetwork) <- vertex_attributes$name ### allows names placed on graphs

summary(thenetwork)
class(thenetwork)

  # This plots the static network in R

plot(thenetwork, displaylabels=TRUE)

  # This produces the plot in a PDF (uncomment to run):

# pdf("BelizeValley_static_network.pdf")
# plot(thenetwork, displaylabels=TRUE)
# dev.off()

############ TEMPORAL NETWORK ############

dynamicCollabs <- networkDynamic(
  thenetwork,
  edge.spells = edges_dynamic,
  vertex.spells =  nodes_dynamic
)
summary(dynamicCollabs)

  # Check the temporal network
network.dynamic.check(dynamicCollabs)

  # Plot our dynamic network as a filmstrip
filmstrip(dynamicCollabs, displaylabels = TRUE, frames = 9)  # need to play with options

  # plot at time points we choose
plot(network.collapse(dynamicCollabs, onset=480, terminus=490), displaylabels=TRUE, xlab="Time 480")

  # Calculate how to plot an animated version of the dynamic network
compute.animation(
  dynamicCollabs,
  animation.mode = "kamadakawai",
  slice.par = list(
    start = 470,
    end = 900,
    interval = 10,
    aggregate.dur = 20,
    rule = "any"
  )
)

render.d3movie(
  dynamicCollabs,
  displaylabels = TRUE,
  # This slice function makes the labels work
  vertex.tooltip = function(slice) {
    paste(
      "<b>Name:</b>", (slice %v% "name"),
      "<br>",
      "<b>Region:</b>", (slice %v% "region")
    )
  }
)

render.animation(
  dynamicCollabs,
  displaylabels = TRUE
)

  # This saves the animation as an mp4  (uncomment to run):
#saveVideo(ani.replay(), video.name="BelizeValley.mp4")  


## a 3D view over time
timePrism(dynamicCollabs, at=c(550,650,750), displaylabels=TRUE, planes = TRUE, label.cex=0.5)



################################################################
### NETWORK analysis over time

# Plot formation of edges over time
plot(tEdgeFormation(dynamicCollabs, time.interval = .25))

# Calculate and graph the rolling betweenness centralization of the network  NOT WORKING NOW
dynamicBetweenness <- tSnaStats(
  dynamicCollabs,
  snafun = "centralization",
  start = 470,
  end = 900,
  time.interval = 1,
  aggregate.dur = 20,
  FUN = "betweenness"
)
plot(dynamicBetweenness)

# Calculate and graph the rolling connectedness of the network
dynamicConnectedness <- tSnaStats(
  dynamicCollabs,
  snafun = "connectedness",
  start = 400,
  end = 900,
  time.interval = 1,
  aggregate.dur = 20
)
plot(dynamicConnectedness, ylab = "Connectedness")

# Calculate and graph the rolling efficiency of the network
dynamicEfficiency <- tSnaStats(
  dynamicCollabs,
  snafun = "efficiency",
  start = 400,
  end = 900,
  time.interval = 1,
  aggregate.dur = 20
)
plot(dynamicEfficiency, ylab = "Efficiency")



################################################################
##### NODE LEVEL METRICS 

source("NodeLevelFunctions.R")

  #### Degree Centrality

myresults<-degreeCent(470,850) # can choose window of time

  # all vertices
plot(myresults, plot.type = "single",xy.labels = TRUE,xy.lines = TRUE, 
     col=viridis(14,alpha = 1), 
     lty=1:14,
     lwd=3,
     main="Degree Centrality")

  # choose locations using row number of vertices
plot(myresults[,c(2,7)],                                          
     plot.type = "multiple", xy.lines = TRUE, yax.flip = TRUE, 
     main="Degree Centrality")


  #### Other Centrality Metrics

myresults<-evCent(470,850)
dev.off()
plot(myresults, plot.type = "single",xy.labels = TRUE,xy.lines = TRUE, 
     col=viridis(14,alpha = 1), 
     lty=1:14,
     lwd=3,
     main="Eigenvalue Centrality")

plot(myresults[,c(2,7)],plot.type = "multiple",xy.lines = TRUE, yax.flip = TRUE, 
     main="Eigenvalue Centrality")


myresults<-prestigeCent(470,850)
dev.off()
plot(myresults, plot.type = "single",xy.labels = TRUE,xy.lines = TRUE, 
     col=viridis(14,alpha = 1), 
     lty=1:14,
     lwd=3,
     main="Prestige")
plot(myresults[,c(2,7)],plot.type = "multiple",xy.lines = TRUE, yax.flip = TRUE, 
     main="Prestige")

myresults<-betweenCent(470,850)
dev.off()
plot(myresults, plot.type = "single",xy.labels = TRUE,xy.lines = TRUE, 
     col=viridis(14,alpha = 1), 
     lty=1:14,
     lwd=3,
     main="Betweenness Centrality")
plot(myresults[,c(2,7)],plot.type = "multiple",xy.lines = TRUE, yax.flip = TRUE, 
     main="Betweenness Centrality")


################################################################
##### REACHABLE SETS FOR NODES

  # Calculate and store the sizes of forward and backward reachable sets for each node
fwd_reach <- tReach(dynamicCollabs)
bkwd_reach <- tReach(dynamicCollabs, direction = "bkwd")
dev.off()
plot(fwd_reach, bkwd_reach, xlab = "Forward Reach", ylab = "Backward Reach")

  # Calculate and plot the forward reachable paths of node number 14 (YAXHA)
YAXHAFwdPath <- tPath(
  dynamicCollabs,
  v = 14,
  direction = "fwd"
)
plotPaths(
  dynamicCollabs,
  YAXHAFwdPath,
  displaylabels = TRUE,
  vertex.col = "white"
)

  # Calculate and plot the backward reachable paths of node number 9 (Nim)
NimBkwdPath <- tPath(
  dynamicCollabs,
  v = 9,
  direction = "bkwd",
  type = 'latest.depart'
)
plotPaths(
  dynamicCollabs,
  NimBkwdPath,
  path.col = rgb(0, 97, 255, max=255, alpha=166),
  displaylabels = TRUE,
  #edge.label.col = rgb(0,0,0,0),
  vertex.col = "white"
)


