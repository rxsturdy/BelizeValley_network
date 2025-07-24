##### NODE LEVEL MEASURES

  ### degree centrality
degreeCent = function(start, stop){
  mymat<-matrix(0,nrow=(stop-start+1) , ncol=length(vertex_attributes$name) )
  for(i in start:stop){
    t = i - start + 1
    mymat[t,]<-sna::degree(
      network.collapse(dynamicCollabs, 
                       at = i,
                       retain.all.vertices = TRUE
      ), 
      gmode="graph",
      #rescale = TRUE
    )
  }
  mymat<-ts(mymat,start = start)
  colnames(mymat) <- vertex_attributes$name
  return(mymat)
}

### Eigenvalue centrality
evCent = function(start, stop){
  mymat<-matrix(0,nrow=(stop-start+1) , ncol=length(vertex_attributes$name) )
  for(i in start:stop){
    t = i - start + 1
    mymat[t,]<-sna::evcent(
      network.collapse(dynamicCollabs, 
                       at = i,
                       retain.all.vertices = TRUE
      ), 
      gmode="graph"
    )
  }
  mymat<-ts(mymat,start = start)
  colnames(mymat) <- vertex_attributes$name
  return(mymat)
}

### Prestige 
prestigeCent = function(start, stop){
  mymat<-matrix(0,nrow=(stop-start+1) , ncol=length(vertex_attributes$name) )
  for(i in start:stop){
    t = i - start + 1
    mymat[t,]<-sna::prestige(
      network.collapse(dynamicCollabs, 
                       at = i,
                       retain.all.vertices = TRUE
      ), 
      gmode="graph"
    )
  }
  mymat<-ts(mymat,start = start)
  colnames(mymat) <- vertex_attributes$name
  return(mymat)
}

### Betweenness centrality 
betweenCent = function(start, stop){
  mymat<-matrix(0,nrow=(stop-start+1) , ncol=length(vertex_attributes$name) )
  for(i in start:stop){
    t = i - start + 1
    mymat[t,]<-sna::betweenness(
      network.collapse(dynamicCollabs, 
                       at = i,
                       retain.all.vertices = TRUE
      ), 
      gmode="graph"
    )
  }
  mymat<-ts(mymat,start = start)
  colnames(mymat) <- vertex_attributes$name
  return(mymat)
}



library(help=sna)
