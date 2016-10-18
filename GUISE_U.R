library(igraph)

graph_d <- erdos.renyi.game(500, .5, directed=FALSE, loops=FALSE)
edges <- as_edgelist(graph_d)
graph_u <- graph_from_edgelist( as.matrix(edges), directed=FALSE)

write.table(edges, file= "./Samples/Actual.txt", row.names = FALSE, col.names = FALSE)

graphlet_neighbors <- function(Graphlets, i)
{
  connected <- c( 
    is.connected( induced_subgraph( graph_u, as.numeric(c(Graphlets[i, c("Node2", "Node3", "Node4")]) ) ) ),  
    is.connected( induced_subgraph( graph_u, as.numeric(c(Graphlets[i, c("Node1", "Node3", "Node4")]) ) ) ),  
    is.connected( induced_subgraph( graph_u, as.numeric(c(Graphlets[i, c("Node1", "Node2", "Node4")]) )  ) ), 
    is.connected( induced_subgraph( graph_u, as.numeric(c(Graphlets[i, c("Node1", "Node2", "Node3")]) )  ) )
  )
  neighbors_sample <- vector()
  delete <- vector()
  if ( connected[1] == TRUE )
  {
    neighbors <- unique( c( 
      neighbors(graph_u, Graphlets$Node2[i]),
      neighbors(graph_u, Graphlets$Node3[i]),
      neighbors(graph_u, Graphlets$Node4[i]))
    )
    neighbors <- setdiff(neighbors, c(Graphlets$Node2[i], Graphlets$Node3[i], Graphlets$Node4[i], Graphlets$Node1[i]))
    neighbors_sample <- c(neighbors_sample, neighbors)
    if(length(neighbors) > 0)
    {
    delete <- c(delete, rep(Graphlets$Node1[i], length(neighbors)) )
    }
  }
  if ( connected[2] == TRUE )
  {
    neighbors <- unique( c(
      neighbors(graph_u, Graphlets$Node1[i]),
      neighbors(graph_u, Graphlets$Node3[i]),
      neighbors(graph_u, Graphlets$Node4[i]))
    )
    neighbors <- setdiff(neighbors, c(Graphlets$Node2[i], Graphlets$Node3[i], Graphlets$Node4[i], Graphlets$Node1[i]))
    neighbors_sample <- c(neighbors_sample, neighbors)
    if(length(neighbors) > 0)
    {
      delete <- c(delete, rep(Graphlets$Node2[i], length(neighbors)) )
    }
    }
  if ( connected[3] == TRUE )
  {
    neighbors <- unique( c(
      neighbors(graph_u, Graphlets$Node1[i]),
      neighbors(graph_u, Graphlets$Node2[i]),
      neighbors(graph_u, Graphlets$Node4[i]))
    )
    neighbors <- setdiff(neighbors, c(Graphlets$Node2[i], Graphlets$Node3[i], Graphlets$Node4[i], Graphlets$Node1[i]))
    neighbors_sample <-c(neighbors_sample, neighbors)
    if(length(neighbors) > 0)
    {
      delete <- c(delete, rep(Graphlets$Node3[i], length(neighbors)) )
    }
    }
  if ( connected[4] == TRUE )
  {
    neighbors <- unique( c(
      neighbors(graph_u, Graphlets$Node1[i]),
      neighbors(graph_u, Graphlets$Node2[i]),
      neighbors(graph_u, Graphlets$Node3[i])) 
      )
    neighbors <- setdiff(neighbors, c(Graphlets$Node2[i], Graphlets$Node3[i], Graphlets$Node4[i], Graphlets$Node1[i]))
    neighbors_sample <- c(neighbors_sample, neighbors)  # Could implement similar check legnth >0 for neighbors if is an issue
    if(length(neighbors) > 0)
    {
      delete <- c(delete, rep(Graphlets$Node4[i], length(neighbors)) )
    }
    }
frame <- data.frame(neighbors_sample, delete)
colnames(frame) <- c("Neighbor", "Delete")
return(frame)
}

GUISE <- function(edges, samplesize) 
{
  Node1 <- sample((1:vcount(graph_u)), 1)  
  Node2 <- sample(neighbors(graph_u, Node1),1)
  a <- unique( c(neighbors(graph_u, Node1), neighbors(graph_u, Node2)) )
  Node3 <- sample(a[-c(Node1, Node2)],1)
  a <- unique( c(neighbors(graph_u, Node1), neighbors(graph_u, Node2), neighbors(graph_u, Node3)) )
  Node4 <- sample(a[-c(Node1, Node2, Node3)],1)
  Graphlets <- cbind(Node1, Node2, Node3, Node4)
  Graphlets <- data.frame(Graphlets)
  colnames(Graphlets) <- c("Node1", "Node2", "Node3", "Node4")
  i=1
  while ( i <= (samplesize-1) )
  {
    new_graphlet <- vector()
    if( anyNA(Graphlets[i, c("Node1", "Node2", "Node3", "Node4")]== TRUE) )
    {
      return(Graphlets)
    }
    neighboring <- graphlet_neighbors(Graphlets, i)
    if(nrow(neighboring) == 0 )
    {
      newgraphlet = Graphlets[i-1]  #Can change to a random jump
    }
    di <- nrow(neighboring)
    while ( length(new_graphlet) == 0 )
    {
      index <- sample(1:(nrow(neighboring)),1)
      new_node <- neighboring$Neighbor[index]
      delete <- neighboring$Delete[index]
      Graphlets_i <- as.numeric(Graphlets[i, c("Node1", "Node2", "Node3", "Node4")])
      keep <- setdiff(Graphlets_i, delete)
      candidate <- data.frame(new_node, keep[1], keep[2], keep[3])
      if (anyNA(candidate) == TRUE)
      {
        return(Graphlets)
      }
      colnames(candidate) <- c("Node1", "Node2", "Node3", "Node4")
      dj <- nrow(graphlet_neighbors(candidate, 1))
      didj <- di/dj
      if ( didj > 1 )
      {
        new_graphlet <- candidate
      } else {
        r <- runif(1,0,1)
        if (r <= didj )
        {
          new_graphlet <- candidate
        }
      }
    }
    Graphlets <- rbind(Graphlets, new_graphlet)
    i=i+1
  }
  i=1
  while (i <= samplesize)
  {
    sample <- Graphlets[i,c('Node1','Node2','Node3', 'Node4')]
    subgraph <- induced_subgraph(graph_u, as.numeric(sample))
    edgelist <- as_edgelist(subgraph)
   # write.table(edgelist, file=sprintf("%sSample_%i.txt", "./Samples/", i), row.names = FALSE, col.names = FALSE)
    i=i+1
  }
  return(Graphlets)
}

GUISE(edges, 200)
