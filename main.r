library(igraph)
library(dplyr)

el=read.csv("src/Dataset-Astar-Example-EDGES.csv") # read the 'Dataset-Astar-Example-EDGES.csv' file
heuristics <- el[el$type==" heuristic",]
el1<- el[el$type!=" heuristic",]

g=graph.data.frame(el1,directed=FALSE)
E(g)$width <- E(g)$weight
E(g)$color <- "pink"
E(g)$label<- E(g)$weight

plot(g)

IDA_star <- function(g,heuristics, start, goal){
  bound = h(start)
  end.loop =F
  while(end.loop){
    end.loop=T
  }
  
  search <- function(node, g, bound){
    f = g + h(node)
    if(f > bound){
      return(f)
    }
    if(is_goal(node)){
  }
    
  }

  h <- function(start){
      val<- heuristics[heuristics$from==start & heuristics$to==goal,"weight"]
      if(!any(val)) return(0)
      else return(as.numeric(val))
  }
  
}
