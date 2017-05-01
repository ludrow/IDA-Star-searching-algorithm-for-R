library(igraph)
library(dplyr)

el=read.csv("src/Dataset-Astar-Example-EDGES.csv") # read the 'Dataset-Astar-Example-EDGES.csv' file
heuristics <- el[el$type==" heuristic",]
el1<- el[el$type!=" heuristic",]

gr=graph.data.frame(el1,directed=FALSE)
E(gr)$width <- E(gr)$weight
E(gr)$color <- "pink"
E(gr)$label<- E(gr)$weight

plot(gr)



#gScore <- as.list(rep(Inf,length(V(gr)))) 
#names(gScore) = names(as.list(V(gr)))

#fScore <- as.list(rep(Inf,length(V(gr)))) 
#names(fScore) = names(as.list(V(gr)))


h <- function(start, goal){
  val<- heuristics[heuristics$from==start & heuristics$to==goal,"weight"]
  if(!any(val)) return(0)
  else return(as.numeric(val))
}
dist_between <- function(current, neighbor){
  ei<-get.edge.ids(gr, c(current,neighbor))
  val <- E(gr)$weight[ei]
  if(!any(val)) return(NA)
  else return(val)
}

#start = 'A'
#goal = 'G'

gScore <- as.list(rep(Inf,length(V(gr)))) 
IDA_star <-function(root, goal, gr){
  bound = h(root, goal)
  found = F
  g=0.0
  gScore <- as.list(rep(Inf,length(V(gr)))) 
  names(gScore) <- names(as.list(V(gr)))
  gScore[[root]] <<- 0
  while(found==F){
    print(paste("starting new loop with bound= ",bound))
    #  t := search(root, 0, bound)
    t = DFS(root, g, bound, root, gr, el1, goal)
    # if t = FOUND then return bound
    if(t[[1]]==T){
      return(list(T,t[[2]]))}
    # if t = Inf then return NOT_FOUND
    if(t[[2]]==Inf){return(list(F,0))}
    bound = t[[2]]
    found = t[[1]]
  }
  
}

DFS <- function(n, g, bound, ring, graph_, el_, goal){
  # f := g + h(node)
  f = g + h(n, goal)
  print(paste(ring,'f=',f))
  
  #  if f > bound then return f
  if(f > bound) return(list(F,f))
  
  # if is_goal(node) then return FOUND
  if(n==goal){
    print(ring)
    return(list(T,gScore[[n]]))
  }
  min= Inf
  
  # for succ in successors(node) do
  for(neighbor in names(neighbors(graph_, n))){
    if(grepl(neighbor, ring))next
    
    gScore[[neighbor]] <<- gScore[[n]]+dist_between(n, neighbor)
    
    ring_tmp = paste(ring,'-',neighbor )
    gi = gScore[[neighbor]]
    print(paste(ring, '?',neighbor , 'g=',gi))
    # t := search(succ, g + cost(node, succ), bound)
    t <- DFS(neighbor, gi, bound, ring_tmp, graph_, el_, goal)
    # if t = FOUND then return FOUND
    if(t[[1]] == T){return(list(T,t[[2]]))}
    # if t < min then min := t
    if (t[[2]]<min){min = t[[2]]}
  }
  return(list(F,min))
}




