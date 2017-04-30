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

start = 'A'
goal = 'G'

gScore <- as.list(rep(Inf,length(V(gr)))) 
names(gScore) = names(as.list(V(gr)))

fScore <- as.list(rep(Inf,length(V(gr)))) 
names(fScore) = names(as.list(V(gr)))


h <- function(start){
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


IDA_star <-function(root){
  bound = h(root)
  found = F
  g=0.0
  gScore <<- as.list(rep(Inf,length(V(gr)))) 
  names(gScore) <<- names(as.list(V(gr)))
  # fScore[[start]] <<- h(start)
  gScore[[start]] <<- 0
  i=0
  gScore
  while(found==F){
    print(paste("starting while loop, bound= ",bound))
    #  t := search(root, 0, bound)
    t = DFS(root, g, bound, root, gr, el1)
    print(paste('returnning t=', t))
    # if t = FOUND then return bound
    if(t[[1]]==T){return(list(T,t[2]))}
    # if t = Inf then return NOT_FOUND
    if(t[[2]]==Inf){return(list(F,0))}
    bound = t[[2]]
    found = t[[1]]
    i = i+1
    if(i==5){break}
    
  }
}


DFS <- function(n, g, bound, ring, graph_, el_){
  # f := g + h(node)
  f = g + h(n)
  print(paste("  Analyzing ",ring, ' bound =', bound, ' f=', f))
  #  if f > bound then return f
  if(f > bound){return(list(F,f))}
  # if is_goal(node) then return FOUND
  if(n==goal){return(list(T,gScore[[n]]))}
  print(paste('n=',n,' goal=',goal,' ',n==goal))
  min= Inf
  # for succ in successors(node) do
  for(neighbor in names(neighbors(graph_, n))){
    print(paste('    analizing neighbor ',neighbor , 'of ',n , ' from ', toString(names(neighbors(gr,n))) ))
    if(gScore[[neighbor]]==Inf){
      gScore[[neighbor]] <<- gScore[[n]]+dist_between(n, neighbor)
      # print(paste('updating gScore ',neighbor,' g=', gScore[[neighbor]], ' g(n)=',gScore[[n]]))
     }
    gi = gScore[[neighbor]]
    el_tmp = el_[el_$from!=n,]
    gr_tmp <<- graph.data.frame(el_tmp,directed=FALSE)
    # t := search(succ, g + cost(node, succ), bound)
    ring_tmp = paste(ring,'-',neighbor )
    t <- DFS(neighbor, gi, bound, ring_tmp, gr_tmp, el_tmp)
    print(paste('   returnning t=', t))
    # if t = FOUND then return FOUND
    if(t[[1]] == T){return(list(T,gScore[[neighbor]]))}
    # if t < min then min := t
    if (t[[2]]< min){min = t[[2]]}
  }
  print(paste('minimum ',min))
    return(list(F,min))
}




