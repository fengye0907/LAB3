#' The algorithm takes a graph and an initial node and calculates the
#' shortest path from the initial node to every other node in the graph.
#'
#' @param graph A data.frame with three variables (v1, v2 and w) that
#' contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node A numeric scalar that exist in the graph.
#'
#' @return The shortest path to every other node from the starting node as a vector.
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#' @export
#'
dijkstra <- function(graph, init_node){
  name <- c("v1", "v2", "w")
  stopifnot(is.data.frame(graph), ncol(graph)==3, is.numeric(init_node),
            init_node%in%unique(graph[,1]), name==names(graph))
  vec <- vector()    # this vector records the shortest paths of all nodes
  v1 <- graph[[1]]
  v2 <- graph[[2]]
  w <- graph[[3]]
  pn <- unique(v1)
  n <- length(pn)    # the number of the nodes

  vec[pn] <- Inf     # assume all the distances of nodes are infinities in the beginning
  vec[init_node] <- 0  # the distance from init_node to itself is 0

  a <- init_node
  set1 <- init_node    # used nodes
  set2 <- setdiff(pn,set1)  # non-used nodes

  while(length(set1)!=n){
    idx <- v1==a
    m <- sum(idx)
    p_con <- v2[idx]
    p_val <- w[idx]
    t_con <- vector()
    t_val <- vector()
    for(j in 1:m){       # check whether the connected nodes whether is used or not
      if(!p_con[j]%in%set1){
        t_con <- c(t_con, p_con[j])
        t_val <- c(t_val, p_val[j])
      }
    }
    p_con <- t_con
    p_val <- t_val

    m <- length(p_con)
    for(j in 1:m){
      t <- vec[a]+p_val[j]
      if(vec[p_con[j]]>t)    # if the new distance is shorter than before, than
        vec[p_con[j]] <- t   # replace the old distance by the new one
    }
    a <- which(vec==min(vec[p_con]))   # new init_node
    set1 <- c(set1,a)
    set2 <- setdiff(pn, set1)
  }
  vec
}
