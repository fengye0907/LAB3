#' Dijkstras algorithm takes a graph and an initial node and calculates the shortest path from the
#' initial node to every other node in the graph.
#'
#' @param graph A data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node A numeric scalar that exist in the graph.
#'
#' @return The shortest path to every other node from the starting node as a vector.
#'
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s algorithm

dijkstra <- function(graph, init_node){
  name <- c("v1", "v2", "w")
  stopifnot(is.data.frame(graph), ncol(graph)==3, is.numeric(init_node),
            init_node%in%unique(graph[,1]), name==names(graph))
  vec <- vector()
  v1 <- graph[[1]]
  v2 <- graph[[2]]
  w <- graph[[3]]
  pn <- unique(v1)
  n <- length(pn)

  vec[pn] <- Inf     # record all the distance of points from start
  vec[init_node] <- 0
  a <- init_node
  set1 <- init_node    # used
  set2 <- setdiff(pn,set1)  # non-used

  while(length(set1)!=n){
    idx <- v1==a
    m <- sum(idx)
    p_con <- v2[idx]
    p_val <- w[idx]
    t_con <- vector()
    t_val <- vector()
    for(j in 1:m){
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
      if(vec[p_con[j]]>t)
        vec[p_con[j]] <- t
    }
    a <- which(vec==min(vec[p_con]))
    set1 <- c(set1,a)
    set2 <- setdiff(pn, set1)
  }
  vec
}
