#' MST Clustering
#' @param corr_matrix correlation matrix for all legs
#' @param leg_names character vector of names of legs
#' @param connections character vector of leg names which are valid connections
#' @param corr_threshold correlation threshold used to split clusters. Default 0.5.
#' @export

mst_clustering_threshold <- function(corr_matrix,
                                     leg_names,
                                     connections,
                                     corr_threshold=0.5){
  colnames(corr_matrix) <- leg_names
  rownames(corr_matrix) <- leg_names
  #invert graph
  g_inv <- invert_graph(leg_names=leg_names, connections=connections)
  g_t <- igraph::graph.adjacency(adjmatrix=g_inv, mode="undirected")
  #add weights
  g_t_edges <- igraph::get.edgelist(g_t)
  num_edges <- nrow(g_t_edges)
  g_t_weights <- numeric(length=num_edges)
  for (i in 1:num_edges){
    g_t_weights[i] <- 1 - corr_matrix[g_t_edges[i,1],g_t_edges[i,2]]
  }
  g_t_edges_w <- cbind(g_t_edges, g_t_weights)
  g_t_weighted <- igraph::graph.data.frame(g_t_edges_w,directed=FALSE)
  #obtain minimum spanning tree
  mst_g_t <- igraph::mst(g_t_weighted)
  #obtain clusters
  mst_edges <- igraph::get.edgelist(mst_g_t)
  mst_weights <- igraph::get.edge.attribute(mst_g_t)$g_t_weights
  edges_remove <- which(mst_weights > (1 - corr_threshold))
  new_graph <- igraph::delete_edges(mst_g_t, edges_remove)
  #return clusters
  components <- igraph::decompose(new_graph, min.vertices=1)
  cluster_list <- list()
  for (c in 1:length(components)){
    cluster_list[[c]] <- igraph::get.vertex.attribute(components[[c]])$name
  }
  return(list(cluster_list = cluster_list,
              adj_mat_corr = igraph::as_adjacency_matrix(new_graph, type="both", sparse=F)))
}
