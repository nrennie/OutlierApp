#' Cluster by line
#' @export

line_clustering <- function(leg_names){
  leg_lines <- sapply(leg_names,
                      function(x) substr(x,
                                         start=which(strsplit(x, "")[[1]]=="_")+1,
                                         stop=nchar(x)))
  lines <- unique(leg_lines)
  cluster_list <- list()
  for (l in 1:length(lines)){
    cluster_list[[l]] <- leg_names[which(leg_lines == lines[l])]
  }
  return(cluster_list)
}


