#' Network edge colour function
#' @export

col_choice <- function(type,
                       edges) {
  qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(RColorBrewer::brewer.pal,
                             qual_col_pals$maxcolors,
                             rownames(qual_col_pals)))
  set.seed(123)
  if (type == "Leg"){
    leg_col <- col_vector[1:nrow(edges)]
    return(leg_col)
  }
  if (type == "Cluster"){
    clust_col <- sapply(edges$cluster, function(x) col_vector[x])
    return(clust_col)
  }
  if (type == "Line"){
    lines <- sapply(edges$leg, function(x) substr(x,
                                                  start=which(strsplit(x, "")[[1]]=="_")+1,
                                                  stop=nchar(x)))
    if (length(unique(lines)) > 2){
      line_col <- suppressWarnings(sapply(factor(lines, labels=1:length(unique(lines))),
                                          function(x) col_vector[x]))
    }
    else {
      line_col <- suppressWarnings(sapply(factor(lines, labels=1:length(unique(lines))),
                                          function(x) c("#1C86EE", "#cd1076")[x]))
    }
    return(line_col)
  }
  if (type == "Network"){
    net_col <- rep("#1C86EE", nrow(edges))
    return(net_col)
  }
}
