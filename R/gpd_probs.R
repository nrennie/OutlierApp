#' GPD probabilities
#' @export

gpd_probs <- function(data_matrix){
  #data matrix is matrix of
  output_matrix <- matrix(NA, nrow=nrow(data_matrix), ncol=3)
  output_matrix[,1] <- rownames(data_matrix)
  colnames(output_matrix) <- c("Dep. Date", "Outlier Prob (%)", "Leg(s) Affected")
  #count how many legs an outliers in (vector)
  num_legs_outlier <- apply(data_matrix, 1, function(x) sum(x > 0))
  #sum up values
  diffs <- apply(data_matrix, 1, function(x) sum(x*(x>0)))
  #fit gpd
  m <- suppressWarnings(POT::fitgpd(data=diffs, threshold=0, est = "mle"))
  output_matrix[,2] <- POT::pgpd(q=diffs, loc=0, scale=m$fitted.values[1], shape=m$fitted.values[2])
  for (i in 1:nrow(data_matrix)){
    #add legs affected
    legs_affected <- which(data_matrix[i,] > 0)
    if (length(legs_affected) > 0){
      output_matrix[i,3] <- paste(colnames(data_matrix)[legs_affected], collapse = ', ')
    }
  }
  p <- output_matrix[,2]
  return(output_matrix[order(-as.numeric(p)),])
}
