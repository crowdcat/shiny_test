which_bucket <- function(y, fiver) {
  if (is.na(y)) {
    return("Trust NA")
  }
  if (length(fiver) <= 2) {
    return(paste("All trust scores between", min(fiver), "and", max(fiver)))
  }
  for (i in 1:(length(fiver)-1)) {
    if ((y >= fiver[i]) & (y < fiver[i+1])) {
      return(paste("Trust ",fiver[i], " to ", fiver[i+1], sep=""))
    }
  }
  return(paste("Trust ", fiver[(length(fiver)-1)], " to ", fiver[length(fiver)], sep=""))
}
