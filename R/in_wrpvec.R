#Internal function  \code{.wrpvec}
#
#Function used to create a string from a fector of strings. Used into
#show methods to create a "A, ..., Z" string.
#
#@param vector_names Vector with the strings to "zip".
#@return A single string formated as "A, ..., Z".
#@examples
#.wrpvec(c("elm1"))
#.wrpvec(c("elm1", "elm2"))
#.wrpvec(c("elm1", "elm2", "elm3"))
#.wrpvec(c("elm1", "elm2", "elm3", "elm4"))
.wrpvec <- function(vector_names) {
  if (length(vector_names) == 0) {
    return("none")
  } else if (length(vector_names) < 2) {
    return(vector_names)
  } else if (length(vector_names) == 2) {
    return(paste0(vector_names[1], ", ", vector_names[2]))
  } else {
    nn <- length(vector_names) - 1
    return(paste0(vector_names[1], ", ..., ", vector_names[nn]))
  }
}
