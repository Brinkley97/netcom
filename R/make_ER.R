#' @title Makes a Gilbert Random Network
#'
#' @description Makes a network by adding a node according to the Gilbert random mechanism. Nodes can only attach to previously grown nodes.
#' 
#' @param size The size to make network.
#' 
#' @param p Probability possible edges exist. Needs to be between zero (no edge) and one (edge).
#'
#' @return An adjacency matrix.
#' 
#' @references BOOK: Network Science [P 75] by Albert
#' 
#' @examples
#' # Example usage
#' size <- 7  # Set the number of nodes
#' new_network <- make_ER(x = size, p = 0.37)  
#' print(new_network)
#'
#' @export

make_ER <- function(x, p) {
  # Create a full x by x adjacency matrix with random values between 0 and 1
  rand_num <- stats::runif(x - 1)
  # cat("rand_num:", rand_num, "\n")
  
  # Return a boolean vector where to_link is True, thus add a link
  to_link <- (rand_num <= p)
  # cat("to_link:", to_link, "\n")
  
  # Changing the Trues to 1 and Falses to 0
  create_linkages <- c(1 * to_link, 0)
  # cat("create_linkages:", create_linkages, "\n")
  
  # Create adjacency matrix
  make_network <- matrix(create_linkages, nrow = x, ncol = x)
  
  # Ensure no self-loops by setting the diagonal elements to 0
  diag(make_network) <- 0
  
  return(make_network)
}


