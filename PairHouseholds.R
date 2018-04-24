# Tom Rutter 
# August 29th
# Generates Pairs of Voters
# ============================================================================
rm(list = ls())

##setwd("~/Dropbox (Stanford Law School)/Plus3/Replicate_Pairing/3_PairVoters")
setwd("~/Dropbox/Plus3/Replicate_Pairing/3_PairVoters")

require(distances)
require(igraph)

# ============================================================================
# Pair Voters.  
# ============================================================================
# ============================================================================
# Prepare the C++ code, which accompanies the following academic article: 
# Kolmogorov, V. Math. Prog. Comp. (2009) 1: 43.
# The c++ code is http://pub.ist.ac.at/~vnk/software.html#BLOSSOM5. 
## setwd("BlossomV/")
## system("make")
## setwd("..")
# ==========================================================================
# Generate vector of districts to loop over
districts <- c(10, 13, 18, 20, 21, 23, 24, 25, 28, 29, 31, 32, 33, 34, 
               40, 42, 50, 51, 68, 81, 82, 83, 84, 85, 86, 87, 94)

# Function to get corresponding voter_ID from row_id x.
GetID <- function(x) {
  return(sub.dat[sub.dat$row_id == x, "DWID"])
}

# set seed to allow for reproducability of random sampling. 
set.seed(29111995)
# =========================================================================
# Main Loop 

for (i in districts) {
  
  print(paste0("District ", i))
  
  # Inputs: 
  sub.dat <- readRDS(paste0("Inputs/District_", i, "_OnePerHouse.rds"))
  
  # Save outputs to BlossomV Directory, where the C++ code can be called. 
  setwd("BlossomV/")
  
  # Note that the blossom algorithm will only run on a even number of nodes. If you try run 
  # it on an odd number, it will say: 
  # # of nodes is odd: perfect matching cannot exist
  # So if n is odd, we need to drop a node. 
  # Drop a row randomly. 
  n <- nrow(sub.dat)
  if (n %% 2 == 1) {
    random_row_to_drop <- sample(1:n, size = 1)
    sub.dat <- sub.dat[- random_row_to_drop, ]
  }
  
  # Generate a row id for later use: 
  sub.dat$row_id <- 0:(nrow(sub.dat) - 1)
  
  # Calculate Adjacency Matrix
  DistanceMatrix <- distances::distances(sub.dat, id_variable = "DWID", dist_variables = c("X", "Y"))
  
  # Assign DistanceMatrix to be matrix form: 
  DistanceMatrix <- as.matrix(DistanceMatrix)
  
  # The algorithm rounds down small numbers to 0, and does not handle zeros very well. 
  # To get around this, apply two monotonic transformations to the DistanceMatrix
  DistanceMatrix <- 10 + DistanceMatrix
  DistanceMatrix <- 1000 * DistanceMatrix
  
  
  # Create the graph from this adjacency matrix that will be the input to the blossom algorithm. 
  Graph_For_Pairing <- graph_from_adjacency_matrix(DistanceMatrix, 
                                                   mode = "undirected", 
                                                   weighted = TRUE, 
                                                   diag = FALSE, 
                                                   add.colnames = NA)
  
  num.nodes <- length(V(Graph_For_Pairing)) 
  num.edges <- length(E(Graph_For_Pairing)) 
  
  # Need to check that the number of edges is what we would expect given 
  # the number of nodes: 
  test <- 0.5 * num.nodes * (num.nodes - 1)
  print("This should be TRUE (number of edges equal to expected number):")
  test2 <- isTRUE(test == num.edges)
  print(test2)
  
  # Save graph to be used in blossom algorithm: 
  file_name <- paste0("Graph_For_Pairing_", i, ".txt")
  
  write_graph(Graph_For_Pairing, file_name, "ncol")
  
  # -------------------------------------------------------
  # The next section of code is essential to create a text file
  # to be used as an input to the blossom algorithm which is in the
  # correct format. 
  edges <- read.table(file_name)
  FirstLine <- paste(num.nodes, num.edges, sep = " ")
  writeLines(FirstLine, paste0(file_name))
  write.table(edges, paste0(file_name), append = TRUE, col.names = FALSE, row.names = FALSE)
  
  # --------------------------------------------------------
  
  system(paste0("./blossom5 -e ", file_name, " -w Matching_", i, ".txt"))
  
  
  # Remove the graph from memory (to conserve memory space)
  setwd("..")
  system(paste0("rm BlossomV/", file_name))
  
  # Copy the matching from the BlossomV folder into the output folder. 
  system(paste0("cp BlossomV/Matching_", i, ".txt Outputs/Matching_", i, ".txt"))
  
  setwd("Outputs/")
  mat <- read.table(paste0("Matching_", i, ".txt"), colClasses = "character")
  
  # The first row of mat just gives the number of nodes in the graph and edges in the matching, 
  # hence we want to drop this. 
  mat <- mat[2:nrow(mat), ]
  
  names(mat) <- c("pair.1", "pair.2")
  
  # Get row_id of match for each voter, store in match_row_id, 
  # then use this to obtain match_DWID
  for (x in mat$pair.1) {
    sub.dat[sub.dat$row_id == x, "match_row_id"] <- mat[mat$pair.1 == x, "pair.2"]
    z <- sub.dat[sub.dat$row_id == x, "match_row_id"]
    sub.dat[sub.dat$row_id == x, "match_DWID"] <- GetID(z)
  }
  for (x in mat$pair.2) {
    sub.dat[sub.dat$row_id == x, "match_row_id"] <- mat[mat$pair.2 == x, "pair.1"]
    z <- sub.dat[sub.dat$row_id == x, "match_row_id"]
    sub.dat[sub.dat$row_id == x, "match_DWID"] <- GetID(z)
  }
  
  # Now need to import the geographic characteristics of the match. 
  # Need to generate average geographic charateristics for the pair
  for (x in sub.dat$match_row_id) {
    sub.dat[sub.dat$match_row_id == x, "match.X"] <- sub.dat[sub.dat$row_id == x, "X"]
    sub.dat[sub.dat$match_row_id == x, "match.Y"] <- sub.dat[sub.dat$row_id == x, "Y"]
  }
  for (x in sub.dat$row_id) {
    sub.dat[sub.dat$row_id == x, "Lon.pair"] <- 
      0.5 * (sub.dat[sub.dat$match_row_id == x, "X"] + sub.dat[sub.dat$row_id == x, "X"])
    sub.dat[sub.dat$row_id == x, "Lat.pair"] <- 
      0.5 * (sub.dat[sub.dat$match_row_id == x, "Y"] + sub.dat[sub.dat$row_id == x, "Y"])
  }
  
  file <- paste0("District_", i, "_Voters_With_Pairs.rds")
  saveRDS(sub.dat, file)
  
  # add DWID to mat.
  names(mat) <- c("pair.1.row_id", "pair.2.row_id")
  mat$pair.1.DWID <- sapply(mat$pair.1.row_id, GetID)
  mat$pair.2.DWID <- sapply(mat$pair.2.row_id, GetID)
  file <- paste0("District_", i, "_pairs.rds")
  saveRDS(mat, file)
  
  setwd("..")

}



