##############################################
#Plus3 function: Match volunteers to voters

# This function requires the R package "clue", for the linear optimatization of volunteer-voter distances. 
# Documentation for "clue" is available here: https://cran.r-project.org/web/packages/clue/clue.pdf
##############################################

runMatchingAlgorithm <- function(volunteers, voters, district, ref.dist){
  
  ##############################################
  #returns:
  # a dataframe with one row per volunteer and four columns: the volunteer ID, and the matched voter IDs (NA if no requested match)
  
  #arguments:
  #volunteers = data frame of volunteers with the following fields: RecipientID, districtnumber, 
    #geocodedlon, geocodedlat, matchnumber (number of requested matches)
  #voters = data frame of available voters with the following fields: PKID, Lon, Lat, District
  #district = the district to run matches for
  #ref.dist = a dataframe with one row per district, and columns representing percentiles of distances as a reference
  
  # The way this function will work is: 
  # - it calculates a distance matrix between volunteers and voters. 
  # - it replicates the rows of the distance matrix based on number of matches requested by volunteer in that row
  # - it runs solve_LSAP (Hungarian algorithm for bipartite matching)
  # - it then assigns that number of matches to each volunteer, using solve_LSAP, 
  # - it uses the row number of the voter in the voter data frame to obtain the voter id, using GetID function: 
  ##############################################
  ##############################################
  # In the future, Alison would like to implement safeguards to prevent volunteers being matched to themselves. 
  # I.e., they are assigned their own entry in Catalist's dataset. 
  # This happened in (at least) five cases for us, all of which would have been avoided if we had 
  # explicitly added a condition disallowing volunteers to be assigned to voters with the same first and last name. 
  ##############################################
  ##############################################
  
  #0.) Helper functions
  # ---------------------------------------------------------------
  #Small function to get voter ids based on row index 
  GetID <- function(x) {
    return(voters[x, "PKID"])
  }
  # function to calculate distance matrix 
  EuclideanDistance <- function(x,y) {
    a <- (x - volunteers$geocodedlon)
    b <- (y - volunteers$geocodedlat)
    return((a^2 + b^2)^0.5)
  }

  #1.) Prep matching data
  # ---------------------------------------------------------------
  #Subset voters and volunteers by specified district
  voters <- voters[which(voters[,c("District")] == district), ]
  volunteers <- volunteers[which(volunteers$districtnumber == district & volunteers$matchnumber > 0), ]
  ref <- ref.dist[which(ref.dist$X == district),]
  
  #Create numeric vector of match requests
  n <- volunteers$matchnumber
    
  #Stop everything if not enough voters to complete the match requests
  if(nrow(voters) <  sum(n)){
    #Augment log
    stop.message <- paste0(proc.name, ": runMatchingAlgorithm: Not enough voters left in assignment pool for ", district, ". Skipping district.")
    addToLog(stop.message, console_file, section_break = TRUE)
    #Throw warning to std error
    warning(stop.message)
    return(stop.message)
  }
  
  #Ensure lat long variables are numeric for distance calculation
  volunteers[,c("geocodedlon", "geocodedlat")] <- apply(volunteers[,c("geocodedlon", "geocodedlat")], 2, as.numeric)
  voters[,c("Lon", "Lat")] <- apply(voters[,c("Lon", "Lat")], 2, as.numeric)
  
  #2.) Calculate volunteer-voter distance matrix
  # ---------------------------------------------------------------
  #If only one volunteer to match, need to convert vector into 1xn matrix
  if(nrow(volunteers)==1){
    IncidenceMatrix1 <- t(as.matrix(mapply(EuclideanDistance, voters$Lon, voters$Lat))) 
  }else{
    IncidenceMatrix1 <- mapply(EuclideanDistance, voters$Lon, voters$Lat) 
  }
 
  #3.) Prepare incidence matrix
  # ---------------------------------------------------------------
  # Replicate each row of IncidenceMatrix1 (so that each volunteer will be assigned 
  # to n voters). Store this new incidence matrix in IncidenceMatrixN. 
  row.indices <- unlist(sapply(1:nrow(volunteers), FUN = function(x)return(rep(x, as.numeric(n[x])))))
  
  #Special case of one volunteer requesting one match only, need to convert vector into 1xn matrix
  if(length(row.indices)==1){
    IncidenceMatrixN <- t(as.matrix(IncidenceMatrix1[row.indices, ]))
  }else{
    IncidenceMatrixN <- IncidenceMatrix1[row.indices, ]
  }

  #4.) Find closest matches for each volunteer
  # ---------------------------------------------------------------
  # The solution to the linear assignment problem (a 
  # mapping of one row of InceidenceMatrix1 to each column)
  solution <- solve_LSAP(IncidenceMatrixN, maximum = FALSE)
  numeric.solution <- as.numeric(solution)
  
  #.5) Identify voter assignments for each volunteer based on requested match number
  # ---------------------------------------------------------------
  # Get index of first match for each volunteer
  first.match.indices <-  c(1 + cumsum(c(0, n)))[1:length(n)]
  
  # Get vector of assignments based on first match index (SHOULD RETURN A LIST)
  voter.assignments <- sapply(1:nrow(volunteers), FUN = function(x)return(
    numeric.solution[first.match.indices[x]:(first.match.indices[x]+(n[x]-1))]),
    simplify = FALSE)
  
  #.6) Perform sanity checks
  # ---------------------------------------------------------------
  # Tally number of voters assigned to each volunteer
  voter.lengths <- unlist(lapply(voter.assignments, length))

  #Check to see that number of voters assigned matches number requested
  if(sum(n==voter.lengths) != length(n)){
    #Augment log
    stop.message <- paste0(proc.name, ": runMatchingAlgorithm: Volunteers not assigned their requested number of voters in ", district, ". Skipping district.")
    addToLog(stop.message, console_file, section_break = TRUE)
    #Save workspace
    save.image(paste0(debug.ws.path, gsub("-|:| ", "", runtime), "_", district, "_runMatchingAlgorithm.RData"))
    #Throw warning to std error
    warning(stop.message)
    return(stop.message)
  }
  
  #Check to see whether any matches are in the worst 10% of distances
  #.) Only use indices of volunteers that are within their district
  volunteer.indices <- which(volunteers$geocodedindistrict==1)
  if(length(volunteer.indices) > 0){
    match.distances <- sapply(volunteer.indices, FUN = function(x)return(IncidenceMatrix1[x, c(voter.assignments[[x]])]))
    match.worst <- match.distances > as.numeric(ref$X90.)
  }else{
    match.worst <- 0
  }

  #Throw warning if any matches are in the worst 10%
  if(sum(match.worst) > 0){
    #Augment log
    stop.message <- paste0(proc.name, ": runMatchingAlgorithm: One or more matches in the worst 10% of possible matches for ", district, ". Skipping district.")
    addToLog(stop.message, console_file, section_break = TRUE)
    #Save workspace
    save.image(paste0(debug.ws.path, gsub("-|:| ", "", runtime), "_", district, "_runMatchingAlgorithm.RData"))
    #Throw warning to std error
    warning(stop.message)
    return(stop.message)
  }
  
  #7.) Return voter IDs in a standardized dataframe
  # ---------------------------------------------------------------
  #Get IDs for matches
  voter.assignment.ids <- lapply(voter.assignments, GetID)
  
  #Add placeholders for third assigned voter
  voter.assignment.ids <- lapply(voter.assignment.ids, FUN = function(x)return(c(x, rep(NA, 3-length(x)))))
  
  #Convert to dataframe
  voter.assignment.df <- data.frame(cbind(volunteers$RecipientID, 
                                              t(do.call(cbind, lapply(voter.assignment.ids, as.matrix)))))
  names(voter.assignment.df) <- c("RecipientID", "new_match_1_ID", "new_match_2_ID", "new_match_3_ID")
    
  return(voter.assignment.df)
}


