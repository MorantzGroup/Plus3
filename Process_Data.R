# Tom Rutter 

# April 3rd 2018 
# revised April 19th 2018

# Merge Together the data from Plus3 

rm(list = ls())

# Directories: 

home.dir <- "~/Dropbox (Stanford Law School)/Plus3/Replication_For_GitHub/Analysis/1_Process_Data"
input.dir <- file.path(home.dir, "Inputs")
output.dir <- file.path(home.dir, "Outputs")

# Libraries
library(geosphere)

# Seed 
set.seed(29111995)

# =======================================
# Data Import: 
setwd(input.dir)

## Voter Characteristics Data: 
Voter_Characteristics <- 
  read.csv("Final_Treatment_AND_Control_Voters.csv", 
           stringsAsFactors = F)

## Voter Turnout Data: 
Voter_Turnout <- 
  read.csv("VA_2017_Turnout_Data.csv", 
           stringsAsFactors = F) 

## Voter Turnout Data: 
Voter_Contact_Form_Names <- 
  read.csv("Voter_Contact_Form_Names.csv", 
           stringsAsFactors = F) 

Voter_Contact_Form_Responses <- 
  read.csv("Voter_Contact_Form_Responses.csv", 
           stringsAsFactors = F) 

## Original Catalist Data 
Original_Catalist_Data <- 
  read.csv("CatalistData_AllDistricts.csv", 
           stringsAsFactors = F)

## Geocoded Voters
Geocoded_Voters <- 
  read.csv("Geocoded_All_Districts.csv", 
           stringsAsFactors = F)

## Hand_Coded_Other_Binary_Data <- 
Other_Binary_Data <- 
  read.csv("Data_Other_Binary_Equals_1_Coded.csv", 
           stringsAsFactors = F)

## Volunteers From Qualtrics <- 
Volunteers_Qualtrics <- 
  read.csv("Volunteers_From_Qualtrics_04_17_18.csv", 
           stringsAsFactors = F)

Volunteer_Voter_Matches <- 
  read.csv("VolunteerVoterMatchArchive.csv", 
           stringsAsFactors = F)
# ========================================
# Clean the Volunteers Contact List from Qualtrics: 

## Rename the first column 
names(Volunteers_Qualtrics)[1] <- 
  "RecipientID"

# Drop Sandy, who was just a test, and two volunteers who signed up after the election 
Volunteers_Qualtrics <- 
  Volunteers_Qualtrics[(Volunteers_Qualtrics$RecipientID %in% Voter_Contact_Form_Names$RecipientID), ]
# ========================================
# Merge Voter Characteristics Data and Voter Turnout Data. 
# Note - Catalist changed some of the DWIDs between our initial data 
# request, and our request for turnout data, although they were adamant 
# that the new voters they sent us with DWIDs that did not match were 
# the same as the voters we requested. Sure enough, their first and last names 
# match, even though the DWIDs do not, so for these voters, we match on first and 
# last name. 

## Give the Voter Turnout data some more helpful variable names. 
names(Voter_Turnout) <- c("FIRSTNAME", "LASTNAME", "STATE1", "DWID", "STATE2", "2017Turnout")

## Merge on DWID - note first name and last name are also shared by these voters, hence 
## merge on that as well to prevent multiple name variables in the new dataset. 
Merged_Data <- 
  merge(Voter_Turnout, Voter_Characteristics, by = c("FIRSTNAME", "LASTNAME", "DWID"))

## Now merge the voters whose DWIDs do not match. 
unsuccessful_merges_turnout <- 
  Voter_Turnout[!(Voter_Turnout$DWID %in% Voter_Characteristics$DWID), ]
unsuccessful_merges_turnout$DWID <- NULL # To avoid two different DWIDs for one observation 
unsuccessful_merges_voter_characteristics <- 
  Voter_Characteristics[!(Voter_Characteristics$DWID %in% Voter_Turnout$DWID), ]

merges_by_name <-  
  merge(unsuccessful_merges_turnout, unsuccessful_merges_voter_characteristics, 
        by = c("FIRSTNAME", "LASTNAME"))

merges_by_name <- # reorder columns before rbinding data
  merges_by_name[, names(Merged_Data)]

## Bind together the voters merged on DWID and the voters merged purely by name. 
All_Merged_Data <- rbind(Merged_Data, merges_by_name)

## Create Turnout Variable that we can use in regressions
### Define funciton
BinaryTurnout <- function(x) {
  ifelse(x != "", 1, 0)
}
### Apply function to create turnout variable. 
All_Merged_Data$BinaryTurnout <- 
  sapply(All_Merged_Data$`2017Turnout`, BinaryTurnout)

# ================================
# ================================
# Merge additional voter characteristics onto our voter data, such as how they were contacted. 

# -------------------------------------------------------------------
## First, we need to clean the Voter Contact Form Responses Dataset: 

### Drop the anonymous responses: 
Voter_Contact_Form_Responses <- 
  Voter_Contact_Form_Responses[!is.na(Voter_Contact_Form_Responses$RecipientID), ]

### Drop the horizontal rules columns: 
Voter_Contact_Form_Responses <- 
  Voter_Contact_Form_Responses[, names(Voter_Contact_Form_Responses)[!grepl("Horizontal.rule", names(Voter_Contact_Form_Responses))]]

### Rename the Columns to something more user-friendly: 
names(Voter_Contact_Form_Responses) <- 
  c(names(Voter_Contact_Form_Responses)[1:20], 
    paste0(c("Postcard", 
             "Social_Media", 
             "Email", 
             "Phone", 
             "In-person", 
             "Other", 
             "Other_Description", 
             "Text", 
             "Voting_Intention", 
             "Voting_Intention_Other_Description"), 
           rep(1:100, each = 10)), 
    c("Additional_Voters", "NA."))


### Drop all duplicate responses from Voter_Contact_Form_Responses 
### (since there is a later, more up-to-date response that we should use). 
### I.e., only use the most recent response from each volunteer. 

#### re-order data in reverse chronological order (it is already ordered in chronological order)
Voter_Contact_Form_Responses <- 
  Voter_Contact_Form_Responses[nrow(Voter_Contact_Form_Responses):1, ]
#### Drop all duplicate recipient (volunteer) IDs. 
Voter_Contact_Form_Responses <- 
  Voter_Contact_Form_Responses[!duplicated(Voter_Contact_Form_Responses$RecipientID), ] # goes from 1,708 responses to 529

# -----------------------------------------------------------------
## We need to create a dataset for each contacted voter, with if/how they were contacted. 

### First, get the names of the voters who were assigned to a volunteer into a data frame. 

#### Drop all duplicate responses from Voter_Contact_Form_Names (note we just did this for Voter_Contact_Form_RESPONSES)
#### (since there is a later, more up-to-date list of voter names that we should use)
##### re-order data in reverse chronological order (it is already orderd in chronological order)
Voter_Contact_Form_Names <- 
  Voter_Contact_Form_Names[nrow(Voter_Contact_Form_Names):1, ]
##### Drop all duplicates (i.e. responses for which we have a later response that weakly dominates it in terms of informativeness)
Voter_Contact_Form_Names <- 
  Voter_Contact_Form_Names[!duplicated(Voter_Contact_Form_Names$RecipientID), ] # goes from 3,224 to 1,088

#### Loop through this list, creating a new row in our data frame for each new voter name. 
##### Note that this loop explicitly drops all volunteers who were not assigned any voters. 
##### In our case, the volunteers in the dataset "Voter_Contact_Form_Names" that were never assigned voters 
##### are those who never completed an address verification form. This gives us the figure of 
##### 1,051 volunteers who were assigned to voters. . 
k <- 0 
names_list <- list()
for (i in 1:nrow(Voter_Contact_Form_Names)) {
  for (j in 1:100) {
    if (!is.na(Voter_Contact_Form_Names[i, paste0("FirstName_", j)])) {
      k <- k + 1
      first_name <- Voter_Contact_Form_Names[i, paste0("FirstName_", j)]
      last_name <- Voter_Contact_Form_Names[i, paste0("LastName_", j)]
      recipient_id <- Voter_Contact_Form_Names[i, "RecipientID"]
      matrix <- matrix(c(first_name, last_name, recipient_id, j), nrow = 1, ncol = 4)
      dat <- as.data.frame(matrix)
      names(dat) <- c("First_Name", "Last_Name", "RecipientID", "ColumnNumber")
      names_list[[k]] <- dat
    } else {
      next
    }
  }
}

#### Combine list into data frame. 
Treated_Voters_Data <- do.call(rbind, names_list) # 5,068, not 5,070, since two voters were assigned over the 100 limit, so were never received by their volunteer. 
Treated_Voters_Data$RecipientID <- 
  as.character(Treated_Voters_Data$RecipientID)

#### Remove new objects from environment just to clear things up. 
rm(i, 
   j, 
   k, 
   first_name, 
   last_name, 
   recipient_id, 
   BinaryTurnout, 
   matrix, 
   dat, 
   names_list, 
   merges_by_name, 
   unsuccessful_merges_turnout, 
   unsuccessful_merges_voter_characteristics) 


### Record volunteer longitude and latitude: 
Volunteers_Qualtrics_Geocoordinates <- 
  Volunteers_Qualtrics[, c("RecipientID", 
                           "geocodedlon", 
                           "geocodedlat")]

names(Volunteers_Qualtrics_Geocoordinates) <- 
  c("RecipientID", 
    "VolunteerLongitude", 
    "VolunteerLatitude")

Treated_Voters_Data <- 
  merge(Treated_Voters_Data, 
        Volunteers_Qualtrics_Geocoordinates, 
        by = "RecipientID")

### Add additional characteristics: 
important_data_names <- 
  c("Postcard", 
    "Social_Media", 
    "Email", 
    "Phone", 
    "In-person",
    "Text", 
    "Other", 
    "Other_Description", 
    "Voting_Intention", 
    "Voting_Intention_Other_Description")

for (field in important_data_names) {
  Treated_Voters_Data[, field] <- NA
  for(i in 1:nrow(Treated_Voters_Data)) {
    temp <- Voter_Contact_Form_Responses[Voter_Contact_Form_Responses$RecipientID == Treated_Voters_Data[i, "RecipientID"], 
                                         paste0(field, Treated_Voters_Data[i, "ColumnNumber"])]
    if(length(temp == 1)) {
      Treated_Voters_Data[i, field] <- temp
    } else {
      Treated_Voters_Data[i, field] <- NA
    }
    
    
  }
}

### Remove new objects to clean up environment
rm(field, 
   i, 
   id, 
   temp, temp, 
   vol_lat, 
   vol_lon)

### Convert these additional characteristics into binary variables: 
Treated_Voters_Data$Postcard_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$Postcard), 1, 0)

Treated_Voters_Data$Social_Media_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$Social_Media), 1, 0)

Treated_Voters_Data$Email_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$Email), 1, 0)

Treated_Voters_Data$Phone_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$Phone), 1, 0)

Treated_Voters_Data$Text_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$Text), 1, 0)

Treated_Voters_Data$In_Person_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$"In-person"), 1, 0)

Treated_Voters_Data$Other_Binary <- 
  ifelse(!is.na(Treated_Voters_Data$Other), 1, 0)

Binary_Contact_Methods <- 
  c("Postcard_Binary",
    "Social_Media_Binary",
    "Email_Binary",
    "Phone_Binary",
    "Text_Binary",
    "In_Person_Binary",
    "Other_Binary")

Treated_Voters_Data$Any_Contact <- 
  ifelse(Treated_Voters_Data$Postcard_Binary == 1 |
           Treated_Voters_Data$Social_Media_Binary == 1 | 
           Treated_Voters_Data$Email_Binary == 1 | 
           Treated_Voters_Data$Phone_Binary == 1 | 
           Treated_Voters_Data$Text_Binary == 1 | 
           Treated_Voters_Data$In_Person_Binary == 1, 
         1, 0)
# ---------------------------------------------
## Now we need to merge this data back onto our main dataset. 


### Drop Duplicated Rows in Volunteer Voter Matches data 
Volunteer_Voter_Matches$DuplicateTest <- 
  paste0(Volunteer_Voter_Matches$RecipientID, 
         Volunteer_Voter_Matches$new_match_1_ID, 
         Volunteer_Voter_Matches$new_match_2_ID, 
         Volunteer_Voter_Matches$new_match_3_ID)

Volunteer_Voter_Matches <- 
  Volunteer_Voter_Matches[!duplicated(Volunteer_Voter_Matches$DuplicateTest), ] # drops 1,304 out of 3,071
### Convert Data to Long Format
Volunteer_Voter_Matches_Long <- list()
k <- 0
for (i in 1:nrow(Volunteer_Voter_Matches)) {
  match_one <- Volunteer_Voter_Matches[i, c("RecipientID", "new_match_1_ID", "runtime")]
  match_two <- Volunteer_Voter_Matches[i, c("RecipientID", "new_match_2_ID", "runtime")]
  match_three <- Volunteer_Voter_Matches[i, c("RecipientID", "new_match_3_ID", "runtime")]
  
  names(match_one) <- 
    c("RecipientID", "DWID", "Match_Date")
  names(match_two) <- 
    c("RecipientID", "DWID", "Match_Date")
  names(match_three) <- 
    c("RecipientID", "DWID", "Match_Date")
  
  Volunteer_Voter_Matches_Long[[k + 1]] <- match_one
  Volunteer_Voter_Matches_Long[[k + 2]] <- match_two
  Volunteer_Voter_Matches_Long[[k + 3]] <- match_three
  
  k <- k + 3
}

Volunteer_Voter_Matches_Long_Dataframe <- 
  do.call(rbind, Volunteer_Voter_Matches_Long)

### Drop all NAs 
Volunteer_Voter_Matches_Long_Dataframe <-
  Volunteer_Voter_Matches_Long_Dataframe[!is.na(Volunteer_Voter_Matches_Long_Dataframe$DWID), ]

### Clean Up Environment 
rm(match_one, 
   match_two, 
   match_three, 
   Volunteer_Voter_Matches_Long, 
   i, 
   k)

### Merge the CCNs back to the voter data. 
Volunteer_Voter_Matches_Long_Dataframe$i <- 1

Volunteer_Voter_Matches_Long_Dataframe$ColumnNumber <- 
  ave(Volunteer_Voter_Matches_Long_Dataframe$i, 
      Volunteer_Voter_Matches_Long_Dataframe$RecipientID, 
      FUN = seq_along) 

Treated_Voters_Data$ColumnNumber <-
  as.numeric(Treated_Voters_Data$ColumnNumber)

Treated_Voters_Data <- 
  merge(Treated_Voters_Data, 
        Volunteer_Voter_Matches_Long_Dataframe[, c("RecipientID", "ColumnNumber", "DWID")], 
        by = c("RecipientID", "ColumnNumber"))

### Now that we have the CCN, merge back the other characteristics from All_Merged_Data
orig.names <- 
  c("2017Turnout", 
    "BinaryTurnout", 
    "District", 
    "Treatment")

new.names <- 
  c("Turnout2017", 
    "BinaryTurnout", 
    "District", 
    "Treatment")

Treated_Voters_Data[, new.names] <- 
  All_Merged_Data[match(Treated_Voters_Data$DWID, All_Merged_Data$DWID), orig.names]


# -------------------------------------------------------------
# Find the control voter for each pair: 

districts <- 
  as.numeric(gsub('[^0-9]', '', names(table(Treated_Voters_Data$District))))

## Household data by district
## Bind it together into a large dataset. 

j <- 0
for (i in districts) {
  j <- j + 1
  file2 <- paste0("Pairs_Each_District/District_", i, "_pairs.rds")
  assign(paste0("pairs.", i), readRDS(file2))
  if (j == 1) {
    pairs_list <- get(paste0("pairs.", i))
  } else {
    pairs_list <- rbind(pairs_list, get(paste0("pairs.", i)))
  }
}

pairs_indID <- pairs_list[, c("pair.1.DWID", "pair.2.DWID")]
pairs_indID_MIRROR <- pairs_list[, c("pair.2.DWID", "pair.1.DWID")]
names(pairs_indID_MIRROR) <- names(pairs_indID)
list_of_all_pairs <- 
  rbind(pairs_indID, pairs_indID_MIRROR)
names(list_of_all_pairs) <- 
  c("BaseDWID", "PairDWID")

## Append the PairDWID onto our data: 
Treated_Voters_Data$PairDWID <- NA
for (i in 1:nrow(Treated_Voters_Data)) {
  base_dwid <- Treated_Voters_Data[i, "DWID"]
  pair_dwid <- list_of_all_pairs[list_of_all_pairs$BaseDWID == base_dwid, 
                                 "PairDWID"]
  Treated_Voters_Data[i, "PairDWID"] <- pair_dwid
}

## Create a separate control dataset:  
Control_Data <- 
  matrix(NA, nrow = nrow(Treated_Voters_Data), ncol = ncol(Treated_Voters_Data))
Control_Data <- 
  as.data.frame(Control_Data)

names(Control_Data) <- 
  names(Treated_Voters_Data)

for (i in 1:nrow(Treated_Voters_Data)) {
  control_dwid <- Treated_Voters_Data[i, "PairDWID"]
  Control_Data[i, "DWID"] <- control_dwid
  for (z in important_data_names) {
    Control_Data[, z] <- NA 
  }
  for (y in Binary_Contact_Methods) {
    Control_Data[, y] <- 0
  }
  Control_Data[, "Any_Contact"] <- 0 
  
  turnout_2017 <- 
    All_Merged_Data[All_Merged_Data$DWID == control_dwid, "2017Turnout"]
  binary_turnout <- 
    All_Merged_Data[All_Merged_Data$DWID == control_dwid, "BinaryTurnout"]
  district <- 
    All_Merged_Data[All_Merged_Data$DWID == control_dwid, "District"]
  treatment <- 
    All_Merged_Data[All_Merged_Data$DWID == control_dwid, "Treatment"]
  
  Control_Data[i, "Turnout2017"] <- 
    turnout_2017
  Control_Data[i, "BinaryTurnout"] <- 
    binary_turnout
  Control_Data[i, "District"] <- 
    district
  Control_Data[i, "Treatment"] <- 
    treatment
}

## Bind together this treatment and control data.
dat <- rbind(Treated_Voters_Data, Control_Data)

## Add the clustering variable for the pair level: 
### First, genrate the cluster variable in the pairs dataset. 
pairs_indID$Cluster <- 1:nrow(pairs_indID)

### Now pull it onto our dataset: 
dat$ClusterPair <- NA
for (i in 1:nrow(dat)) {
  dwid <- dat[i, "DWID"]
  if (dwid %in% pairs_indID$pair.1.DWID) {
    dat[i, "ClusterPair"] <- 
      pairs_indID[pairs_indID$pair.1.DWID == dwid, "Cluster"]
  } else if (dwid %in% pairs_indID$pair.2.DWID) {
    dat[i, "ClusterPair"] <- 
      pairs_indID[pairs_indID$pair.2.DWID == dwid, "Cluster"]
  } else {
    dat[i, "ClusterPair"] <- NA # zero cases of this
  }
}


## Add on multiple variables from the original data, which we will use to test the unconfoundedness assumption. 
Original_Catalist_Data$Male <- 
  ifelse(Original_Catalist_Data$GENDER == "male", 1, 0)

Original_Catalist_Data$Black <- 
  ifelse(Original_Catalist_Data$RACE == "black", 1, 0)

Original_Catalist_Data$Asian <- 
  ifelse(Original_Catalist_Data$RACE == "asian", 1, 0)

Original_Catalist_Data$Hispanic <- 
  ifelse(Original_Catalist_Data$RACE == "hispanic", 1, 0)


## Add on characteristics from the original Catalist Data

orig.names <- c("votepropensity2017G", "Partisanship", "AGE", "Male", "Black", "Hispanic", "Asian", "RACE", "GENDER")
new.names <- c("VotePropensity", "Partisanship", "Age", "Male", "Black", "Hispanic", "Asian", "RACE", "GENDER")
dat[, new.names] <- Original_Catalist_Data[match(dat$DWID, Original_Catalist_Data$DWID), orig.names]

## For the control, pull the volunteer coordinates and the methods used from the treatment data: 
for (i in 1:nrow(dat)) {
  if (dat[i, "Treatment"] == 1) { # only apply to control voters: 
    next 
  } else {
    dwid <- dat[i, "DWID"]
    dat[i, c(Binary_Contact_Methods, "VolunteerLatitude", "VolunteerLongitude")] <- 
      dat[!is.na(dat$PairDWID) & dat$PairDWID == dwid, c(Binary_Contact_Methods, "VolunteerLatitude", "VolunteerLongitude")]
  }
}

## Calculate Distances: 
c("lon", "lat")

dat[, c("lon", "lat")] <- 
  Geocoded_Voters[match(dat$DWID, Geocoded_Voters$DWID), c("lon", "lat")]

dat$DistanceToVolunteer <- 
  distHaversine(cbind(dat$lon, dat$lat), 
                cbind(dat$VolunteerLongitude, dat$VolunteerLatitude)) * 3.28084


other.var.names <- 
  c("Actual_Contact",
    "Effort_Made",                           
    "In_Person_Effort",
    "In_Person_Interacted_With_Someone_Else",
    "Left_Literature",                       
    "Phone_Effort", 
    "Phone_Interaction_With_Someon_Else",
    "Incorrect_Address",                     
    "Incorrect_Phone",
    "Voicemail",
    "Volunteer_Themselves",                  
    "Volunteer_FamilyMember", 
    "Nemesis")

dat <- merge(dat, 
             Other_Binary_Data[, c("DWID", other.var.names)], 
             all.x = T, 
             by = "DWID")

## Copy across data to control pair

treatment_pair_ids <- 
  dat[dat$Treatment == 1, "PairDWID"]

dat[dat$DWID %in% treatment_pair_ids, other.var.names] <- 
  dat[match(treatment_pair_ids, dat$PairDWID), other.var.names]

## Write out the dataset: 
setwd(output.dir)
write.csv(dat, "All_Treatment_Control_Data_Additional_Variables.csv")

# =========================================
# =========================================
# END OF CODE