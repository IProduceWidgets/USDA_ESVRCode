## THIS CODE IS FOR PULLING CUSTOM QUERIES FROM THE EVRI DATABASE ##

#### HOW TO USE THIS SCRIPT: ####

### Set the directory where the datafiles are stored.
# At the moment this requires multiple (at least 2) files in .csv format.
# This is because for some reason the EVRI data can't be downloaded in a single
# batch, and requires multiple downloads. I simply downloaded each region
# individually, and have collected the data within R here, being sure to drop
# duplicate entries. This should give us the whole EVRI database. You could
# Instead include only a subset of the database if you wanted for some reason.

DataDirectory <- 'C:/Users/avery/Desktop/Research/USDA_ESV/EVRI'

#### Libraries / functions ####
require(stringi)
require(data.table)
require(tidyr)
require(dplyr)

## The function MessyStringLevels returns all levels of the messy string
# variable which is passed to it (with messy values seperated by '|').
# Use this to check for conflicts in different variables / to understand where
# which EVRI facets the new varibles come from.
# !! the variable must be passed as a character, i.e. put it in quotes.
#
# example use w/ debug data:
# MessyStringLevels(TestData, "messy_string")

MessyStringLevels <- function(DATA, variable) {
  return(unique(unlist(strsplit(unlist(DATA[,variable]), split="\\|"))))
}


## The function ColToDummy parses a character "variable" from a dataframe DATA 
# when there are multiple relevant strings separated by the '|' symbol. It then
# appends new dummy columns to DATA named for the parsed strings, where a value
# of 1 indicates the string appears in "variable" and 0 indicates it does not.
# This function can only handle a single variable at a time, and must be called
# individually for each messy column in DATA.
# !! the variable must be passed as a character, i.e. put it in quotes.
#
# example use w/ debug data: 
# A <- ColToDummy(TestData, "messy_string")

ColToDummy <- function(DATA, variable) {
  #sanitize variable to character values
  #DATA[,variable] <- as.character(DATA[,variable])
  
  #Get list of column names in result
  resCol <- unique(unlist(strsplit(unlist(DATA[,variable]), split="\\|")))
  
  #Get dimension of result
  nresCol <- length(resCol)
  nresRow <- nrow(DATA)
  
  #Create empty matrix with dimensions same as desired result
  mat <- matrix(rep(0, nresRow * nresCol), nrow = nresRow, dimnames = list(as.character(1:nresRow), resCol))
  
  #split each messy string by '|'
  ll <- strsplit(unlist(DATA[,variable]), split="\\|")
  
  #Get coordinates of mat which we need to set to 1
  coords <- do.call(rbind, lapply(1:length(ll), function(i) cbind(rep(i, length(ll[[i]])), ll[[i]] )))
  
  #Set mat to 1 at appropriate coordinates
  mat[coords] <- 1    
  
  #Bind the mat to original data.table
  return(cbind(DATA, mat))
  
}


## The function CleanMessyStrings is a wrapper for calling ColToDummy for
# many variables, with an option to drop or keep the source variable.

CleanMessyStrings <- function(DATA, variables, KeepMessy = T) {
  TempDF <- DATA
  
  for (i in length(variables)) {
    TempDF <- ColToDummy(DATA, variables[i])
  }
  
  return(
    if(KeepMessy == T) TempDF else TempDF %>% select(-variables)
    )
}

#### Data Read-In ####
# This will read every file of the type csv in the directory given.
# If you need to run a subsample of the EVRI data files you can either
# make a new directory to store the files, or index the files with either a 
# file name list or a more sophisticated Regex. Changing directories is probably
# the simplest solution though. 

## This is set in the 'HOW TO USE THIS SCRIPT:' header.
# DataDirectory <- 'C:/Users/avery/Desktop/Research/USDA_ESV/EVRI'

## Collects all the file directories into a list.
Filelist <- list.files(
  full.names = T,
  path = DataDirectory,
  pattern = "\\.csv"
)

## Collapses all the files from Filelist into a single dataframe.
DATA <- lapply(
  Filelist,
  FUN=read.csv,
  header = T,
  sep = ",",
  quote = "\""
  ) %>%
  do.call("rbind", .) %>%
  unique()

#### Here is a list of EVRI variables which appear to be messy strings: ####
## It seems they never repeat level names, so we dont have to handle duplicates.
## use MessyStringLevels(DATA, "variable") to see available levels.
#
# Checked Aug. 20, 2022. on full EVRI dataset
#
# Country                                   : (176 unique)
# General.environmental.assets              : (47 unique, 'Other assets')
# General.type.of.goods.and.services.valued : (6 unique)
# Environmental.stressor                    : (14 unique)
# Study.type                                : (3 unique)
# Available.information.within.study        : (7 unique)
# Years.of.data                             : (92 unique)
# Economic.measure.s.                       : (10 unique, 'Other')
# Valuation.technique.s.                    : (24 unique)

A <- ColToDummy(DATA, "General.environmental.assets")

####
# Debug stuff #####

## generates a simplified test dataset
testN <- 10 # change this to get a larger test dataframe. 1000 works well.
TestData <- data.table(ID = c(1:testN), messy_string = "") %>%
  rowwise %>%
  transmute(
    ID,
    messy_string = paste(
      stri_rand_strings(sample(1:5, 1, replace=TRUE), 2, pattern = "[a-z]"),
      collapse="|"
    )
  ) %>%
  ungroup()

