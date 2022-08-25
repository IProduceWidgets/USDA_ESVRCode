## THIS CODE IS FOR PULLING CUSTOM QUERIES FROM THE EVRI DATABASE ##
#### Libraries ####
require(rlang)
require(stringi)
require(data.table)
require(tidyr)
require(dplyr)
require(stringr)

#### Functions ####

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
  #return(cbind(DATA, mat))
  
  #Return the new dummy variables
  return(as.data.frame(mat))
  
}


## The function CleanMessyStrings is a wrapper for calling ColToDummy for
# many variables, with an option to drop or keep the source variable.

CleanMessyStrings <- function(DATA, variables, KeepMessy = T) {
  TempDF <- DATA
  
  for (i in 1:length(variables)) {
    Temp <- ColToDummy(DATA, variables[i])
    TempDF <- cbind(TempDF, Temp)
  }
  
  return(
    if(KeepMessy == T) TempDF else TempDF %>% select(-all_of(variables))
    )
}

## Useful Variable Collections ####

# Contains all messy string variables from the input DATA.
# These are facets and should be excluded from keyword searches.
AllMessyStrings <- c(
  'Country',
  'General.environmental.assets',
  'General.type.of.goods.and.services.valued',
  'Environmental.stressor',
  'Study.type',
  'Available.information.within.study',
  'Years.of.data',
  'Economic.measure.s.',
  'Valuation.technique.s.',
  'Document.type' # this is not actually messy, but it is a facet variable.
)

# Contains all publication info.
PublicationInfo <- c(
  'Author.s.',
  'Study.Title',
  'Study.source',
  'Publication.year',
  'Document.type'
)

# Contains all context fields (mostly paragraphs of text).
AllContextFields <- c(
  'Study.Title',
  'Country',
  'State...province',
  'Location',
  'Study.population.characteristics',
  'Specific.environmental.goods..services.or.asset',
  'Measure.of.environmental.change',
  'Specific.environmental.stressor',
  'Study.methodology.description',
  'Information.on.the.valuation.equation.function',
  'Estimated.values',
  'Discount.rate',
  'Abstract..English.',
  'Abstract..French.'
)

#### HOW TO USE THIS SCRIPT: ####

# 1.
### Set the directory where the datafiles are stored.
# At the moment this requires multiple (at least 2) files in .csv format.
# This is because for some reason the EVRI data can't be downloaded in a single
# batch, and requires multiple downloads. I simply downloaded each region
# individually, and have collected the data within R here, being sure to drop
# duplicate entries. This should give us the whole EVRI database. You could
# Instead include only a subset of the database if you wanted for some reason.

DataDirectory <- 'C:/Users/avery/Desktop/Research/USDA_ESV/EVRI'

#2.
### Construct your search in the "Your Query" section.

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

## Collapses all the files from Filelist into a single dataframe if necessary.
DATA <- if (length(Filelist > 1)) {
    lapply(
      Filelist,
      FUN=read.csv,
      header = T,
      sep = ",",
      quote = "\""
    ) %>%
    do.call("rbind", .) %>%
    unique()
  } else {
    read.csv(
      Filelist, 
      header = T,
      sep = ",",
      quote = "\"")
  }
  
## Creates a dataframe with the messy string vars expanded into dummy columns
DATAexpanded <- DATA %>%
  CleanMessyStrings(AllMessyStrings)
# For reasons beyond comprehension, the facet 'Human health' shows up in both 
# General.environmental.assets and in General.type.of.goods.and.services.valued
# They do not however have the same values for all entries. Yikes.
# This fixes that issue, by appending a number to any subsequent entry of a
# non-unique variable name. 
# In this case 'Human health' in General.type.of.goods.and.services.valued
# becomes 'Human health.1'
DATAexpanded <- DATAexpanded[,c(1:length(DATAexpanded))]


  #### Recoding Tiered Facets ####
## Some values should be tiered, but are not.
# i.e. to get all "Animal" studies you need to include all of:
# "Fish", "Mammals", "Endangered species", "Birds", "Invertebrates", & "Animals"
# This can be fixed by mutating the dummies for the higher tiers to be 1 if any
# of their sub-tiers are 1. This has to be done by hand though:
DATAexpanded <- DATAexpanded %>%
  mutate(
    
  )

#### Your Query ####

## Here is a list of EVRI variables which appear to be messy strings: ###
# It seems they never repeat level names, so we dont have to handle duplicates.
# Available levels are variable names in DATAexpanded coded as dummies.
#
# use MessyStringLevels(DATA, "variable") to see available levels.
#
# Checked Aug. 20, 2022. on full EVRI dataset
#
# Variable                                    # of Levels, notable variables
#___________________________________________|___________________________________
# Country                                   : (176 unique)
# General.environmental.assets              : (47 unique, 'Other assets', 'Human health')
# General.type.of.goods.and.services.valued : (6 unique, 'Human health.1')
# Environmental.stressor                    : (14 unique)
# Study.type                                : (3 unique)
# Available.information.within.study        : (7 unique)
# Years.of.data                             : (92 unique)
# Economic.measure.s.                       : (10 unique, 'Other')
# Valuation.technique.s.                    : (24 unique)
# Document.type                             : (8 unique)

### Construct Your Query
# see 'Useful Variable Collections' for some shortcuts.

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    ##### Messy String / Facet operators help ####
    # Facets from the messy string variables can be filtered for by their new
    # variable names as logical operators:
    #
    # Ex:
    # Study.type has 3 levels; 
    # "Primary", "Secondary/benefits transfers", & "Meta/synthesis analysis"
    # 
    # Primary == T  # will provide only primary studies and exclude Secondary or
    #               # Meta studies.
    # 
    # Logical operators can be used to create very specific queries.
    # (Variables with spaces can be indexed with backticks)
    #
    # Primary == T & (`Willingness to pay` == T | `Willingness to accept`)
    #               # This will provide all primary studies on WTP or WTA.
    #
    #
    ##### Keyword Searches help ####
    # Context fields (or any variable) can be searched over for keywords via 
    # regular expressions in stringr::str_detect
    #
    # Ex:
    # Abstract..English. contains a short description of each study.
    #
    # str_detect(Abstract..English., "Birds|Bees")
    #               # This will provide any study which contains
    #               # 'Birds' or 'Bees' 
    #
    # str_detect(Abstract..English., "(?i)Birds|Bees")
    #               # This does the same as before, but uses a regex modifier to
    #               # to make the search case insensitive. 
    #
    #
    # Facet Operators and Keyword searches can of course be used together for
    # even more refined queries.
    #
    # Primary == T & str_detect(Abstract..English., "(?i)Birds|Bees")
    #
    #
    # Multiple context field variables can be searched over by passing them to
    # str_detect concatenated.
    #
    # Primary == T & str_detect(
    #   paste0(
    #     Abstract..English.,
    #     Study.methodology.description,
    #     Measure.of.environmental.change
    #     ),
    #   "(?i)Birds|Bees"
    #   )
    #
    # Here is a search over all* context fields:
    #
    # Primary == T & str_detect(
    #   paste0(
    #     Study.Title,
    #     Country,
    #     State...province,
    #     Location,
    #     Study.population.characteristics,
    #     Specific.environmental.goods..services.or.asset,
    #     Measure.of.environmental.change,
    #     Specific.environmental.stressor,
    #     Study.methodology.description,
    #     Information.on.the.valuation.equation.function,
    #     Estimated.values,
    #     Discount.rate,
    #     Abstract..English.,
    #     Abstract..French.
    #     ),
    #   "(?i)Birds|Bees"
    #   )
    #
    #####
    
    # Primary == T & str_detect(
    #   paste0(
    #     Study.Title,
    #     Country,
    #     State...province,
    #     Location,
    #     Study.population.characteristics,
    #     Specific.environmental.goods..services.or.asset,
    #     Measure.of.environmental.change,
    #     Specific.environmental.stressor,
    #     Study.methodology.description,
    #     Information.on.the.valuation.equation.function,
    #     Estimated.values,
    #     Discount.rate,
    #     Abstract..English.,
    #     Abstract..French.
    #   ),
    #   "(?i)Birds|Bees"
    # )
    
    Primary == T & 
    Journal == T &
    (
      Fish == T | 
      Mammals == T | 
      `Endangered species` == T | 
      Birds == T | 
      Invertebrates == T |
      Animals == T
    )
    
    ####
  )

#### Summary and Table Outputs ####


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

