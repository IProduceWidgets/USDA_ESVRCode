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
    # Environmental Assets
    `Water General` = if_else(
          (
            `Water General`   == 1 |
            `Fresh water`     == 1 |
            `Salt water`      == 1 |
            `Drinking water`  == 1 |
            `Ground water`    == 1 |
            Estuaries         == 1 |
            Canals            == 1
          ),
        1,0
      ),
    `Land General` = if_else(
          (
            `Land General`                  == 1 |
            Landscape                       == 1 |
            `Wetlands/constructed wetlands` == 1 |
            `Parks and open spaces`         == 1 |
            `Agricultural land`             == 1 |
            Beach                           == 1 |
            Soil                            == 1 |
            `Surface mining reclamation`    == 1
          ),
        1,0
      ),
    Animals = if_else(
          (
            Animals               == 1 |
            Fish                  == 1 |
            Mammals               == 1 |
            Birds                 == 1 |
            `Endangered species`  == 1 |
            Invertebrates         == 1
          ),
        1,0
      ),
    Plants = if_else(
          (
            Plants            == 1 |
            Forest            == 1 |
            `Trees or Plants` == 1 |
            Woodland          == 1 |
            Crops             == 1 |
            Riparian          == 1 |
            Rainforest        == 1 |
            Heather           == 1
          ),
        1,0
      ),
    Human = if_else(
          (
            Human           == 1 |
            `Human health`  == 1 |
            `Human capital` == 1 
          ),
        1,0
      ),
    `Air General` = if_else(
          (
            `Air General` == 1 |
            Local         == 1 |
            Regional      == 1 |
            Global        == 1
          ),
        1,0
      ),
    `Man-Made Environment / Infrastructure` = if_else(
          (
            `Man-Made Environment / Infrastructure` == 1 |
            `Other assets`                          == 1 |
            Buildings                               == 1 |
            `Flood control/dams`                    == 1 |
            `Cultural monuments`                    == 1 |
            `Weather information`                   == 1 
          ),
        1,0
      ),
    `Pro-Environmental Measures` = if_else( #Not in original Metadata.
          (
            `Eco-Labels and Certifications` == 1 |
            Offsets                         == 1
          ),
        1,0
      ),
    `Micro-organism` = if_else( # for some reason this isn't in General.environmental.assets
          (
            `Micro-organism`  == 1 |
            Fungi             == 1 |
            Bacteria          == 1 |
            Viruses           == 1
          ),
        1,0
      ),
    # Valuation Techniques
    `Stated Preference or Simulated Market Price` = if_else(
          (
            `Stated Preference or Simulated Market Price`             == 1 |
            `Contingent valuation - dichotomous choice (referendum)`  == 1 |
            `Contingent valuation - open ended`                       == 1 |
            `Choice experiment`                                       == 1 |
            `Contingent valuation - payment card`                     == 1 |
            `Contingent valuation - iterative bidding`                == 1 |
            `Conjoint analysis`                                       == 1 |
            `Combined revealed and stated preference`                 == 1 |
            `Contingent ranking`                                      == 1
          ),
        1,0
      ),
    `Revealed Preference` = if_else(
          (
            `Revealed Preference`                                   == 1 |
            `Hedonic property`                                      == 1 |
            `Travel cost method - single site`                      == 1 |
            `Travel cost method - multi-site - regional / hedonic`  == 1 |
            `Travel cost method - RUM`                              == 1 |
            `Averting behavior (preventing, defensive)`             == 1 |
            `Count data models`                                     == 1 |
            `Hedonic wage`                                          == 1 |
            `Life satisfaction`                                     == 1
          ),
        1,0
      ),
    `Actual Market Pricing Methods` = if_else(
          (
            `Actual Market Pricing Methods`             == 1 |
            `Actual expenditure/market price of output` == 1 |
            `Replacement costs`                         == 1 |
            `Change in productivity`                    == 1 |
            `Experimental cash market value`            == 1 |
            `Demand analysis`                           == 1
          ),
        1,0
      )
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
    #   returns true for any occurrence in the contexts (a logical 'OR').
    #   You could of course use two calls of str_detect to get an 'AND'.
    #   The 'NOT' operator (!) is also permitted, though it seems less useful.
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
    
 # Create your query here.   
    Primary == T &
    (Journal == T | `Report (government/non-government)` == T) &
    Animals == T &
    str_detect(
      paste0(
        Study.Title,
        Country,
        State...province,
        Location,
        Study.population.characteristics,
        Specific.environmental.goods..services.or.asset,
        Measure.of.environmental.change,
        Specific.environmental.stressor,
        Study.methodology.description,
        Information.on.the.valuation.equation.function,
        Estimated.values,
        Discount.rate,
        Abstract..English.,
        Abstract..French.
      ),
      "(?i)Wolf|Wolves"
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

