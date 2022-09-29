#### This script should be called by EVRI_Sorter.R, which cleans the datafiles
# and passes them to this script.
# This script contains the queries for the gap analysis write-up. (Sept. 2022)
#
# While I could probably make it more compact with a for loop, I've chosen to do
# all the queries by hand so they can serve as examples for future inquiries.

#### functions for the summaries so they are nice and compact. ####

Analysis_Results <- function(DATA){# This is not a robust function.
  # This function will only work on the QueryOut dataframe it expects.
  Results <- QueryOut %>%
    transmute(
      Author.s.,
      Study.Title,
      Study.source,
      Publication.year,
      Document.type,
      Years.of.data
    )
  return(Results)
}

Analysis_Summary <- function(DATA){ # This is not a robust function.
  # This function will only work on the QueryOut dataframe it expects.
  Summary <- bind_rows(
    DATA %>%
      summarise(
        Studies = dplyr::n(),
        PubYearMax = max(Publication.year),
        PubYearMin = min(Publication.year),
        DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
        DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
        Countries = toString(sort(MessyStringLevels(., "Country")))
      ),
    DATA %>%
      rowwise() %>%
      filter( # This is a filter for each of the 38 OECD nation as of Sept. 2022
        max(!!!syms(OECD)) == 1
      ) %>%
      ungroup() %>%
      summarise(
        Studies = dplyr::n(),
        PubYearMax = max(Publication.year),
        PubYearMin = min(Publication.year),
        DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
        DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
        Countries = "OECD Nations (2022)"
      ),
    DATA %>%
      filter(`United States` == 1) %>%
      summarise(
        Studies = dplyr::n(),
        PubYearMax = max(Publication.year),
        PubYearMin = min(Publication.year),
        DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
        DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
        Countries = "United States"
      )
  )
  return(Summary)
}  

## A function that makes one giant dataframe from the queries for plotting.

PivotYear <- function(QUERY){
  QUERY %>%
    pivot_longer(cols = matches("[1-2][0-9][0-9][0-9]"), names_to = "Year") %>%
    filter(value == 1) %>%
    select(-value) %>%
    mutate(Year = as.numeric(Year))
}

##### EVRI Top-Level Statistics ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
    (Journal == T | `Report (government/non-government)` == T)
    
    #### This is the end of the query
  )

QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Top_Level_Publications.xlsx")
           )

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Top_level_Summary.xlsx")
           )

CollectedQueries <- PivotYear(QueryOut) %>%
  mutate(Q = "Top_level",
         Pubs = length(unique(InternalID))
  )


##### Animals, “Habitat|Habitats|Wildlife” ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
    (Journal == T | `Report (government/non-government)` == T) &
    #Animals == T &
    
    # This is the Keyword/regex search
    str_detect(
      paste0(!!!syms(AllContextFields)),
      "(?i)(Habitat)|(Habitats)|(Wildlife)"
    )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Wildlife_Habitat_Publications.xlsx")
           )

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Wildlife_Habitat_Summary.xlsx")
           )

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Wildlife_Habitat",
             Pubs = length(unique(InternalID))
      )
  )

##### Animals, Birds ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
    (Journal == T | `Report (government/non-government)` == T) &
    Animals == T &
    Birds == T
      
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Birds_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Birds_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Birds",
             Pubs = length(unique(InternalID))
      )
  )

##### Animals, “Insect|Insects|Bees|Honeybees|Butterflies|Butterfly” #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      Animals == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Insect|Insects|Bees|Honeybees|Butterflies|Butterfly)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Insects_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Insects_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Insects",
             Pubs = length(unique(InternalID))
      )
  )

##### Water general #### 


QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Water General` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Water_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Water_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Water",
             Pubs = length(unique(InternalID))
      )
  )

##### Water general, “Water quality” #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Water General` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Water quality)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Water_Quality_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Water_Quality_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Water_Quality",
             Pubs = length(unique(InternalID))
      )
  )

##### "Aquatic&(Habitat|Habitats)” #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)Aquatic"
      ) &
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Habitat|Habitats)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Aquatic_Habitat_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Aquatic_Habitat_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Aquatic_Habitat",
             Pubs = length(unique(InternalID))
      )
  )

##### Air General #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Air General` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Air_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Air_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Air",
             Pubs = length(unique(InternalID))
      )
  )

##### Air General & "Air quality" #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Air General` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Air quality)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Air_Quality_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Air_Quality_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Air_Quality",
             Pubs = length(unique(InternalID))
      )
  )

##### Air General & "(Green House Gas)|(Green House)|(GHG)" #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Air General` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Green House Gas)|(Green House)|(GHG)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "GHG_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "GHG_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "GHG",
             Pubs = length(unique(InternalID))
      )
  )

#### Other Queries ####
  
  ##### Animal General #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Animals` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Animal_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Animal_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Animal",
             Pubs = length(unique(InternalID))
      )
  )

  ##### Land General #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Land General` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Land_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Land_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Land",
             Pubs = length(unique(InternalID))
      )
  )

  ##### Duck #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Animals` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Duck)|(Ducks)|(Drake)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Duck_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Duck_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Ducks",
             Pubs = length(unique(InternalID))
      )
  )

  ##### Horse #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Animals` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Horse)|(pony)|(Stallion)|(Ponies)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Horse_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Horse_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Horses",
             Pubs = length(unique(InternalID))
      )
  )

  ##### Wolf #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Animals` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Wolf)|(Wolves)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Wolf_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Wolf_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Wolves",
             Pubs = length(unique(InternalID))
      )
  )

  ##### Deer #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Animals` == T &
      
      # This is the Keyword/regex search
      str_detect(
        paste0(!!!syms(AllContextFields)),
        "(?i)(Deer)|(Doe)"
      )
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Deer_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Deer_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Deer",
             Pubs = length(unique(InternalID))
      )
  )

##### Landscape ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Land General` == T &
      Landscape == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Landscape_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Landscape_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Landscape",
             Pubs = length(unique(InternalID))
      )
  )

##### Wetlands ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Land General` == T &
      `Wetlands/constructed wetlands` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Wetlands_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Wetlands_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Wetlands",
             Pubs = length(unique(InternalID))
      )
  )

##### Open Spaces ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Land General` == T &
      `Parks and open spaces` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Open_Spaces_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Open_Spaces_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Open_Spaces",
             Pubs = length(unique(InternalID))
      )
  )

##### Ag land ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Land General` == T &
      `Agricultural land` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Ag_land_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Ag_land_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Ag_land",
             Pubs = length(unique(InternalID))
      )
  )

##### Beaches ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Land General` == T &
      `Beach` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Beaches_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Beaches_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Beaches",
             Pubs = length(unique(InternalID))
      )
  )

##### Plants #### 

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Plants` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Plants_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Plants_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Plants",
             Pubs = length(unique(InternalID))
             )
  )

################# Econ Measures ##################
#### Stated preference

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Stated Preference or Simulated Market Price` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Stated_Pref_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Stated_Pref_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Stated_Pref",
             Pubs = length(unique(InternalID))
      )
  )

#### Revealed Preference

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Revealed Preference` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Revealed_Pref_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Revealed_Pref_Summary.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Revealed_Pref",
             Pubs = length(unique(InternalID))
      )
  )

#### Market_Pricing

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
      (Journal == T | `Report (government/non-government)` == T) &
      `Actual Market Pricing Methods` == T
    
    #### This is the end of the query
  )


QueryResults <- Analysis_Results(QueryOut)
write_xlsx(QueryResults,
           file.path(OutputDirectory, "Market_Pricing_Publications.xlsx")
)

QuerySummary <- Analysis_Summary(QueryOut)
write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Market_Pricing.xlsx")
)

CollectedQueries <- CollectedQueries %>%
  bind_rows(
    PivotYear(QueryOut) %>%
      mutate(Q = "Market_Pricing",
             Pubs = length(unique(InternalID))
      )
  )

#### Plots ####
# This makes a bunch of plots using the collected queries, indexing the
# different searches by the Q variable.
source("Plots.R")
