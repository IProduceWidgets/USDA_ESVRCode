#### This script should be called by EVRI_Sorter.R, which cleans the datafiles
# and passes them to this script.
# This script contains the queries for the gap analysis write-up. (Sept. 2022)
#
# While I could probably make it more compact with a for loop, I've chosen to do
# all the queries by hand so they can serve as examples for future inquiries.

##### EVRI Top-Level Statistics ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
    (Journal == T | `Report (government/non-government)` == T)
    
    #### This is the end of the query
  )

QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Top_Level_Publications.xlsx")
           )

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Top_level_Summary.xlsx")
           )


##### Animals, “Habitat|Habitats|Wildlife” ####

QueryOut <- DATAexpanded %>%
  dplyr::filter(
    
    # This is facet binary operators
    Primary == T &
    (Journal == T | `Report (government/non-government)` == T) &
    Animals == T &
    
    # This is the Keyword/regex search
    str_detect(
      paste0(!!!syms(AllContextFields)),
      "(?i)(Habitat)|(Habitats)|(Wildlife)"
    )
    
    #### This is the end of the query
  )


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Wildlife_Habitat_Publications.xlsx")
           )

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Wildlife_Habitat_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Birds_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Birds_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Insects_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Insects_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Water_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Water_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Water_Quality_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Water_Quality_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Aquatic_Habitat_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Aquatic_Habitat_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Air_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Air_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "Air_Quality_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "Air_Quality_Summary.xlsx")
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


QueryResults <- QueryOut %>%
  transmute(
    Author.s.,
    Study.Title,
    Study.source,
    Publication.year,
    Document.type,
    Years.of.data
  )

write_xlsx(QueryResults,
           file.path(OutputDirectory, "GHG_Publications.xlsx")
)

QuerySummary <- QueryOut %>%
  summarise(
    Studies = dplyr::n(),
    PubYearMax = max(Publication.year),
    PubYearMin = min(Publication.year),
    DataYearMax = max(MessyStringLevels(.,"Years.of.data")),
    DataYearMin = min(MessyStringLevels(.,"Years.of.data")),
    Countries = toString(MessyStringLevels(., "Country"))
  )

write_xlsx(QuerySummary,
           file.path(OutputDirectory, "GHG_Summary.xlsx")
)