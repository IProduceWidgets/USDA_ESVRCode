library(ggplot2)

lengthUnique <- function(x){
  return(c(y = median(x)+2, label = length(x)))
}

print(
CollectedQueries %>%
  filter(Q %in% c("Air", "Water", "Land", "Animal", "Plants")) %>%
  ggplot(aes(y=Year, x=Q))+
    
    geom_boxplot(color = "Blue",
                 fill = "dark grey",
                 width = .5)+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.9,
                width = 1.21
                )+
    stat_summary(fun.data = lengthUnique,
                 geom = "text",
                 fun = median
                 )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")+
    #theme_grey()
    labs(title = "World Scope: (# obs)",
         x = "Densities: Data-Years x Publication")
    # annotate("text",
    #          x = 1:length(Q),
    #          y = aggregate(length(unique(InternalID)) ~ Q, FUN=median)
    #          )
)

print(
CollectedQueries %>%
  rowwise() %>%
  filter( # This is a filter for each of the 38 OECD nation as of Sept. 2022
    max(!!!syms(OECD)) == 1
  ) %>%
  ungroup() %>%
  filter(Q %in% c("Air", "Water", "Land", "Animal", "Plants")) %>%
  ggplot(aes(y=Year, x=Q))+
  
  geom_boxplot(color = "Blue",
               fill = "dark grey",
               width = 0.5)+
  geom_violin(scale="count",
              color = NA,
              fill = "light blue",
              alpha=.8,
              width = 1.3
  )+
  stat_summary(fun.data = lengthUnique,
               geom = "text",
               fun = median
  )+
  #geom_line()+
  geom_hline(yintercept=2022, color="Red")+
  #theme_grey()
  labs(title = "OECD Nations: (# obs)",
       x = "Densities: Data-Years x Publication")
)

print(
CollectedQueries %>%
  filter(`United States` == 1) %>%
  filter(Q %in% c("Air", "Water", "Land", "Animal", "Plants")) %>%
  ggplot(aes(y=Year, x=Q))+
  
  geom_boxplot(color = "Blue",
               fill = "dark grey",
               width = 0.5)+
  geom_violin(scale="count",
              color = NA,
              fill = "light blue",
              alpha=.8,
              width = 1.5
  )+
  stat_summary(fun.data = lengthUnique,
               geom = "text",
               fun = median
  )+
  #geom_line()+
  geom_hline(yintercept=2022, color="Red")+
  #theme_grey()
  labs(title = "USA Only: (# obs)",
       x = "Densities: Data-Years x Publication")
)

# print(
#   CollectedQueries %>%
#     filter(`United States` == 1) %>%
#     filter(Q != "Top_level") %>%
#     ggplot(aes(y=Year, x=Q))+
#     
#     geom_boxplot(color = "Blue", fill = "dark grey")+
#     geom_violin(scale="area",
#                 color = NA,
#                 fill = "light blue",
#                 alpha=.8,
#                 width = 1
#     )+
#     geom_dotplot(binaxis ='y', stackdir = 'center', dotsize = .4, binwidth=1)+
#     #geom_line()+
#     geom_hline(yintercept=2022, color="Red")
#   #theme_grey()
# )

######################################################

print(
  CollectedQueries %>%
    rowwise() %>%
    filter( # This is a filter for each of the 38 OECD nation as of Sept. 2022
      max(!!!syms(OECD)) == 1
    ) %>%
    ungroup() %>%
    filter(Q %in% c("Wildlife_Habitat", "Birds","Insects", "Water_Quality", "Aquatic_Habitat","Air_Quality", "GHG")) %>%
    ggplot(aes(y=Year, x=Q))+
    
    geom_boxplot(color = "Blue",
                 fill = "dark grey",
                 width = 0.5)+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.9,
                width = 1.27
    )+
    stat_summary(fun.data = lengthUnique,
                 geom = "text",
                 fun = median
    )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")+
    #theme_grey()
    labs(title = "OECD Nations: (# obs)",
         subtitle = "Narrower Categories",
         x = "Densities: Data-Years x Publication")
)

print(
  CollectedQueries %>%
    filter(`United States` == T) %>%
    filter(Q %in% c("Wildlife_Habitat", "Birds","Insects", "Water_Quality", "Aquatic_Habitat","Air_Quality", "GHG")) %>%
    ggplot(aes(y=Year, x=Q))+
    
    geom_boxplot(color = "Blue",
                 fill = "dark grey",
                 width = 0.5)+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.9,
                width = 1.27
    )+
    stat_summary(fun.data = lengthUnique,
                 geom = "text",
                 fun = median
    )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")+
    #theme_grey()
    labs(title = "USA Only: (# obs)",
         subtitle = "Narrower Categories",
         x = "Densities: Data-Years x Publication")
)

######################################################

print(
  CollectedQueries %>%
    filter(`United States` == T) %>%
    filter(Q %in% c("Landscape", "Wetlands","Open_Spaces", "Beachs", "Ag_land")) %>%
    ggplot(aes(y=Year, x=Q))+
    
    geom_boxplot(color = "Blue",
                 fill = "dark grey",
                 width = 0.5)+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.9,
                width = 1.27
    )+
    stat_summary(fun.data = lengthUnique,
                 geom = "text",
                 fun = median
    )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")+
    #theme_grey()
    labs(title = "USA Only: (# obs)",
         subtitle = "Land General",
         x = "Densities: Data-Years x Publication")
)



#######################################################



print(
  CollectedQueries %>%
    filter(Q %in% c("Ducks", "Horses", "Wolves", "Deer", "Insects")) %>%
    ggplot(aes(y=Year, x=Q))+
    
    geom_boxplot(color = "Blue",
                 fill = "dark grey",
                 width = 0.5)+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.9,
                width = 2.2
    )+
    stat_summary(fun.data = lengthUnique,
                 geom = "text",
                 fun = median
    )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")+
    #theme_grey()
    labs(title = "World Scope: (# obs)",
         suntitle = "Animals",
         x = "Densities: Data-Years x Publication")
)

##################################################

print(
  CollectedQueries %>%
    filter(`United States` == T) %>%
    filter(Q %in% c("Stated_Pref", "Revealed_Pref", "Market_Pricing")) %>%
    ggplot(aes(y=Year, x=Q))+
    geom_boxplot(color = "Blue",
                 fill = "dark grey",
                 width = 0.5)+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.9,
                width = 1.2
    )+
    stat_summary(fun.data = lengthUnique,
                 geom = "text",
                 fun = median
    )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")+
    #theme_grey()
    labs(title = "USA Only: (# obs)",
         subtitle = "Valuation Techniques",
         x = "Densities: Data-Years x Publication")
)
