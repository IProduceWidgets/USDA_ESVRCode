library(ggplot2)

print(
CollectedQueries %>%
  filter(Q != "Top_level") %>%
  ggplot(aes(y=Year, x=Q))+
    
    geom_boxplot(color = "Blue", fill = "dark grey")+
    geom_violin(scale="count",
                color = NA,
                fill = "light blue",
                alpha=.8,
                width = 2.2
                )+
    #geom_line()+
    geom_hline(yintercept=2022, color="Red")
    #theme_grey()
)

print(
CollectedQueries %>%
  rowwise() %>%
  filter( # This is a filter for each of the 38 OECD nation as of Sept. 2022
    max(!!!syms(OECD)) == 1
  ) %>%
  ungroup() %>%
  filter(Q != "Top_level") %>%
  ggplot(aes(y=Year, x=Q))+
  
  geom_boxplot(color = "Blue", fill = "dark grey")+
  geom_violin(scale="count",
              color = NA,
              fill = "light blue",
              alpha=.8,
              width = 2.2
  )+
  #geom_line()+
  geom_hline(yintercept=2022, color="Red")
#theme_grey()
)

print(
CollectedQueries %>%
  filter(`United States` == 1) %>%
  filter(Q != "Top_level") %>%
  ggplot(aes(y=Year, x=Q))+
  
  geom_boxplot(color = "Blue", fill = "dark grey")+
  geom_violin(scale="count",
              color = NA,
              fill = "light blue",
              alpha=.8,
              width = 2.2
  )+
  #geom_line()+
  geom_hline(yintercept=2022, color="Red")
#theme_grey()
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
