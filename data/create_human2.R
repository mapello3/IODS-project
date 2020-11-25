#Exercise 5. Dimensionality reduction
#Author: Marina Peris
#Date: 25.11.2020

#The dataset used for this exercise can be found here: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt

##Data wrangling

  human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

  dim(human) #The dataset contains 195 observations and 19 variables
  str(human) #There are mainly numeric variables but some are type character (i.e. Country, GNI)
  
  #The dataset consists of records of variables used to calculate the Human Development Index in the 195 countries in the world, such as Life expectancy, years of schooling, etc.
  
  
  library(stringr)
  library(dplyr)

  GNI <- str_replace(human$GNI, pattern=",", replace ="")
  human$GNI <- GNI%>%as.numeric() #Transforming GNI to numeric
  class(human$GNI)
  human <- mutate(human, GNI)
  
  
  keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
  human <- select(human, one_of(keep)) #Excluding unneeded variables
  
  
  data.frame(human[-1], comp = complete.cases(human)) #New column indicating if there are NAs (FALSE) in each row
  human_ <- filter(human, complete.cases(human))  #Keep only complete cases (TRUE = no NAs)  

  
  last <- nrow(human_) - 7
  human_ <- human_[1:last, ] #Removing observations related to regions
  dim(human_) #155 observations and 9 variables
  
  
  rownames(human_) <- human_$Country #Defining the row names of the data by the country names
  human_$Country <- NULL #Removing the country name column from the data
  dim(human_) #155 observations and 8 variables
  
  
  write.csv(human_, "human_.csv", row.names = TRUE) #Saving the data
  check_data <- read.csv("./human_.csv")
  
  
  
##Analysis
  
  human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt", sep  =",", header = T)

  summary(human)
  
  library(ggplot2)  
  library(GGally)
  
  p <- ggpairs(human, lower = list(combo = wrap("facethist", bins = 20)))
  
  require(corrplot)
  require(tidyverse)
  cor_matrix<-cor(human) %>% round(digits=2)
  corrplot(cor_matrix, method="circle")
  
  
  pca_human <- prcomp(human)
  biplot(pca_human, choices = 1:2, cex = c(0.8, 1), col = c("grey40", "deeppink2"))
  
  human_std <- scale(human)
  pca_humanstd <- prcomp(human_std)
  summary(pca_humanstd)
  biplot(pca_humanstd, choices = 1:2, cex = c(0.8, 1), col = c("grey40", "deeppink2"))

  require(factoextra)
  
  fviz_eig(pca_humanstd)
  fviz_pca_ind(pca_humanstd,
               col.ind = "cos2", # Color by the quality of representation
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )

  
  install.packages("FactoMineR")
  require(FactoMineR)
  data(tea)
  dim(tea)
  str(tea)
  
  
  keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")
  tea_time <- select(tea, one_of(keep_columns))
  gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
  
  mca <- MCA(tea_time, graph = FALSE)
      summary(mca)
  
  
  plot(mca, invisible=c("ind"), habillage = "quali")
  
  
      