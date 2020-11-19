#Exercise 4. Clustering and classification
#Author: Marina Peris
#Date: 19.11.2020


##Data wrangling

  hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)  

  gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")          


  #Exploring the data

  dim(hd) #195 observations and 8 variables
  str(hd)

  dim(gii) #195 observations and 10 variables
  str(gii)
  
  summary(hd)
  summary(gii)

  #Simplifying variable names 

  colnames(hd)[1:8] <- c("HDI_rank", "Country", "HDI", "LE", "EYS", "MYS", "GNI", "GNI_rank")
  colnames(gii)[1:10] <- c("GII_rank", "Country", "GII", "MMR", "ABR", "PR", "SE_F", "SE_M", "LFPR_F", "LFPR_M")
  
  #New variables
  
  library(dplyr)
  
  gii <- mutate(gii, "edu2F/edu2M" = (SE_F/SE_M)) #New variable: ratio of Female and Male populations with secondary education in each country
  gii <- mutate(gii, "labF/labM" = (LFPR_F/LFPR_M)) #New variable: ratio of labour force participation of females and males in each country
  
  
  #Joining both datasets
  
  join_by <- "Country"
  
  human <- inner_join(hd, gii, by = join_by, suffix = c(".hd", ".gii"))
  
  dim(human) #195 observations and 19 variables (includes Country)
  str(human)
  
  write.csv(human, "human.csv") #Saving the new dataset


##Analysis

  #Load data

  install.packages("MASS")
  require(MASS)

  data(Boston)  
  
  #Exploring the data
  
  dim(Boston)  
  str(Boston)
  head(Boston)

    #Data description: the dataset contains 506 observations of 14 variables, describing housing values of suburbs in Boston.
    #I.e. crime rate by town, NO ppm, pupil-teacher ratio, etc.
  
  #Graphical overview
  
  library(ggplot2)  
  library(GGally)
  
  pairs(Boston) #scatterplots by pairs of variables
  
  p <- ggpairs(Boston[, -c(4, 9)], lower = list(combo = wrap("facethist", bins = 20)))
  
  summary(Boston)
    summary(Boston$crim)
      boxplot(Boston$crim)
      hist(Boston$crim)
    summary(Boston$crim)
      boxplot(Boston$crim)
      hist(Boston$crim)
    summary(Boston$crim)
      boxplot(Boston$crim)
      hist(Boston$crim)
    summary(Boston$crim)
      boxplot(Boston$crim)
      hist(Boston$crim)

    install.packages("corrplot")
    install.packages("tidyverse")
    require(corrplot)
    require(tidyverse)
      
    cor_matrix<-cor(Boston) %>% round(digits=2) #Round the matrix
      
    corrplot(cor_matrix, method="circle")
      
    
  #Standarizing 
  
      boston_scaled <- scale(Boston)

      boston_scaled <- as.data.frame(boston_scaled)
      
      summary(boston_scaled)
      summary(Boston)
      
      bins <- quantile(boston_scaled$crim)
      crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label=c("low", "med_low", "med_high", "high"))
      table(crime)
      boston_scaled <- dplyr::select(boston_scaled, -crim)
      boston_scaled <- data.frame(boston_scaled, crime)
      
      n <- nrow(boston_scaled)
      ind <- sample(n,  size = n * 0.8)
      train <- boston_scaled[ind,]
      test <- boston_scaled[-ind,]
      correct_classes <- test$crime
      test <- dplyr::select(test, -crime)
      
      
  #Linear discriminant analysis    
      
      lda.fit <- lda(crime~., data = train)
      summary(lda.fit)
      classes <- as.numeric(train$crime) 
      
      lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
        heads <- coef(x)
        arrows(x0 = 0, y0 = 0, 
               x1 = myscale * heads[,choices[1]], 
               y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
        text(myscale * heads[,choices], labels = row.names(heads), 
             cex = tex, col=color, pos=3)
      }
      

      plot(lda.fit, dimen = 2, col = classes, pch = classes)
      lda.arrows(lda.fit, myscale = 1)
      
      lda.pred <- predict(lda.fit, newdata = test)
      table(correct = correct_classes, predicted = lda.pred$class)
      
      
      data(Boston)
      
      boston_standarized <- scale(Boston)
      
      boston_standarized <- as.data.frame(boston_standarized)
      
      
      dist_eu <- dist(boston_standarized) #Euclidean distance
      summary(dist_eu)
      
      dist_mh <- dist(boston_standarized, method = "manhattan")#Manhattan distance
      summary(dist_mh)
      
      
  #K-means
      
      km <-kmeans(boston_standarized, centers = 3)
      km$cluster
      # plot the Boston dataset with clusters
      pairs(boston_standarized[6:10], col = km$cluster)
      
      
      set.seed(123)
      
      #Determine the number of clusters
      k_max <- 10
      
      #Calculate the total within sum of squares
      twcss <- sapply(1:k_max, function(k){kmeans(boston_standarized, k)$tot.withinss})
      
      #Visualize the results
      qplot(x = 1:k_max, y = twcss, geom = 'line')
      
      ##According to the plot, I change the number of clusters to 2
      
      km <-kmeans(boston_standarized, centers = 2)
      
      pairs(boston_standarized, col = km$cluster)

      pairs(boston_standarized[6:10], col = km$cluster)
      
      
      
      
  
    