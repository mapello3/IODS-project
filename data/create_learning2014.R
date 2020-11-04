#Auhtor: Marina Peris Llopis
#Date: 4-11-2020
#Script for exercise 2, course "Introduction to Open Data Science"

#Data wrangling
##Load data

  lrn14 <- read.csv(url("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"), header = T, sep = "\t") 
  
##Explore the data
  
  head(lrn14) #show first 6 rows of the data
  str(lrn14) #all variables as integers or numeric except for gender (character)
  dim(lrn14) #the data frame contains 183 observations and 60 variables
  
##New analysis data set
  
  install.packages("dplyr")
  library(dplyr)

  deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31") #grouping variables
  surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32") #grouping variables
  strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28") #grouping variables
  
  deep_columns <- select(lrn14, one_of(deep_questions)) #select the columns related to deep learning and create column 'deep' by averaging
  lrn14$deep <- rowMeans(deep_columns)
  
  surface_columns <- select(lrn14, one_of(surface_questions)) #select the columns related to surface learning and create column 'surf' by averaging
  lrn14$surf <- rowMeans(surface_columns)
  
  strategic_columns <- select(lrn14, one_of(strategic_questions)) #select the columns related to strategic learning and create column 'stra' by averaging
  lrn14$stra <- rowMeans(strategic_columns)
  

  keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points") #Keep only variables of interest
  learning2014 <- select(lrn14, one_of(keep_columns))
  learning2014 <- filter(learning2014, Points != 0) #New data frame with the scaled variables of interest and excluded observations with 0 points
  
  dim(learning2014) #166 observations and 7 variables
  
  
##Export analysis
  
  setwd("C:/Users/marina/Documents/repos/IODS-project/data") #set working directory
  write.csv(learning2014, "learning2014.csv")
  
##Check analysis file
  
  learning2014_check <- read.csv("learning2014.csv", header = T)
  str(learning2014_check)
  head(learning2014_check)

  
#Analysis
##Load data
  
  students2014 <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt"), header = T, sep = ",") 
  str(students2014) #same data frame as created earlier, but imported again
                    #data are obtained from a survey about approaches to learning
  
  dim(students2014) #data set contains 166 observations and 7 variables
  
##Data overview
  
  summary(students2014) #general summary with all the variables
    summary(students2014$gender) #individual summary for variable gender, type = character (F/M)
        table(students2014$gender) #more females (110) than males (56) in the study
    summary(students2014$age) #mean age is 25.51, range 17-55
        hist(students2014$age) #many young subjects (age 20-30) and a few older ones (age>40)
                                #makes sense, usually students are younger and there is less old people (I would say mature for this range) as students
        boxplot(students2014$age) #also visible here
    summary(students2014$attitude) #I don't know what each value means (1-5), but seems that values 3-4 are the most chosen ones
        hist(students2014$attitude) #seems to follow a normal distribution
        boxplot(students2014$attitude)
    summary(students2014$deep)
        boxplot(students2014$deep) #some outliers
    summary(students2014$stra)
        boxplot(students2014$stra)
    summary(students2014$surf)
        boxplot(students2014$surf)
    summary(students2014$points)
        boxplot(students2014$points) #max points = 33, min = 7, many subjects with 20-25 points
        hist(students2014$points) #seems to follow a normal distribution
    
    install.packages("ggplot2")
    install.packages("GGally")
    library(ggplot2)  
    library(GGally)
       
    pairs(students2014[-1]) #scatterplots by pairs of variables, difficuult to observe relationships
    
    p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), 
                 lower = list(combo = wrap("facethist", bins = 20)))
    p #these graphs reveal very significant correlations between attitude-points (+) and surf-deep (-)
    
    
##Regression model
    
    my_model1 <- lm(points ~ attitude + stra +surf, data = students2014) #linear regression model with points as response variable and attitude, stra, surf as independent variables
    summary(my_model1) #only attitude has a significant influence in points, better to chose other variables
    
    my_model2 <- lm(points ~ attitude + stra, data = students2014) 
    summary(my_model2) #stra is non significant
    
    my_model3 <- lm(points ~ attitude + age + gender + deep, data = students2014)
    summary(my_model3) #age, gender and deep are non significant
    
    
    my_model4 <- lm(points ~ attitude, data = students2014) #after trying all the variables, attitude seems to be the only significant variable to explain points
    
    summary(my_model4) #there is a positive relationship between points and attitude, as attitude increases, so does points
                        #according to this model, when attitude changes by 1 unit, points increase by 3.5 units on average 
                        #the R2 is around 0.19, so the variance in the variable attitude explains about 19% of the variance in the variable points

    ##Diagnostics plots  
    
    par(mfrow = c(2,2))
    plot(my_model4, which = c(1, 2, 5))
    
      #Looking at the Q-Q plot, we can reasonably say that our model fits the assumption of normality, as the errors follow a normal distribution (points follow the diagonal in the plot)
      #There is no evident pattern in the Residuals vs Fitted plots, what indicates that we can assume constant variance in our model
      #In the Residuals vs Leverage, does not seem to be any observation pulling or influencing too much the regression line.
    
    
    
    
  
  
  
  