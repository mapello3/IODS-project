#Author: Marina Peris Llopis
#Date: 12.11.2020
#Script Exercise 3 "Logistic Regression"
#Data: dataset about students achievement in secondary education of two Portuguese schools.
#Data source: P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. 
              #In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference 
              #(FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.


#Exercise 3: Data wrangling

  ##Load data

    mat <- read.csv("data/student-mat.csv", sep = ";")

    por <- read.csv("data/student-por.csv", sep = ";")

  ##Explore the data

    dim(mat) #dataset with 395 observations or records and 33 variables
    str(mat) #only character and integers
    
    dim(por) #dataset with 649 observations or records and 33 variables
    str(por) #only character and integers, same variables as in "mat"
    
  ##Join both datasets
    
    library(dplyr)
    
    join_by <- c("school","sex","age","address","famsize","Pstatus","Medu",
                 "Fedu","Mjob","Fjob","reason","nursery","internet")
    
    math_por <- inner_join(mat, por, by = join_by, suffix = c(".math", ".por"))

    dim(math_por) #382 observations and 53 variables (63 original variables - 13 joints)
    str(math_por) #all the variables survived the joint as character or integer
    
    
  ##Combine duplicated answers   
    
    #create a new data frame with only the joined columns
    alc <- select(math_por, one_of(join_by))
    
    #the columns in the datasets which were not used for joining the data
    notjoined_columns <- colnames(mat)[!colnames(mat) %in% join_by]
    
    #print out the columns not used for joining
    notjoined_columns
    
    #for every column name not used for joining...
    for(column_name in notjoined_columns) {
      #select two columns from 'math_por' with the same original name
      two_columns <- select(math_por, starts_with(column_name))
      #select the first column vector of those two columns
      first_column <- select(two_columns, 1)[[1]]
      
      #if that first column vector is numeric...
      if(is.numeric(first_column)) {
        #take a rounded average of each row of the two columns and
        #add the resulting vector to the alc data frame
        alc[column_name] <- round(rowMeans(two_columns))
      } else { # else if it's not numeric...
        #add the first column vector to the alc data frame
        alc[column_name] <- select(two_columns, 1)[[1]]
      }
    }

  ##New alc_use variable
    
    alc <- mutate(alc, alc_use = (Dalc + Walc) / 2) #by using the average
    
    alc <- mutate(alc, high_use = alc_use > 2) #new logical column
    
  ##Glimpse at the data
    
    glimpse(alc)
    dim(alc) #variables now reduced to 35, 382 observations. Everything seems to be in order
    
  ##Export new data
    
    write.csv(alc, "alc.csv")


#Exercise 3: Analysis    
    
    Alc_consumption <- read.csv(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt "), header = T, sep = ",") 
    
    colnames(Alc_consumption) #this dataset contains information about students in secondary education 
                              #of two Portuguese schools (e.g, alcohol consumption, internet access at home, etc)
                              #as well as their grades in two subjects (maths and portuguese).
    
    
  ##Potential relationships
    
    #My hypothesis is that the variables sex, number of school absences and parent's 
    #cohabitation status can influence alcohol consumption. Let's check it.
    
    library(ggplot2)  
    library(GGally)
    
    boxplot(as.factor(Alc_consumption$sex), Alc_consumption$alc_use, ylab = "Alcohol_use", xlab = "Sex")
    plot(Alc_consumption$absences, Alc_consumption$high_use)
    boxplot(as.factor(Alc_consumption$Pstatus), Alc_consumption$high_use)

    library(gmodels)
    CrossTable(Alc_consumption$high_use, Alc_consumption$Pstatus)
    CrossTable(Alc_consumption$high_use, Alc_consumption$absences)
    
    install.packages("tidyr")
    library(tidyr) 
    library(dplyr)
    
    gather(Alc_consumption) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()
    
    pairs(Alc_consumption[,-c()])
    
    
  ##Logistic regression
    
    Model1 <- glm(high_use ~ Pstatus + absences + sex, data = Alc_consumption, family = "binomial")
    summary(Model1)    
    
    #According to Model1, absences and sex influence significantly and in a positive way the use of alcohol. 
    #The status of parents does not have a significant effect on use of alcohol.
    #I was correct in two of the potential relationships, but not with Pstatus.
    
    Model2 <- glm(high_use ~ absences + sex, data = Alc_consumption, family = "binomial") #Adjusted model
    summary(Model2) 
    
    OR <- coef(Model2) %>% exp #Calculate Odds Ratio
    CI <- confint(Model2)
    CI <- exp(CI)
    cbind(OR, CI) #Odds ratio and confidence interval for each independent variable.
                  #Non of the intervals contains the value 1, which would result in an undefined probability
    
    
  ##Calculating probabilities
    
    probabilities <- predict(Model2, type = "response")
    
    alc <- mutate(Alc_consumption, probability = probabilities)
    alc <- mutate(alc, prediction = probability > 0.5)
    select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)    
    table(high_use = alc$high_use, prediction = alc$prediction)    
    
    library(dplyr)
    library(ggplot2)
    
    g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))
    g + geom_point()    

    #Calculating the error
    
    loss_func <- function(class, prob) {
      n_wrong <- abs(class - prob) > 0.5
      mean(n_wrong)
    }
    
    loss_func(class = alc$high_use, prob = alc$probability)
    
    loss_func(class = alc$high_use, prob = 0)
    
    