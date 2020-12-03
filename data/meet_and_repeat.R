#Analysis of longitudinal data
#Author: Marina Peris



##Data wrangling

  BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
  RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep  ="", header = T)

  #Short form data
  
  names(BPRS) #Repeated measures of subjects during 8 weeks
  names(RATS)
  
  dim(BPRS) #40 observations and 11 variables
  dim(RATS) #16 observations and 13 variables
  
  glimpse(BPRS)
  glimpse(RATS)
  
  str(BPRS) #All the variables are integers
  str(RATS) #Same here
  
  summary(BPRS)
  summary(RATS)
  
  
  #Converting some categorical variables to factors
  
  library(dplyr)
  library(tidyr)
  
  BPRS$treatment <- factor(BPRS$treatment)
  BPRS$subject <- factor(BPRS$subject)
  
  RATS$ID <- factor(RATS$ID)
  RATS$Group <- factor(RATS$Group)
  
  
  #Transforming to long format
  
  BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
  RATSL <-  RATS %>% gather(key = weeks, value = bprs, -Group, -ID)

  BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(BPRSL$weeks, 5, 5))) #New week column
  RATSL <-  RATSL %>% mutate(Time = as.integer(substr(RATSL$weeks, 3, 3))) #New Time column
  
  
  #Looking at the long form data
  
  names(BPRSL) #Repeated measures of subjects during 8 weeks
  names(RATSL)
  
  dim(BPRSL) #Now there are 360 observations and 5 variables
  dim(RATSL) #Now there are 176 observations and 5 variables
  
  glimpse(BPRSL)
  glimpse(RATSL)
  
  str(BPRSL) #First two columns are factors, third is character and the other two are integers
  str(RATSL) #Same here
  
  summary(BPRSL)
  summary(RATSL)
  
  #The main difference between the datasets before and after transformation to long form is that 
  #now each measurement done each week occupies a row in the dataset, whereas in the previous format
  #those measurements where located in different columns. The transformation has reduced the number of 
  #variables, as now week and Time are considered as factor variables.
  #This way, the same subject has measurements in different rows, instead of in different columns.

  
  #Exporting the modified datasets
  
  write.csv(BPRSL, "BPRSL.csv")
  write.csv(RATSL, "RATSL.csv")
  
  
  
  
##Analysis Chapter 8 book
  
  RATSL <- read.csv("RATSL.csv")
  
  RATSL$ID <- factor(RATSL$ID)
  RATSL$Group <- factor(RATSL$Group)
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  ggplot(RATSL, aes(x = Time, y = bprs, linetype = ID)) +
    geom_line() +
    scale_linetype_manual(values = rep(1:10, times=4)) +
    facet_grid(. ~ Group, labeller = label_both) +
    theme(legend.position = "none") + 
    scale_y_continuous(limits = c(min(RATSL$bprs), max(RATSL$bprs)))
  
  
  RATSL <- RATSL %>%
    group_by(Time) %>%
    mutate(stdbprs = bprs - mean(bprs)/sd(bprs)) %>%
    ungroup()
  
  ggplot(RATSL, aes(x = Time, y = stdbprs, linetype = ID)) +
    geom_line() +
    scale_linetype_manual(values = rep(1:10, times=4)) +
    facet_grid(. ~ Group, labeller = label_both) +
    scale_y_continuous(name = "standardized bprs")
  
  
  n <- RATSL$Time %>% unique() %>% length()
  
  RATSS <- RATSL %>%
    group_by(Group, Time) %>%
    summarise(mean = mean(bprs), se = sd(bprs)/sqrt(n)) %>%
    ungroup()
  
  ggplot(RATSS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
    geom_line() +
    scale_linetype_manual(values = c(1,2,3)) +
    geom_point(size=3) +
    scale_shape_manual(values = c(1,2,3)) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
    theme(legend.position = c(0.9,0.35)) +
    scale_y_continuous(name = "mean(bprs) +/- se(bprs)")
  
  RATSS8S <- RATSL %>%
    group_by(Group, ID) %>%
    summarise(mean = mean(bprs)) %>%
    ungroup()
  
  ggplot(RATSS8S, aes(x = Group, y = mean)) +
    geom_boxplot() +
    stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
    scale_y_continuous(name = "mean(bprs), Time")
  
  ggplot(RATSS, aes(x = Time, y = mean)) +
    geom_boxplot() +
    stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
    scale_y_continuous(name = "mean(bprs), Time")
  

##Analysis Chapter 9 book
  
  BPRSL <- read.csv("BPRSL.csv")
  
  BPRSL$treatment <- factor(BPRSL$treatment)
  BPRSL$subject <- factor(BPRSL$subject)

  
  BPRSL_lm <- lm(bprs ~ week + treatment, data = BPRSL)
  summary(BPRSL_lm)
  
  library(lme4)
  BPRSL_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = BPRSL, REML = FALSE) #Random intercept model with random effects
  
  BPRSL_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = BPRSL, REML = FALSE) #Random intercept and slope model with random effects
  summary(BPRSL_ref1)  
  
  anova(BPRSL_ref1, BPRSL_ref) #ANOVA checking possible improvement

  BPRSL_ref2 <- lmer(bprs ~ week + treatment + (week | subject) + week*treatment, data = BPRSL, REML = FALSE)
  summary(BPRSL_ref2)
  anova(BPRSL_ref2, BPRSL_ref1)
  
  BPRSL$fitted <- fitted(BPRSL_ref2)
  
  plot(BPRSL$bprs, BPRSL$fitted, xlab="Original bprs", ylab="Modeled bprs", col = BPRSL$treatment)
  
  
    
  