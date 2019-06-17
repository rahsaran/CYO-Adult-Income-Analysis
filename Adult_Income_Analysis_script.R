#####Setup#####

#Install the required packages
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) 
  install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) 
  install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) 
  install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) 
  install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(GoodmanKruskal)) 
  install.packages("GoodmanKruskal", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) 
  install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) 
  install.packages("ROCR", repos = "http://cran.us.r-project.org")

#Load the required packages
library(tidyverse)
library(ggthemes)
library(caret)
library(purrr)
library(randomForest)
library(Rborist)
library(DescTools)
library(GoodmanKruskal)
library(corrplot)
library(ROCR)

#There are 3 options to download the data

#Option 1 - The code below downloads and then loads the data into the current working directory.
if(!file.exists("./adult.data")){
  
  fileUrl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"  
  
  download.file(fileUrl, destfile = "./adult.data")
  
}

#Option 2 - You may go to this link and manually download the file
#and save it in your current working directory
#"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data".

#Option 3 - You may download from the github repository for this project
#and save it in your current working directory
# "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data".

#Once you have downloaded the file /adult.data in your current working directory,
#please run the below code to load it into the variable called census.

census <- read.table("adult.data", sep = ",", header = FALSE)
colnames(census) <- c("age", "workclass", "fnlwgt", 
                      "education", "education.num", 
                      "marital.status", "occupation",
                      "relationship", "race", "sex", 
                      "capital.gain", "capital.loss", 
                      "hours.per.week", "native.country", "income")


#####Exploratory Data Analysis & Visualization#####


###Dataset

#Top-level look at data
head(census)
str(census)
dim(census)


###Income

#calculate proportion of observations by income, or distribution of income across observations
incomec <- census %>% group_by(income) %>% summarize(n = n(), proportion = n/nrow(census))

#the below code gives a visual representation of this
incomec %>% ggplot(aes(income, proportion)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  ylim(c(0,1)) +
  geom_text(label = round(incomec$proportion, 2), 
            vjust = -0.5, color = "black", size = 5) + 
  theme_classic() + 
  ggtitle("Income")

#Convert income to a binary factor variable with classes "0" & "1"
census <- mutate(census, incomeLevel = ifelse(income == " <=50K", 0, 1))

#Convert income to a binary numeric variable with classes 0 & 1
census$incomeLevel <- census$incomeLevel %>% factor(levels = c("0", "1"))
census <- mutate(census, incomeNumeric = as.numeric(incomeLevel)-1)


###Age

#calculate proportion of observations by age, or distribution of age across observations
summary(census$age)
census %>% ggplot() + 
  geom_bar(aes(x = age)) + 
  theme_minimal() + 
  ggtitle("Age")

#first we do a preliminary bucketing of age into groups of 5 years
census <- mutate(census, ageGroup = round(age/5)*5)

#and then observe the average proportion with income >50K for these age groups
census %>% 
  group_by(ageGroup) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric)) %>% 
  ggplot(aes(ageGroup, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  ggtitle("Age Group")

#Based on the plot, we will group age further into 4 groups - <25, 25-35, 40-60, >60
#which have very distinct proportions of people with income >50K
census <- mutate(census, ageGroup2 = 
                   ifelse(ageGroup < 30, "<30", 
                          ifelse(ageGroup < 40, "30-40", 
                                 ifelse(ageGroup < 60, "40-60", ">60"))))

#Now we calculate the proportion with income >50K for these age groups
agec <- census %>% 
  group_by(ageGroup2) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#We see this visually with the below code
agec %>% 
  ggplot(aes(ageGroup2, prop_more_than_50K)) + 
  ylim(c(0,0.5)) +
  geom_text(label = round(agec$prop_more_than_50K, 2), 
            vjust = -0.5, color = "black", size = 4) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = c("<30","30-40","40-60",">60")) +
  theme_classic() +
  ggtitle("Relationship of Age with income")


### Race

#calculate proportion of observations by race, or distribution of race across observations
racec <- census %>% group_by(race) %>% summarize(n = n(), proportion = n/nrow(census))

#see this visually
racec %>% ggplot(aes(race, proportion)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() +
  geom_text(label = round(racec$proportion, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  theme_classic() + 
  ggtitle("Race")

#calculate proportion with income >50K for each race
racec2 <- census %>% 
  group_by(race) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
racec2 %>% 
  ggplot(aes(race, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,0.5)) +
  coord_flip() +
  geom_text(label = round(racec2$prop_more_than_50K, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = racec2$race[order(racec2$prop_more_than_50K)]) +
  theme_classic() + 
  ggtitle("Relationship of Race with Income")

#calculate average educational level for each race
racec3 <- census %>% 
  group_by(race) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric), education = mean(education.num))
racec3


### Sex

#calculate proportion of observations by sex, or distribution of sex across observations
sexc <- census %>% group_by(sex) %>% summarize(n = n(), proportion = n/nrow(census))

#see this visually
sexc %>% ggplot(aes(sex, proportion)) + 
  geom_bar(stat = "identity", width = 0.5) +
  ylim(c(0,1)) +
  geom_text(label = round(sexc$proportion, 2), 
            vjust = -0.5, color = "black", size = 5) + 
  theme_classic() + 
  ggtitle("Sex")

#calculate proportion with income >50K for each sex
sexc2 <- census %>% 
  group_by(sex) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
sexc2 %>% 
  ggplot(aes(sex, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,0.5)) +
  geom_text(label = round(sexc2$prop_more_than_50K, 2), 
            vjust = -0.5, color = "black", size = 4) + 
  theme_classic() + 
  ggtitle("Relationship of Sex with Income")


### Native.country

summary(census$native.country)
mean(census$native.country == " United-States")

#group all observations into 2 buckets of native country - "United-States" & "Other"
census <- mutate(census, native.country.group = 
                   ifelse(native.country == " United-States", "United-States", "Other"))

#calculate proportion with income >50K for both these buckets
nativec <- census %>% 
  group_by(native.country.group) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
nativec %>% 
  ggplot(aes(native.country.group, prop_more_than_50K)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  ylim(c(0,0.5)) +
  geom_text(label = round(nativec$prop_more_than_50K, 2), 
            vjust = -0.5, color = "black", size = 4) + 
  theme_classic() + 
  ggtitle("Relationship of Native Country with Income")


### Workclass

#calculate proportion of observations by workclass, 
#or distribution of workclass across observations
workc <- census %>% group_by(workclass) %>% summarize(n = n(), proportion = n/nrow(census))

#see this visually
workc %>% ggplot(aes(workclass, proportion)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() +
  geom_text(label = round(workc$proportion, 2), 
            hjust = -0.5, color = "black", size = 4) +
  theme_classic() + 
  ggtitle("Workclass")

#calculate proportion with income >50K for each workclass
workc2 <- census %>% 
  group_by(workclass) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
workc2 %>% 
  ggplot(aes(workclass, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() +
  geom_text(label = round(workc2$prop_more_than_50K, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = workc2$workclass[order(workc2$prop_more_than_50K)]) +
  theme_classic() + 
  ggtitle("Relationship of Workclass with Income")


### Occupation

#calculate proportion of observations by occupation, 
#or distribution of occupation across observations
occupationc <- census %>% group_by(occupation) %>% 
  summarize(n = n(), proportion = n/nrow(census))

#see this visually
occupationc %>% ggplot(aes(occupation, proportion)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() +
  geom_text(label = round(occupationc$proportion, 2), 
            hjust = -0.5, color = "black", size = 3) +
  theme_classic() + 
  ggtitle("Occupation")

#calculate proportion with income >50K for each occupation
occupationc2 <- census %>% 
  group_by(occupation) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
occupationc2 %>% 
  ggplot(aes(occupation, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() +
  geom_text(label = round(occupationc2$prop_more_than_50K, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = occupationc2$occupation[order(occupationc2$prop_more_than_50K)]) +
  theme_classic() + 
  ggtitle("Relationship of Occupation with Income")


### hours.per.week

summary(census$hours.per.week)
census %>% ggplot() + 
  geom_bar(aes(x = hours.per.week)) + 
  theme_minimal() + 
  ggtitle("Hours per week")
mean(census$hours.per.week == 40)

#Since this is integer, let's first convert it into groups to assess the impact on income.
#There are 3 clear groups that stand out - 
#those who work 40 hours, those who work less, and those who work more

#convert all values of hours.per.week to one of 3 categories - <40, 40, >40
census <- mutate(census, hours.group = 
                   ifelse(hours.per.week < 40, "<40",
                          ifelse(hours.per.week == "40", "40", ">40")))

#calculate proportion with income >50K for each of these categories
hoursc <- census %>% 
  group_by(hours.group) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
hoursc %>% 
  ggplot(aes(hours.group, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,0.5)) +
  geom_text(label = round(hoursc$prop_more_than_50K, 2), 
            vjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = c("<40","40",">40")) +
  theme_classic() + 
  ggtitle("Relationship of Working Hours with Income")


### education & education.num

summary(census$education)
summary(census$education.num)

#compare education with education.num
table(census$education, census$education.num)
census %>% group_by(education.num) %>% summarize(education = first(education))

#calculate proportion of observations by education.num, 
#or distribution of education.num across observations
census %>% ggplot() + 
  geom_bar(aes(x = education.num)) + 
  theme_minimal() + 
  ggtitle("Education")

#calculate proportion with income >50K for each value of education.num
educationc <- census %>% 
  group_by(education) %>% 
  summarize(education.num = first(education.num),prop_more_than_50K = mean(incomeNumeric))

#see this visually
educationc %>% 
  ggplot(aes(education, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() +
  geom_text(label = round(educationc$prop_more_than_50K, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = educationc$education[order(educationc$education.num)]) +
  theme_classic() + 
  ggtitle("Relationship of Education with Income")


### Relationship

#calculate proportion of observations by relationship, 
#or distribution of relationship across observations
relationshipc <- census %>% group_by(relationship) %>% 
  summarize(n = n(), proportion = n/nrow(census))

#see this visually
relationshipc %>% ggplot(aes(relationship, proportion)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() + 
  geom_text(label = round(relationshipc$proportion, 2), 
            hjust = -0.5, color = "black", size = 3) + 
  theme_classic() + 
  ggtitle("Relationship")

#calculate proportion with income >50K for each value of relationship
relationshipc2 <- census %>% 
  group_by(relationship) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
relationshipc2 %>% 
  ggplot(aes(relationship, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,0.75)) +
  coord_flip() +
  geom_text(label = round(relationshipc2$prop_more_than_50K, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = relationshipc2$relationship[order(relationshipc2$prop_more_than_50K)]) +
  theme_classic() + 
  ggtitle("Relationship of Relationship with Income")


### marital.status

#calculate proportion of observations by marital status, 
#or distribution of marital status across observations
maritalc <- census %>% group_by(marital.status) %>% 
  summarize(n = n(), proportion = n/nrow(census))

#see this visually
maritalc %>% ggplot(aes(marital.status, proportion)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,1)) +
  coord_flip() + 
  geom_text(label = round(maritalc$proportion, 2), 
            hjust = -0.5, color = "black", size = 3) + 
  theme_classic() + 
  ggtitle("Marital Status")

#calculate proportion with income >50K for each value of marital.status
maritalc2 <- census %>% 
  group_by(marital.status) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
maritalc2 %>% 
  ggplot(aes(marital.status, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,0.75)) +
  coord_flip() +
  geom_text(label = round(maritalc2$prop_more_than_50K, 2), 
            hjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = maritalc2$marital.status[order(maritalc2$prop_more_than_50K)]) +
  theme_classic() + 
  ggtitle("Relationship of Marital status with Income")

#compare relationship vs marital.status
table(census$relationship, census$marital.status)


### capital.gain & capital.loss

summary(census$capital.gain)

#see proportion with positive capital gain
capGain <- census %>% filter(capital.gain > 0) %>% summarize(n = n())
capGain/nrow(census)

summary(census$capital.loss)

#see proportion with positive capital loss
capLoss <- census %>% filter(capital.loss > 0) %>% summarize(n = n())
capLoss/nrow(census)

census %>% filter(capital.gain >0) %>% summarize(sum(capital.loss))
census %>% filter(capital.loss >0) %>% summarize(sum(capital.gain))

#create new capitalMovement variable
census <- mutate(census, capitalMovement = 
                   ifelse(capital.gain > 0, capital.gain, 
                          ifelse(capital.loss > 0, -capital.loss, 0)))
summary(census$capitalMovement)

#Since capitalMovement is integer, let's first convert it into groups 
#to assess the impact on income. There are 3 clear groups that stand out - 
#those with positive capital movement, those with negative, and those with none.

census <- mutate(census, capital.group = 
                   ifelse(capitalMovement < 0, "Negative",
                          ifelse(capitalMovement == 0, "None", "Positive")))

#now calculate proportion with income >50K for each of these groups
capitalc <- census %>% 
  group_by(capital.group) %>% 
  summarize(prop_more_than_50K = mean(incomeNumeric))

#see this visually
capitalc %>% 
  ggplot(aes(capital.group, prop_more_than_50K)) + 
  geom_bar(stat = "identity") + 
  ylim(c(0,0.75)) +
  geom_text(label = round(capitalc$prop_more_than_50K, 2), 
            vjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = c("Negative","None","Positive")) +
  theme_classic() + 
  ggtitle("Relationship of Capital Movement with Income")


### Replacing "?" in workclass & occupation

table(census$occupation, census$workclass)

#create data frame of all combinations of non-"?" values
#of occupation & workclass
workclass_vs_occupation <- as.data.frame(table(census$workclass, census$occupation)[2:9,2:15])
colnames(workclass_vs_occupation)<-c("Workclass","Occupation","Count")

#calculate total number of such combinations
total<-sum(workclass_vs_occupation$Count)

#for each combination, what proportion of observations have that combination
workclass_vs_occupation$proportion<-workclass_vs_occupation$Count/total

#cumulative proportion till a particular point. This will be used in the next section
workclass_vs_occupation$cumulate<-cumsum(workclass_vs_occupation$proportion)

set.seed(1, sample.kind = "Rounding")

#create random numbers between 0 & 1, one for every instance of "?"
randomNumbers <- runif(sum(census$occupation=="?"))

#calculates the number of entries till that random number crosses the cumulative proportion.
#essentially, we have to replace the missing "?" values with a combination of 
#occupation & workclass that is present. So, the missing 1843 values have to be replaced by
#one of these 112 combinations in a proportionate manner. The below code calculates that
#proportionate division.
ind <- rowSums(vapply(workclass_vs_occupation$cumulate,
                      function(x) x<=randomNumbers, logical(length(randomNumbers))))+1
a<-as.character(workclass_vs_occupation$Workclass)[ind]

#get the entries which have missing "?" values
missing_occupation <-which(census$occupation=="?")
missing_workclass <- which(census$workclass=="?")
missing_only_occ <- setdiff(missing_occupation,missing_workclass)

#replace each of the missing "?" values with an existing value
census$workclass[missing_workclass]= as.character(workclass_vs_occupation$Workclass)[ind][1:1836]
census$occupation[missing_workclass]= as.character(workclass_vs_occupation$Occupation)[ind][1:1836]
census$occupation[missing_only_occ] <- as.character(workclass_vs_occupation$Occupation)[ind][1837:1843]
table(census$occupation, census$workclass)


#####Modelling#####

#create train & test sets
y <- census$incomeLevel
set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- census[-index, ]
test_set <- census[index, ]

#create table of results
results<-data.frame(Model=character(),
                    Accuracy=numeric(),
                    F1_score=numeric(),
                    Sensitivity=numeric(),
                    Specificity=numeric(),
                    stringsAsFactors = FALSE)


### Logistic Regression

#run glm model on all factor variables
glm_fit1 <- train_set %>%
  glm(incomeLevel ~ age + race + sex + capitalMovement + hours.per.week + 
        education.num + marital.status + relationship + workclass + 
        occupation, data = ., family = "binomial")

#predict the cutoff probability value
p_hat_logit1 <- predict(glm_fit1, train_set, type = "response")

summary(p_hat_logit1)

cutoffs <- c(0, seq(0.01,0.39,0.01), 1)

#tuning-calculate the FPR & TPR for each value of p
prob_cutoff1 <- map_df(cutoffs, function(x){
  y_hat_logit1 <- ifelse(p_hat_logit1 < x, "0", "1") %>% factor(levels = c("0","1"))
  list(method = "Logistic Regression - all vars", 
       p = x,
       FPR = 1 - specificity(y_hat_logit1, train_set$incomeLevel),
       TPR = sensitivity(y_hat_logit1, train_set$incomeLevel))
})

# calculate distance of our model at each point from the ideal (0,1) point.
#the (0,1) point on the ROC curve indicates sensitivity & specificity = 1.
prob_cutoff1 <- mutate(prob_cutoff1, distance = sqrt((FPR-0)^2 + (TPR-1)^2))

#the best value of p is that for which this distance is the lowest.
best_prob1 <- prob_cutoff1$p[which.min(prob_cutoff1$distance)]
best_prob1

#generate ROC curve
prob_cutoff1 %>% ggplot(aes(FPR, TPR)) + geom_line(aes(col = method)) + theme_minimal()

#get prediction y_hat for y
y_hat_logit1 <- ifelse(p_hat_logit1 < best_prob1, "0", "1") %>% factor

#calculate various result metrics for this model based on the predicted value of y_hat
results[1, "Model"] <- "Logistic - All Vars"
results[1,"Accuracy"] <- confusionMatrix(y_hat_logit1, train_set$incomeLevel)$overall["Accuracy"]
results[1,"F1_score"] <- F_meas(y_hat_logit1, train_set$incomeLevel)
results[1, "Sensitivity"] <- sensitivity(y_hat_logit1, train_set$incomeLevel)
results[1, "Specificity"] <- specificity(y_hat_logit1, train_set$incomeLevel)
results %>% knitr::kable()

summary(glm_fit1)

#run glm on selected factor variables excluding those with high p-values
glm_fit2 <- train_set %>%
  glm(incomeLevel ~ age + sex + capitalMovement + hours.per.week + 
        marital.status + relationship + workclass + occupation, 
      data = ., family = "binomial")
p_hat_logit2 <- predict(glm_fit2, train_set, type = "response")
summary(p_hat_logit2) 

#tuning-calculate the FPR & TPR for each value of p
cutoffs <- c(0, seq(0.02,0.39,0.01), 1)
prob_cutoff2 <- map_df(cutoffs, function(x){
  y_hat_logit2 <- ifelse(p_hat_logit2 < x, "0", "1") %>% factor
  list(method = "Logistic Regression - excl insignificant", 
       p = x,
       FPR = 1 - specificity(y_hat_logit2, train_set$incomeLevel),
       TPR = sensitivity(y_hat_logit2, train_set$incomeLevel))
})

#calculate distance from (0,1) and value of p which minimizes this
prob_cutoff2 <- mutate(prob_cutoff2, distance = sqrt((FPR-0)^2 + (TPR-1)^2))
best_prob2 <- prob_cutoff2$p[which.min(prob_cutoff2$distance)]
best_prob2

#generate ROC curve
prob_cutoff2 %>% ggplot(aes(FPR, TPR)) + geom_line(aes(col = method)) + theme_minimal()

#get prediction
y_hat_logit2 <- ifelse(p_hat_logit2 < best_prob2, "0", "1") %>% factor

#Note results
results[2, "Model"] <- "Logistic - excl insignificant"
results[2,"Accuracy"] <- confusionMatrix(y_hat_logit2, train_set$incomeLevel)$overall["Accuracy"]
results[2,"F1_score"] <- F_meas(y_hat_logit2, train_set$incomeLevel)
results[2, "Sensitivity"] <- sensitivity(y_hat_logit2, train_set$incomeLevel)
results[2, "Specificity"] <- specificity(y_hat_logit2, train_set$incomeLevel)
results %>% knitr::kable()

#select categorical variables
train_set_categorical <- subset(train_set,select = 
                                  c(workclass, marital.status, occupation, 
                                    relationship, race, sex))

#calculate Goodman-Kruskal tau for correlation
plot(GKtauDataframe(train_set_categorical))

#select numerical variables
train_set_numerical <- subset(train_set, 
                              select = c(age, hours.per.week, education.num, capitalMovement))
#calculate correlation
cor(train_set_numerical)

#create a data frame for all combinations of categorical vs numerical variables
cat_vs_num <- as.data.frame(matrix(NA,nrow = 6, ncol = 4))
rownames(cat_vs_num) <- 
  c("workclass", "marital.status", "occupation", "relationship", 
    "race", "sex")
colnames(cat_vs_num) <- c("age", "hours.per.week", "education.num", "capitalMovement")

#perform Kruskal test on each combination.
for(i in 1:6){
  for(j in 1:4){
    x_var<-train_set[colnames(cat_vs_num)[j]][[1]]
    y_var<-train_set[rownames(cat_vs_num)[i]][[1]]
    cat_vs_num[i,j] <- kruskal.test(x=x_var,g=y_var)$p.value
  }
}
cat_vs_num
#gives that sex & education.num are correlated

#see variation of income with sex
sex_table<-table(train_set$sex, train_set$incomeLevel)
sex_table

#see variation of income with each value of education.num
educationnum_table<-table(train_set$education.num, train_set$incomeLevel)
educationnum_total<-rowSums(educationnum_table)
educationnum_proportion<-educationnum_table/educationnum_total
educationnum_proportion
#there is a clear increase in 1's with increase in education.num

#build 3rd glm model, excluding only marital.status
glm_fit3 <- train_set %>%
  glm(incomeLevel ~ age + race + sex + capitalMovement + 
        hours.per.week + education.num + relationship + 
        workclass + occupation, data = ., family = "binomial")
p_hat_logit3 <- predict(glm_fit3, train_set, type = "response")
summary(p_hat_logit3)

#tuning-calculate FPR & TPR, & p which gives lowest value of distance from (0,1)
cutoffs <- c(0, seq(0.01,0.39,0.01), 1)
prob_cutoff3 <- map_df(cutoffs, function(x){
  y_hat_logit3 <- ifelse(p_hat_logit3 < x, "0", "1") %>% factor
  list(method = "Logistic Regression - excl correlated", 
       p = x,
       FPR = 1 - specificity(y_hat_logit3, train_set$incomeLevel),
       TPR = sensitivity(y_hat_logit3, train_set$incomeLevel))
})
prob_cutoff3 <- mutate(prob_cutoff3, distance = sqrt((FPR-0)^2 + (TPR-1)^2))
best_prob3 <- prob_cutoff3$p[which.min(prob_cutoff3$distance)]
best_prob3

#generate ROC curve
prob_cutoff3 %>% ggplot(aes(FPR, TPR)) + geom_line(aes(col = method)) + theme_minimal()

#get prediction
y_hat_logit3 <- ifelse(p_hat_logit3 < best_prob3, "0", "1") %>% factor

#note results
results[3, "Model"] <- "Logistic - excl correlated"
results[3,"Accuracy"] <- confusionMatrix(y_hat_logit3, train_set$incomeLevel)$overall["Accuracy"]
results[3,"F1_score"] <- F_meas(y_hat_logit3, train_set$incomeLevel)
results[3, "Sensitivity"] <- sensitivity(y_hat_logit3, train_set$incomeLevel)
results[3, "Specificity"] <- specificity(y_hat_logit3, train_set$incomeLevel)
results %>% knitr::kable()

#run the final glm model on the train set. This includes all factor variables except
#native.country & marital.status
glm_fit <- train_set %>%  glm(incomeLevel ~ age + race + sex + capitalMovement + 
                                hours.per.week + education.num + relationship + 
                                workclass + occupation, data = ., family = "binomial")

#Model applied on train set, and results noted.
p_hat_logit_train <- predict(glm_fit, train_set, type = "response")
y_hat_logit_train <- ifelse(p_hat_logit_train < 0.27, "0", "1") %>% factor
results[4, "Model"] <- "Logistic final - train set"
results[4,"Accuracy"] <- confusionMatrix(y_hat_logit_train, train_set$incomeLevel)$overall["Accuracy"]
results[4,"F1_score"] <- F_meas(y_hat_logit_train, train_set$incomeLevel)
results[4, "Sensitivity"] <- sensitivity(y_hat_logit_train, train_set$incomeLevel)
results[4, "Specificity"] <- specificity(y_hat_logit_train, train_set$incomeLevel)

#Model applied on test set, and results noted.
p_hat_logit_test <- predict(glm_fit, test_set, type = "response")
y_hat_logit_test <- ifelse(p_hat_logit_test < 0.27, "0", "1") %>% factor
results[5, "Model"] <- "Logistic final - test set"
results[5,"Accuracy"] <- confusionMatrix(y_hat_logit_test, test_set$incomeLevel)$overall["Accuracy"]
results[5,"F1_score"] <- F_meas(y_hat_logit_test, test_set$incomeLevel)
results[5, "Sensitivity"] <- sensitivity(y_hat_logit_test, test_set$incomeLevel)
results[5, "Specificity"] <- specificity(y_hat_logit_test, test_set$incomeLevel)

results %>% knitr::kable()


### KNN

set.seed(1, sample.kind = "Rounding")

#this fits knn to our data with all factor variables taken in the model
knn_fit <- knn3(incomeLevel ~ age + race + sex + occupation + workclass + 
                  capitalMovement + hours.per.week + education.num + 
                  marital.status + relationship, train_set, k = 5)

#calculate the prediction y_hat from this model
y_hat_knn <- predict(knn_fit, test_set, type = "class") %>% factor(levels = c("0", "1"))

#note the results from KNN
results[6, "Model"] <- "KNN"
results[6,"Accuracy"] <- confusionMatrix(y_hat_knn, test_set$incomeLevel)$overall["Accuracy"]
results[6,"F1_score"] <- F_meas(y_hat_knn, test_set$incomeLevel)
results[6, "Sensitivity"] <- sensitivity(y_hat_knn, test_set$incomeLevel)
results[6, "Specificity"] <- specificity(y_hat_knn, test_set$incomeLevel)
results %>% knitr::kable()

#convert to numeric variables
train_set <- mutate(train_set, 
                    raceNum = ifelse(race == "White", 0, 1), 
                    sexNum = ifelse(sex == "Female", 0, 1), 
                    relationshipNum = ifelse(relationship %in% c("Husband", "Wife"), 1, 0))
col_index_knn <- which(colnames(train_set) %in% 
                         c("age","capitalMovement","hours.per.week","education.num", 
                           "raceNum", "sexNum", "relationshipNum"))

#create control for 10-fold cross-validation
control_knn <- trainControl(method = "cv",number = 10, p= 0.9)

#create index to sample 5000 observations with each run to make it faster
n <- 5000
index <- sample(nrow(train_set), n)
set.seed(1, sample.kind = "Rounding")

#default value of k is 5. Try values 3, 5, 7, 9 to see which works best.
knn_train <- train(train_set[index,col_index_knn],train_set$incomeLevel[index], 
                   method = "knn", tuneGrid = data.frame(k = c(3,5,7,9)), trControl = control_knn)
knn_train$bestTune

#build second knn model with k=9
knn_fit2 <- knn3(incomeLevel ~ age + race + sex + occupation + workclass + 
                   capitalMovement + hours.per.week + education.num + 
                   marital.status + relationship, train_set, k = 9)

#generate prediction for y_hat with this model
y_hat_knn2 <- predict(knn_fit2, test_set, type = "class") %>% factor(levels = c("0", "1"))

#note results from this model
results[7, "Model"] <- "KNN - tuned"
results[7,"Accuracy"] <- confusionMatrix(y_hat_knn2, test_set$incomeLevel)$overall["Accuracy"]
results[7,"F1_score"] <- F_meas(y_hat_knn2, test_set$incomeLevel)
results[7, "Sensitivity"] <- sensitivity(y_hat_knn2, test_set$incomeLevel)
results[7, "Specificity"] <- specificity(y_hat_knn2, test_set$incomeLevel)
results %>% knitr::kable()

### Random Forest

set.seed(1, sample.kind = "Rounding")

#fit random forest to all variables
rf_fit <- randomForest(incomeLevel~age + race + sex + occupation + 
                         workclass + capitalMovement + hours.per.week+education.num +
                         marital.status + relationship, data = train_set)

#calculate prediction y_hat from this model
y_hat_rf <- predict(rf_fit, test_set)

#note results from this model
results[8, "Model"] <- "Random Forest"
results[8,"Accuracy"] <- confusionMatrix(y_hat_rf, test_set$incomeLevel)$overall["Accuracy"]
results[8,"F1_score"] <- F_meas(y_hat_rf, test_set$incomeLevel)
results[8, "Sensitivity"] <- sensitivity(y_hat_rf, test_set$incomeLevel)
results[8, "Specificity"] <- specificity(y_hat_rf, test_set$incomeLevel)
results %>% knitr::kable()

#run Rborist on the data
col_index <- which(colnames(train_set) %in% 
                     c("age","race","sex","occupation","workclass","capitalMovement",
                       "hours.per.week","education.num", "relationship", "marital.status"))
set.seed(1, sample.kind = "Rounding")
rb_fit <- Rborist(train_set[, col_index], train_set$incomeLevel)

#note predictions from Rborist
y_hat_rb <- predict(rb_fit, test_set[, col_index])$yPred %>% factor(levels = c("0","1"))

#note results from this model
results[9, "Model"] <- "Rborist"
results[9,"Accuracy"] <- confusionMatrix(y_hat_rb, test_set$incomeLevel)$overall["Accuracy"]
results[9,"F1_score"] <- F_meas(y_hat_rb, test_set$incomeLevel)
results[9, "Sensitivity"] <- sensitivity(y_hat_rb, test_set$incomeLevel)
results[9, "Specificity"] <- specificity(y_hat_rb, test_set$incomeLevel)
results %>% knitr::kable()

control_rf <- trainControl(method = "cv",number = 5, p= 0.8)
#we use 5-fold cross validation to reduce time taken

grid <- expand.grid(mtry = c(2, 3, 4, 5))
#mtry default is sqrt(variables) = ~3.2, hence testing 2, 3, 4, 5

#tune for different values of mtry, with 5000 random observations sampled, and 50 trees
set.seed(1, sample.kind = "Rounding")
rf_train <- train(train_set[, col_index], train_set$incomeLevel,
                  method = "rf", 
                  nTree = 50,
                  trControl = control_rf, tuneGrid = grid, nSamp = 5000)
ggplot(rf_train) + theme_minimal()
rf_train$bestTune

set.seed(1, sample.kind = "Rounding")
#build final model with mtry =2
rf_fit2 <- randomForest(incomeLevel~age + race + sex + occupation + 
                          workclass + capitalMovement + hours.per.week+education.num +
                          marital.status + relationship, mtry = 2, data = train_set)

#note predictions from this model
y_hat_rf2 <- predict(rf_fit2, test_set) %>% factor(levels = c("0", "1"))

#note results
results[10, "Model"] <- "Random Forest - tuned"
results[10,"Accuracy"] <- confusionMatrix(y_hat_rf2, test_set$incomeLevel)$overall["Accuracy"]
results[10,"F1_score"] <- F_meas(y_hat_rf2, test_set$incomeLevel)
results[10, "Sensitivity"] <- sensitivity(y_hat_rf2, test_set$incomeLevel)
results[10, "Specificity"] <- specificity(y_hat_rf2, test_set$incomeLevel)
results %>% knitr::kable()

#the same exercise as above, with 100 trees specified
set.seed(1, sample.kind = "Rounding")
rf_fit3 <- randomForest(incomeLevel~age + race + sex + occupation + 
                          workclass + capitalMovement + hours.per.week+education.num +
                          marital.status + relationship, mtry = 2, nTree = 100, data = train_set)
y_hat_rf3 <- predict(rf_fit3, test_set) %>% factor(levels = c("0", "1"))
results[11, "Model"] <- "Random Forest - final"
results[11,"Accuracy"] <- confusionMatrix(y_hat_rf3, test_set$incomeLevel)$overall["Accuracy"]
results[11,"F1_score"] <- F_meas(y_hat_rf3, test_set$incomeLevel)
results[11, "Sensitivity"] <- sensitivity(y_hat_rf3, test_set$incomeLevel)
results[11, "Specificity"] <- specificity(y_hat_rf3, test_set$incomeLevel)
results %>% knitr::kable()

#####Results#####

results[c(5,7,11), ] %>% knitr::kable()

confusionMatrix(y_hat_logit_test, test_set$incomeLevel)
confusionMatrix(y_hat_rf2, test_set$incomeLevel)