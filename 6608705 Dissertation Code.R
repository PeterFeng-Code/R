# MSc Business Analytics Dissertation
# Student ID: 6608705

# Clears all objects in "global environment"
rm(list = ls())

# clears the console area
cat("\014")

# Sets R random numbers to start at the same sequence
set.seed(1234)

# Machine Learning & Visualisation Individual Assignment
print("Student ID: 6608705 Dissertation:HR Analytics Of Different Factors Affect Employee Attrition")

source("4labfunctions.R")
source("lab4DataPrepNew.R")


# Install packages and load libraries
myLibraries <- c(
  "scatterplot3d",
  "caret",
  "plyr",
  "corrplot",
  "ggplot2",
  "scales",
  "ggthemes",
  "e1071",
  "lattice",
  "Epi",
  "party",
  "cowplot",
  "MASS",
  "tidyverse",
  "readr",
  "pROC",
  "rpart",
  "rpart.plot",
  "randomForest",
  "formattable",
  "repr",
  "treemapify",
  "plotly",
  "plotrix",
  "ggcorrplot",
  "RColorBrewer"
)

library(pacman)
pacman::p_load(char = myLibraries, install = TRUE, character.only = TRUE)

# load the data
hr <- read_csv("HR-Employee-Attrition-Data.csv")

### Data Preprocessing

# check for missing value
colSums(is.na(hr)) # no missing value identified

# check all data type is correct
str(hr)


# remove unused variables
hr <- hr[, -c(9, 10, 22, 27)]

# change numerical variable to factors
hr$Education <- factor(hr$Education)
hr$EnvironmentSatisfaction <- factor(hr$EnvironmentSatisfaction)
hr$JobInvolvement <- factor(hr$JobInvolvement)
hr$JobLevel <- factor(hr$JobLevel)
hr$JobSatisfaction <- factor(hr$JobSatisfaction)
hr$PerformanceRating <- factor(hr$PerformanceRating)
hr$RelationshipSatisfaction <- factor(hr$RelationshipSatisfaction)
hr$StockOptionLevel <- factor(hr$StockOptionLevel)
hr$WorkLifeBalance <- factor(hr$WorkLifeBalance)
hr$Attrition <- factor(hr$Attrition)
hr$BusinessTravel <- factor(hr$BusinessTravel)


# checking for outlier
boxplot(hr$DailyRate, main="outlier check", boxwex=0.1)$out
boxplot(hr$MonthlyIncome, main="outlier check", boxwex=0.1)$out


### *************Data Visualisation************* ###

# Descriptive Statistics

# Employee Attrition (figure 5 and 6 in the report)
ggplot(hr, aes(x = Attrition)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

# Employee Attrition in percentange
attrition_plot <- ggplot(hr, aes(x = Attrition)) +
  ggtitle("Attrition rate") +
  xlab("Attrition") +
  geom_bar(aes(y = (..count..) / sum(..count..)), width = 0.5) +
  ylab("Attrition number") +
  scale_y_continuous(labels = scales::percent)
print(attrition_plot)



# age and employee attrition split into group
group_age <- hr %>%
  mutate(
    Age = case_when(
      Age > 0 & Age <= 18 ~ "18 Years Old",
      Age > 18 & Age <= 30 ~ "18-30 Years Old",
      Age > 30 & Age <= 40 ~ "31-40 Years Old",
      Age > 40 & Age <= 55 ~ " 41-55 Years Old",
      Age > 55 ~ "55 Years Old and Older"
    )
  )

# age and employee attrition (figure 7)
ggplot(group_age, aes(x = Age, fill = Attrition)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")

group_age %>%
  filter(Age >= 55 & Attrition == "Yes") %>%
  select(JobRole) %>%
  count(JobRole)




# Highest percentage of attrition by JobRole (figure 8)
options(repr.plot.width = 8, repr.plot.height = 5)

# Average Salary
job.sal <- hr %>%
  select(JobRole, MonthlyIncome) %>%
  group_by(JobRole) %>%
  summarize(avg = mean(MonthlyIncome))


p1 <- ggplot(job.sal, aes(x = JobRole, y = MonthlyIncome)) +
  geom_bar(stat = "identity", width = .5, fill = "#2eb5fe") +
  labs(
    title = "Salary by Job Role",
    subtitle = "Income",
    x = "Job Role",
    y = "Income"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6))
print(p1)




# attrition by job role (figure 9)
options(repr.plot.width = 20, repr.plot.height = 10)
attr.job <- hr %>%
  select(JobRole, Attrition) %>%
  group_by(JobRole, Attrition) %>%
  summarize(amount = n()) %>%
  mutate(pct = round(prop.table(amount), 2) * 100) %>%
  arrange(pct)

nofunc <- colorRampPalette(c("#2ed1fe", "#2e93fe", "#2e73fe"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))

yes.attr <- attr.job %>%
  filter(Attrition == "Yes") %>%
  arrange(JobRole)
no.attr <- attr.job %>%
  filter(Attrition == "No") %>%
  arrange(JobRole)

par(mar = pyramid.plot(no.attr$pct, yes.attr$pct,
                       labels = unique(attr.job$JobRole),
                       top.labels = c("No", "", "Yes"), main = "Attrition by Job Role",
                       gap = 42, show.values = T, rxcol = yesfunc(9), lxcol = nofunc(15)
))




# percentage salary hike and employee attrition (figure 10)
per.sal <- hr %>%
  select(Attrition, PercentSalaryHike, MonthlyIncome) %>%
  ggplot(aes(x = PercentSalaryHike, y = MonthlyIncome)) +
  geom_jitter(aes(col = Attrition), alpha = 0.5) +
  theme_economist() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#58FA58", "#FA5858")) +
  labs(title = "Percentage Salary Increase and Employee Attrition") +
  theme(
    plot.title = element_text(hjust = 0.5, color = "white"), plot.background = element_rect(fill = "#0D7680"),
    axis.text.x = element_text(colour = "white"), axis.text.y = element_text(colour = "white"),
    axis.title = element_text(colour = "white")
  )+ 
  labs(x="Percentage Salary Increase",y="Monthly Income")+ 
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"))
print(per.sal)


# job satisfaction and employee attrition (figure 11)
ggplot(hr, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

# job satisfaction by job role (figure 12)
ggplot(hr, aes(x = JobRole, fill = JobSatisfaction)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  scale_fill_discrete(name = "Job Satisfaction", labels = c("low", "medium", "high", "very high"))





# workingEnvironment Satisfaction by job role (figure 13)
options(repr.plot.width = 8, repr.plot.height = 5)

env.attr <- hr %>%
  select(EnvironmentSatisfaction, JobRole, Attrition) %>%
  group_by(JobRole, Attrition) %>%
  summarize(avg.env = mean(EnvironmentSatisfaction))

ggplot(env.attr, aes(x = JobRole, y = avg.env)) +
  geom_line(aes(group = Attrition), color = "#58ACFA", linetype = "dashed") +
  geom_point(aes(color = Attrition), size = 3) +
  theme(
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
  ) +
  labs(title = "Working Environment and Employee Attrition", y = "Avg Working Environment Satisfaction", x = "Job Role") +
  scale_color_manual(values = c("#58FA58", "#FA5858"))


env.attr2 <- hr %>%
  select(EnvironmentSatisfaction, JobRole, Attrition) %>%
  group_by(JobRole, Attrition)

ggplot(env.attr2, aes(x = JobRole, y = EnvironmentSatisfaction)) +
  geom_line(aes(group = Attrition), color = "#58ACFA", linetype = "dashed") +
  geom_point(aes(color = Attrition), size = 3) +
  theme_economist()+
  theme(
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
    plot.background = element_rect(fill = "#FFF1E0")
  ) +
  labs(title = "Working Environment and Employee Attrition 2", y = "Working Environment Satisfaction", x = "Job Role") +
  scale_color_manual(values = c("#58FA58", "#FA5858"))



# reationship with manager and empoyee attrition 

years_manager <- hr %>%
  mutate(
    YearsWithCurrManager = case_when(
      YearsWithCurrManager >= 0 & YearsWithCurrManager <= 5 ~ "0-5 years",
      YearsWithCurrManager >= 6 & YearsWithCurrManager <= 10 ~ "6-10 years",
      YearsWithCurrManager >= 11 & YearsWithCurrManager <= 15 ~ "10-15 years",
      YearsWithCurrManager >= 16 & YearsWithCurrManager <= 20 ~ "16-20 years",
      YearsWithCurrManager >= 21 ~ "21 years and longer"
    )
  )

# years with manager and empoyee attrition (figure 14)
ggplot(years_manager, aes(x = YearsWithCurrManager, fill = Attrition)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")


# reationship satisfaction, years with manager and empployee attrition (figure 15)
relation.attr <- hr %>%
  select(RelationshipSatisfaction, YearsWithCurrManager, Attrition) %>%
  group_by(RelationshipSatisfaction, Attrition) %>%
  summarize(avg.reation = mean(YearsWithCurrManager))

ggplot(relation.attr, aes(x = RelationshipSatisfaction, y = avg.reation)) +
  geom_line(aes(group = Attrition), color = "#58ACFA", linetype = "dashed") +
  geom_point(aes(color = Attrition), size = 3) +
  theme_economist() +
  theme(
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90),
    plot.background = element_rect(fill = "#FFF1E0")
  ) +
  labs(title = "Reationship Satisfaction and Employee Attrition", y = "Avg Years With Manager", x = "Relationship Satisfaction") +
  scale_color_manual(values = c("#58FA58", "#FA5858"))




# work life balance (figure 16)
options(repr.plot.width = 3, repr.plot.height = 5)

attritions <- hr %>% filter(Attrition == "Yes")

attritions$WorkLifeBalance <- as.factor(attritions$WorkLifeBalance)

department <- attritions %>%
  select(Department, WorkLifeBalance) %>%
  group_by(Department, WorkLifeBalance) %>%
  dplyr::summarize(count = n()) %>%
  ggplot(aes(x = fct_reorder(WorkLifeBalance, -count), y = count, fill = Department)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Department) +
  theme_economist() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "#94d6d6")) +
  scale_fill_manual(values = c("#FA5882", "#819FF7", "#FE2E2E")) +
  geom_label(aes(label = count, fill = Department), colour = "white", fontface = "italic") +
  labs(title = "Work Life Balance by Department", x = "Work and Life Balance", y = "Number of Employees")

department


# work life balane and employee attrition rate (figure 17)
ggplot(hr, aes(x = WorkLifeBalance, fill = Attrition)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0.8, colour = "black")

worklifebalance_rate<- hr %>% 
  select(JobRole, WorkLifeBalance, Attrition) %>% 
  group_by(JobRole) %>% 
  filter(Attrition=='Yes', WorkLifeBalance==4) %>% 
  count(JobRole)

print(worklifebalance_rate)

# overtime at work (figure 18)
overtime_percent <- hr %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% 
  ggplot(aes(x="", y=pct, fill=OverTime)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  theme_tufte() + scale_fill_manual(values=c("#d3ebe6", "#e69e2c")) + 
  geom_label(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), colour = "black",  fontface = "italic")+
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), plot.subtitle=element_text(color="white"),
        axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="black"), 
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid", colour ="black")) + 
  labs(title="Overtime and Employee Attrition", x="", y="") 
print(overtime_percent)



# business travel (figure 19)
work_bal_pct <- hr %>% select(Attrition, BusinessTravel, WorkLifeBalance) %>% group_by(Attrition, BusinessTravel) %>% 
  summarize(count=n()) %>% mutate(pct=round(prop.table(count),2) * 100) %>%
  ggplot(aes(x=Attrition, y=pct, fill=BusinessTravel, color=Attrition)) + geom_bar(stat='identity') + facet_wrap(~BusinessTravel) + theme_minimal() +  
  theme(legend.position="none") + 
  geom_label(aes(label=paste0(pct, "%"), fill = BusinessTravel), colour = "white", fontface = "italic")  + 
  scale_fill_manual(values=c("#e69053", "#aebef5", "#f02222")) +
  scale_color_manual(values=c("#808080", "#808080")) + labs(x="Attrition", y="Employee Attrition Percentage (%)") + coord_flip() + 
  theme(plot.background=element_rect(fill="#d0f6f7"))

print(work_bal_pct)


# correlation test (figure 20)

options(repr.plot.width=10, repr.plot.height=7) 

nums <- select_if(hr, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title="Correlation Plot For Different Attribute", 
           ggtheme=theme_minimal())


### Split data into training and testing data
hr_2 <- hr %>% mutate_if(is.character, as.factor)

training_records <- round(nrow(hr_2) * (70 / 100))

training_data <- hr_2[1:training_records, ]

testing_data <- hr_2[-(1:training_records), ]

### *************** Logistic Regression Model ********************* ###

# change the testing value data from word to number
test_expected <- plyr::mapvalues(testing_data$Attrition, from = c("No", "Yes"), to = c(0, 1))

# logistic regression training model
logistic_model_hr <- glm(Attrition ~ ., data = training_data, family = binomial)
print(summary(logistic_model_hr))

# logistic regression testing model
predict_data_logModel <- predict(logistic_model_hr, testing_data, type = "response")

# confusion matrix
measures <- NdetermineThreshold(
  test_expected = test_expected,
  test_predicted = predict_data_logModel,
  plot = TRUE,
  title = "Logistic Regression Model"
)

NprintMeasures(results = measures, title = "Logistic Regression Model")

#Rank the importance of the variables based on logistic regression in descending order (figure 21)
importance<-as.data.frame(caret::varImp(logistic_model_hr , scale = TRUE))
row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
barplot(t(importance[order(importance$Overall),,drop=FALSE]),las=3, border = 0, cex.names =0.5)
### ************** Decision Tree Model ************************ ###

# Decision Tree Training Model

dt_hr <- rpart(Attrition ~ ., data = training_data, method = "class")
print(summary(dt_hr))

rpart.plot(dt_hr, extra = 106, cex = 0.6)

predict_dt_hr <- predict(dt_hr, testing_data)

# confusion matrix and ROC curve for decision tree
probabilityYesChurn_dt <- predict_dt_hr[, "Yes"]
measures_dt <- NdetermineThreshold(
  test_expected = test_expected,
  test_predicted = probabilityYesChurn_dt,
  plot = TRUE,
  title = "Decision Tree Model"
)

NprintMeasures(results = measures_dt, title = "Decision Tree Model")

threshold_dt <- measures_dt$threshold
print(paste("threshold for decision tree is", threshold_dt))

suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(tree))

var_imp <- data.frame(dt_hr$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$dt_hr.variable.importance, 2)
var_imp$dt_hr.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp %>%
  ggplot(aes(x = reorder(features, importance), y = importance, fill = features)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "black"), plot.subtitle = element_text(color = "black"),
    axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    legend.background = element_rect(
      fill = "#FFF9F5",
      size = 0.5, linetype = "solid",
      colour = "black"
    )
  ) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(colorCount)) +
  geom_label(aes(label = paste0(importance, "%")), colour = "white", fontface = "italic", hjust = 0.6) +
  labs(title = "Attributes Importance for the Decision Tree Model", x = "Attributes", y = "Importance Index")

print(feature_importance)
### ************ Random Forest Model *********************** ###

rf_hr <- randomForest(Attrition ~ ., data = training_data, ntree = 1500)
print(importance(rf_hr))
print(varImpPlot(rf_hr))

rf_predicted <- predict(rf_hr, testing_data, type = "prob")

# confusion matrix and ROC curve for random forest
probabilityYesChurn_rf <- rf_predicted[, "Yes"]
measures_rf <- NdetermineThreshold(
  test_expected = test_expected,
  test_predicted = probabilityYesChurn_rf,
  plot = TRUE,
  title = "Random Forest Model"
)

NprintMeasures(results = measures_rf, title = "Random Forest Model")

threshold_rf <- measures_rf$threshold
print(paste("threshold for random forest is", threshold_rf))





### new feature selection and machine learning model analysis

# multicollinearity on the logistic regression to select new features
car::vif(logistic_model_hr)


hr_3<-dplyr::select(hr_2,-c(EducationField, MaritalStatus, StockOptionLevel, 
                            PercentSalaryHike, PerformanceRating, StockOptionLevel, TotalWorkingYears, YearsAtCompany, 
                            YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager))



training_records_2 <- round(nrow(hr_3) * (70 / 100))

training_data_2 <- hr_3[1:training_records, ]

testing_data_2 <- hr_3[-(1:training_records), ]

test_expected_2 <- plyr::mapvalues(testing_data_2$Attrition, from = c("No", "Yes"), to = c(0, 1))


# new logistic regression after multicollinearity filtering
logistic_model_hr_2 <- glm(Attrition ~ ., data = training_data_2, family = binomial)
print(summary(logistic_model_hr_2))

# logistic regression testing model
predict_data_logModel_2 <- predict(logistic_model_hr_2, testing_data_2, type = "response")

# confusion matrix
measures_2 <- NdetermineThreshold(
  test_expected = test_expected_2,
  test_predicted = predict_data_logModel_2,
  plot = TRUE,
  title = "Logistic Regression Model 2"
)

NprintMeasures(results = measures_2, title = "Logistic Regression Model 2")

#Rank the importance of the variables based on logistic regression in descending order (figure 21)
importance_2<-as.data.frame(caret::varImp(logistic_model_hr_2 , scale = TRUE))
row.names(importance_2)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance_2))
barplot(t(importance_2[order(importance_2$Overall),,drop=FALSE]),las=3, border = 0, cex.names =0.5)


# new decision tree after multicollinearity filtering
dt_hr_2 <- rpart(Attrition ~ ., data = training_data_2, method = "class")
print(summary(dt_hr_2))

rpart.plot(dt_hr_2, extra = 106, cex = 0.6)

predict_dt_hr_2 <- predict(dt_hr_2, testing_data_2)

# confusion matrix and ROC curve for decision tree
probabilityYesChurn_dt_2 <- predict_dt_hr_2[, "Yes"]
measures_dt_2 <- NdetermineThreshold(
  test_expected = test_expected_2,
  test_predicted = probabilityYesChurn_dt_2,
  plot = TRUE,
  title = "Decision Tree Model 2"
)

NprintMeasures(results = measures_dt_2, title = "Decision Tree Model 2")

threshold_dt <- measures_dt_2$threshold
print(paste("threshold for decision tree is", threshold_dt))

var_imp_2 <- data.frame(dt_hr_2$variable.importance)
var_imp_2$features <- rownames(var_imp_2)
var_imp_2 <- var_imp_2[, c(2, 1)]
var_imp_2$importance <- round(var_imp_2$dt_hr_2.variable.importance, 2)
var_imp_2$dt_hr_2.variable.importance <- NULL

colorCount_2 <- length(unique(var_imp_2$features))
feature_importance_2 <- var_imp_2 %>%
  ggplot(aes(x = reorder(features, importance), y = importance, fill = features)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "black"), plot.subtitle = element_text(color = "black"),
    axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    legend.background = element_rect(
      fill = "#FFF9F5",
      size = 0.5, linetype = "solid",
      colour = "black"
    )
  ) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(colorCount_2)) +
  geom_label(aes(label = paste0(importance, "%")), colour = "black", fontface = "italic", hjust = 0.6) +
  labs(title = "Feature Importance for the Decision Tree Model 2", x = "Attributes", y = "Importance Index")

print(feature_importance_2)




# new random forest after multicollinearity filtering
rf_hr_2 <- randomForest(Attrition ~ ., data = training_data_2, ntree = 1500)
print(importance(rf_hr_2))
print(varImpPlot(rf_hr_2))

rf_predicted_2 <- predict(rf_hr_2, testing_data_2, type = "prob")

# confusion matrix and ROC curve for random forest
probabilityYesChurn_rf_2 <- rf_predicted_2[, "Yes"]
measures_rf_2 <- NdetermineThreshold(
  test_expected = test_expected_2,
  test_predicted = probabilityYesChurn_rf_2,
  plot = TRUE,
  title = "Random Forest Model 2"
)

NprintMeasures(results = measures_rf_2, title = "Random Forest Model 2")

threshold_rf_2 <- measures_rf_2$threshold
print(paste("threshold for random forest is", threshold_rf_2))

