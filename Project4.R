#A.

library(data.table)
library(dplyr)
library(readxl)
library(writexl)

#Import data set.

setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Week 2/Script & Data Week 2")

ces51 = read_excel("W02m_ces_state.xlsx", sheet = "raw")

#Create variable in data set for State Code by subsetting positions 4-5 in the ID variable.
ces51$state = substr(ces51$id, 4, 5)

#Create a variable in data set for Sector by subsetting positions 11-14 in the ID variable.
ces51$sector = substr(ces51$id, 11, 14)

#Subset sectors 0000 and 3100 from data set into a new data set. Spread sectors into new columns and have their values be the total number of employees for each.
ces_51 = subset(ces51, ces51$sector == "0000" | ces51$sector == "3100")
ces_51 = spread(ces_51, sector, jan_2021)
ces_51 = subset(ces_51[,12:14]) #Subset the state, sector, and tot_emp columns from the new data set.
colnames(ces_51) = c("state", "jan_2021_total", "durable") #Rename the columns in the data set.

#Calculate the percentage of sector 3100 workers over the total number of employees for each state (0000) and save to new data set.
ces_51_0 = ces_51 %>% group_by(state) %>% mutate(per = durable / lag(jan_2021_total, default = 0))
ces_51_0[17,]$per = 0 #Change per value for state 11 from NA to 0. 
ces_51_0[62,]$per = 0 #Change per value for state 34 from NA to 0.
ces_51_0 = ces_51_0[!is.na(ces_51_0$per),] #Remove rows in durable column with NA value.

#Subset state and per columns from ces_51_0 data set.
ces_51_0 = subset(ces_51_0[,c(1, 4)])
colnames(ces_51_0) = c("ID", "P3100") #Rename columns in data set.
ces_51_0 = ces_51_0[-9, ]
ces_51_0$State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
"Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
"New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
"Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
"Washington", "West Virginia", "Wisconsin", "Wyoming") #Add state column to data set.                                   
setcolorder(ces_51_0, c("ID", "State", "P3100")) #Reorder columns in data set.                                                                        

#Export data set to Excel file.
outputDir = "C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 4"
write_xlsx(ces_51_0, paste0(outputDir, "_ces_state_p3100.xlsx"))

#B.

library(leaps)

setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 4/Data_project4/Data")

#Import data sets.
mincome = read_excel("ACS_17_5YR_S1903.xlsx") #Median household income from Census.
education = read_excel("ACS_17_5YR_S1501.xlsx") #Education data from Census.
dmv = read_excel("dmv_zip.xlsx") #California DMV data.

#Subset mincome and education data sets and rename variables.
income17 = mincome[, c(2,8)]
colnames(income17) = c("id", "income")
income17$income = as.numeric(gsub("-", "NA", income17$income)) #Substitute -s for NAs in income variable.

ed17 = education[, c(2,64,78,90,102,114,126,138,150,184,220,256,292)]
colnames(ed17) = c("id", "pop17", "below9", "g912", "hs", "scollege", "associate", "bachelor", "higher", "p2534", "p3544", "p4564", "p65a") #p2534: percentage of people ages 25 to 34. p65a: percentage of people with age above 65. 
str(ed17) 
ed17[, 3:9] = sapply(ed17[, 3:9],as.numeric) #Change variables 3-9 from character to numeric types.

#Calculate CHCI and add it to ed17 data set.
ed17$chci17_a25_44 = (((50 * ed17$below9) + (100 * ed17$g912) + (120 * ed17$hs) + (130 * ed17$scollege) + (140 * ed17$associate) + (190 * ed17$bachelor) + (230 * ed17$higher)) / 100)
ed17$chci17_a25_44 = gsub("NA", "0", ed17$chci17_a25_44) #Substitute NAs for 0s in income variable.

#Merge income17 data, ed17 data, and zip data.
chci_zip = merge(dmv, ed17, by = "id", all.x = T)
chci_zip = merge(chci_zip, income17, by = "id", all.x = T)

#Subset id, p_beph, pop17, chci_a25_44, and income columns from data set.
chci_zip = subset(chci_zip[, c(1, 4, 15, 27, 28)])                                                 
chci_zip$chci17_a25_44 = as.numeric(chci_zip$chci17_a25_44)

#Run a  multivariate regression on the chci_zip data with p_beph as the dependent variable.
lm.fit0 = lm(chci_zip$p_beph ~ chci_zip$pop17 + chci_zip$chci17_a25_44 + chci_zip$income, data = chci_zip)
summary(lm.fit0) #Adjusted R^2 = 0.57.

#Based on the results of multivariate linear regression, all 3 variables are associated with EV adoption rate.
#All 3 have t values > 2. The CHCI and income variables have t values of 17.9 and 16.2 respectively, whereas 
#population only has a t value of 2.3. Thus, CHCI and income are the strongest predictors of EV adoption %. The
#adjusted R^2 for this regression model is 0.57.

#Plot correlation between median household income and EV adoption %.
p = plot(chci_zip$income, chci_zip$p_beph, ylim = c(0, 10), main = "Correlation Between Median Household Income and EV Adoption %", 
         xlab = "Median Household Income (Dollars)", ylab = ("EV Adoption Pecentage"), cex.main = 0.9, pch = 20, col = "blue")
abline(lm(p_beph~income, data=ev), col="red", lwd=3) 

#The plot shows a linear positive correlation between median household income and EV adoption %.

#C.

library(caret)
library(Hmisc)

#Import breast cancer data set. 
uciwd = "https://archive.ics.uci.edu/ml/machine-learning-databases/"
mldata = paste0(uciwd,"breast-cancer-wisconsin/breast-cancer-wisconsin.data")
bcancer = read.csv(mldata, header = F)

#Rename data frame columns.
colnames(bcancer) = c("ID", "clump_thick", "cell_size", "cell_shape", "marginal", "epithelial", "nuclei",
                      "chromatin", "nucleoli", "mitoses", "class")

#Replace "?" in nuclei variable with NAs and change it to a numeric variable.
table(bcancer$nuclei)
bcancer$nuclei = as.numeric(gsub("\\?", "NA", bcancer$nuclei))

#Use imputation method to replace NAs in nuclei variable.
#Note: Cannot have both Hmisc and e1071 libraries activated or else it won't work.
bcancer$nuclei = impute(bcancer$nuclei, mean) #Replaces NAs with mean value.

#Change class variable so that a "2" represents "benign" and a "4" represents "malignant". Also change class variable to a factor.
bcancer$class = ifelse(bcancer$class =="2", "benign","malignant")
bcancer$class = as.factor(bcancer$class)

#Use rpart method to plot a tree chart for the classification of the bcancer data.
fit.tree = train(class ~., data = bcancer, method = "rpart") #rpart denotes a tree model.
fancyRpartPlot(fit.tree$finalModel) #Two most important predictors are cell_size and cell_shape.

#Use 10-fold cross validation to analyze LDA, Tree, KNN, Bayesian GLM, SVM, Random Forest, and XGBoosting models.
control = trainControl(method = "cv", number = 10) #Specify resampling procedure to be used in train function (10-fold cross validation).
metric = "Accuracy" #There are other metrics, such as "RMSE" or "ROC", which need more complex setting.

#Linear Discriminant Analysis (LDA).
set.seed(99)
fit.lda = train(class ~., data = bcancer, method = "lda", metric = metric, trControl = control)

#Classification and Regression Trees (CART).
set.seed(99)
fit.cart = train(class ~., data = bcancer, method = "rpart", metric = metric, trControl = control)

#k-Nearest Neighbors (KNN).
set.seed(99)
fit.knn = train(class ~., data = bcancer, method = "knn", metric = metric, trControl = control)

#Bayesian Generalized Linear Model. 
set.seed(99)
fit.logi = train(class ~., data = bcancer, method = "bayesglm", metric = metric, trControl = control)

#Support Vector Machines (SVM).
set.seed(99)
fit.svm = train(class ~., data = bcancer, method = "svmRadial", metric = metric, trControl = control)

# Random Forest.
set.seed(99)
fit.rf = train(class ~., data = bcancer, method = "rf", metric = metric, trControl = control)

#Gradient Boosting Machines/XGBoost.
set.seed(99)
fit.xgb = train(class ~., data = bcancer, method = "xgbLinear", metric = metric, trControl = control)

#Summarize accuracy and kappa for each of the the models.
results = resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, logi = fit.logi, svm = fit.svm, rf = fit.rf, xgb = fit.xgb))
summary(results)

#Mean accuracy and kappa values for each  model:
#LDA: 0.96, 0.91
#Tree: 0.93, 0.86
#KNN: 0.64, 0.10 
#Bayesian GLM: 0.97, 0.92
#SVM: 0.95, 0.90
#Random Forest: 0.97, 0.93 <-- best model!
#XGBoosting: 0.96, 0.92

#Summarize the Best Model.
#The random forest model is the best model because it has the highest mean accuracy and kappa, which adjusts for the 
#possibility of a correct prediction by chance alone. 
