# Load necessary libraries
install.packages(c("GGally", "ggmap", "mosaic", "treemap", "caret", "gains", "rpart", "rpart.plot", "pROC", "styler", "ggplot2", "gplots"))
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(readxl)
library(ggplot2)
library(gplots)
library("gridExtra")
# Read the data from the Excel file
alzheimer <- read_excel("alzheimer.xlsx")

# Convert Alzimer column to a factor variable
alzheimer$Alzimer <- as.factor(alzheimer$Alzimer)

# Display summary statistics for columns 3 to 10
summary(alzheimer[, 3:10])

#Finding Features with missing data
sapply(alzheimer, function(x) sum(is.na(x)))

#Checking outliers for the missing data features
par(mfcol = c(1, 2))
boxplot((alzheimer$SES), main = "SES", ylab = "Values")
boxplot((alzheimer$MMSE), main = "MMSE", ylab = "Values")

#Converting categorical data to numerical
alzheimer$Alzimer <- ifelse(alzheimer$Alzimer == "Nondemented", 0, 1)
alzheimer$`M/F` <- ifelse(alzheimer$`M/F` == "M", 1, 0)

#Imputing the missing values with median for both the features
medianses <- median(alzheimer$SES, na.rm = TRUE)
alzheimer$SES[is.na(alzheimer$SES)] <- medianses

medianMMSE <- median(alzheimer$MMSE, na.rm = TRUE)
alzheimer$MMSE[is.na(alzheimer$MMSE)] <- medianMMSE

#Cross checking
which(is.na(alzheimer))


#Finding corrlinearity between the features
cor((alzheimer))

#Data visualization

#a)Stackbarplot with gender features
stackchartab <- table(alzheimer$`M/F`, alzheimer$Alzimer)
plot.new()
barplot(stackchartab, col = c("blue", "red"), legend = rownames(stackchartab), xlab = "dementia", ylab = "No. of Patients", ylim = c(0, 200))


#Converting the feature as factor
alzheimer$Alzimer <- as.factor(alzheimer$Alzimer)


#Histogram of both the factor i.e demented or not for all the features to find the relaion
p1 <- ggplot(alzheimer, aes(x = MMSE, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")

p2 <- ggplot(alzheimer, aes(x = ASF, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")

p3 <- ggplot(alzheimer, aes(x = eTIV, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")

p4 <- ggplot(alzheimer, aes(x = nWBV, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")


p5<-ggplot(alzheimer, aes(x = EDUC, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")

p6 <- ggplot(alzheimer, aes(x = Age, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")

p7 <- ggplot(alzheimer, aes(x = CDR, color = Alzimer)) +
  geom_histogram(fill = "white", alpha = 0.5, position = "identity")

#Plotting at once for better comparision
grid.arrange(p5, p4, p6, nrow = 3)


# Classification

#Data partitioning with 75% for training and remaining 25% for validation
#myIndex <- createDataPartition(alzheimer$Alzimer, p = 0.75, list = FALSE)
#myIndex

trainSet <- alzheimer[1:281, ]
dim(trainSet)
validationSet <- alzheimer[282:373, ]
dim(validationSet)
dim(alzheimer)

trainSet$Alzimer <- as.factor(trainSet$Alzimer)
validationSet$Alzimer <- as.factor(validationSet$Alzimer)

# Fit logistic regression model
Logistic_Model <- glm(Alzimer ~ CDR + Age + MMSE + EDUC + nWBV + eTIV + ASF, family = binomial, data = trainSet)

Logistic_Model
summary(Logistic_Model)

# Make predictions on validation set
pHat1 <- predict(Logistic_Model, validationSet, type = "response")
pHat1

# Calculate performance metrics
# (Accuracy, Sensitivity, Specificity)
# Display confusion matrix
yHat1 <- ifelse(pHat1 >= 0.1, 1, 0)

yTP1 <- ifelse((yHat1 == 1 & validationSet$Alzimer == 1), 1, 0)

yTN1 <- ifelse((yHat1 == 0 & validationSet$Alzimer == 0), 1, 0)


sprintf("Accuracy measure for model1 = %f", 100 * mean(validationSet$Alzimer == yHat1))
sprintf("Sensitivity for Model1 = %f", 100 * (sum(yTP1) / sum(validationSet$Alzimer == 1)))
sprintf("Specificity for Model1 = %f", 100 * (sum(yTN1) / sum(validationSet$Alzimer == 0)))


# Confusion Matrix
cf <- caret::confusionMatrix(data=factor(yHat1),reference=validationSet$Alzimer,positive = "0")
print(cf)



#K-fold

myControl <- trainControl(method = "repeatedcv", number = 4, repeats = 5)

Model1 <- train(Alzimer ~ CDR + Age + MMSE + EDUC + nWBV + eTIV + ASF, data = trainSet, trControl = myControl, method = "glm", family = binomial, metric = "Accuracy")
Model1

summary(Model1)

pHat2 <- predict(Model1, validationSet, type = "prob")

yHat2 <- ifelse(pHat2 >= 0.5, 1, 0)

cf <- caret::confusionMatrix(data=factor(yHat1),
                             reference=validationSet$Alzimer)
print(cf)


#--------

# Decision Tree

set.seed(1)
options(scipen = 999)


#creating default tree
default_tree <- rpart(Alzimer ~ ., data = trainSet, method = "class")

summary(default_tree)

prp(default_tree, type = 1, extra = 1, under = TRUE)

#creating full tree
full_tree <- rpart(Alzimer ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)

prp(full_tree, type = 1, extra = 1, under = TRUE)


printcp(full_tree)

#creating prune tree using the cp value with the minimal xerror
pruned_tree <- prune(full_tree, cp = 0.0096618)
plot.new()
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

validationSet$Alzimer <- as.factor(validationSet$Alzimer)

predicted_class <- predict(pruned_tree, validationSet, type = "class")

confusionMatrix(predicted_class, validationSet$Alzimer, positive = "0")

#-----ROC curve

# ROC Curve for Logistic Regression Model
roc_lr <- roc(validationSet$Alzimer, pHat1)
auc_lr <- auc(roc_lr)

# ROC Curve for Decision Tree Model
roc_dt <- roc(as.numeric(validationSet$Alzimer) - 1, as.numeric(predict(pruned_tree, validationSet, type = "class")))
auc_dt <- auc(roc_dt)

# Plot ROC Curves
plot(roc_lr, col = "blue", main = "ROC Curve Comparison", col.main = "black", lwd = 2, cex.main = 1.2, percent = TRUE)
lines(roc_dt, col = "red", lwd = 2)
legend("bottomright", legend = c(paste("Logistic Regression (AUC =", round(auc_lr, 2), ")", sep = ""),
                                 paste("Decision Tree (AUC =", round(auc_dt, 2), ")", sep = "")),
       col = c("blue", "red"), lty = 1, lwd = 2)

# Display AUC values
sprintf("AUC for Logistic Regression: %f", auc_lr)
sprintf("AUC for Decision Tree: %f", auc_dt)


