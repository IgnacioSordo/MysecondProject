# Pima Indian Diabetes Dataset

# load libraries
library(mlbench)
library(caret)
library(Hmisc)
library(randomForest)
library(Matrix)
library(glmnet)
library (pROC)
library(ROCR)
library(funModeling)
library(dplyr)
library(smotefamily)
library(mlbench)
library(ggpubr)
library(onewaytests)

# load data
data(PimaIndiansDiabetes)
# rename dataset to keep code below generic
dataset <- PimaIndiansDiabetes

#dataset description.

head(dataset)

# cuantitative variables
df = dataset[,c(1:8)] 

# statitical descriptives
estadisticaDescriptiva <- t(do.call(data.frame, 
                                    list(mean = apply(df, 2, mean),
                                         Desv.Estandar = apply(df, 2, sd),
                                         Mediana = apply(df, 2, median),
                                         IQR = apply(df,2,IQR),
                                         Min = apply(df, 2, min),
                                         Max = apply(df, 2, max),
                                         Rango = apply(df, 2,max)-apply(df, 2,min),
                                         Cuartil1 = apply(df,2,quantile,probs = c(0.25)),
                                         Cuartil3 = apply(df,2,quantile,probs = c(0.75)),
                                         N = apply(df,2,length),
                                         ErrorEstandar = apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaLower = apply(df,2,mean)-1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         IC95MediaUpper = apply(df,2,mean)+1.96*apply(df,2,sd)/sqrt(apply(df,2,length)),
                                         Varianza = apply(df, 2, var)
                                    )))
estadisticaDescriptiva

# Normality Test "Pregnant Vairable"

df <- dataset [1]
dvector= df$pregnant
shapiro.test(dvector) 


#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Distribución Normal") + ylab("Cuartiles reales") +
  theme_minimal() +
  ggtitle("QQ PLOT pregnant") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [1]
dvector= df$pregnant

# Normality Test "Glucose Vairable"

df <- dataset [2]
dvector= df$glucose
shapiro.test(dvector) 

#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT glucose") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [2]
dvector= df$glucose

# Normality Test "Pressure Variable"

df <- dataset [3]
dvector= df$pressure
shapiro.test(dvector) 

#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT pressure") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [3]
dvector= df$pressure

# Normality Test "Triceps Variable"

df <- dataset [4]
dvector= df$triceps
shapiro.test(dvector) 

#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT triceps") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [4]
dvector= df$triceps

# Normality Test "Insulin Variable"

df <- dataset [5]
dvector= df$insulin
shapiro.test(dvector)

#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT insulin") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [5]
dvector= df$insulin

# Normality Test "Mass Variable"

df <- dataset [6]
dvector= df$mass
shapiro.test(dvector)

#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT mass") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [6]
dvector= df$mass

# Normality Test "Pedigree Variable"

df <- dataset [7]
dvector= df$pedigree
shapiro.test(dvector)
#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT pedigree") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [7]
dvector= df$pedigree

# Normality Test "Pedigree Variable"

df <- dataset [8]
dvector= df$age
shapiro.test(dvector)
#...hacemos el qqplot
name1 <- names(df)
ggqqplot(df, x = names(df),color = "#FF6666",add.params = list(color = "black"))+
  xlab("Normal Distribution") + ylab("Real quartiles") +
  theme_minimal() +
  ggtitle("QQ PLOT age") +
  theme(plot.title = element_text(hjust = 0.5))

df <- dataset [8]
dvector= df$age


#para ver si las variables presentan diferencias estadisticamente significativas.

bf.test(pregnant ~ diabetes, data = dataset)
bf.test(glucose ~ diabetes, data = dataset)
bf.test(pressure~ diabetes, data = dataset)
bf.test(triceps ~ diabetes, data = dataset)
bf.test(insulin ~ diabetes, data = dataset)
bf.test(mass ~ diabetes, data = dataset)
bf.test(pedigree ~ diabetes, data = dataset)
bf.test(age~ diabetes, data = dataset)

wilcox.test(pregnant~ diabetes, data = dataset, paired=FALSE)
wilcox.test(glucose~ diabetes, data = dataset, paired=FALSE)
wilcox.test(pressure~ diabetes, data = dataset, paired=FALSE)
wilcox.test(triceps~ diabetes, data = dataset, paired=FALSE)
wilcox.test(insulin~ diabetes, data = dataset, paired=FALSE)
wilcox.test(mass~ diabetes, data = dataset, paired=FALSE)
wilcox.test(pedigree~ diabetes, data = dataset, paired=FALSE)
wilcox.test(age~ diabetes, data = dataset, paired=FALSE)


# define an 70%/30train/test split of the dataset
split=0.70

trainIndex <- createDataPartition(dataset$diabetes, p=split, list=FALSE)
data_train <- dataset[ trainIndex,]
data_test  <- dataset[-trainIndex,]


# set-up test options
control <- trainControl(method="cv", number=5)
seed <- (338)
metric <- "Accuracy"

#  train algorithms

# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(diabetes~., data=data_train, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(diabetes~., data=data_train, method="glm", metric=metric, trControl=control)
# kNN
set.seed(seed)
fit.knn <- train(diabetes~., data=data_train, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# CART
set.seed(seed)
fit.cart <- train(diabetes~., data=data_train, method="rpart", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(diabetes~., data=data_train, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(diabetes~., data=data_train, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(diabetes~., data=data_train, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Compare algorithms
results <- resamples(list(lda=fit.lda, logistic=fit.glm,
                          knn=fit.knn, cart=fit.cart, 
                          bagged_CART=fit.treebag, rf=fit.rf, Stochastic_Gradient_Boosting=fit.gbm))
# Table comparison
summary(results)

# Dot-plot comparison
dotplot(results)

# Matriz de confusion con datos de validacion
print("Accuracy Test Data Linear Discriminant Analysis")
pred <- predict(newdata=data_test,fit.lda)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)


# Matriz de confusion con datos de validacion
print("Accuracy Test Data Linear Logistic Regression")
pred <- predict(newdata=data_test,fit.glm)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)


# Matriz de confusion con datos de validacion
print("Accuracy Test Data Knn")
pred <- predict(newdata=data_test,fit.knn)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)

# Matriz de confusion con datos de validacion
print("Accuracy Test Data Cart")
pred <- predict(newdata=data_test,fit.cart)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)


# Matriz de confusion con datos de validacion
print("Accuracy Test Data Bagged Cart")
pred <- predict(newdata=data_test,fit.treebag)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)

# Matriz de confusion con datos de validacion
print("Accuracy Test Data Random Forest")
pred <- predict(newdata=data_test,fit.rf)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)

# Matriz de confusion con datos de validacion
print("Accuracy Test Data Stochastic Gradient Boosting")
pred <- predict(newdata=data_test,fit.gbm)
real <- as.factor(data_test$diabetes)
cm_train_tot <- caret::confusionMatrix(data=pred,reference=real)
print(cm_train_tot)