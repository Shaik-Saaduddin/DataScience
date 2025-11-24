#1. Program 1

x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
cor_test_result <- cor.test(x, y, method = "pearson")
print(cor_test_result)

data <- data.frame(
 var1 = c(1, 2, 3, 4, 5),
 var2 = c(2, 4, 6, 8, 10),
 var3 = c(5, 6, 7, 8, 9)
)
cor_matrix <- cor(data, method = "pearson")
print(cor_matrix)

group1 <- c(1, 2, 3, 4, 5)
group2 <- c(2, 3, 4, 5, 6)
t_test_result <- t.test(group1, group2, var.equal = TRUE)
print(t_test_result)

group <- factor(c("A", "A", "B", "B", "C", "C"))
value <- c(1, 2, 3, 4, 5, 6)
anova_result <- aov(value ~ group)
summary(anova_result)

#2. Program 2

library(dplyr)
library(ggplot2)
library(corrplot)

cars_multi <- read.csv("cars_multi.csv")
cars_price <- read.csv("cars_price.csv")

head(cars_multi)
head(cars_price)
dim(cars_multi)
dim(cars_price)

cars <- left_join(cars_multi, cars_price, by = "ID")
colnames(cars)
sum(!complete.cases(cars))
summary(cars)
str(cars)

ggplot(cars, aes(mpg)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of MPG", y = "Count") +
  theme_classic()

ggplot(cars, aes(cylinders)) +
  geom_bar() +
  labs(title = "Cylinders", y = "Count") +
  theme_classic()

boxplot(cars$displacement, main = "Box Plot Displacement", xlab = "", ylab = "Displacement")

count(cars[as.character(cars$horsepower) == "?", ])

ggplot(cars, aes(weight)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Weight", y = "Count") +
  theme_classic()

ggplot(cars, aes(acceleration)) +
  geom_density() +
  labs(title = "Density of Acceleration") +
  theme_classic()

to_Plot <- as.data.frame(table(cars$model))
colnames(to_Plot) <- c("Model", "Frequency")

ggplot(to_Plot, aes(x = Model, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Model") +
  theme_classic()

ggplot(cars, aes(origin)) +
  geom_bar() +
  labs(title = "Origin", y = "Count") +
  theme_classic()

cars$price <- as.integer(cars$price)

boxplot(cars$price, main = "Price Box Plot", xlab = "", ylab = "Price")

to_Plot <- as.data.frame(table(cars$price))
colnames(to_Plot) <- c("Price", "Frequency")

ggplot(head(to_Plot[order(-to_Plot[, 2]), ]), aes(x = reorder(Price, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Common Price", x = "Price") +
  theme_classic() +
  coord_flip()

cars$horsepower <- as.numeric(as.character(cars$horsepower))
cars <- cars[complete.cases(cars), ]
cars <- cars[, -1]

nums <- sapply(cars, is.numeric)
correlations <- cor(cars[, nums])
corrplot(correlations, order = "hclust")

#3. Program 3

library(dplyr)
library(ggplot2)
library(caret)
library(datarium)

data("marketing", package = "datarium")
head(marketing)
dim(marketing)
summary(marketing)
str(marketing)

sum(!complete.cases(marketing))

ggplot(marketing, aes(sales)) +
 geom_histogram(binwidth = 5) +
 labs(title = "Histogram of Sales", y = "Count") +
 theme_classic()

ggplot(marketing, aes(x = youtube, y = sales)) +
 geom_point() +
 stat_smooth()

cor(marketing$sales, marketing$youtube)

set.seed(123)
training.samples <- marketing$sales %>% createDataPartition(p = 0.8, list = FALSE)

train.data <- marketing[training.samples,]
test.data <- marketing[-training.samples,]

model <- lm(sales ~ youtube, data = train.data)
summary(model)$coef

predictions <- model %>% predict(test.data)

RMSE(predictions, test.data$sales)
R2(predictions, test.data$sales)

newdata <- data.frame(youtube=c(0,1000))
predictions <- model %>% predict(newdata)
predictions

#4. Program 4

library(tidyverse)
library(ggplot2)
library(caret)
library(datarium)

data("marketing", package = "datarium")
head(marketing)
dim(marketing)
summary(marketing)
str(marketing)

sum(!complete.cases(marketing))

ggplot(marketing, aes(sales)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Sales", y = "Count") +
  theme_classic()

ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()

cor(marketing$sales, marketing$youtube)
cor(marketing$sales, marketing$facebook)
cor(marketing$sales, marketing$newspaper)

set.seed(123)
training.samples <- marketing$sales %>% createDataPartition(p = 0.8, list = FALSE)

train.data <- marketing[training.samples,]
test.data <- marketing[-training.samples,]

model <- lm(sales ~ youtube + facebook + newspaper, data = train.data)
model <- lm(sales ~ ., data = train.data)

summary(model)$coef

predictions <- model %>% predict(test.data)

RMSE(predictions, test.data$sales)
R2(predictions, test.data$sales)

newdata <- data.frame(youtube=2000, facebook=1000, newspaper=1000)
predictions <- model %>% predict(newdata)
predictions

#5. Program 5

loan <- read.csv("credit_data.csv")
str(loan)

loan.subset <- loan[c('Creditability','Age..years.','Sex...Marital.Status','Occupation','Account.Balance','Credit.Amount','Length.of.current.employment','Purpose')]

str(loan.subset)
head(loan.subset)

normalize <- function(x) {
 return ((x - min(x)) / (max(x) - min(x))) }

loan.subset.n <- as.data.frame(lapply(loan.subset[,2:8], normalize))
head(loan.subset.n)

set.seed(123)
dat.d <- sample(1:nrow(loan.subset.n),size=nrow(loan.subset.n)*0.7,replace=FALSE)

train.loan <- loan.subset[dat.d,]
test.loan <- loan.subset[-dat.d,]

train.loan_labels <- loan.subset[dat.d,1]
test.loan_labels <-loan.subset[-dat.d,1]

library(class)

NROW(train.loan_labels)

knn.26 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=26)
knn.27 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=27)

ACC.26 <- 100 * sum(test.loan_labels == knn.26)/NROW(test.loan_labels)
ACC.27 <- 100 * sum(test.loan_labels == knn.27)/NROW(test.loan_labels)

ACC.26
ACC.27

table(knn.26 ,test.loan_labels)
knn.26

table(knn.27 ,test.loan_labels)
knn.27

library(caret)
confusionMatrix(table(knn.26 ,test.loan_labels))

i=1
k.optm=1
for (i in 1:28){
 knn.mod <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=i)
 k.optm[i] <- 100 * sum(test.loan_labels == knn.mod)/NROW(test.loan_labels)
 k=i
 cat(k,'=',k.optm[i],'') }

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#6. Program 6

library(recommenderlab)
library(ggplot2)
library(data.table)

df_data <- fread('data.csv')
df_data[, InvoiceDate := as.Date(InvoiceDate)]

df_data[Quantity <= 0, Quantity := NA]
df_data[UnitPrice <= 0, UnitPrice := NA]
df_data <- na.omit(df_data)

setkeyv(df_data, c('StockCode', 'Description'))
itemCode <- unique(df_data[, .(StockCode, Description)])
setkeyv(df_data, NULL)

df_train_ori <- dcast(df_data, CustomerID ~ StockCode, 
                      value.var = 'Quantity', fun.aggregate = sum, fill = 0)

CustomerID <- df_train_ori[, 1]
if (ncol(df_train_ori) > 3508) {
  df_train_ori <- df_train_ori[, -c(1, 3504:3508)]
} else {
  df_train_ori <- df_train_ori[, -1]
}

for (i in names(df_train_ori)) {
  df_train_ori[is.na(get(i)), (i) := 0]
}

df_train <- as.matrix(df_train_ori)
df_train <- df_train[rowSums(df_train) > 5, colSums(df_train) > 5]
df_train <- binarize(as(df_train, "realRatingMatrix"), minRating = 1)

which_train <- sample(x = c(TRUE, FALSE), size = nrow(df_train), replace = TRUE, prob = c(0.8, 0.2))
y <- df_train[!which_train]
x <- df_train[which_train]

recommender_models <- recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")
recommender_models$IBCF_binaryRatingMatrix$parameters

method <- 'IBCF'
parameter <- list(method = 'Jaccard')
n_recommended <- 5
n_training <- 1000

recc_model <- Recommender(data = x, method = method, parameter = parameter)
model_details <- getModel(recc_model)

recc_predicted <- predict(object = recc_model, newdata = y, n = n_recommended, type = "topNList")
as(recc_predicted, "list")[1:5]

user_1 <- CustomerID[as.integer(names(recc_predicted@items[1]))]
vvv <- recc_predicted@items[[1]]
vvv <- rownames(model_details$sim)[vvv]
itemCode[vvv]
