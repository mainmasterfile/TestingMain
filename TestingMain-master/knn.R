i=1
y = "AtBat"
k=5
#################################Eucledian#########################
euclidean <- function(train, test, i, y)
{
  a <- NULL
  a <- (train[,names(train) != y] - c(test[i,names(test) != y]))^2
  a <- cbind.data.frame(a,sm = rowSums(a))
  return(a)
}
###################################################################
#############################Regression############################
knn_reg <- function(train, test, y, k)
{
  mat <- NULL
  for(i in 1:nrow(test))
  {
    a <- cbind.data.frame(dep = train[,y],euclidean(train, test, i, y))
    a <- a[order(a$sm),]
    a <- a[c(1:k),]
    pred <- mean(a$dep)
    mat <- rbind(mat, pred)
  }
  return(mat)
}
###########################################################################
a
b <- knn_reg(train, test, "AtBat", 5)
pred_001
#######################################################################
###########################classification################################

knn_class <- function(k, train, test, y)
{
  mat <- rep(NA, nrow(test))
  for(i in 1:nrow(test))
  {
    a <- cbind.data.frame(dep = train[,y],euclidean(train, test, i, y))
    a <- a[order(a$sm),]
    a <- a[c(1:k),]
    pred <- as.character(a$dep[max(table(a$dep))])
    if(length(pred)>1)
    {
      pred = sample(pred, 1)
    }
    
    #mat <- (rbind(mat, pred))
    mat[i] <- pred # to reduce memory allocation time
    print(c(i, mat[i]))
  }
  #error <- calc_class_err(actual = test$label, predicted = b)
  return(as.vector(mat))
}
######################################################################
######################################################################
y="Species"
library(class)
pred_001 = knn(train = train[,names(train) != y], test = test[,names(test) != y], cl = train[,"Species"], k = 10)
k = c(1:10)
b <- knn_class(5, train, test, "Species")
sapply(k, knn_class, train, test, "Species")
###################################################################################
###################################################################################
train = read.csv("/Users/apurvgarg/Downloads/fashionmnist/fashion-mnist_train.csv")
test = read.csv("/Users/apurvgarg/Downloads/fashionmnist/fashion-mnist_test.csv")
k=seq(5,40, 5)
b <- knn_class(5, train, test[1:10000,], "label")

test_pred <- sapply(5, knn_class, train = train, test = test, y = "label")
###################################################################################
###################################################################################
get_PVE = function(pca_out) {
  pca_out$sdev ^ 2 / sum(pca_out$sdev ^ 2)
}

train_pca = prcomp(train[,!(names(train) %in% "label")], scale = TRUE)
a <- get_PVE(train_pca)[1:50]
train_pca_data <- train_pca$x[1:50]

###################################################################################
train_pca_data <- data.frame(label = train$label, train_pca$x)

#we are interested in first 50 PCAs
train_pca_data <- train_pca_data[,1:51]
###################################################################################
###################################################################################
test_data_pca <- predict(train_pca, newdata = test)
test_data_pca <- cbind.data.frame(label = test$label, test_data_pca)

#select the first 30 components
test_data_pca <- test_data_pca[,1:51]
ptm <- proc.time()
test_pred <- sapply(k, knn_class, train = train_pca_data, test = test_data_pca, y = "label")
#b <- knn_class(5, train_pca_data, test_data_pca, "label")
proc.time() - ptm

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

############
k <- seq(7,50,7)
#######################################################


