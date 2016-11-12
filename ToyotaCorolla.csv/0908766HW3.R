library(leaps)
library(e1071)
library(broom)
library(ggplot2)
library(car)

corolla = read.csv(file = "Data/ToyotaCorolla.csv")

# Become familiar with the data

head(corolla)

# Recode categorical variable into numeric so I can run calculations
corolla$FuelCode[corolla$FuelType=="CNG"] <- 1
corolla$FuelCode[corolla$FuelType=="Diesel"] <- 2
corolla$FuelCode[corolla$FuelType=="Petrol"] <- 3
corolla$FuelCode[corolla$FuelType=="NA"] <- 0
corolla <- corolla[-4]
head(corolla)

# Drop the textcolumn


# Verify it worked
head(corolla)

# Descriptive stats (in lieu of 9 separate descriptive stats runs, simply graph them)
plot(Price ~ Age, data = corolla)
plot(Price ~ KM, data = corolla)
plot(Price ~ HP, data = corolla)
plot(Price ~ MetColor, data = corolla)
plot(Price ~ Automatic, data = corolla)
plot(Price ~ CC, data = corolla)
plot(Price ~ Doors, data = corolla)
plot(Price ~ Weight, data = corolla)
plot(Price ~ FuelCode, data = corolla)

# Build a linear model using all variables
corolla.m1 <- lm(Price ~ ., data = corolla)
corolla.m1.summary <- summary(corolla.m1)

# Check confidence interval
corolla.m1.confint <- confint(corolla.m1)

# Get it ready to plot
x <- corolla[,2:10] # Independent variables
y <- corolla[,1] # Dependent variables

# Check correlation

corolla.cor <- cor(corolla) #Age accounts for >87% of variation in price, so we will build a separate linear model with just age (m2)

# Build a regtab with the model to check which variables have a significant influence
corolla.out <- summary(regsubsets(x, y, nbest = 1, nvmax = ncol(x),force.in = NULL, force.out = NULL, method = "exhaustive"))
corolla.regtab <- cbind(corolla.out$which, corolla.out$rsq, corolla.out$adjr2, corolla.out$cp)
colnames(corolla.regtab) <- c("(Intercept)","Age","KM","HP", "MetColor", "Automatic", "CC", "Doors", "Weight", "FuelCode",
                              "R-Sq", "R-Sq (adj)", "Cp")

# Create a second model with just age because of the high correlation
corolla.m2 <- lm(Price ~ Age, data = corolla) 
corolla.m2.summary <- summary(corolla.m2) 

# Show results
print(corolla.m2.summary)

# Check second model's confidence interval
corolla.m2.confint <- confint(corolla.m2)
print(corolla.m2.confint)

## Build a third model dropping all the variables that did not have high enough P-values in model 1 to warrant inclusion
# Restructure data to drop insignificant variables
corollaM3 <-corolla[-5]
corollaM3 <-corollaM3[-7]
corollaM3 <-corollaM3[-5]
corollaM3 <-corollaM3[-7]

# Third model with restructured data
corolla.m3 <- lm(Price ~ ., data = corollaM3) 
corolla.m3.summary <- summary(corolla.m3) # Show results
print(corolla.m3.summary)

# Check third model's confidence interval
corolla.m3.confint <- confint(corolla.m3)
print(corolla.m3.confint)

n <- length(corolla$Price) # Get the number of elements
diff <- dim(n) # Set the dimension of the container object
percdiff <- dim(n) # Set the dimension of the container object

for (k in 1:n) {
  train1 <- c(1:n)
  
  # the R expression "train1[train1 != k]" picks from train1 those
  # elements that are different from k and stores those elements in the
  # object train.
  # For k = 1, train consists of elements that are different from 1; that
  # is 2, 3, ..., n.
  train <- train1[train1 != k]
  
  # Create the linar model for the all but one element
  m1 <- lm(Price ~ ., data = corolla[train,])
  
  # Predict the missing value based on the model
  pred <- predict(m1, newdat = corolla[-train,])
  
  # What is the real value
  obs <- corolla$Price[-train]
  
  # Calculate the delta between observed and predicted
  diff[k] <- obs - pred
  
  # Calculate the relative difference between observed and predicted
  percdiff[k] <- abs(diff[k]) / obs
}
corolla.m1.me <- mean(diff) # mean error
corolla.m1.rmse <- sqrt(mean(diff**2)) # root mean square error
corolla.m1.mape <- 100*(mean(percdiff)) # mean absolute percent error

# Repeat process, but for second model which only needs to check age
n <- length(corolla$Price)
diff <- dim(n)
percdiff <- dim(n)
for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 !=k ]
  m2 <- lm(Price ~ Age, data = corolla[train,])
  pred <- predict(m2, newdat = corolla[-train,])
  obs <- corolla$Price[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k]) / obs
}
corolla.m2.me <- mean(diff)
corolla.m2.rmse <- sqrt(mean(diff**2))
corolla.m2.mape <- 100*(mean(percdiff))

# Third repetition for third model
for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 != k]
  m3 <- lm(Price ~ ., data = corollaM3[train,])
  pred <- predict(m3, newdat = corollaM3[-train,])
  obs <- corollaM3$Price[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k]) / obs
}
corolla.m3.me <- mean(diff)
corolla.m3.rmse <- sqrt(mean(diff**2))
corolla.m3.mape <- 100*(mean(percdiff))

corolla.m1.me   # mean error
corolla.m1.rmse # root mean square error
corolla.m1.mape # mean absolute percent error

corolla.m2.me   # mean error
corolla.m2.rmse # root mean square error
corolla.m2.mape # mean absolute percent error

corolla.m3.me   # mean error
corolla.m3.rmse # root mean square error
corolla.m3.mape # mean absolute percent error

# Model 3 has the lowest root mean square error, therefore the best model

## To use model 3 to predict the car with Dr. Kalisch's specifications, we will need to fill in the blanks required by model 3
#
Median.Mileage <- median(corolla[["KM"]])
cc.vs.weight <- corollaM3[-1]
cc.vs.weight <- cc.vs.weight[-1]
cc.vs.weight <- cc.vs.weight[-1]
cc.vs.weight <- cc.vs.weight[-1]
# We found out earlier weight was moderately correlated with displacement, so we will find a rough estimate of weight based on this displacement
cc.weight.cor <- cor(cc.vs.weight)
predicted.weight <- cc.weight.cor [2,1] * 2000
Age <- 12
KM <- Median.Mileage
HP <- 185
CC <- 2000
Weight <- predicted.weight
kalischcar <- data.frame(Age, KM, HP, CC, Weight)
kalischcar.prediction <-predict(m3, kalischcar)

# Check if the assumptions are met...
## Create data frame with residuals
corolla.f <- fortify(corolla.m3)

## Linearity
### Residual vs Fitted Plot
p1 <- ggplot(corolla.f, aes(x = .fitted, y = .resid)) +
  geom_point() +
  stat_smooth(method = "loess") +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")

## Normality
### Normal Q-Q Plot
p2 <- ggplot(corolla.f, aes(x = qqnorm(.stdresid)[[1]], y = .stdresid)) +
  geom_point(na.rm = TRUE) +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")

corolla.skew <- skewness(corolla.f$.resid)
corolla.kurt <- kurtosis(corolla.f$.resid)

## Equal variance
### Scale-Location Plot
p3 <- ggplot(corolla.f, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
  geom_point(na.rm=TRUE) +
  stat_smooth(method = "loess", na.rm = TRUE) +
  xlab("Fitted Value") +
  ylab(expression(sqrt("|Standardized residuals|"))) +
  ggtitle("Scale-Location")

## Independence
# Perform a Durbin-Watson F-test for autocorrelation
corolla.dw <- durbinWatsonTest(m1)

## Outlier influance
### Cook's Distance Histogram
p4 <- ggplot(corolla.f, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  ggtitle("Cook's distance")

p5 <- ggplot(corolla.f, aes(x =.hat, y = .stdresid)) +
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") +
  ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range = c(1,5)) +
  theme(legend.position="bottom")

ggsave("graphs/linearityAssumption.pdf", p1)
ggsave("graphs/normalityAssumption.pdf", p2)
ggsave("graphs/equalVarianceAssumptions.pdf", p3)
ggsave("graphs/outlierInfluance1Assumptions.pdf", p4)
ggsave("graphs/outlierInfluance2Assumptions.pdf", p4)

