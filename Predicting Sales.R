# Reading file
mydata=read.csv("Advertising.csv")
mydata

head(mydata)

colnames(mydata)[1] <- "case_number"
head(mydata)

sales <- mydata$sales
newspaper <- mydata$newspaper
radio <- mydata$radio
TV <- mydata$TV
case_number <- mydata$case_number
head(mydata)

# Sales is the dependent variable. Where as TV Radio and Newspaper are independent.

dim(mydata)

# Correlation
corr = cor(mydata[ -c(1,1)])
corr
# The strongest correlation is between sales and TV at 78%.

reg <- lm(Sales ~ Radio, data = mydata)
reg

summary(reg)

library("ggplot2")
p <- qplot( x = mydata$Radio, y = mydata$Sales, data = mydata) + geom_point()
p + geom_smooth(method = "lm", formula = y ~ x)

## The pattern is that the more radio advertising there is, the higher the sales amount. This line has a positive slope.


# Multiple Linear Model
mlrm_sales <- lm(mydata$Sales ~ mydata$Radio + mydata$TV )
mlrm_sales

summary(mlrm_sales)

# The r squared and the adjusted r squared are .897 and .896. This is a great fit for our data because it is over .75. This is a better fit than the last model.

# Multiple Linear Regression Model using all relevant independent
mlrm_sales2 <- lm( mydata$Sales ~ mydata$Radio + mydata$TV + mydata$Newspaper)
mlrm_sales2

summary(mlrm_sales2)

# The r squared is .897 and the adjusted r squared is .895. Both are great fits for the model with only slight variation.

print("model 1")
radio = 69
predicted_sales_radio =  9.3116 + (0.20250) * (radio)
predicted_sales_radio

print("Model 2")
radio = 69
TV = 255
predicted_sales_TVradio = 2.92110 + 0.18799 * (radio) + 0.04575 * (TV)
predicted_sales_TVradio

print("model 3")
radio = 69
TV = 255
newspaper = 75
predicted_sales_all = 2.938889 + 0.188530 * (radio) + 0.045765 * (TV) - 0.001037 * (newspaper)
predicted_sales_all
