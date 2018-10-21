---
layout: archive
title: ""
permalink: /metrics1/
author_profile: true
redirect_from:
  - /Resume
---

{% include base_path %}

### Linear Regression with one regressor

This example uses a panel data set on test performance, school characteristics, and student demographic backgrounds for California school districts, 1998-1999. 

The question we have in mind is whether or not student-teacher ratio (STR) affects student test scores (testscr). We can represent this relationship using the population regression line as:

$y_i = \beta_0+\beta_1 x_i + \epsilon_i$

where $y_i$ represents testscr of school $\textit{i}$, $x_i$ represents STR of school $\textit{i}$ and $\epsilon_i$ a random disturbance. In this case, $\beta_1$ is our parameter of interest, which represents the expected change in test score for a unit change in STR (in this case, a unit means one student more per teacher).

First, we import the data set from the web site, and given that it is formatted for Stata (.dta), we need to first install the package "foreign" (it allow us to use data formatted for another econometric software) using "install.packages()" command (from now on we will omit this and we will just call the package assuming we have installed it before):

```{r}
install.packages("foreign")
```

Now we call the package using the command "library()" and import the data set as a data.frame object using the command "read.dta()":

```{r warning=FALSE, message=FALSE}
library(foreign)
a = "http://fmwww.bc.edu/ec-p/data/stockwatson/caschool.dta"
data_set = read.dta(a)
# class() command tell us what kind of object we have
class(data_set) 
```

We should be able to see an object called "data_set" in the environment (upper-right side of R-studio). We can check some descriptive statistics of its content using the commands "summary()" or look at the first 6 observations of each variable using the command "head()":

```{r warning=FALSE, message=FALSE}
summary(data_set)
head(data_set)
```

We can use a plot to check graphically whether it appears to be a relationship between the two variables of interest (Note that the command "attach()" tells R that we are going to use a particular data.frame, so we can use directly the names of the variables inside the data.frame):

```{r warning=FALSE, message=FALSE}
# Scatter plot 
attach(data_set)
plot(str,testscr)
```

Now we can run our first linear regression with the command "lm()" creating an object called "reg1" containing the outcome of the regression. We can then summarize this outcome with "summary()":

```{r warning=FALSE, message=FALSE}
reg1 = lm(testscr~str,data=data_set)
summary(reg1)
```

The summary shows us the value of the estimated coefficients, standard errors, t values, p values, residual standard errors (SER), R squared, Adjusted R squared, F-statistic and p-value of this F-statistic (we will see later their meanings). Note that we have to specify the data.frame with the data for the regression.

The previous regression uses the standard OLS formula to compute the standard errors, which assumes homoskedasticity (the sequence of disturbances have the same finite variance). However, we will be using standard errors that are robust to heteroskedasticity (in other words, we are not going to be assuming homoskedasticity). To do this we call the packages "lmtest" and "sandwich", to use the commands "coeftest()" and "vcovHC()":

```{r warning=FALSE, message=FALSE}
library(lmtest)
library(sandwich)
# Now we use robust standard errors 
coeftest(reg1, vcov = vcovHC(reg1, "HC1"))
```

These packages also allows us to compute confidence intervals ($\beta_1=\{\hat{\beta_1}\pm 1.96SE(\hat{\beta_1})\}$):

```{r}
# Confidence interval
coefci(reg1, vcov = vcovHC(reg1, "HC1"))
```

We can see the regression line in a plot (note that we first have to write the independent variable "x" and then our independent variable "y" in the command "plot()"):

```{r warning=FALSE, message=FALSE}
reg1 = lm(testscr~str)
plot(str,testscr)
abline(reg1)
```

From the regression output we obtain coefficients, predicted values and residuals (let see the first 6 values of them):

```{r warning=FALSE, message=FALSE}
b.hat = coef(reg1)
b.hat
testscr.hat = fitted(reg1)
head(testscr.hat)
u.hat = resid(reg1)
head(u.hat)
```

Let's confirm some properties of OLS:

```{r warning=FALSE, message=FALSE}
# Confirm property (1) of OLS, mean of u equal zero:
mean(u.hat)

# Confirm property (2) of OLS, residual uncorrelated to x:
cor(str, u.hat)

# Confirm property (3) of OLS, expected value conditional on mean of x equal to mean of y:
mean(testscr)
b.hat[1] + b.hat[2] * mean(str)
```

We can compute $R^2$ in three different ways:

```{r warning=FALSE, message=FALSE}
var(testscr.hat) / var(testscr)
1 - var(u.hat) / var(testscr)
cor(testscr, testscr.hat)^2
```

Finally, we can do hypothesis testing about the coefficient $\beta_1$. Let's replicate the test reported after "coeftest()" command:

```{r}
library(lmtest)
library(sandwich)
# Store coefficients and standard errors into summary1 
summary1 = coeftest(reg1, vcov = vcovHC(reg1, "HC1"))
summary1 
# Let's check the t and p-value of the null beta_1=0
z = summary1[2,1]/summary1[2,2]
z
# The p value according to the normal distribution is:
2*pnorm(-abs(z))
# The p value according to the Student t distribution s:
2*pt(-abs(z),df=reg1$df.residual)
```

We get the same when using the Student t distribution (which assumes disturbances are normally distributed).

### Regression with a simulate a data set

We generate a very small sample of 30 observations of a random variable X and a disturbance Z that are iid distributed normal, and create an outcome Y using the equation $Y_i = 1 + 2X_i + Z_i$ (think about this equation as the population linear regression from which the sample comes):

```{r}
# We fix a value for the seed in order to replicate each time the same random numbers
set.seed(1)
Z = rnorm(30,mean=0,sd=4)
X = rnorm(30,mean=0,sd=2)
Y = 1 + 2*X + Z
plot(Y,X)
```

Now suppose we only observe Y and X, and we would like to estimate the slope of  $Y_i = \beta_0 + \beta_1X_i + Z_i$:

```{r}
reg2 = lm(Y~X)
summary(reg2)
library(lmtest)
library(sandwich)
coeftest(reg2, vcov = vcovHC(reg2, "HC1"))
```

We got a beta close to 2. Let see what we get with a bigger sample:

```{r}
set.seed(1)
Z = rnorm(5000,mean=0,sd=4)
X = rnorm(5000,mean=0,sd=2)
Y = 1 + 2*X + Z
plot(Y,X)
reg3 = lm(Y~X)
summary(reg3)
library(lmtest)
library(sandwich)
coeftest(reg3, vcov = vcovHC(reg3, "HC1"))
```

With a sample of 5000 instead of 30 we get something much closer. Note also that we are not able to reject the null of the intercept equal to 0 in the small sample (we know that the true value is 1).

Let's run a t test with the null hypothesis $H_0:\beta_1=2$ that we know it is true:

```{r}
# First we store the coefficients and standard errors into summary3
summary3 = coeftest(reg3, vcov = vcovHC(reg3, "HC1"))
# Create the t-statistic 
z3 = (summary3[2,1]-2)/summary3[2,2]
z3
# The p value according to the normal distribution is:
2*pnorm(-abs(z3))
```

We fail to reject the null hypothesis.

Finally, notice what happen to the plot if we decrease the variance of the disturbance keeping the variance of X as before:

```{r}
set.seed(1)
Z = rnorm(5000,mean=0,sd=.1)
X = rnorm(5000,mean=0,sd=2)
Y = 1 + 2*X + Z
plot(Y,X)
```
