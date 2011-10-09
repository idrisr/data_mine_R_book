
#****************************************************************************
#******************** 2.1 Problem Description and Objectives ****************
#****************************************************************************


# Problem Description and Objectives
    
# high concentrations of algae in rives is a big eco problem. being able to
# predict algae blooms is valuable.

# Several water samples were collected.
# monitoring chemical analysis is easy, while biological analysis is slow and
# expensive.

#****************************************************************************
#************************ 2.2 Data Description ******************************
#****************************************************************************
# two main datasets
# first consists of 200 water samples
# each observation is the aggregate of several water samples from the same river
# over a period of 3 months, during same season of year

# second contains info on 140 extra observations. uses the same basic structure,
# but it does not include information about the seven harmful algae frequencies.
# This is like test data. 
# This is a predictive data mining task. Our main goal is to get a model that
# allows us to predict the value of a certain target variable given the values of
# a set of predictor variables. this model may also provide indications on which
# predictor variables have a larger impact on the target variable. 
# That is, the model may provide a comprehensive description of the factors that
# influence the target variable.

#****************************************************************************
#************************ 2.3 Loading the Data into R ***********************
#****************************************************************************
require(DMwR)
data(algae)

#****************************************************************************
#************************ 2.4 Data Visualization and Summarization **********
#****************************************************************************

summary(algae)
hist(algae$mxPH, prob = T)

# better graphic
require(car) # an R Companion to Applied Regression
par(mfrow = c(1,2)) # dividing graphics output to one line by two columns area
hist(algae$mxPH, prob = T, xlab = '', main = 'Histogram of Maximum pH Value',
     ylim = 0:1) #simple histogram, automatically put in row 1, col 1
lines(density(algae$mxPH, na.rm = T)) # adds kernel density estimate line, 
            # removes NA, otherise errors out. 
rug(jitter(algae$mxPH)) # adds rug to histogram, with jitter
qqPlot(algae$mxPH, main = 'Normal QQ plot of maximum pH') # plots the variable
# values against the theoretical quantiles of a normal distribution. Function
# also plots an envelope with the 95% confidence interval of the normal
# distribution (dashed lines).
# As we can see, there are several low values that break the assumptions of a
# normal distribution with 95% confidence.
par(mfrow = c(1, 1)) # why do we need this call?

boxplot(algae$oPO4, ylab = 'Orthophosphate (oPO4)')
rug(jitter(algae$oPO4), side = 2) # can you do rug in ggplot2?
abline(h = mean(algae$oPO4, na.rm = T), lty = 2) # add dashed mean line. Good
                                                 # idea for boxplots

# When we encounter outliers, we are often interested in examining the
# observations that have these 'strange' values.

# Here are two ways of doing this

plot(algae$NH4, xlab = '')
abline(h = mean(algae$NH4, na.rm = T), lty = 1)
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2)
abline(h = median(algae$NH4, na.rm = T), lty = 3)
identify(algae$NH4) # interactively lets you pick which points to look at 

plot(algae$NH4, xlab = '')
clicked.lines <- identify(algae$NH4)
algae[clicked.lines, ]

algae[algae$NH4 > 19000, ]
# output of this is strange because R doesn't know how to compare the NAs.
# Do this to exclude NAs
algae[!is.na(algae$NH4) & algae$NH4 > 19000,]

# Suppose we would like to study the distribution of the values, of say, algae
# a1 We could use any of the possibilities discussed before. However, if we 
# wanted to study how this distribution depends on other variables, new tools are 
# required.

# enter lattice
require(lattice)
bwplot(size ~ a1, data = algae, ylab = 'River Size', xlab = 'Algal A1')
# my very own ggplot2 interpretation
ggplot(algae, aes(size, a1)) + geom_boxplot() + coord_flip()

# an interesting variant of this type of plot that gives us more information on
# the distribution of the variable plotted, are box-percentile plots, which are
# available through the package Hmisc. 
require(Hmisc)
bwplot(size ~ a1, data = algae, panel = panel.bpplot, 
       probs = seq(.01, .49, by = .01), datadensity = TRUE, ylab = 'River Size', 
       xlab = 'Algal A1')

# Dots are the mean value of the frequency of that algal for the different river
# sizes. Vertical lines represent the quartiles and median. small dashes are
# actual values. 
# what do the blue lines represent?

# Continue on Page 50. 9/1/2011
min02 <- equal.count(na.omit(algae$mnO2), number = 4, overlap = 1/5)
stripplot(season ~ a3|mnO2, data = algae[!is.na(algae$mnO2),]) # 

#****************************************************************************
#************************ 2.5 Unknown Values ********************************
#****************************************************************************

# whenever dealing with a dataset with missing values, we can follow several
# strategies.
# 
# 1. Remove the cases with unknowns
# 2. Fill in the unknown values by exploring the correlations between variables
# 3. Fill in the unknown values by exploring the similarities between cases
# 4. Use tools that are able to handle these values.

#*************** 2.5.1 Removing the Observations with Unknown Values ********
# reasonable when proportion of unknowns is small with respect to size of 
# available dataset

# complete.cases provides a vector of Boolean values with a TRUE for each row
# where all columns are present
algae[!complete.cases(algae), ]

nrow(algae[!complete.cases(algae), ])

# to remove these missing values do
algae <- na.omit(algae)

# selectively get rid of values
algae <- algae[-c(62, 199), ]

# in problems where the visual inspection of all the cases with unknowns is
# unfeasible due to their number, we need to be able to find the rows with a
# large number of NAs. The following code gives you the number of unknown values
# in each row of the algae dataset.
apply(algae, 1, function(x) sum(is.na(x)))
data(algae)

# manyNAs is a custom function in DMwR
algae <- algae[-manyNAs(algae, 0.2), ]

#******** 2.5.2 Filling in the Unknowns with the Most Frequent Values *******
# fastest and simplest is use some statistic of centrality
# mode, mean, median, etc
# for normal distrib, use mean, for skewed use median
qqPlot(algae$mxPH, main = 'Normal QQ plot of maximum pH')

# this shit is way too cumbersome
algae[48, 'mxPH'] <- mean(algae$mxPH, na.rm = T)

hist(algae$Chla)
algae[is.na(algae$Chla), 'Chla'] <- median(algae$Chla, na.rm = T)

par(mfrow = c(1, 2)) # dividing graphics output to one line by two columns area
hist(algae$Chla)
# par(mfrow = c(1, 1)) # dividing graphics output to one line by two columns area
qqPlot(algae$Chla)

# Can also do it like this
data(algae)
algae <- algae[-manyNAs(algae), ]

# manyNAs is a function from DMwR
algae <- algae[-manyNAs(algae), ]
algae <- centralImputation(algae)

#******** 2.5.3 Filling in the Unknowns by Exploring Correlations ***********
# an alternative for getting less biased estimatorrs of the unknown values is to
# explore the relationships between variables. You can use the correlations
# between the variable values, you could discover that a varibales is highly
# correlated with something, leading to better fills for missing values. 
cor(algae[, 4:18], use = 'complete.obs')
# a prettier output, use complete.obs to avoid any rows with missing values
symnum(cor(algae[, 4:18], use = 'complete.obs'))

# PO4 and oPO4 are strongly correlated (above 0.9).
# we can use this, but first we need the linear relationship between these
# variables
data(algae)
algae <- algae[-manyNAs(algae), ]
po.lm <- lm(PO4 ~ oPO4, data = algae)  # 
fillPO4 <- function(oP) {
    if (is.na(oP))
        return(NA)
    else 
        return(42.897 + 1.293 * oP)
}

# see where the nas are
# in sapply, get the same na rows, then grab the correlated column, send to 
# fillPO4
algae[is.na(algae$PO4), 'PO4'] <- sapply(algae[is.na(algae$PO4), 'oPO4'],
                                         fillPO4)
algae$season <- factor(algae$season, levels = c('spring', 'summer', 'autumn',
                                                'winter'))
# more lattice fun 
# still some blanks, what to do?
# lets look at histograms for the factors
histogram(~mxPH | season, data = algae)
# the histograms all look fairly similar, no big help is filling in the missing
# values

# No values for size * speed, low size and small speed, the very thing we need
histogram(~mxPH | size * speed, data = algae)
ggplot(algae, aes(mxPH)) + geom_histogram(fill = 'steelblue', 
                 binwidth = 0.5) + facet_wrap(season~speed)

stripplot(size ~ mxPH | speed, data = algae, jitter = TRUE)

# This is a tedious process because there are too many combinations to analyze.
# nonetheless, this is a method that can be applied in small datasets with few
# nominal values

# Continue on Page 60. 9/2/2011

#* 2.5.4 Filling in the Unknown Values by Exploring Similarities Between Cases *

# Instead of exploring the correlation between columns of a dataset, we can try 
# to use the similarities between the rows to fill in the unknown values.
# We can use this approach to fill in all unknowns with the expection of the 
# two samples with too many NAs. 
data(algae)
algae <- algae[-manyNAs(algae),]

# The approach in this section assumes that if two water samples are similar,
# and one of them has an unknown value in some variable, there is a high
# probability that this value is similar to the value of the other sample. In
# order to use this intuitively appealing method, we need to define the notion
# of similarity. This notion is usually defined using a metric over the
# multivariate space of the variables used to describe the observations. Many
# metrics exist in the literature, but a common choice is the Euclidean
# distance. This distance can be informally defined as the square root of the
# sum of the squared differences between the values of any two cases, that is,
# d(x, y)  = sqrt(sum(x_i - y_i)^2)

# We'll use this metric to find the ten most similar cases of any water sample
# with some unknown value in a variable, and then use their values to fill in
# the unknown. 
# We'll consider two way of using their values. 

# The first just calculates the median of the values of the ten nearest
# neighbors to fill in the gaps. In case of unknown nominal variables (which do
# not occur in our algae dataset), we would use the most frequent value (the
# mode) among the neighbors.

# The second method uses a weighted average of the values of the neighbors. The
# weights decrease as the distance to the case of the neighbors increases. We
# use a Gaussian kernel function to obtain the weights from the distances. If
# one of the neighbors is at a distance d from the case to fill in, its value
# will enter the weighted average with a weight given by: 
# w(d) = e^-d

# This idea is implemented in function knnImputation() available in the book
# package. This function uses a variant of the Euclidean distance to find the k
# nearest neighbors of any case. This variant allows the application of the
# function to datasets with both nominal and continuous variables. 

# d(x, y)  = sqrt( gamma_i * sum(x_i - y_i)^2)

# where gamma_i determines the distance between two values on variable i and is
# given by 

# gamma(v1, v2) = 1           if i is nominal and v1 != v2
#                 0           if i is nominal and v1 = v2
#                 (v1 - v2)^2 if i is numeric

# These distances are calculated after normalizing the numeric values, that is:
# y_i = (x_i - x_mean) / sd_x

# Overall, an interesting idea I don't totally *yet* understand.
algae <- knnImputation(algae, k = 10)


#****************************************************************************
#************************ 2.6 Obtaining Prediction Models *******************
#****************************************************************************

# The main goal of this case study is to obtain predictions for the frequency
# values of the seven algae in a set of 140 water samples. Given that these
# frequencies are numbers, we are facing a regression task. 

# The task is to obtain a model relating a numerical variable to a set of other
# explanatory variables.  This model can be used either to predict the value of
# the target variable for future observations of the explanatory variables, or
# to provide a better understanding of the interactions among the variables in
# our problem.

# We'll explore two different preditive models that could be applied to the
# algae domain: 
# 1. Multiple linear regression
# 2. Regression Trees
# This choice was mainly guided by illustrative purposes in the context of this
# book, and not as a consequence of some formal model selection step.

# Still, these models are 2 good alternatives for regression problems as they 
# are quite different in terms of assumptions regarding their shape of the
# regression function being approximated and they are easy to interpret and fast
# to run on any computer. 

# These models handle missing data in different ways. the implementation of
# linear regression in R can't handle unknown values, the implementation of
# regression trees handles those values naturally. Therefore, we'll follow a
# different path concerning the prep of the data before model construction.

# For linear regression we'll use one of the methods in 2.5
# For regression trees we'll use the original 200 values

#******************** 2.6.1 Multiple Linear Regression *************************

# This is one of the most used statistical data analysis techniques. These
# models obtain an additive function relating a target variable to a set of
# predictor variables. This additive function is a sum of terms of the form 
# Bi * Xi, where Xi is a predictor variable and Bi is a number.

# fill in missing values
require(DMwR)
data(algae)
algae <- algae[-manyNAs(algae), ] 
clean.algae <- knnImputation(algae, k = 10)

# lets start by learning how to obtain a linear regression model for predicting
# the frequency of one of the algae
lm.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12])

# the function lm obtains a linear regression model. 
# The first argument of this function indicates the functional form of the
# model. in this example, we want a model that predicts the variable a1 using
# all other variables present in the data, which is the meaning of the dot
# character. For instance, if we wanted to predict a1 as a function of the
# variables mxPH and NH4, we should have indicated the model as 
# a1 ~ mxPH + NH4
# There are other variants of this model language, called formulas in R, that
# we'll get into
# The result of the function is an object that contains the linear model info.
# We can see more details using summary.
summary(lm.a1)

# Check out what R did with the nominal (factor) variables. 
# For each factor variable with k levels, R will create k-1 auxiliary variables.
# These variables have the values 0 or 1. A value of 1 means that the associated 
# value of the factor is 'present', and that will also mean that the other
# auxiliary variables will have the value 0. 
# if all k-1 variables are 0, then it means that the factor variable has the
# remaining kth value. Looking at the summary presented above, we can see that R
# has created three auxiliary variables for the factor season 
# (seasonspring, seasonsummer, and seasonwinter). 

# This means that if we have a water sample with the value 'autumn' in the
# variable season, all three variables will be set to zero.

# The application of the function summary() to a linear model gives some
# diagnostic information concerning the residuals (ie the errors) of the fit of
# the linear model to the used data. The residuals should have a mean zero and
# should have a normal distribution (and be as small as possible).

# A Pr(>|t) = .0001 has the meaning that we are 99.99% confident that the
# coefficient is not null. 

# In summary, only for the coefficients that have some symbol in front of them
# can we reject the hypothesis that they may be null with at least 90%
# confidence.

# Another piece of relevant diagnostics information outputted by R are they R^2
# coefficients (multiple and adjusted). These indicate the degree of fit of the
# model to the data, that is, the proportion of variance in the data that is
# explained by the model. Values near 1 are better (nearly 100% of variance
# explained by model), and the smaller numbers indicate a larger lack of fit. 

# The adjusted coefficient is more demanding as it takes into account the number
# of parameters of the regression model.

# Finally, we can test the null hypothesis that there is no dependence of the
# target variable on any of the explanatory variables, that is, 
# H0: B1 = B2 = ... Bm = 0.

# The F-statistic can be used for this purpose by comparing it to a critical
# value. R provides the confidence level at which we are sure to reject the null
# hypothesis. Thus, a p-level of .0001 means that we are 99.99% confident that
# the null hypothesis is not true. Usually, if the model fails this test ( a
# p-value that is too high, for example, higher than 0.1), it makes no sense to
# look at the t-tests on the individual coefficients.

plot(lm.a1) # shows a sequence of plots telling us about the model

# The proportion of variance explained by this model is not very impressive
# (32.0%). Still, we can reject the hypothesis that the target variable does not
# depend on the predictors ( the p value of the F test is very small). Looking
# at the significance of some of the coefficients, we may question the inclusion
# of some of them in the model. There are several methods for simplifying
# regression models. In this section we explore a method usually known as
# backward elimination.

# We will start our study of simplifying the linear model using the anova()
# function. When applied to a single linear model, this function will give us a
# sequential analysis of variance of the model fit. That is, the reductions in
# the residual sum of squares (the total error of the model) as each term of the
# formula is added in turn. Do it like this:

anova(lm.a1)

# Analysis of Variance Table
# 
# Response: a1
#            Df Sum Sq Mean Sq F value    Pr(>F)    
# season      3     85    28.2  0.0905 0.9651944    
# size        2  11401  5700.7 18.3088  5.69e-08 ***
# speed       2   3934  1967.2  6.3179 0.0022244 ** 
# mxPH        1   1329  1328.8  4.2677 0.0402613 *  
# mnO2        1   2287  2286.8  7.3444 0.0073705 ** 
# Cl          1   4304  4304.3 13.8239 0.0002671 ***
# NO3         1   3418  3418.5 10.9789 0.0011118 ** 
# NH4         1    404   403.6  1.2963 0.2563847    
# oPO4        1   4788  4788.0 15.3774 0.0001246 ***
# PO4         1   1406  1405.6  4.5142 0.0349635 *  
# Chla        1    377   377.0  1.2107 0.2726544    
# Residuals 182  56668   311.4                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# These results indicate that the variable season is the variable that least
# contributes to the reduction of the fitting error of the model. 
# We can remove it from the model like this:

lm2.a1 <- update(lm.a1, . ~ . - season)

# The update function can be used to perform small changes to an existing linear
# model. Above we use it to remove the variable season from the lm.a1 model.

summary(lm2.a1)

# The fit has improved slightly but still not too good. We can carry out a more
# formal comparion between the two models by using again anova, this time both
# models at the same time.

anova(lm.a1, lm2.a1)
# Analysis of Variance Table
# 
# Model 1: a1 ~ season + size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + 
#     PO4 + Chla
# Model 2: a1 ~ size + speed + mxPH + mnO2 + Cl + NO3 + NH4 + oPO4 + PO4 + 
#     Chla
#   Res.Df   RSS Df Sum of Sq      F Pr(>F)
# 1    182 56668                           
# 2    185 57116 -3   -447.62 0.4792 0.6971

# This function performs an analysis of variance of the two models using an 
# F-test to assess the significance of the differences. In this case, the sum of
# the squared errors has decreased (-448), the comparision shows that the
# differnces are not significant ( a value of .6971 tells us that with only
# around 30% confidence we can say they are different).

# But, this new model is simpler. In order to check if we can remove more
# coefficients, use anova on the new model. This process would continue until we
# have no candidate coefficients for removal. However, to simplify our backward
# elimination process, R has a function that does this. 

# The following creates a linear model that results from applying the backward
# elimination method to the initial method we have obtained (lm.a1)
final.lm <- step(lm.a1)

# The function step uses the Akaike Information Criterion to perform model
# search. The search uses backward elimination by default, but with the
# parameter direction you may use older algos. 

summary(final.lm)

# The proportion of variance explained by this model is still not very 
# interesting. This kind of proportion is usually considered a sign that the
# linearity assumptions of this model are inadequate for the domain.

#******************** 2.6.2 Regression Trees *********************************

# Let's know look at a different kind of regression available in R. 
# we'll use a regression tree to predict the values of the frequencies of algal
# a1.
# These models handle missing values, so we need to only remove samples 62 and 
# 199.
require(rpart)
data(algae)
algae <- algae[-manyNAs(algae), ]
rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])

plot(rt.a1)
text(rt.a1)
prettyTree(rt.a1)
summary(rt.a1)

# A regression tree is a hierarchy of logical tests on some of the explanatory
# variables. Tree-based models automatically select the more relevant variables,
# therefore not all variables need to appear in the tree. 

# Trees are usually obtained in two steps. Initially, a large tree is grown, and
# then this tree is pruned by deleting bottom nodes through a process of
# statistical elimination. This process has a the goal of avoiding overfitting.
# This has to do with the fact that an overly large tree will fit the training
# data almost perfectly, but will be capturing spurious relationships of the
# given dataset (overfitting it), and thus will perform badly with unseen data.

# Overfitting is a common problem. 

# The function rpart only grows until a certain criteria is met. the tree stops
# growing when:

# 1. the decrease in the deviance goes below a certain threshold (cp)
# 2. the number of samples in the node is less than another thershold (minsplit)
# 3. the tree depth exceeds another value (maxdepth)

# These thresholds are controlled by the parameters cp, minsplit, and maxdepth. 
# Their default values are .01, 20, and 30, respectively. If we want to avoid 
# the overfitting problem we should always check the validity of these default
# criteria. This can be done through a process of post-pruning the obtained
# tree

# rpart implements a pruning method called cost complexity pruning. This method
# uses the values of the parameters cp that R calcs for each node of the tree.

# The pruning method tries to estimate the value of cp that ensures the best
# compromise between predictive accuracy and tree size. Given a tree obtained
# with rpart() function, R can produce a set of sub-trees of this tree and
# estimate their predictive performance. 
printcp(rt.a1)
plotcp(rt.a1)

# The tree produced by rpart() is the last tree of this list (tree 9). This tree
# has a cp value of 0.01 (the default value of this parameter), includes nine
# tests and has a relative error (compared to the root node) of 0.354.

# R estimates using an internal process of ten-fold cross-validation, that the
# tree will have an average relative error of .70241 +- .11523. 
# Using the information provided by these more reliable estimates of
# performance, which avoid the overfitting problem, we can observe that
# theoretically we'd be better off using tree number 8 which has a lower
# estimated error. 

# An alt selection rules is to choose the best tree according to the 1 - SE
# rule. This consists of looking at the cross validation error estimates
# ('xerror' columns) and their standard deviations ('xstd' column).

# In this case, the 1-SE tree is the smallest tree with error less than 
# 0.67733 + 0.10892 = 0.78625, which in this case is tree number 2 with 1 test
# and an estimated error of 0.73358. If we want this tree instead, 

rt2.a1 <- prune(rt.a1, cp = 0.08)

# The book package provides the function rpartXse() that automates this process
# and takes as argument the se value, defaulting to 1.
(rt.a1 <- rpartXse(a1 ~ ., data = algae[, 1:12]))

