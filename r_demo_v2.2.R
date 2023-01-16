####################################################################
###       R demonstration v.2.2                                  ###
###       Introduction to Sociology Methods                      ###
###       Carolina M. Siniscalchi                  Jan/2023      ###
####################################################################

### The dataset used here was sourced from ICSPR (https://www.icpsr.umich.edu/web/ICPSR/studies/30943#).

### This dataset contains data from the New Orleans portion of the American Housing Survey, 2009.
### The survey has several parts that are summarized in eight R data files, with their respective codebooks.

### The first step is to load the data. Here, we will focus on two of the eight datafiles, 
### the first contains details about journey to work (Part 2) and the second has 
### demographic data from the participant households (Part 4).

## first, let's define a working directory, where all saved files and figures will go:
setwd("~/Desktop/R_demo/")
getwd()

## Now, we'll use the load function to load the data:
load("~/Desktop/R_demo/30943-0002-Data.rda")
load("~/Desktop/R_demo/30943-0004-Data.rda")

## We can click on the data objects on the right side menu to take a look on the data files.
## we can also use the codebook dowloaded with the datafile to understand the variables.

## as we'll manipulate the data in different ways, it's easier if we subset the original dataset so it contains 
## only the variables we are insterested in. 

## use the command below to subset the work journey files and create a new dataframe with only some of the variables:
travel <- data.frame(da30943.0002$CONTROL, da30943.0002$TRAN, da30943.0002$TIMEJ, da30943.0002$DISTJ)
## let's use the head command to take a look at the first few lines of our new object:
head(travel)

## our columns, that represent the study variabels, have complicated names.
## we can use the next command to change the variable names, then use the head command again to check the results.
colnames(travel) <- c("CONTROL", "transport_mode", "journey_time", "journey_distance")
head(travel)

## let's do the same for the other data file, to create an object with demographic information.
demographics <- data.frame(da30943.0004$CONTROL, da30943.0004$HHSEX, da30943.0004$ZINC2, 
                           da30943.0004$HHAGE, da30943.0004$HKMOVR)
colnames(demographics) <- c("CONTROL", "gender", "income", "age", "moved")
head(demographics)

## Have you noticed we kept a column called "CONTROL" in both objects? 
## This is a control number from the study used to identify households across all parts of the survey.
## This number comes in handy if we are looking to combine datafiles. 

## We can use the merge command to combine different objects, using identifiers contained in both objects.
combined <- merge(travel, demographics, by = "CONTROL")

## What happened to the number of observations in the combined dataframe in relation to the original ones?
## The merge function automatically excludes observations that are missing from one of the objects.

## Let's start with some basic descriptive statistics

## check maximum and minimum values of time spend on the journey to work:
max(combined$journey_time)

### It is interesting that we have some observations on our dataset with a very high journey-to-work time (996 min!).
### This can be a strategy to code missing or non applicable values on datasets, and it is important that we know that,
### as extreme values will affect any statistics performed on the dataset. 
### One way to understand variable coding, is looking on the study codebook (in this case, the codebook doesn't have 
### any information on these values).

### We can quickly subset our dataset to see if this high journey time is associated with other variables.
## this command asks to return any rows that have a value equal to 996 on the journey_time column. 
head(combined[combined$journey_time == '996',])

### It seems that this value was assigned to people that work from home, where journey time and distance is non-applicable. 
### R has several options to deal with variable value assigment. We'll use a boolean statement to find and replace those '996'
### values with 'NA', in all columns where it shows up.

combined$journey_time <- ifelse(combined$journey_time==996, NA, combined$journey_time)
combined$journey_distance <- ifelse(combined$journey_distance==996, NA, combined$journey_distance)

## let's check our variables now:
summary(combined$journey_time)
summary(combined$journey_distance)

### Now that our variable values are recoded, we can go back to our descriptive statistics. 
### The 'summary' command we used above is handy to look at data, in the case of a quantitative variable like ours, 
### it returns  basic statistics about it. 
### Additionaly, base R has commands to calculate several individual descriptive statistics. Let's try some:

### R is very powerful for data visualization. We can quickly plot the distribution of our journey time variable 
### as an histogram. We are using the package ggplot to create the graph. It has a specific syntax, where parts of the plot
### are defined in different parts of the command:
library(ggplot2)
ggplot(combined, aes(x=journey_time))+
  geom_histogram(binwidth=10, col="grey", fill="#5D1725")+
  labs(x="journey time to work (min)", y = "number of households")+
  theme_bw()

### Note the warning message displayed. ggplot automatically remove the non-numeric values (NA) from the plot.

## we can also easily save our plotted graph as a pdf:
pdf(file="journey_time_distribution.pdf", width = 7, height = 7)
ggplot(combined, aes(x=journey_time))+
  geom_histogram(binwidth=10, col="grey", fill="#5D1725")+
  labs(x="journey time to work (min)", y = "number of households")+
  theme_bw()
dev.off()

## these histograms are available on the github repository as "journey_time_histograms.pdf".

### Another way to visualize the distribution of this variable would be by using a boxplot, which summarizes 
### the percentiles of this specific variable (and the IQR and outliers).
ggplot(combined, aes(x="", y=journey_time))+
  geom_boxplot(fill="lightblue")+
  labs(x="journey time to work (min)", y="")+
  theme_bw()

### Boxplots can be tricky to interpret with a wide spread of data. 
### We can add individual record points by adding another line to our command:
ggplot(combined, aes(x="", y=journey_time))+
  geom_boxplot(fill="lightblue")+
  labs(x="journey time to work (min)", y="")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw()

### This second boxplot doesn't look very attractive, and it's fairly crowded. 
### Another useful type of plot in this situation would be a violin plot.
### The width of the violin changes to represent how many individual records have that specific value.
### We can also add a boxplot to our violin, to represent the percentiles of the data:
ggplot(combined, aes(x = "", y = journey_time,)) + 
  geom_violin(fill="lightblue") +
  geom_boxplot(width=0.1, alpha=0.2) +
  labs(x="journey time to work (min)", y="")+
  theme_bw()

## plots are in "journey_time_boxplots_violins.pdf".

### R also offers great tools to analyze qualitative or categorical variables. 
### Let's take a look at our transportation mode variable:
summary(combined$transport_mode)

## Here, our 'summary' command gives us the number of observations in each category (factor with multiple levels). 

### The categories in our transportation variable have, in addition to the mode, a numeric code used in the survey.
### We can recode the variable to remove these codes, as this categorical variable is coded as a factor in R.
## than we reassign the levels with the new names:
levels(combined$transport_mode) <- c("Car", "Truck", "Van", "Bus_Streetcar", "Subway", "Railroad",
                                     "Taxicab", "Motorcycle", "Bicycle", "Other","Walked", "Works_home")

# we can also remove categories that we are not interested in, or combine different levels to make more significant
# categories. Here we'll first combine the "Bus_Streetcar", "Subway", "Railroad", and "Taxicab" levels into one
# ("Public_transit"), and remove the "Works_home" category.

# First we'll create a new, duplicate column, than reassign the levels, renaming those we want to combine:
combined$transport_mode_reduced <- combined$transport_mode
levels(combined$transport_mode_reduced) <- c("Car", "Truck", "Van", "Public_transit", "Public_transit", "Public_transit",
                                             "Public_transit", "Motorcycle", "Bicycle", "Other","Walked", "Works_home")
# check the work:
table(combined$transport_mode_reduced)

# Now we'll remove the work from home level. Note that we are creating a new object here:
drop.combined <- combined[combined$transport_mode_reduced %in% c("Car", "Truck", "Van", "Public_transit", 
                                                                 "Motorcycle", "Bicycle", "Other","Walked"), ]
drop.combined <- droplevels(drop.combined)

# check your work!
table(drop.combined$transport_mode_reduced)

## We can go ahead and rename the levels of our gender and moved variables, to make our lives easier in the future:
levels(drop.combined$gender) <- c('male', 'female')
levels(drop.combined$moved) <- c('yes', 'no')

## going back to our categorical variables, there a couple of cool things we can do:
### We can modify the table command with'prop.table' to obtain relative frequencies:
prop.table(table(combined$transport_mode_reduced))

### We can also use other packages to display nicer tables. Let's take a quick look at the package "formattable",
### which has a similar usage to ggplot and prepares tables in HTML format:
library(formattable)

## first let's see the basic visualization:
formattable(freq.table)

## it looks neater, but the raw proportions make it harder to see. Let's convert them to percentages and sort the values:
freq.table$Frequency <- percent(freq.table$Frequency)
freq.table <- freq.table[order(freq.table$Frequency, decreasing = TRUE),]
## now let's display the table again, adding a color bar to the frequency column to display the percentages:
formattable(freq.table, 
            align = c("l", "r"),
            list(Frequency = color_bar("lightpink")))

## this fancy table can be saved using the 'export' function on the RStudio viewer.

### CROSSTABULATION
### this is a nice bridge into a common method to look for associations between categorical variables:
### the two-way table or crosstabulation.
## We'll use the 'table' function that we used before to look into the association of gender and transportation mode:

tab.transp <- table(drop.combined$transport_mode_reduced, drop.combined$gender)
tab.transp

## we can also calculate the marginal distributions, for both variables:
margin.table(tab.transp, 1)
margin.table(tab.transp, 2)

## we can also calculate the conditional distributions by using the 'prop.table' command:
## we can generate this table in 2 ways, by row or by column:
prop.table(tab.transp, 1)   # by row
prop.table(tab.transp, 2)   # by column


### Another great way to analyze the crosstabulation results is through bar plots.
### here we'll use a ggplot function to split our results in several plots:
ggplot(drop.combined, aes(x=gender, y=..prop.., group=1))+ 
  geom_bar(fill="lightpink")+
  facet_wrap(~transport_mode_reduced)+
  scale_y_continuous(labels = scales::percent)+ 
  labs(x="transport mode by gender", y=NULL)+
  theme_bw()

#### QUANTITATIVE VARIABLES
### There are several tools available to explore associations between quantitative variables. One of the starting points is
### to plot a scatterplot to see the relative distribution of the two variables.
### Let's look more closely on the relationship between income and distance from work for this example.
### More specifically, if income level (independent variable) can predict how far people live 
### from where they work (dependent variable).
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_point(alpha=0.2)+
  labs(x="household income", y="distance travelled to work (miles)")+
  scale_x_continuous(labels = scales::dollar)+
  theme_bw()

## we can calculate the correlation coefficient of these two variables:
## correlation coefficients vary from -1 to 1, zero means no correlation, and the signs indicate the direction of the correlation.
cor.income <- cor(drop.combined$income, drop.combined$journey_distance)
cor.income
## we can infer this is a positive but weak correlation. This value can be influenced by non-linearity and outliers.

### R provides many ways to infer the associations between variables. Another tool we could use is to fit a linear model.
## Here we'll use income as the independent variable (the cause) and journey distance as the dependent variable (the effect). 
model1 <- lm(journey_distance ~ income, data = drop.combined)
summary(model1)
## The p-value of this model is high and our test fails to reject the hypothesis that there is no association between our variables.
## The coefficients estimated by the model predicts that a one dollar increase in income is associated 
## with a 0.000002161753 increase in miles traveled to work, and our R-squared shows that the mode explains very little of the
## variation in the data.

## We can add an ordinary least square (regression) line to the scatterplot from before:
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_smooth(method="lm", se=TRUE)+
  geom_point(alpha=0.2)+
  labs(x="household income", y="distance travelled to work (miles)")+
  scale_x_continuous(labels = scales::dollar)+
  theme_bw()
# plot in "journey_time_income.pdf".

## this is not the best regression line, as you can see. The blue line is the OLS, 
## and the grey shadowing is the confidence interval band for the line.

## We can adapt our original model to account for other independent variables that could be interfering with our regression.
## We just need to adapt the formula to include another independent term, in this example, age:

model2a <- lm(journey_distance ~ income + age, data = drop.combined)
summary(model2a)

# We can also include a categorical variable as independent in this model:
model2b <- lm(journey_distance ~ income + age + gender, data = drop.combined)
summary(model2b)

## as we can see on the summary, the relationship among these variables in non significant, 
## and very little of the variation found in journey time can be explained by age or income (see the multiple R-squared value).

## we can produce a publication-level table with our model summaries using the stargazer library:
library(stargazer)
stargazer(model1, model2a, model2b, type ="html", title = "Linear model results", align = TRUE, 
          dep.var.labels="Distance travelled to work", covariate.labels=c("Income", "Age", "Gender"), 
          no.space=TRUE, out = "linear.html")

### detecting non-linearity
### Smoothing is a technique for estimating the relationship in a scatterplot without 
## the assumption that this relationship is linear. There are many methods to do this on R, including neighbor means and 
## locally estimated scatterplot smoothing (LOESS), and general additive model (GAM). Each is recommended for specific cases,
## mostly depending on the number of observations on the dataset. 
## The geom_smooth function on ggplot calculates smoothing lines automatically, with the option of changing the method used. 

## Here we'll use the general additive model, which is the function default and recommended for 1000+ observations.
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm", color="blue", se=FALSE)+
  geom_smooth(color="red", se=FALSE)+
  scale_x_continuous(labels = scales::dollar)+
  labs(x="household income", y="distance travelled to work (miles)")+
  theme_bw()

## the non-linearity of the relationship between these two variables becomes evident here (see the red line)
## One way to control for non-linearity in the relationship between variables is to log transform one or more variables. 
## Income or wages is a variable that is frequently log-transformed in studies, due to the fact that it increases very rapidly.
## We just have to be careful with the interpretation of the generated models.

## log transformation does not work in null values, because they become infinite after the transformation,
## so we'll first replace all the null values in our two variables with NA, so they are excluded from the transformation. 
## we are slightly increasing the amount of missing data in our dataset here. 
drop.combined$journey_distance_clean <- ifelse(drop.combined$journey_distance==0, NA, drop.combined$journey_distance)
drop.combined$income_clean <- ifelse(drop.combined$income==0, NA, drop.combined$income)

## log transformation of income within the model:
model1a <- lm(journey_distance_clean ~ log(income_clean), data = drop.combined)
summary(model1a)
# the interpretation is trickier here, but by plugging the coefficients into the linear regression formula, the interpretation 
# is that a 1% increase in income incurs in a ~0.08 mile increase in journey to work.
# Besides, the p-value indicates that the results are significant.

## if we plot this new regression, with the smoothing line, the results are quite surprising:
ggplot(drop.combined, aes(x=income_clean, y=journey_distance_clean))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm", color="blue", se=FALSE)+
  geom_smooth(color="red",se=FALSE)+
  scale_x_log10(labels = scales::dollar)+
  labs(x="household income", y="distance travelled to work (miles)")+
  theme_bw()

#### ANOVA
#### One statistical test that is widely used in social sciences is the ANOVA (analysis of variance), 
## to understand mean difference between groups, when the independent variable(s) is(are) categorical.
## It can be easily implemented in R, with several functions that can be used.
## Let's try is using income and gender. We are trying to infer if differences in income are associated to gender.

## One way ANOVA: let's test if mean income is similar between genders. Gender is our independent variable:
## We are using tha aov function from the base stats package.
one.way <- aov(income ~ gender, data = drop.combined)
summary(one.way)
## we reject the null hypothesis of no difference between mean income between gender. 
## We reject the null hypothesis that the variation caused by the independent variable is due to chance.

## Two way ANOVA are used when more than one independent variable is availale.
## we'll add age as a second independent variable, but first we'll categorize it into intervals, 
## since we can't use quantitative independent variables in this test.
drop.combined$age_group <- cut(drop.combined$age, breaks=seq(from=17, to=93, by=10), right=FALSE)
summary(drop.combined$age_group)


## we can test if age and gender have an interaction effect instead of an additive effect (see the * sign):
two.way.int <- aov(income ~ gender * age_group, data = drop.combined)
summary(two.way.int)
## besides both gender and age having an effect on income, there is extra variation that is 
## explained by the interaction between these two variables (e.g., women of a certain age would be impacted differently than 
## men of that age).

## We can use the Akaike Information Criteria to compare these 3 models and indicate which one is a better fit:
## we'll use a different package that helps with plotting comparative AIC tables:
library(AICcmodavg)

model.set <- list(one.way, two.way, two.way.int)
model.names <- c("one.way", "two.way", "interaction")
aictab(model.set, modnames = model.names)

## When using AIC, the lowest value indicates the better fit (in this case the first line). This model has 85% of the AIC weight,
# which means it explains 85% of the total variation in the dependent variable that can be explained by the full set of models.

## we can do a Tukey’s Honestly Significant Difference (Tukey’s HSD) post-hoc test to see pairwise comparisons and check 
## where the differences are:
tukey.two.way<-TukeyHSD(two.way.int)
tukey.two.way
# we can see that the difference between genders is significant (p value < 0.05), decrease of ~25K in female income.
# and between some age groups (e.g. between 37-47 and 17-27), and between male, 27-37 and female 17-27.

#### LOGIT MODELS
## The logit model and the glm function allow us to make predictions when our dependent variable is a binary outcome. 
## We use this model instead of linear regressions here because our outcome is binary (0 or 1), and linear regression 
## can generate estimated probabilities that increase or decrease infinitely, generating nonsensical values on our model.
## A log-odds (or logit) transformation stretches all the estimated probabilities into a curve contained between 0 and 1.

## we will explore this model using our variable "moved", which contains answers to the question "did the home owner/renter 
## had to move due to hurricane Katrina?". This a classic example of yes/no outcome. 

## let's try a simple case looking into the association of gender and having had to move:
model.gender <- glm(moved~gender, data=drop.combined, family=binomial(logit))
summary(model.gender)

# the interpretation is fairly complex here, but the odds of a woman having had to move due to Katrina is 1.05 that of a man.

# we can also include both gender and income as independent variables:
model.both <- glm(moved~gender*income, data=drop.combined, family=binomial(logit))
summary(model.both)

## how can we interpret that?
## each one-dollar increase in income will increase the log-odds of moving by 0.0000001450
## being a woman will decrease the log-odds of moving by 0.02563
## the interaction between being a woman and the income will decrease the log-odds of moving by 0.0000002958
## the null and residual deviance are very close in value, which indicates this is not a good model

# we can use the anova function to perform a likelihood ratio test to see which of our nested models is a best fit:
anova(model.gender, model.both, test="LRT")
## We want to pay attention to how much our model's deviance reduces from the null model deviance.
## the deviance is very close among all models, showing they are pretty much equal in their power to explain the data, 
## and the p-value is quite high, so we fail to reject the null hypothesis. 

## and finally, we can save the dataframe that we've been working on in case we want to repeat some of this tests in the future:
write.csv(drop.combined, file = "NOLA_housing_survey_subset.csv")

