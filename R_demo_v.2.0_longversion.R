####################################################################
###       R demonstration v.2.0                                  ###
###       Introduction to Sociology Methods                      ###
###       Carolina M. Siniscalchi                  Jan/2023      ###
####################################################################


### The dataset used here was sourced from ICSPR (https://www.icpsr.umich.edu/web/ICPSR/studies/30943#).

### This dataset is composed by data from the New Orleans part of the American Housing Survey, 2009.
### The survey has several parts that are summarized in 8 R data files.

### The first step is to load the data. Here, we will focus on two datafiles, 
### the first contains details about journey to work and the second has 
### demographic data from the participating households.

## first, let's define a working directory, where all saved files and figures will go:
setwd("~/")
getwd()

## Now, we'll use the load function to load the data:
load("~/Desktop/R_demo/30943-0002-Data.rda")
load("~/Desktop/R_demo/30943-0004-Data.rda")

## let's take a look at these data files:
head(da30943.0002)
head(da30943.0004)

## this visualization is quite complicated to understand. We can also click on the data objects on the right side menu.
## we can also use the codebook dowloaded with the datafile to understand the variables.

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

## The "str" function can be used to investigate the structure of a dataframe:
str(combined)

## Let's start with some basic descriptive statistics

## check maximum and minimum values of time spend on the journey to work:
max(combined$journey_time)
min(combined$journey_time)

## we can also use the range function to obtain max and min:
range(combined$journey_time)

### It seems interesting that we have some observations on our dataset with a very high journey-to-work time (996 min!).
### This can be a strategy to code missing or non applicable values on datasets, and it is important that we know that,
### as extreme values will affect any statistics performed on the dataset. 
### One way to understand variable coding, is looking on the study codebook (in this case, the codebook doesn't have 
### any information on these values).

### We can quickly subset our dataset to see if this high journey time is associated with other variables.
## this command asks to return any rows that have a value equal to 996 on the journey_time column. 
combined[combined$journey_time == '996',]

### It becomes clearer that this value was assigned to people that work from home, where journey_time is non-applicable. 
### R has several options to deal with variable value assigment. We'll use a boolean statement to find and replace those '996'
### values with 'NA', in all columns where it shows up.

combined$journey_time <- ifelse(combined$journey_time==996, NA, combined$journey_time)
combined$journey_distance <- ifelse(combined$journey_distance==996, NA, combined$journey_distance)

## let's check our variables now:
summary(combined$journey_time)
summary(combined$journey_distance)

### Now that our variable values are recoded, we can go back to our descriptive statistics. 
### The 'summary' command we used above is handy to look at data, in the case of a quantitative variable like ours, 
### it returns some basic information about it. 

## Additionaly, base R has commands to calculate several individual descriptive statistics. Let's try some:

## mean
mean(combined$journey_time)

### When we apply this specific command, we get only an "NA", and that's because our dataset now has missing values. 
### We need to modify our commands to account for that:
mean(combined$journey_time, na.rm = TRUE)

### Let's try with other indexes:
## median
median(combined$journey_time, na.rm = TRUE)
## first and third quartile
quantile(combined$journey_time, 0.25, na.rm = TRUE)
quantile(combined$journey_time, 0.75, na.rm = TRUE)
quantile(combined$journey_time, 0.98, na.rm = TRUE)
## interquartile range:
IQR(combined$journey_time, na.rm = TRUE)
## standard deviation and variance
sd(combined$journey_time, na.rm = TRUE)
var(combined$journey_time, na.rm = TRUE)

### R is very powerful for data visualization. We can quickly plot the distribution of our journey time variable 
### as an histogram. We are using the package ggplot to create the graph. It has a specific syntax, where parts of the plot
### are defined in different parts of the command:
ggplot(combined, aes(x=journey_time))+
  geom_histogram(binwidth=5, col="black", fill="lightblue")+
  labs(x="journey time to work (min)", y = "number of households")+
  theme_bw()

### Note the warning message displayed. R (and ggplot) automatically remove the non-numeric values (NA) from the plot.

## we can changed the width of the bin if we want to make our graph more or less detailed:
ggplot(combined, aes(x=journey_time))+
  geom_histogram(binwidth=2, col="black", fill="lightblue")+
  labs(x="journey time to work (min)", y = "number of households")+
  theme_bw()

ggplot(combined, aes(x=journey_time))+
  geom_histogram(binwidth=10, col="black", fill="lightblue")+
  labs(x="journey time to work (min)", y = "number of households")+
  theme_bw()

## we can also easily save our plotted graph as a pdf:
pdf(file="journey_time_distribution.pdf", width = 10, height = 7)
ggplot(combined, aes(x=journey_time))+
  geom_histogram(binwidth=10, col="black", fill="lightblue")+
  labs(x="journey time to work (min)", y = "number of households")+
  theme_bw()
dev.off()

### Another way to visualize the distribution of this variable would be by using a boxplot, which summarizes 
### the percentiles of this specific variable (and the IQR and outliers).
ggplot(combined, aes(x="", y=journey_time))+
  geom_boxplot(fill="lightblue")+
  labs(x="journey time to work (min)")+
  theme_bw()

### Boxplots can be tricky to interpret with a wide spread of data. 
### We can add individual records by adding another line to our command:
ggplot(combined, aes(x="", y=journey_time))+
  geom_boxplot(fill="lightblue")+
  labs(x="journey time to work (min)")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw()

### This second boxplot doesn't look very attractive, and it's fairly crowded. 
### Another useful type of plot in this situation would be a violin plot, which can be quickly plotted using a similar command:
ggplot(combined, aes(x="", y=journey_time))+
  geom_violin(fill="lightblue")+
  labs(x="journey time to work (min)")+
  theme_bw()

### The width of the violin changes to represent how many individual records have that specific value.
### We can also add a boxplot to our violin, to represent the percentiles of the data:
ggplot(combined, aes(x = "", y = journey_time,)) + 
  geom_violin(fill="lightblue") +
  geom_boxplot(width=0.1, color="grey90", alpha=0.2) +
  labs(x="journey time to work (min)")+
  theme_bw()

### R also offers great tools to analyze qualitative or categorical variables. 
### Let's take a look at our transportation mode variable:

summary(combined$transport_method)

## Here, our 'summary' command gives us the number of observations in each category. 
## These numbers could also be obtained with the command 'table':
table(combined$transport_method)

### We can modify this command with the 'prop.table' command to obtain relative frequencies:
prop.table(table(combined$transport_method))

### The categories in our transportation variable have, in addition to the mode, a numeric code used in the survey.
### We can recode the variable to remove these codes, as this categorical variable is coded as a factor in R.

## first we check all the levels present:
levels(combined$transport_method)
## than we reassign the levels with the new names:
levels(combined$transport_method) <- c("Car", "Truck", "Van", "Bus_Streetcar", "Subway", "Railroad",
                                        "Taxicab", "Motorcycle", "Bicycle", "Other","Walked", "Works_home")

### R also makes it easier to save results of commands as tables or text files.
### Let's save the frequency table we created as a tab-separated file.
## we first create a dataframe with the frequency table, then assign column names:
freq.table <- data.frame(prop.table(table(combined$transport_method)))
colnames(freq.table) <- c("Transportation_mode", "Frequency")
## than we use the write.table command to save it:
write.table(freq.table, file = "transportation.frequency.tsv", sep = "\t", row.names = FALSE, col.names = TRUE)

### We can also use other packages to display nicer tables. Let's take a quick look at the package "formattable",
### which has a similar usage to ggplot and prepares tables in HTML format:

## first let's see the basic visualization:
formattable(freq.table)

## it looks neater, but the raw proportions make it harder to see. Let's use the 'percent' function to change it:
freq.table$Frequency <- percent(freq.table$Frequency)
## now let's display the table again, adding a color bar to the frequency column to display the percentages:
formattable(freq.table, 
            align = c("l", "r"),
            list(Frequency = color_bar("lightpink")))

## this fancy table can be saved using the 'export' function on the RStudio viewer.

### As we did with the quantitative variable, we can use a barplot to analyze the distribution of our observations:
ggplot(combined, aes(x=transport_method))+
  geom_bar(fill="#6E1B2D")+
  labs(x="transportation mode", y="percent")+
  theme_bw()

## we can easily modify our code to use proportions instead of raw numbers, and flip the legends:
ggplot(combined, aes(x=transport_method, y=..prop.., group=1))+
  geom_bar(fill="#6E1B2D")+
  labs(x="transportation mode", y="percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## we can also flip the bars to a horizontal position:
ggplot(combined, aes(x=transport_method, y=..prop.., group=1))+
  geom_bar(fill="#6E1B2D")+
  labs(x="transportation mode", y="percent")+
  scale_y_continuous(labels=scales::percent)+
  coord_flip()+
  theme_bw()

### We can easily use ggplot functionalities to plot more elaborate graphs.
### First, let's use a violin plot to see the distribution of journey time in relation to transportation mode

ggplot(combined, aes(x = transport_method, y = journey_time, fill = transport_method)) +
  geom_violin(trim = TRUE) + ylim(min(combined$journey_time), (max(combined$journey_time))) +
  labs(x="Transportation Mode", y="Journey Time (min)")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## we can improve the proportions of the plot by saving it as a pdf
pdf(file="journey_time_transport.pdf", width = 13, height = 7)
ggplot(combined, aes(x = transport_method, y = journey_time, fill = transport_method)) +
  geom_violin(trim = TRUE) + ylim(min(combined$journey_time), (max(combined$journey_time))) +
  labs(x="Transportation Mode", y="Journey Time (min)") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()

### We can see here that two categories of our transportation were dropped due to the 
### low number of observations (subway and railroad), besides our work from home category, as the jorney time is NA. 
### We can improve the graph by removing these levels from our variable:

drop.combined <- combined[combined$transport_method %in% c("Car", "Truck", "Van", "Bus_Streetcar", 
                                                       "Taxicab", "Motorcycle", "Bicycle", 
                                                       "Other","Walked"), ]
drop.combined <- droplevels(drop.combined)

## let's check our plot again:
ggplot(drop.combined, aes(x = transport_method, y = journey_time, fill = transport_method)) +
  geom_violin(trim = TRUE) + ylim(min(combined$journey_time), (max(combined$journey_time))) +
  labs(x="Transportation Mode", y="Journey Time (min)") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## we can quickly modify this plot to show the distribution of journey time by gender:
ggplot(drop.combined, aes(x = transport_method, y = journey_time, fill = sex)) +
  geom_violin(trim = TRUE) + ylim(min(combined$journey_time), (max(combined$journey_time))) +
  labs(x="Transportation Mode", y="Journey Time (min)", fill = "Gender") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## this is pretty cool, but we can see that our gender variable is coded with 1 and 2, which is not immediately informative.
## Let's recode our variable. Here it's important to check the codebook, as the factor level reassignment is done by numerical order.
levels(drop.combined$sex) <- c('male', 'female')
levels(drop.combined$moved) <- c('yes', 'no')

### this is a nice bridge into a common method to look for associations 
### between categorical variables: the two-way table or crosstabulation.

## We'll use the 'table' function that we used before to look into the association of gender and transportation mode:

tab.method <- table(drop.combined$transport_method, drop.combined$sex)
tab.method

## we can also calculate the marginal distributions, for both variables:
margin.table(tab.method, 1)
margin.table(tab.method, 2)

## we can also calculate the conditional distributions by using the 'prop.table' command:
## we can generate this table in 2 ways, by row or by column:
prop.table(tab.method, 1)
prop.table(tab.method, 2)

## let's make this table look nicer:
## first let's go by row, to see if there is a gender difference by transport method:
cond.table <- data.frame(unclass(prop.table(tab.method, 1)))
cond.table$male <- percent(cond.table$male)
cond.table$female <- percent(cond.table$female)

formattable(cond.table, list(area(col = male:female) ~ color_tile("transparent", "goldenrod")))

### Another great way to analyze the crosstabulation results is through bar plots.
### here we'll use a ggplot function to split our results in several plots:
ggplot(drop.combined, aes(x=sex, y=..prop.., group=1))+ 
  geom_bar(fill="goldenrod")+
  facet_wrap(~transport_method)+
  scale_y_continuous(labels = scales::percent)+ 
  labs(x="transport method by gender", y=NULL)+
  theme_bw()

## Let's look at some methods to compare a quantitative and a categorical variable.
## The first one would be a comparative boxplot, similar to the comparative violin plots we saw before.
## We'll work with journey distance and transportation mode.

dist.boxplot <- ggplot(drop.combined, aes(x=reorder(transport_method, journey_distance, mean), y=journey_distance))+
  geom_boxplot(fill="seagreen", outlier.color = "purple")+
  labs(x=NULL, y="journey to work distance")+
  coord_flip()+
  theme_bw()
plot(dist.boxplot)

## we can also look at the mean difference among each catgory
mjourney <- tapply(drop.combined$journey_distance, drop.combined$transport_method, mean)
mjourney <- sort(mjourney, decreasing=FALSE)
mjourney <- as.data.frame.table(mjourney)
colnames(mjourney) <- c("mode", "distance")

library(ggalt)
dist.mean <- ggplot(mjourney, aes(x=mode, y=distance))+
  geom_lollipop()+
  coord_flip()+
  labs(x=NULL, y="mean distance to work")+
  theme_bw()
plot(dist.mean)

### save both plots as a pdf:
library(ggpubr)
pdf(file="transportmode_distance.pdf", width = 20, height = 8)
ggarrange(dist.boxplot, dist.mean,
          ncol = 2, nrow = 1)
dev.off()

### There are several tools available to explore associations between quantitative variables.
### Let's look more closely on the relationship between income and distance from work.
### More specifically, if income level (independent variable) can predict how far people live from where they work (dependent variable).
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_point(alpha=0.2)+
  labs(x="household income", y="distance travelled to work")+
  scale_x_continuous(labels = scales::dollar)+
  theme_bw()

## we can also calculate the correlation coefficient of these two variables:
cor.income <- cor(drop.combined$income, drop.combined$journey_distance)
cor.income
## we can infer this is a positive but weak correlation. This value can be influenced by non-linearity and outliers.
## we can also calculate the confidence interval for this correlation coefficient:
library(psychometric)
CIr(cor.income, nrow(drop.combined), 0.95)
## this shows us that the correlation between income and distance to work is uncertain in the general population.

## we can also calculate the p-value from this correlation coefficent to understand the association better:
n <- nrow(drop.combined)
se <- sqrt((1-cor.income^2)/(n-2))
se
cor.income/se
2*pt(-0.6091722, n-2)

## therefore, we fail to reject the null hypothesis that income and distance to work show no association. 
## Meaning that there is no association between these two variables in this dataset.

### R provides many ways to infer the associations between variables. Another tool we could use is to fit a linear model on our data:
model1 <- lm(journey_distance ~ income, data = drop.combined)
summary(model1)
## The p-value of this model is in line with what we saw above, our test fails to reject the hypothesis that 
## there is no association between our variables.

## We can add an ordinary least square (regression) line to the scatterplot from before:
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_smooth(method="lm", se=TRUE)+
  geom_point(alpha=0.2)+
  labs(x="household income", y="distance travelled to work")+
  scale_x_continuous(labels = scales::dollar)+
  theme_bw()

## the model predicts that a one dollar increase in income is associated with a 0.000002161753 increase in miles traveled to work.

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

ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_point(alpha=0.2)+
  geom_line()+
  scale_x_continuous(labels = scales::dollar)+
  labs(x="household income", y="distance travelled to work")+
  theme_bw()

### smoothing by neighbor means:
drop.combined$journey_distance.smooth1 <- runmed(drop.combined$journey_distance, 501)
drop.combined$journey_distance.smooth2 <- runmed(drop.combined$journey_distance, 5)

ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_point(alpha=0.2)+
  geom_line(col="grey", size=1, alpha=0.7)+
  geom_line(aes(y=journey_distance.smooth2), col="red", size=1, alpha=0.7)+
  geom_line(aes(y=journey_distance.smooth1), col="blue", size=1, alpha=0.7)+
  scale_x_continuous(labels = scales::dollar)+
  labs(x="household income", y="distance travelled to work")+
  theme_bw()

## locally estimated scatterplot smoothing (LOESS)
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm", color="red", se=FALSE)+
  geom_smooth(se=FALSE, method="loess")+
  scale_x_continuous(labels = scales::dollar)+
  labs(x="household income", y="distance travelled to work")+
  theme_bw()

## using the general additive model (GAM) (default of geom_smooth for 1000+ observations)
ggplot(drop.combined, aes(x=income, y=journey_distance))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm", color="red", se=FALSE)+
  geom_smooth(se=FALSE)+
  scale_x_continuous(labels = scales::dollar)+
  labs(x="household income", y="distance travelled to work")+
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

### ### Splines
## another way to deal with non-linearity is to use a spline, in which "hinges" are used in the regression line to 
## allow for different linear effects in different subsets of our data. We'll work a simple example with only one hinge, 
## but R has packages creates specifically for that, for example, the "splines" package.

# We'll use our age and income variables here. Let's first take a look at a linear regression between them, with a smoothing line: 
ggplot(drop.combined, aes(x=age, y=income_clean))+
  geom_jitter(alpha=0.2)+
  geom_smooth(method="lm", color="red", se=FALSE)+
  geom_smooth(se=FALSE)+
  scale_y_continuous(labels = scales::dollar)+
  labs(x="age", y="income")+
  theme_bw()

# we see two main inflection points on the smoothing line, the first one at ~40 y.o. 
## We'll model it by creating a new independent variable which will contain the value of "age-40".
drop.combined$age.spline <- ifelse(drop.combined$age<40, 0, drop.combined$age-40)
model1c <- lm(income_clean~age+age.spline, data=drop.combined)
summary(model1c)

## our model predicts that for individuals 40 yo and younger, a 1-year increase in age is associated with $2441 
## on average income. For individuals older than 40, a 1-year increase in age is associated with a decrease of $373 in income.

# to plot this spline, we'll create a new dataset, containing a column for ages found on our dataset,
# then we'll use the command "predict" to collect the predicted income values for the ages contained in our dataset, 
# from the model we just calculated, than we'll plot this data as a line:
predict_df <- data.frame(age=17:93)
predict_df$age.spline <- ifelse(predict_df$age<40, 0, predict_df$age-40)
predict_df$income <- predict(model1c, newdata=predict_df)

ggplot(drop.combined, aes(x=age, y=income))+
  geom_jitter(alpha=0.01, width = 1)+
  geom_smooth(se=FALSE)+
  geom_line(data=predict_df, color="red", size=1.5, alpha=0.75)+
  theme_bw()
# our spline regression (in red) fits much closer to the smoothing line we initially plotted. 
# plots in "non-linearity_regressions.pdf".

### One issue that can arise from the data-generating process is data-based multicolinearity, 
## meaning that two or more of the variables in the data are highly correlated. 
## There are multiple ways of dealing with this issue that usually comes down to what the researcher wants to do with the data,
## but here I want to show a nice visualization of this issue. 
# the obvious step would be to get the correlation coefficient of the variables (values closer to 1 mean higher correlation):
# we can also use the corrgram package to plot this correlations:
library(corrgram)
corrgram(drop.combined[,c("income","age", "journey_distance", "journey_time")],
         upper.panel="panel.cor", lower.panel="panel.pts")




### ANOVA test
## One way ANOVA: let's test if mean income is similar between genders. Gender is our independent variable:
one.way <- aov(income ~ sex, data = drop.combined)
summary(one.way)
## we reject the null hypothesis of no difference between mean income between gender. 
## We reject the null hypothesis that the variation caused by the independent variable is due to chance.

## Two way ANOVA: we'll add age as a second independent variable, but first we'll categorize it into intervals:
drop.combined$age_group <- cut(drop.combined$age, breaks=seq(from=10, to=100, by=10), right=FALSE)
summary(drop.combined$age_group)

## now the test:
two.way <- aov(income ~ sex + age_group, data = drop.combined)
summary(two.way)

## we can also test if age and sex have an interaction effect instead of an additive effect:
two.way.int <- aov(income ~ sex * age_group, data = drop.combined)
summary(two.way.int)
## some of the variation is explained by the interaction between these two terms

## We can use the Akaike Information Criteria to compare these 3 models and indicate which one is a better fit:
library(AICcmodavg)

model.set <- list(one.way, two.way, two.way.int)
model.names <- c("one.way", "two.way", "interaction")
aictab(model.set, modnames = model.names)

## When using AIC, the lowest value indicates the better fit (in this case the first line). This model has 87% of the AIC weight,
# which means it explains 87% of the total variation in the dependent variable that can be explained by the full set of models.

## we can plot the residuals to check the assumption of homoscedasticity:
par(mfrow=c(2,2))
plot(two.way.int)
par(mfrow=c(1,1))

## our residuals plot seems ok, but our Q-Q plot shows a large variation from the theoretical residuals

## we can do a Tukey’s Honestly Significant Difference (Tukey’s HSD) post-hoc test to see pairwise comparisons and check 
## where the differences are:

tukey.two.way<-TukeyHSD(two.way.int)
tukey.two.way

# we can see that the difference between genders is significant (p value < 0.05), 
# and between some age groups (e.g. between 30-40 and 20-30), and between male, 50-60 and female 30-40.

tukey.int <- TukeyHSD(aov(income ~ sex : age_group, data = drop.combined))
tukey_matrix <- as.matrix(tukey.int) 
df_res <- as.data.frame(tukey_matrix[1])
plot(tukey_matrix, col= ifelse(df_res[,4]<0.05, 'red', 'black'), las=1, cex.lab=0.05)


#### the logit model
## The logit model and the glm function allow us to make predictions when our dependent variable is a binary outcome. 
## We use this model instead of linear regressions here because our outcome is binary (0 or 1), and linear regression 
## can generate estimated probabilities that increase or decrease infinitely, generating nonsensical values on our model.
## A log-odds (or logit) transformation stretches all the estimated probabilities into a curve contained between 0 and 1.

## we will explore this model using our variable "moved", which contains answers to the question "did the home owner/renter 
## had to move due to hurricane Katrina?". This a classic example of yes/no outcome. 

## let's try a simple case looking into the association of gender and having had to move:
model.gender <- glm(moved~sex, data=drop.combined, family=binomial(logit))
summary(model.gender)

# the interpretation is fairly complex here, but the odds of a woman having had to move due to Katrina is 1.05 that of a man.
## in this case, the odds could be calculated straight from a two-way table between the two categorical variables, as described below:

table(drop.combined$sex, drop.combined$moved)
831*231/(1004*182) # 1.050529

## let's redo the model, but using income (quantitative) as the independent variable:
model.income <- glm(moved~income, data=drop.combined, family=binomial(logit))

# we can also include both variables as independent:
model.both <- glm(moved~income*sex, data=drop.combined, family=binomial(logit))
summary(model.both)

## how can we interpret that?
## each one-dollar increase in income will increase the log-odds of moving by 0.0000001450
## being a woman will decrease the log-odds of moving by 0.02563
## the interaction between being a woman and the income will decrease the log-odds of moving by 0.0000002958
## the null and residual deviance are very close in value, which indicates this is not a good model

# we can use the anova function to perform a likelihood ratio test to see which of our models is a best fit:
anova(model.gender, model.income, model.both, test="LRT")
## the deviance is very close among all models, showing they are pretty much equal in their power to explain the data, 
## and the p-value is quite high, so we fail to reject the null hypothesis. 

## and finally, we can save the dataframe that we've been working on in case we want to repeat some of this tests in the future:
write.csv(drop.combined, file = "NOLA_housing_survey_subset.csv")

