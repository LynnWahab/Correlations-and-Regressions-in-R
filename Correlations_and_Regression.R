
##SET WORKING DIRECTORY
setwd("~/Desktop") ###MODIFY THIS FILE PATH FOR YOUR COMPUTER
getwd()

##LOAD DATA FILE
#Load Exam Anxiety.dat Call it "exam".
exam <- read.delim("Exam Anxiety.dat", header=TRUE)

#Examine structure
str(exam)

##VISUALIZE RELATIONSHIPS
#Create a scatterplot of Exam Anxiety and Exam Performance. Call it scatter
#Include a line of best fit. 
library(ggplot2)
#you want y to be outcome and x to be predictor
scatter <-ggplot(exam, aes(x=Exam, y= Anxiety)) + geom_point() + geom_smooth(method=lm, se=F) + labs(x="Exam Scores", y="Anxiety Levels") + labs(title="Relationship beween Exam Scores and Anxiety Levels") + theme(plot.title = element_text(hjust=0.5)) 

##CALCULATE CORRELATION COEFFICIENTS
#Specified variables
cor(exam$Exam, exam$Anxiety, use = "complete.obs", method = "pearson")

#Correlation Matrix - Multiple Variables
#pearsons correlation cant be done on nonnumeric variables
cor(exam, use = "complete.obs", method = "pearson") ## WHY DOESN'T THIS WORK?

cor(exam[1:4], use="complete.obs", method="pearson")
#or
cor(exam[,names(exam)!="Gender"], use="complete.obs", method="pearson")
#or
cor(exam[,-5], use="complete.obs", method="pearson")


##CONDUCT HYPOTHESIS TEST ON CORRELATION COEFFICIENT
#TWO TAILED if you dont know if correlation is going to be positive or negative
cor.test(exam$Exam, exam$Anxiety, method="pearson")
#this is more conservative (we will use it in this case )

##LEFT TAILED if you think correlation is going to be negative
cor.test(exam$Exam, exam$Anxiety, alternative="less", method="pearson")
#one-tailed are easier to reject null hyp, less conservative

##RIGHT TAILED if you think its going to be positive
cor.test(exam$Exam, exam$Anxiety, alternative="greater", method="pearson")

#The output has a t-test. the p-value says that the chances of getting this value, if really it shouldve been zero, is (the p-val) very small
#if the scatterplot shows neg- relationship for sure, in this case you would do directional (left-tailed)

#CONDUCT A REGRESSION ANALYSIS TO ASSESS WHETHER ANXIETY IS A SIGNIFICANT PREDICTOR OF EXAM GRADES.

r1<-lm(exam$Anxiety ~ exam$Exam) #lm = linear model, it represents exam scores as a function of anxiety
#The order is important, youre predicting exam scores from anxiety, and so anxiety is y-axis (its the outcome), score is predictor
summary(r1)

#Now we want to ask ourselves: is this a better predictor than just relying on the mean
#y-hat or anxiety-hat is: y-hat = beta-0 + beta-1 (x)
#beta-0 = 90.87
#beta-1 = -0.29

#if the null hypothesis is right and says its not a predictor, than beta-1 will be zero
#its -0.29, lets see if thats significantly far from zero. to answer than question, lets do a t-test
#this is included in the summary: if really theres no relationship, and -0.29 is really like zero, than the 
#chances of obtaining this data is less than 5% (we know this because we see Pr <2e-16)
#this, as a result, tells me that grades are a significant predictor of anxiety

#To figure out of the regression model is significantly better than the mean, theres an f-test (this is also included in the above summary)
#F(1, 101)=24.38, p<0.00003
#so if this regression model is no better than the mean, the chances of getting a 24.38 is less that 5%
#so, the regression model IS better than the mean

#finally, we have the r-squared = 0.1945, so this indicates that the amount of variability in one variable thats accounted 
#for by the other variable is 19.45%. You can explain 19% of those differences... just by knowing your grade. 

#NOW REPORT THE RESULTS: (found in slides)
# the predictor is significant (t-test)
# amnt of variability explained
# how well the model fits the data (f-test)

##PRACTICE
#Load dataset called "Muscial Profits.dat". Call it raw_data.
raw_data <- read.delim("Musical Profits.dat", header=TRUE)

##Inspect your data. What are the variables in this dataset. What type are they?
str(raw_data)
#The variables: adverts, sales, airplay, attract
#TypesL num, int, int, int

##VISUALIZE RELATIONSHIPS
#install.packages("ggplot2")
library(ggplot2)
#Create a scatterplot showing the relationship between airplays and sales. Call it scatter_airplay
#Give it a centered title that says "Relationship beween Airplays and Album Sales". Label your axes correctly.
scatter_airplay <-ggplot(raw_data, aes(x=airplay, y= sales)) + geom_point() + geom_smooth(method=lm, se=F) + labs(x="Airplays", y="Album Sales") + labs(title="Relationship beween Airplays and Album Sales") + theme(plot.title = element_text(hjust=0.5)) 
#note: use airplay to predict sales (this should be given in question)

#Create a scatterplot showing the relationship between attractiveness ratings and advertisements. Call it scatter_adverts
#Give it a centered title that says "Relationship beween Attractiveness Ratings and Advertisements. Label your axes correctly.
scatter_adverts <-ggplot(raw_data, aes(x=adverts, y= attract)) + geom_point() + geom_smooth(method=lm, se=F) + labs(x="Advertisements", y="Attractiveness Rating") + labs(title="Relationship beween Attractiveness Ratings and Advertisements") + theme(plot.title = element_text(hjust=0.5)) 
#note: use adverts as the predictor for attract

##CALCULATE CORRELATION COEFFICIENTS
#Get the full Pearson's correlation matrix. 
#Save this to an object called sales_pearsons.
sales_pearsons <- cor(raw_data, use = "complete.obs", method = "pearson") 

##CONDUCT TWO-TAILED CORRELATION AND REGRESSION HYPOTHESIS TESTS FOR:
# 1. Between Airplay and Sales
cor.test(raw_data$airplay, raw_data$sales, method="pearson")
r2<-lm(raw_data$airplay ~ raw_data$sales)
summary(r2)

# 2. Between Attractiveness Ratings and Adverts
cor.test(raw_data$attract, raw_data$adverts, method="pearson")
r3<-lm(raw_data$attract ~ raw_data$adverts)
summary(r3)

#State your hypotheses and interpret the results in the context of the hypotheses. 
# 1. H0: The true correlation is zero. There is no relationship between the two variables. rho = 0
#    H1: The true correlation not zero. There is a relationship between the two variables. rho ≠ 0
 
#NOTE: do correlation, report everything, then do correlation and report: 

# There is a significant relationship between airplays and sales, 
#   r = 0.60, p<.05, two-tailed. Number of album sales goes up as number of airplays inceases.
#   Airplays account for 35.87% of variance in album sales.
#   Airplays significantly predict one’s album sales, t(198)=10.52, p<.001, two-tailed. 
#   The regression model fits the data well overall, F(1,198)=110.7, p<.001. 

# 2. H0: The true correlation is zero. There is no relationship between the two variables. rho = 0
#    H1: The true correlation not zero. There is a relationship between the two variables. rho ≠ 0

# There is no significant relationship between atrractiveness ratings and advertisements, 
#   r = 0.08, p>.05, two-tailed. Number of advertisements does not go up as attractiveness inceases.
#   Advertisements account for 0.65% of variance in attractiveness ratings.
#   Advertisements do not significantly predict one’s attractiveness ratings, t(198)=1.14, p<.001, two-tailed. 
#   The regression model fits the data well overall, F(1,198)=1.3, p>.05. 

