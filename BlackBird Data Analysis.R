setwd("~/MT5766 Statistical Problem Solving/Case Study 4/Data")
library(ggplot2)
set.seed(125)

####Data Cleaning ####
#reads in data
blackbird_data <- read.csv("blackbird_v3.csv")

#cleans data so it is usable for fitting GLMs of weight by getting rid of U ages and sexes and getting rid of NA weights 
blackbird_data_no_NA_weights <- blackbird_data[which(blackbird_data$weight!="NA" & blackbird_data$age != "U" & blackbird_data$sex != "U"),]

#stores the data of birds that are male and female 
blackbird_data_male_data <- blackbird_data[which(blackbird_data$sex=="M"),]
blackbird_data_female_data <- blackbird_data[which(blackbird_data$sex=="F"),]


#stores the data of birds that are male and female and gets rid of data with N/A weights
blackbird_weight_male_data <- blackbird_data_no_NA_weights[which(blackbird_data_no_NA_weights$sex=="M"),]
blackbird_weight_female_data <- blackbird_data_no_NA_weights[which(blackbird_data_no_NA_weights$sex=="F"),]

#stores data of birds that are definetly British based on criteria in case study brief and then stores birds that dont fit this criteria
definetly_british_blackbirds <- blackbird_data[which(blackbird_data$age=="J" | blackbird_data$month %in% seq(5,8)),]
unsure_origin__birds <- blackbird_data[which(!(blackbird_data$age=="J" | blackbird_data$month %in% seq(5,8))),]

#edits this data so it is usable for weight analysis by getting rid of NA weights
definetly_british_blackbirds_weight <- blackbird_data_no_NA_weights[which(blackbird_data_no_NA_weights$age=="J" | blackbird_data_no_NA_weights$month %in% seq(5,8)),]
unsure_origin__birds_weight <- blackbird_data_no_NA_weights[which(!(blackbird_data_no_NA_weights$age=="J" | blackbird_data_no_NA_weights$month %in% seq(5,8))),]

#store birds that come from non-GBR ring scheme
definetly_not_british_birds <- blackbird_data[which(blackbird_data$scheme!="GBT"),]







####Plotting Data-frame construction ####
#code to create table that makes plot for number of birds of different ages and sexes
number_of_ages_for_sexes <- data.frame(
  age = levels(factor(blackbird_data$age)),
  Male = numeric(length(levels(factor(blackbird_data$age)))),
  Female = numeric(length(levels(factor(blackbird_data$age)))),
  Male_NA_Weights_Omitted = numeric(length(levels(factor(blackbird_data$age)))),
  Female_NA_Weights_Omitted = numeric(length(levels(factor(blackbird_data$age))))
)

number_of_ages_for_sexes


for (i in 1:length(levels(factor(blackbird_data$age)))){
  number_of_ages_for_sexes[i,2] <- length(blackbird_data_male_data[which(blackbird_data_male_data$age==levels(factor(blackbird_data$age))[i]),]$weight)
  number_of_ages_for_sexes[i,3] <- length(blackbird_data_female_data[which(blackbird_data_female_data$age==levels(factor(blackbird_data$age))[i]),]$weight)
  number_of_ages_for_sexes[i,4] <- length(blackbird_weight_male_data[which(blackbird_weight_male_data$age==levels(factor(blackbird_data$age))[i]),]$weight)
  number_of_ages_for_sexes[i,5] <- length(blackbird_weight_female_data[which(blackbird_weight_female_data$age==levels(factor(blackbird_data$age))[i]),]$weight)
}

number_of_ages_for_sexes

Acounts <- number_of_ages_for_sexes[1,2:5]
Fcounts <- number_of_ages_for_sexes[2,2:5]
Jcounts <- number_of_ages_for_sexes[3,2:5]
Ucounts <- number_of_ages_for_sexes[4,2:5]

Acounts <- c(Acounts[1,1],Acounts[1,2],Acounts[1,3],Acounts[1,4])
Fcounts <- c(Fcounts[1,1],Fcounts[1,2],Fcounts[1,3],Fcounts[1,4])
Jcounts <- c(Jcounts[1,1],Jcounts[1,2],Jcounts[1,3],Jcounts[1,4])
Ucounts <- c(Ucounts[1,1],Ucounts[1,2],Ucounts[1,3],Ucounts[1,4])

plottingtable <- table(1:4,1:4)

plottingtable[,1] <- Acounts
plottingtable[,2] <- Fcounts
plottingtable[,3] <- Jcounts
plottingtable[,4] <- Ucounts

rownames(plottingtable) <- c("Male","Female","Male N/As Omitted","Female N/As Omitted")
colnames(plottingtable) <- levels(factor(blackbird_data$age))

####Sex of Birds plotting code####
#stores number of birds of each sex with and without N/A weights removed
Malecounts <-  length(blackbird_data[which(blackbird_data$sex=="M"),]$weight)
Femalecounts <- length(blackbird_data[which(blackbird_data$sex=="F"),]$weight)
Uncounts <- length(blackbird_data[which(blackbird_data$sex=="U"),]$weight)
Malecountsweights <-  length(blackbird_data_no_NA_weights[which(blackbird_data_no_NA_weights$sex=="M"),]$weight)
Femalecountsweights <- length(blackbird_data_no_NA_weights[which(blackbird_data_no_NA_weights$sex=="F"),]$weight)
Uncountsweights <- length(blackbird_data_no_NA_weights[which(blackbird_data_no_NA_weights$sex=="U"),]$weight)

sexcounts<- c(Malecounts,Femalecounts,Uncounts)
sexcountsweight <- c(Malecountsweights,Femalecountsweights,Uncountsweights)


####Plotting Code####

#plots weight against wingspan
plot(blackbird_data$weight,blackbird_data$wing,main="Weight vs Wingspan",xlab="Weight",ylab="Wingspan")

#plots barchart of number of the sexes of birds of different ages to see how excluding unidentified birds would impact analysis 
barplot(plottingtable,beside=T,legend = rownames(plottingtable),xlab="Age of Bird",ylab="Number",main="Number of Birds of Each Sex For Each Age")

#plots bar chart of number of birds of each sex
barplot(sexcounts,beside=T,xlab="Sex ofBird",ylab="Number",names.arg=c("Male","Female","Unidentified"),main="Number of Birds of Each Sex",ylim=c(0,1200))
barplot(sexcountsweight,beside=T,xlab="Sex of Bird",ylab="Number",names.arg=c("Male","Female","Unidentified"),main="Number of Birds of Each Sex With N/A weights Omitted",ylim=c(0,1200))




#weight plots
par(mfrow=c(1,1))
#plots histogram of weights with normal distribution overlayed to see if normal assumption of GLMs is accurate
h <- hist(blackbird_data_no_NA_weights$weight,main="Histogram Of Weights",xlab="Weights")
xfit<-seq(min(blackbird_data_no_NA_weights$weight),max(blackbird_data_no_NA_weights$weight),length=40)
yfit<-dnorm(xfit,mean=mean(blackbird_data_no_NA_weights$weight),sd=sd(blackbird_data_no_NA_weights$weight))
yfit <- yfit*diff(h$mids[1:2])*length(blackbird_data$weight)
lines(xfit, yfit, col="blue", lwd=2)

#plots boxplots of weight vs various variables
boxplot(weight~age,data=blackbird_data, main="Weight By Age",xlab="Age", ylab="Weight")
boxplot(weight~sex,data=blackbird_data, main="Weight By Sex",xlab="Sex", ylab="Weight")
boxplot(weight~year,data=blackbird_data, main="Weight By Year",xlab="Year", ylab="Weight")
boxplot(weight~month,data=blackbird_data, main="Weight By Month",xlab="Month", ylab="Weight")



#wingspan plots

#plots boxplots of wingspan vs sex
boxplot(wing~sex,data=blackbird_data, main="Wing Span By Sex",xlab="Sex", ylab="Wing Span")

par(mfrow=c(1,2))

#plots histogram of wingspans with normal distribution overlayed to see if normal assumption is accurate
h2 <- hist(blackbird_data_male_data$wing,xlab="Wingspan",main="Histogram of Wingspans of Male Birds")
xfit2<-seq(min(blackbird_data_male_data$wing),max(blackbird_data_male_data$wing),length=40)
yfit2<-dnorm(xfit2,mean=mean(blackbird_data_male_data$wing),sd=sd(blackbird_data_male_data$wing))
yfit2 <- yfit2*diff(h2$mids[1:2])*length(blackbird_data_male_data$wing)
lines(xfit2, yfit2, col="blue", lwd=2)

#plots histogram of weights with normal distribution overlayed to see if normal assumption is accurate
h2 <- hist(blackbird_data_female_data$wing,xlab="Wingspan",main="Histogram of Wingspans of Female Birds")
xfit2<-seq(min(blackbird_data_female_data$wing),max(blackbird_data_female_data$wing),length=40)
yfit2<-dnorm(xfit2,mean=mean(blackbird_data_female_data$wing),sd=sd(blackbird_data_female_data$wing))
yfit2 <- yfit2*diff(h2$mids[1:2])*length(blackbird_data_female_data$wing)
lines(xfit2, yfit2, col="blue", lwd=2)



####2 sample randomization test code####

##Function that conducts a randomization test##
#INPUTS:2 lists of integers
#output result of test and pvalue of hypothesis test that means of lists of intergers are the same 
randomizationtestfunction <- function(samp1,samp2,noofrand=1000){
  alldata <- c(samp1,samp2)
  nsamp1 <- length(samp2)
  nsamp2 <- length(samp1)
  nall <- length(alldata)
  #stores difference in means of unrandomised samples
  teststatorig <- mean(samp1)-mean(samp2)
  teststats <- rep(0,noofrand+1)
  #randomly sorts elements of samples into 2 groups of the same size as original 2 samples and computes the mean of these groups
  for (i in 1:noofrand){
    randreorders <- sample(alldata,nall,replace=F)
    randteststat <- mean(randreorders[1:nsamp1])-mean(randreorders[(nsamp1+1):nall])
    teststats[i] <- randteststat
  }
  #stores original test stat as one of the test stats
  teststats[noofrand+1] <- teststatorig
  #calculates pvalue by calculating fraction of test stats that are bigger than original test stat
  pvalue <- length(teststats[teststats>=teststatorig])/length(teststats)
  #adjusts if in different tail of distribution
  if (pvalue>0.5){
    pvalue <- length(teststats[teststats<=teststatorig])/length(teststats)
  }
  pvalue <- 2*pvalue
  if (pvalue <= 0.05){
    return(paste("Reject H0 that means are the same. P-value is",pvalue))
  }
  if(pvalue > 0.05){
    return(paste("Accept H0 that means are the same, P-value is",pvalue))
  }
}


randomizationtestfunction(blackbird_data_male_data$wing,blackbird_data_female_data$wing)

par(mfrow=c(1,1))

#gives number of birds used in analysis
length(blackbird_data_female_data$weight)+length(blackbird_data_male_data$weight)

#gives difference in mean wing length between the male and female blackbirds
mean(blackbird_data_male_data$wing)-mean(blackbird_data_female_data$wing)

#gives mean and standard deviation of winglengths of male and female birds
mean(blackbird_data_male_data$wing)
sd(blackbird_data_male_data$wing)
mean(blackbird_data_female_data$wing)
sd(blackbird_data_female_data$wing)















####GLM Fitting####
par(mfrow=c(1,1))

#gives number of birds used in analysis
length(blackbird_data_no_NA_weights$weight)


#weight as function of variables gamma glms

#gives log of weight as linear function of all variables 
weight_all_variables <- glm(weight~age+year+month+sex+weight,data=blackbird_data_no_NA_weights,family = "Gamma")

#gives log of weight as function of different variables
weight_as_age_sex <- glm(weight~sex+age,data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_month <- glm(weight~month,data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_year <- glm(weight~year,data=blackbird_data_no_NA_weights,family = "Gamma") 

#gives AIC of these models
AIC(weight_all_variables)
AIC(weight_as_age_sex)
AIC(weight_as_month)
AIC(weight_as_year)

#gives log of weight as function of different order polynomials of the month of capture
weight_as_month_parabolic <- glm(weight~poly(month, 2),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_month_3rd_order <- glm(weight~poly(month, 3),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_month_4th_order <- glm(weight~poly(month, 4),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_month_5th_order <- glm(weight~poly(month, 5),data=blackbird_data_no_NA_weights,family = "Gamma")

#gives AIC of these models
AIC(weight_as_month_parabolic)
AIC(weight_as_month_3rd_order)
AIC(weight_as_month_4th_order)
AIC(weight_as_month_5th_order)
    
#gives log of weight as function of different order polynomials of the year of capture 
weight_as_year_parabolic <- glm(weight~poly(year, 2),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_year_3rd_order <- glm(weight~poly(year, 3),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_year_4th_order <- glm(weight~poly(year, 4),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_year_5th_order <- glm(weight~poly(year, 5),data=blackbird_data_no_NA_weights,family = "Gamma")
weight_as_year_6th_order <- glm(weight~poly(year, 6),data=blackbird_data_no_NA_weights,family = "Gamma")

#gives AIC of these models
AIC(weight_as_year_parabolic)
AIC(weight_as_year_3rd_order)
AIC(weight_as_year_4th_order)
AIC(weight_as_year_5th_order)
AIC(weight_as_year_6th_order)

#gives log of weight as function of best AIC polynomials of year and month along with discrete variables
weight_all_variables_complex_relationships <- glm(weight~age+poly(year,5)+poly(month, 3)+sex,data=blackbird_data_no_NA_weights,family = "Gamma")
#gives summary to see which variables to remove
summary(weight_all_variables_complex_relationships)

#gives log of weight as function of  polynomials of year and month along with discrete variables with insignificant terms from last model removed
weight_all_variables_simplified_final_model <- glm(weight~age+poly(year,2)+poly(month, 2)+sex,data=blackbird_data_no_NA_weights,family = "Gamma")

#gives AIC of these models
AIC(weight_all_variables_complex_relationships)
AIC(weight_all_variables_simplified_final_model)

#plots graphs to show suitability of fit of model
plot(weight_all_variables_simplified_final_model)

summary(weight_all_variables_simplified_final_model)

#plots models of log of weight as month of capture over boxplot of weight by month 
plot(blackbird_data_no_NA_weights$month,blackbird_data_no_NA_weights$weight,xlab="Month",ylab="Weight",main="Month vs Weight With GLM Overlayed")
lines(blackbird_data_no_NA_weights[order(blackbird_data_no_NA_weights$month),]$month,predict(weight_as_month_3rd_order,type="response")[order(blackbird_data_no_NA_weights$month)],type="l",col="red")

#plots models of log of weight as month of capture over plot of weight by month 
plot(blackbird_data_no_NA_weights$year,blackbird_data_no_NA_weights$weight,xlab="Year",ylab="Weight",main="Year vs Weight With GLM Overlayed")
lines(blackbird_data_no_NA_weights[order(blackbird_data_no_NA_weights$year),]$year,predict(weight_as_year_5th_order,type="response")[order(blackbird_data_no_NA_weights$year)],col="red")
