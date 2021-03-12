## Saving output in a txt file 
#-------------------------------
sink("C:/Users/krist/Desktop/World Happiness Project/FINAL/4 - DATA ANALYSIS_WorldHappinessProject_output.txt", append = TRUE)
#-------------------------------

#World Happiness Final Project

#Load libraries
library("car")
library("rcompanion")
library(c("tidyr", "devtools"))
library("gmodels")
library("dplyr")
library("tidyr")
library("gmodels")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("ggplot2")
library("PerformanceAnalytics")
library("corrplot")
library("readxl")
library("writexl")

##----------------------------------------------
##### Evaluation question: What country development indicators influence happiness? ##### 
#Multiple Linear Regression
##----------------------------------------------

#WHO & Happiness Dataset

#import file
HealthHappiness = read_excel("C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Data Wrangling/HealthHappiness_File.xlsx",sheet = "Sheet1") 
head(HealthHappiness)
View(HealthHappiness)
unique(HealthHappiness$Country)

summary(HealthHappiness$`Happiness Score`)

## Assumptions for Multiple Linear Regression
#1 There is a linear relationship between x and y.
#2 Homoscedasticity: The error term is normally distributed.
#3 Homogenity of variance: the variance of the error terms is constant for all values of x.
#4 The x's are fixed and measured without error. (In other words, the x's can be considered as known constants.)
#5 Multicollinearity: the observations are independent.
#6 Lack of outliers

#Subset to Both sexes only and specific columns
HealthHappiness2 <- HealthHappiness[,c('Dim1', 'Location', 'Region', 'Country', 'Happiness Score', 'InfantMortality', 'LifeExpectancy', 'MortalityRatePoisoning', 'SuicideRates', 'AlcoholSubstanceAbuse', 'Tobacco', 'DiseaseDeath')]

head(HealthHappiness2)

HealthHappiness3 <- filter(HealthHappiness2, Dim1 == "Both sexes")
head(HealthHappiness3)
View(HealthHappiness3)

HealthHappiness3$HappinessScore <- HealthHappiness3$'Happiness Score'

## Testing Assumptions ##

# 1 Linear Relationship
#Create scatter plots
scatter.smooth(x=HealthHappiness3$LifeExpectancy, y=HealthHappiness3$HappinessScore, main="Happiness Score by Life Expectancy")
#somewhat linear
scatter.smooth(x=HealthHappiness3$MortalityRatePoisoning, y=HealthHappiness3$HappinessScore, main="Happiness Score by Mortality Rate Poisoning")
#logarithmic relationship
scatter.smooth(x=HealthHappiness3$SuicideRates, y=HealthHappiness3$HappinessScore, main="Happiness Score by Suicide Rates")
#somewhat linear
scatter.smooth(x=HealthHappiness3$AlcoholSubstanceAbuse, y=HealthHappiness3$HappinessScore, main="Happiness Score by Alcohol Substance Abuse")
#not linear
scatter.smooth(x=HealthHappiness3$Tobacco, y=HealthHappiness3$HappinessScore, main="Happiness Score by Tobacco Use")
#not linear
scatter.smooth(x=HealthHappiness3$DiseaseDeath, y=HealthHappiness3$HappinessScore, main="Happiness Score by Chronic Disease Death")
#not linear 



#---------------------------------------

# World Bank and Happiness Data

#import file
WorldBank_V2 = read_excel("C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Possible Data Sets/WorldBank_Happiness_Region.xlsx",sheet = "Sheet2") 
View(WorldBank_V2)

#-------------------------------
### 1 Checking Linearity 

# Create scatter plots
scatter.smooth(x=WorldBank_V2$AdolescentSchool, y=WorldBank_V2$HappinessScore, main="Happiness Score by AdolescentSchool")
#somewhat linear - Exclude
scatter.smooth(x=WorldBank_V2$ArmedForces, y=WorldBank_V2$HappinessScore, main="Happiness Score by ArmedForces")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$ArmedForces2, y=WorldBank_V2$HappinessScore, main="Happiness Score by ArmedForces2")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$BasicDrinkWater, y=WorldBank_V2$HappinessScore, main="Happiness Score by BasicDrinkWater")
#somewhat linear 1 - Include
scatter.smooth(x=WorldBank_V2$BasicSanitization, y=WorldBank_V2$HappinessScore, main="Happiness Score by BasicSanitization")
#somewhat linear 2 - Include
scatter.smooth(x=WorldBank_V2$BussinessScore, y=WorldBank_V2$HappinessScore, main="Happiness Score by BussinessScore")
#somewhat linear 3 - Include
scatter.smooth(x=WorldBank_V2$Cellular, y=WorldBank_V2$HappinessScore, main="Happiness Score by Cellular")
#somewhat linear 4 - Include
scatter.smooth(x=WorldBank_V2$Electricity, y=WorldBank_V2$HappinessScore, main="Happiness Score by Electricity")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$FertilityRate, y=WorldBank_V2$HappinessScore, main="Happiness Score by FertilityRate")
#somewhat linear 5 - Include
scatter.smooth(x=WorldBank_V2$FoodInsecurity, y=WorldBank_V2$HappinessScore, main="Happiness Score by FoodInsecurity")
#somewhat linear 6 - Include
scatter.smooth(x=WorldBank_V2$GDPGrowth, y=WorldBank_V2$HappinessScore, main="Happiness Score by GDPGrowth")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$HealthExpGDP, y=WorldBank_V2$HappinessScore, main="Happiness Score by HealthExpGDP")
#somewhat linear 7 - Include
scatter.smooth(x=WorldBank_V2$HealthExpGov, y=WorldBank_V2$HappinessScore, main="Happiness Score by HealthExpGov")
#somewhat linear 8 - Include
scatter.smooth(x=WorldBank_V2$LegalRightsIndex, y=WorldBank_V2$HappinessScore, main="Happiness Score by LegalRightsIndex")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$Manufactoring, y=WorldBank_V2$HappinessScore, main="Happiness Score by Manufactoring")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$NationalIncome, y=WorldBank_V2$HappinessScore, main="Happiness Score by NationalIncome")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$Pop1564, y=WorldBank_V2$HappinessScore, main="Happiness Score by Pop1564")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$Pop65, y=WorldBank_V2$HappinessScore, main="Happiness Score by Pop65")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$PopFem, y=WorldBank_V2$HappinessScore, main="Happiness Score by PopFem")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$PopMale, y=WorldBank_V2$HappinessScore, main="Happiness Score by PopMale")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$PrimarySchool, y=WorldBank_V2$HappinessScore, main="Happiness Score by PrimarySchool")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$RenewableEnergy, y=WorldBank_V2$HappinessScore, main="Happiness Score by RenewableEnergy")
#somewhat linear 9 - Include
scatter.smooth(x=WorldBank_V2$RuralPop, y=WorldBank_V2$HappinessScore, main="Happiness Score by RuralPop")
#somewhat linear 10 - Include
scatter.smooth(x=WorldBank_V2$SecEduc, y=WorldBank_V2$HappinessScore, main="Happiness Score by SecEduc")
#somewhat linear 11 - Include
scatter.smooth(x=WorldBank_V2$SevereFoodInsecurity, y=WorldBank_V2$HappinessScore, main="Happiness Score by SevereFoodInsecurity")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$TradeGDP, y=WorldBank_V2$HappinessScore, main="Happiness Score by TradeGDP")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$TradeinServices, y=WorldBank_V2$HappinessScore, main="Happiness Score by TradeinServices")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$Undernourishment, y=WorldBank_V2$HappinessScore, main="Happiness Score by Undernourishment")
#somewhat linear 12 - Include
scatter.smooth(x=WorldBank_V2$Unemployment, y=WorldBank_V2$HappinessScore, main="Happiness Score by Unemployment")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$UrbanGrowth, y=WorldBank_V2$HappinessScore, main="Happiness Score by UrbanGrowth")
#not linear - Exclude
scatter.smooth(x=WorldBank_V2$UrbanPop, y=WorldBank_V2$HappinessScore, main="Happiness Score by UrbanPop")
#somewhat linear 13 - Include

#The following 13 variables are linear:
#BasicSanitization, BussinessScore, Cellular, FertilityRate, FoodInsecurity, HealthExpGDP, HealthExpGov, RenewableEnergy, RuralPop, SecEduc, UrbanPop, Undernourishment

#-------------------------------
### 2 Testing for homoscedasity

#Test for entire model
model <- lm(HappinessScore ~  BasicSanitization +FertilityRate + BussinessScore +Cellular + FoodInsecurity + HealthExpGDP + HealthExpGov + RenewableEnergy + RuralPop + SecEduc + BasicDrinkWater + Undernourishment, data = WorldBank_V2)
lmtest::bptest(model)
#passes

#Testing the individual variables 

##BasicSanitization (1)**
lmMod1 <- lm(HappinessScore~BasicSanitization, data=WorldBank_V2)
lmtest::bptest(lmMod1)
#does not pass

#correct for homescedasity
WorldBank3 <- na.omit(WorldBank_V2)

lmMod_edit <- caret::BoxCoxTrans(WorldBank3$HappinessScore)
print(lmMod_edit)

View(WorldBank3)

WorldBank3 <- cbind(WorldBank3, dist_newM=predict(lmMod_edit, WorldBank3$HappinessScore))
lmMod_edit <- lm(dist_newM~BasicSanitization, data=WorldBank3)
lmtest::bptest(lmMod_edit)
#now passes

#BussinessScore (2)
lmMod2 <- lm(HappinessScore~BussinessScore, data=WorldBank_V2)
lmtest::bptest(lmMod2)
#passes

#Cellular (3)
lmMod3 <- lm(HappinessScore~Cellular, data=WorldBank_V2)
lmtest::bptest(lmMod3)
#passes

#FertilityRate (4)**
lmMod4 <- lm(HappinessScore~FertilityRate, data=WorldBank_V2)
lmtest::bptest(lmMod4)
#does not pass

lmMod_edit2 <- lm(dist_newM~FertilityRate, data=WorldBank3)
lmtest::bptest(lmMod_edit2)
#now passes

#FoodInsecurity (5)
lmMod5 <- lm(HappinessScore~FoodInsecurity, data=WorldBank_V2)
lmtest::bptest(lmMod5)
#passes

#HealthExpGDP (6)
lmMod6 <- lm(HappinessScore~HealthExpGDP, data=WorldBank_V2)
lmtest::bptest(lmMod6)
#passes

#HealthExpGov (7)
lmMod7 <- lm(HappinessScore~HealthExpGov, data=WorldBank_V2)
lmtest::bptest(lmMod7)
#passes

#RenewableEnergy (8)
lmMod8 <- lm(HappinessScore~RenewableEnergy, data=WorldBank_V2)
lmtest::bptest(lmMod8)
#passes

#RuralPop (9)**
lmMod9 <- lm(HappinessScore~RuralPop, data=WorldBank_V2)
lmtest::bptest(lmMod9)
#does not pass

lmMod_edit9 <- lm(dist_newM~RuralPop, data=WorldBank3)
lmtest::bptest(lmMod_edit9)
#now passes

#SecEduc (10)
lmMod10 <- lm(HappinessScore~SecEduc, data=WorldBank_V2)
lmtest::bptest(lmMod10)
#passes

#UrbanPop (11) -- Skip - this is same as Rural Pop variable
lmMod11 <- lm(HappinessScore~UrbanPop, data=WorldBank_V2)
lmtest::bptest(lmMod11)

#BasicDrinkWater (12)
lmMod12 <- lm(HappinessScore~BasicDrinkWater, data=WorldBank_V2)
lmtest::bptest(lmMod12)
#passes

#Undernourishment (13)
lmMod13 <- lm(HappinessScore~Undernourshiment, data=WorldBank)
lmtest::bptest(lmMod13)
#passes

#-------------------------------
### 3 Testing for homogeneity

#Test entire model
gvlma(model)
#all assumptions are acceptable

#Testing each individual variable
gvlma(lmMod_edit)
gvlma(lmMod2)
gvlma(lmMod3)
gvlma(lmMod4)
gvlma(lmMod_edit2)
gvlma(lmMod5)
#FoodInsecurity
#Global Stat, Kurtosis, and Link Function is violated.
gvlma(lmMod6)
#HealthExpGDP
#Link function violated.
gvlma(lmMod7)
#HealthExpGov
#Link function violated.
gvlma(lmMod8)
#RenewableEnergy
#Skewness violated
gvlma(lmMod9)
gvlma(lmMod10)
gvlma(lmMod11)
gvlma(lmMod12)
#Basic Drinking Water
#Global Stat, Kurtosis, and Link Function is violated.
gvlma(lmMod13)
#Undernourishment
#Global Stat, Kurtosis, and Link Function is violated.

#-------------------------------
### 4 Testing the X as a Known Constant
# assumed

#-------------------------------
### 5 Testing multicollinearity

WorldBank_quant1 <- WorldBank_V2[, c(4,8,9,10,11,13,14,16,17,23,24,25,26,27,28,32,35)]

chart.Correlation(WorldBank_quant1, histogram=FALSE, method="pearson")

corr_matrix <- cor(WorldBank_quant1)
corr_matrix
#all good except possible PopFem and PopMale variables. UrbanPop and RuralPop collinear

#-------------------------------
### 6 Testing for Outliers
model_test1 <- lm(HappinessScore ~  BasicSanitization +FertilityRate + BussinessScore +Cellular + FoodInsecurity + HealthExpGDP + HealthExpGov + RenewableEnergy + RuralPop + SecEduc + BasicDrinkWater + Undernourishment, data = WorldBank_V2)

CookD(model_test1, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
#outliers on rows 36, 72, 88

lev = hat(model.matrix(model_test1))
plot(lev)

WorldBank_V2[lev>.2,]
#need to run with out missing data

WorldBank3[lev>.2,]

car::outlierTest(model_test1)

summary(influence.measures(model_test1))

#remove outliers

WorldBank_V3 = WorldBank_V2[-c(36,72,88),]

model_test2 <- lm(HappinessScore ~  BasicSanitization +FertilityRate + BussinessScore +Cellular + FoodInsecurity + HealthExpGDP + HealthExpGov + RenewableEnergy + RuralPop + SecEduc + BasicDrinkWater + Undernourishment, data = WorldBank_V3)

summary(influence.measures(model_test2))
#need to remove outlier row 168

#remove one more outlier
WorldBank_V4 = WorldBank_V3[-c(168),]

model_test3 <- lm(HappinessScore ~  BasicSanitization +FertilityRate + BussinessScore +Cellular + FoodInsecurity + HealthExpGDP + HealthExpGov + RenewableEnergy + RuralPop + SecEduc + BasicDrinkWater + Undernourishment, data = WorldBank_V4)

#Re-test for outliers
summary(influence.measures(model_test3))
#looks good now, no outliers

#-------------------------------
## Running the multiple linear regression test

summary(model_test3)
#5 significant variables: 
##  FertilityRate*
##  FoodInsecurity**
##  HealthExpGov***
##  RuralPop** 
##  Undernourishment***


##----------------------------------------------
###ANOVA Test###
## Evaluation Question: Does happiness score differ significantly by Region?
##----------------------------------------------

#Import file
Happiness = read_excel("C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Possible Data Sets/World Happiness Report w 2020//2015.xlsx",sheet = "2015") 
View(Happiness)
head(Happiness)

## subset 
Happiness2 <- Happiness[,c('Region', 'Country', 'Happiness Score')]

Happiness2$HappinessScore <- Happiness2$'Happiness Score'

## Assumptions
#1 Normality
#2 Homogeneity of variance
#3 Sample size and Independence

#-------------------------------
# Testing Normality
plotNormalHistogram(Happiness2$HappinessScore)
# normal

#-------------------------------
# Testing homogeneity of variance
Happiness2$HappinessScore <- as.numeric(Happiness2$HappinessScore)

bartlett.test(HappinessScore ~ Region, data=Happiness2)
#assumption violated

#-------------------------------
# sample size & independence is met

#-------------------------------
# Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

ANOVA <- lm(HappinessScore ~ Region, data=Happiness2)
Anova(ANOVA, Type="II", white.adjust=TRUE)

pairwise.t.test(Happiness2$HappinessScore, Happiness2$Region, p.adjust="bonferroni", pool.sd = FALSE)
#significant difference between Australia and New Zealand & all other groups except Western Europe
#significant difference between Central and Easter Europe & all other group except 
#-- will try grouping regions so there are less regions

HappinessMeans <- Happiness2 %>% group_by(Region) %>% summarize(Mean = mean(HappinessScore))
View(HappinessMeans)

## Group Asian, European, African regions

Happiness3 <-Happiness2

Happiness3$Region2 <-ifelse(Happiness3$Region=='Southern Asia', 'Asia', ifelse(Happiness3$Region=='Sub-Saharan Africa', 'Africa', ifelse(Happiness3$Region=='Southeastern Asia', 'Asia', ifelse(Happiness3$Region=='Eastern Asia', 'Asia', ifelse(Happiness3$Region=='Middle East and Northern Africa', 'Africa', ifelse(Happiness3$Region=='Central and Eastern Europe', 'Europe', ifelse(Happiness3$Region=='Western Europe', 'Europe', ifelse(Happiness3$Region=='Latin America and Caribbean','Latin America and Caribbean', ifelse(Happiness3$Region=='North America', 'North America', ifelse(Happiness3$Region=='Australia and New Zealand','Australia and New Zealand', NA))))))))))

View(Happiness3)

#-------------------------------
## Tesing assumptions

# normality
plotNormalHistogram((Happiness3$HappinessScore), main="Histogram of Happiness Scores")
#somewhat normal

#-------------------------------
# Testing for homogeneity of variance

bartlett.test(HappinessScore ~ Region2, data=Happiness3)
#assumption violated

#-------------------------------
# sample size & independence is met

#-------------------------------
# Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

ANOVA2 <- lm(HappinessScore ~ Region2, data=Happiness3)
Anova(ANOVA2, Type="II", white.adjust=TRUE)

pairwise.t.test(Happiness3$HappinessScore, Happiness3$Region2, p.adjust="bonferroni", pool.sd = FALSE)
#Africa is significantly different than all other regions except Asia 
#Asia is significantly different than all other regions
#Australia and New Zealand is significantly different than all other regions except North America
#Europe is significantly different than all other regions except Latin American and Caribbean & North America

HappinessMeans2 <- Happiness3 %>% group_by(Region2) %>% summarize(Mean = mean(HappinessScore))
View(HappinessMeans2)

#-------------------------------
### Box plots
#------------------------------

#Distribution of Happiness Score in 2015
boxplot(Happiness2$HappinessScore,
        main = "Distribution of Happiness Score in 2015",
        xlab = "Happiness Score",
        col = "orange",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE
)

#Distribution of Happiness Scores by Region in 2015
boxplot(HappinessScore~Region,
        data=Happiness2,
        main="Distribution of Happiness Scores by Region in 2015",
        xlab="Region",
        ylab="Happiness Score",
        col="orange",
        border="blue"
)

#-------------------------------
### How does happiness change over time for different regions?
#-------------------------------

#import file
HappinessAll_File3 = read_excel("C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Data Wrangling/HappinessAll_File3.xlsx",sheet = "Sheet4") 
head(HealthHappiness)

#Distribution of Happiness Scores by Year - 2015-2020
boxplot(HappinessScore~Year,
        data=HappinessAll_File3,
        main="Distribution of Happiness Scores by Year",
        xlab="Year",
        ylab="Happiness Score",
        col="orange",
        border="blue"
)

#Distribution of Happiness Scores by Region 2015-2020
boxplot(HappinessScore~Region,
        data=HappinessAll_File3,
        main="Distribution of Happiness Scores by Region 2015-2020",
        xlab="Region",
        ylab="Happiness Score",
        col="orange",
        border="blue",
        geom_boxplot(fatten = 2) 
)

#Happiness Score 2015-2020
box <- ggplot(data=HappinessAll_File3, aes(x=HappinessScore, y=Region))
box + theme_grey(base_size = 18)+ geom_boxplot(aes(fill=HappinessScore, color=Region)) +
        ylab("Region") + ggtitle("2015-2020 Happiness Score Boxplot") 
stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
coord_flip()

#subset for 2015
Happiness_2015  <- filter(HappinessAll_File3, Year == "2015")

#2015 Happiness Score Boxplot by Region
box <- ggplot(data=Happiness_2015, aes(x=HappinessScore, y=Region))
box + theme_grey(base_size = 18)+ geom_boxplot(aes(fill=HappinessScore, color=Region)) +
        ylab("Region") + ggtitle("2015 Happiness Score Boxplot by Region") 
stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
coord_flip()

#-------------------------------
##Exporting to excel

write_xlsx(WorldBank_V3, "C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Possible Data Sets//WorldBank_Happiness.xlsx")
#-------------------------------

##End
