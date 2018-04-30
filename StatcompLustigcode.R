##___Joshua Winter________Statistical Computing Final Project___________Lustig Analysis and Recreation_____________________________________

## sweep plots after each set

##__________________________________________________________________________________________________________________________________________
##______________________________________Average Gini 1980-2008_______________________________________________________________________________

## represents the average gini coefficent for all Latin American countries from 1980 to 2008. The years selected were those that had accurate data from the most countries. 

YearAverageGini<-c(1980,1986,1992,1998,2002,2008)
AverageGiniCoefficient<-c(.51,.514,.519,.531,.533,.51)

## dataframe creation 

AverageInequalitybyYear<-data.frame(YearAverageGini, AverageGiniCoefficient)
View(AverageInequalitybyYear)

## plot

plot.default(YearAverageGini,AverageGiniCoefficient, type = "b", ylab = "Average Inequality Level (Gini Coefficient)", xlab="Year", main = "Average Level of Inequality in Latin America")

##_________________________________________________________________________________________________________________________________________
##________________________________________% of People in LA living on $2.5 and $4 per day___________________________________________________

## Yearmonperday represents the years from 1992-2010, twopointfiveperday is the percent of people in Latin America living on 2.5 dollars per day or less, four per day is the percentage of people in LA living on $4 or less per day, both are expressed as decimals. 

Yearmonperday<-c(1992, 1998, 2000, 2010)
twopointfiveperday<-c(.278, .247, .249, .163)
fourperday<-c(0.444, 0.407, 0.415,0.296)



layout(matrix(c(1,2),2,1)) # 2 plots per page


##plots

plot(Yearmonperday, twopointfiveperday, xlab="Year", ylab = "Percent", main = "Percent of People in LA Living on $2.5 per Day", col="blue", type = "b")

plot(Yearmonperday, fourperday, xlab="Year", ylab = "Percent", main = "Percent of People in LA Living on $4 per Day", col="red", type = "b")



##______________________________________________________________________________________________________________________________________________________
##___________________________________________________Chart Layout for Regressions_______________________________________________________________________


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 

##______________________________________________________________________________________________________________________________________________________
##______________________________________Education by Tier and Inequality 2000-2010______________________________________________________________________

## Y= Average annual percent change gini coefficient , X=average percent change in return to education (all levels), 
## All of the following are average annual: P=percent change in return to primary education, S= percent change in return to secondary eduction, T=percent change in return to tertiary education

Countries<-c("Argentina","Bolivia","Brazil","Chile",	"Costa Rica",	"Dominican Republic",	"Ecuador"	,"El Salvador",	"Guatemala",	"Mexico",	"Nicaragua",	"Panama",	"Paraguay",	"Peru", "Uruguay", "Venezuela")
y=c(-1.3,-2.05, -1.03, -.72,-.47, -.79,-1.99,-1.24,-.1,-1.17,-2.64,-.74,-.39,-.91,-.2,-1.07)
x=c(-1.8, -1.6, -3.533, -2.33, 0, -.7, -2.067, -.43, 1.533, -1.933, -1.8, 0, -3.67, -2.33, .867, -2.03)
P=c(1.3, 2, -4.9, -3.5, -3.7, .8, -1, -2.2, 2, -4.3, -3, 2, -7.5, -6, 1.8, -3.3)
S=c(-3.0, -4, -3.5,-3.5, 2.2, -.8, -.2, -1.9, .5, 1, -.7, -.3, -2.5, 1.3, 2, -3.5)
T=c(-3.7, -2.8, -2.2, 0, 1.5, -2.1, -5, 2.8, 2.1, -2.5, -1.7, -1.7, -1, -2.3, -1.2, .7)

## histograms

hist(y, main="Gini Coefficient", xlab="Average % Change")
hist(P, main = "Primary Education", xlab="Average % Change")
hist(S, main="Secondary Education", xlab="Average % Change")
hist(T, main="Tertiary Education", xlab="Average % Change")

layout(matrix(c(1,2),2,1))

hist(y, main="Gini Coefficient", xlab="Average % Change")
hist(x, main="Aggregate of Education", xlab="Average % Change")


##dataframe creation: Education by tier and inequality

EBTI<-data.frame(Countries,y,x,P,S,T)
list(EBTI)
summary(EBTI)

## regressions

layout(matrix(c(1,2,3,4),2,2))

summary(lm(formula = y ~ P))
plot(lm(formula = y ~ P))

summary(lm(formula = y ~ S))
plot(lm(formula = y ~ S))

summary(lm(formula = y ~ T))
plot(lm(formula = y ~ T))

summary(lm(formula = y ~ P+S))
plot(lm(formula = y ~ P+S))

summary(lm(formula = y ~ P+T))
plot(lm(formula = y ~ P+T))

summary(lm(formula = y ~ S+T))
plot(lm(formula = y ~ S+T))

summary(lm(formula = y ~ P+S+T))
plot(lm(formula = y ~ P+S+T))

summary(lm(formula = y ~ x))
plot(lm(formula = y ~ x))


## Plots

plot(P,y, xlab = "%Change Returns to Primary Ed", ylab="% Change in Average Inequality", main = "% Change in Inequality vs % Change Avg. Returns to Primary Ed")

plot(S,y, xlab = "%Change Returns to Secondary Ed", ylab="% Change in Average Inequality", main = "% Change in Inequality vs % Change Avg. Returns to Secondary Ed")

plot(T,y, xlab = "%Change Returns to Teriary Ed", ylab="% Change in Average Inequality", main = "% Change in Inequality vs % Change Avg. Returns to Tertiary Ed")

plot(x,y, xlab = "Avg. % Change Returns to Ed", ylab="% Change in Average Inequality", main = "% Change in Inequality vs % Change Avg. Returns to Ed")



##______________________________________________________________________________________________________________________________________________________________
##__________________________Supply and Demand of Workers________________________________________________________________________________________________________

## all are average annual percent change for 16 countries 2000-2010, WP represents the change in wage premium for increased (skill), total change is change is chnage in demand of unskilled workers + change in supply of unskilled workers)

CountriesSD<-c("Argentina","Bolivia","Brazil","Chile",	"Costa Rica",	"Dominican Republic",	"Ecuador"	,"El Salvador",	"Guatemala",	"Mexico",	"Nicaragua",	"Panama",	"Paraguay",	"Peru", "Uruguary", "Venezuela")
Changeinsupplyofunskilledworkers<-c(2.4,5.1,4.4,1.1,6,3.4,3.4,-0.03,2.3,2.2,6.6,2.4,6.1,3.8,1.1,4.2)
changeindemandofunskilledworkers<-c(-4.7,-8.7,-5.1,-4.7,0.1,2.8,-6.3,-0.5,-3.3,-6.3,-14.1,-4.4,-10.8,-4.6,-1.4,-10.3)
TotalChangeinsupplyanddemandunskilledworkers<-c(-2.3,-3.6,	-0.7,	-3.6,	6.1,	6.2,	-2.9,	-0.53,	-1,	-4.1,	-7.5,	-2,	-4.7,	-0.8,	-0.3,	-6.1)
WP<-c(-2.4,-4.6,-3.2,-1.9,-2,-0.2,-3.2,-0.1,-1.9,-2.8,-6.9,-2.6,-5.6,-2.8,-0.9,-4.8)

##histograms

hist(Changeinsupplyofunskilledworkers, main=" Change in Supply of Unskilled Workers", xlab="Average % Change")
hist(changeindemandofunskilledworkers, main="Change in Demand of Unskilled Workers", xlab="Average % Change")
hist(TotalChangeinsupplyanddemandunskilledworkers, main="Total Change S&D Unskilled Workers", xlab="Average % Change")
hist(WP,main="Change Wage Premium", xlab="Average % Change")

## creation of dataframes

##complete dataset

GWPSD<-data.frame(CountriesSD, y, WP,changeindemandofunskilledworkers, Changeinsupplyofunskilledworkers, TotalChangeinsupplyanddemandunskilledworkers)
list(GWPSD)
summary(GWPSD)

## Average annual change in gini by country: Simplified version of last dataframe that just shows inequality change by country

AACG<-data.frame(CountriesSD,y)
list(AACG)
summary(AACG)


## Plots

plot(WP, y, xlab ="% Change Wage Premium", ylab="% Change Inequality", main="% Change Wage Premium Unskilled Workers vs % Change Inequality" )

plot(Changeinsupplyofunskilledworkers, y, xlab ="% Change Supply Unskilled Workers", ylab="% Change Inequality", main="% Change Supply Unskilled Workers vs % Change Inequality" )

plot(changeindemandofunskilledworkers, y, xlab ="% Change Demand Unskilled Workers", ylab="% Change Inequality", main="% Change Demand Unskilled Workers vs % Change Inequality" )

plot(TotalChangeinsupplyanddemandunskilledworkers, y, xlab ="Total % Change S&D Unskilled Workers", ylab="% Change Inequality", main="Total % Change S&D Unskilled Workers vs % Change Inequality" )


## Regressions

summary(lm(formula = y ~ WP))
plot(lm(formula = y ~ WP))

summary(lm(formula = y ~ Changeinsupplyofunskilledworkers))
plot(lm(formula = y ~ Changeinsupplyofunskilledworkers))

summary(lm(formula = y ~ changeindemandofunskilledworkers))
plot(lm(formula = y ~ changeindemandofunskilledworkers))

summary(lm(formula = y ~ TotalChangeinsupplyanddemandunskilledworkers))
plot(lm(formula = y ~ TotalChangeinsupplyanddemandunskilledworkers))

summary(lm(formula = y ~ WP+Changeinsupplyofunskilledworkers))
plot(lm(formula = y ~ WP+Changeinsupplyofunskilledworkers))

summary(lm(formula = y ~ WP+changeindemandofunskilledworkers))
plot(lm(formula = y ~ WP+changeindemandofunskilledworkers))

summary(lm(formula = y ~ WP+TotalChangeinsupplyanddemandunskilledworkers))
plot(lm(formula = y ~ WP+TotalChangeinsupplyanddemandunskilledworkers))
