library(regclass)
library(lmtest)
library(orcutt)

#Read file
df<-read.csv("Life_expectancy.csv")

#Fill NA values
for(i in 4:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

#Consider all variables
df
fit1 = lm(Life.expectancy ~ Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+Hepatitis.B+Measles+
                                            BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling, data=df)

#Apply backward step to get best subset of variables
step(fit1)

#Tests on the initial model considering all variables
gqtest(fit1)
VIF(fit1)
shapiro.test(residuals(fit1))

#Apply log to a few variables as a remedy for non-normal residuals and multi-collinearity
fit4 = lm(formula = Life.expectancy ~ log(Adult.Mortality) +  
            Alcohol + (percentage.expenditure) + log(Hepatitis.B) + Measles + 
            BMI + under.five.deaths + Polio + log(Total.expenditure) + Diphtheria + 
            HIV.AIDS + log(GDP) + log(Population) + thinness..1.19.years + 
            Income.composition.of.resources + Schooling, data = df)

#Tests on the transformed variables
VIF(fit4)
dwtest(fit4, alternative="two.sided")

#We see that the model fails the dwtest, so, use the orcutt library for resolving autocorrelation among the variables
coch = cochrane.orcutt(fit4)

#Final model after the remedy is applied
#Apply gg-plots and other plot as well
fit_final = lm(coch)
gqtest(fit_final, alternative='two.sided')
par(mfrow=c(2,2))
plot(fitted(fit_final), residuals(fit_final))
plot(fitted(fit1), residuals(fit1))
hist(residuals(final_fitted))
summary(fit_final)
