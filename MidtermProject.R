library(dplyr)
library(ggplot2)
library(tidyr)

hospital=read.csv("hospitals.csv")

#How big is the data set?
dim(hospital)

#what are the names are of the columns
names(hospital)

#which hospital has the lowest number of beds
hospital %>% filter(Beds == min(Beds))

##which hospital has the lowest expense
hospital %>% filter(Total.Expense == min(Total.Expense))

#How many hospitals deliver babies?
hospital %>% filter(Births>0)%>%nrow()

#Using ggplot, scatterplot number of beds vs Total Expense
ggplot(hospital, aes(x=Beds, y= Total.Expense)) +
  geom_point()

#Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospital, aes(x=Admissions, y= Total.Expense)) +
  geom_point()
#Using dplyr and ggplot, scatterplot beds vs Total Expense
#but only for hospitals that deliver babies

deliverbabies <- hospital %>% filter(Births > 0)

ggplot(deliverbabies, aes(x = Beds, y = Total.Expense)) +
  geom_point()

#What is the distribution of total expenses for hospitals that deliver babies? 
ggplot(deliverbabies, aes(x = Total.Expense)) +
  geom_histogram()

#Pie Chart
totaladmissions <- sum(deliverbabies$Admissions)
totalbirths <- sum(deliverbabies$Births)

admission.births<-data.frame(Category = c("Admissions", "Births"),
                           Value = c(totaladmissions, totalbirths))
ggplot(admission.births, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y")

#Bar Chart
ggplot(deliverbabies, aes(x = factor(1), y = Admissions)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_bar(aes(x = factor(2), y = Total.Expense), stat = "identity", fill = "blue") +
  scale_x_discrete(labels = c("Admissions", "Total Expense"))

#Perform one simple regression (one predictor) to provide recommendations
library(stats)

hospitals = read.csv("hospitals.csv")
colnames(hospitals)
test = hospitals[hospitals$State ==2 ,c("Total.Expense", "Beds", "Admissions") ]


ggplot(hospitals, aes(x=Beds, y = Total.Expense)) +
  geom_point()



g1=lm(Total.Expense ~ Beds, data = hospital)
summary(g1)
anova(g1)




# Interpretation
# The simple regression answered whether there was a relation between beds and total.expenses
# only one p-value was given
# According to the result there is not a correlection between total.expense and beds, 
# due to the p-value being larger than 0.05. Meaning there is probably other factors involved in the total expense.


#multivariate regression
g2=lm(Total.Expense~Beds+Admissions, test)
summary(g2)
anova(g2)

reg0=lm(Total.Expense~Beds, test)
reg1.5=lm(Total.Expense~Beds + Admissions, test)
anova(reg1.5, reg0)



# Conclusion: 
#Opening a small facility of 90–100 beds would fall within the range of the analyzed data, supporting the feasibility of this option.
#The analysis supports the feasibility of opening a small facility within the 90–100 bed range as it is likely to have manageable expenses according to the data.
#Based on the regression analysis, opening a small facility aligns with the findings that the number of beds is a significant predictor of total expenses.
