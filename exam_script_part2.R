library(tidyverse)

raw_data <-read_csv(file="exam_data_f16.csv")

str(raw_data)

#view data as a table
View(raw_data)

#change any 999s etc. to missing variables in R which is N/A
raw_data <- read_csv(file="exam_data_f16.csv" ,na=c("","NA","-999" ,"-888"))

categorical_variables <- select(raw_data, age, education, gender)
categorical_variables$univ <- as.factor(categorical_variables$education)
levels(categorical_variables$univ) <- list("HS"=1,"Finished HS"=2, "Some college"=3, "college grad"=4, "graduate degree"=5)
categorical_variables$prog_year <- as.factor(categorical_variables$gender)
levels(categorical_variables$univ) <- list("Male"=1,"Female"=2)

#create new data set with single set of items (ex. scale for extroversion)
agree_items <- select (raw_data, A1, A2, A3, A4, A5)
conscientious_items <- select (raw_data, C1, C2, C3, C4, C5)
performance_items <- select (raw_data, JP1, JP2, JP3, JP4, JP5)

#check for out of value ranges (any responses outside of what range should be)
psych::describe(agree_items)
psych::describe(conscientious_items)
psych::describe(performance_items)

#make values outside of range into N/A
##check the range of the scale first!!
is_bad_value <- agree_items<1 | agree_items>6
agree_items[is_bad_value] <- NA

#to reverse key items
View(agree_items)
agree_items <- mutate(agree_items,A1=7-A1)
##notice it's different depending on what point scale it is!!
conscientious_items <- mutate(conscientious_items,C4=7-C4)
conscientious_items <- mutate(conscientious_items,C5=7-C5)
performance_items <- mutate(performance_items,JP1=7-JP1)
performance_items <- mutate(performance_items,JP2=7-JP2)

#create a single score for each participant
agreeableness <- psych::alpha(as.data.frame(agree_items) ,check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientious_items) ,check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(performance_items) ,check.keys=FALSE)$scores

#combine all columns into new data frame called analytic_data
analytic_data <- cbind(categorical_variables, agreeableness,conscientiousness,performance)
## to view the data frame
analytic_data

#Saving the data
##.RData
save(analytic_data,file="study1_analytic_data.RData")
##.CSV
write_csv(analytic_data,path="analytic_data_final.csv")
##.SAV
library(haven)
write_sav(analytic_data,path="study1_analytic_data.csv")

###Correlation Table###
analytic.data <- select(analytic_data, agreeableness, conscientiousness, performance, age)

#View correlations between multiple variables, converting data set to data frame
library(apaTables)
apa.cor.table(as.data.frame(analytic.data))
#Save table
apa.cor.table(analytic.data, filename ="Table1.doc")

psych::pairs.panels(as.data.frame(analytic.data),lm=TRUE)

##ALPHA##
alpha(analytic.data, keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)

##MULTIPLE REGRESSION##
my.regression <- lm(performance ~ conscientiousness, data=analytic.data)
summary(my.regression)
apa.reg.table(my.regression)

my.regression.agree <- lm(performance ~ conscientiousness + agreeableness, data=analytic.data)
summary(my.regression.agree)
apa.reg.table(my.regression.agree, filename="Table2_APA.doc", table.number=2)


##MALES##
analytic.data.gender <- analytic_data %>% filter(gender==1) %>% select(-gender)
analytic.data.male <- select(analytic.data.gender, agreeableness, conscientiousness, performance, age)

my.regression.male <- lm(performance ~ conscientiousness, data=analytic.data.male)
summary(my.regression.male)
apa.reg.table(my.regression.male)

my.regression.male.agree <- lm(performance ~ conscientiousness + agreeableness, data=analytic.data.male)
summary(my.regression.male.agree)
apa.reg.table(my.regression.male.agree, filename="Table3_APA.doc", table.number=3)

##FEMALES##
analytic.data.gender.2 <- analytic_data %>% filter(gender==2) %>% select(-gender)
analytic.data.female <- select(analytic.data.gender.2, agreeableness, conscientiousness, performance, age)

my.regression.female <- lm(performance ~ conscientiousness, data=analytic.data.female)
summary(my.regression.female)
apa.reg.table(my.regression.female)

my.regression.female.agree <- lm(performance ~ conscientiousness + agreeableness, data=analytic.data.female)
summary(my.regression.female.agree)
apa.reg.table(my.regression.female.agree, filename="Table4_APA.doc", table.number=4)
