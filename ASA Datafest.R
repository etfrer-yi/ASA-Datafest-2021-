dataset<-read.csv('[insert path to CSV]', header = TRUE, stringsAsFactors = FALSE)
na.omit(dataset)

# print the first 5 rows
head(dataset, 5)
# compute correlation matrix
res<-cor(dataset[sapply(dataset,is.numeric)])
round(res, 2)

# Bunch of summaries 
dast_cat_score<-dataset$DAST_CAT
summary(dast_cat_score)
table(dast_cat_score)
summary(dataset)

# Male vs female
male_scr<-dast_cat_score[dataset$DEM_GENDER=='1']
male_scr_tb<-table(male_scr)
female_scr<-dast_cat_score[dataset$DEM_GENDER=='2']
female_scr_tb<-table(female_scr)
gender_tb<-rbind(male_scr_tb,female_scr_tb)
cat("Table of Observed Frequencies:","\n")
gender_tb







# DEM_AGE
age<-dataset$DEM_AGE10B
age_tb<-table(age, dast_cat_score)
age_tb

# DEM_GENDER
gender<-dataset$DEM_GENDER
gender_tb<-table(gender, dast_cat_score) # does the trick in a single line...
gender_tb
chisq.test(gender_tb)


# DEM_LOCATION
location<-dataset$DEM_LOCATION
location_tb<-table(location, dast_cat_score)
location_tb
chisq.test(location_tb)

# DEM_INCOME
income<-dataset$DEM_INCOME
income_tb<-table(income, dast_cat_score)
income_tb
chisq.test(income_tb)

# DEM_MARITAL
marital<-dataset$DEM_MARITAL
marital_tb<-table(marital, dast_cat_score)
marital_tb
chisq.test(marital_tb)

# DEM_EDU
education<-dataset$DEM_EDU
education_tb<-table(education, dast_cat_score)
education_tb
chisq.test(education_tb)

# DEM_PREG
pregnant<-dataset$DEM_PREG
pregnant_tb<-table(pregnant, dast_cat_score)
pregnant_tb
chisq.test(pregnant_tb)


gender<-dataset$DEM_GENDER
# Opiod
fentanyl_use<-dataset$FENT_USE
table(fentanyl_use, gender)
chisq.test(table(fentanyl_use, gender))


###########################################################################
gender<-dataset$DEM_GENDER
drugs_m<-dataset[dataset$DEM_GENDER == "1", 16:38]
drugs_f<-dataset[dataset$DEM_GENDER == "2", 16:38]

table(drugs_f, drugs_m)

cols<-c(4, 16, 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
dataset[, cols]
drugs_and_gender<-dataset[,cols]
table(droplevels(drugs_and_gender, 2))
barplot(table(droplevels(drugs_and_gender, 1)))


i = 16
while (i < 38) {
  chisq.test(table(dataset$DEM_GENDER, dataset[, c(i)]))
  i = i + 1;
}

x<-dataset$DEM_GENDER
y<-dataset[, c(16)]
y1<-dataset[, c(17)]
z<-lm(y~y+y1)
summary(z)

chisq.test(table(dataset$DEM_GENDER, dataset[, c(16)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(17)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(18)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(19)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(20)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(21)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(22)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(23)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(24)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(25)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(26)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(27)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(28)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(29)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(30)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(31)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(32)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(33)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(34)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(35)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(36)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(37)]))
chisq.test(table(dataset$DEM_GENDER, dataset[, c(38)]))



# THC_USE vs the rest 
thc<-dataset$THC_USE
other_drugs_use<-dataset[c(16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38)]
table(thc, other_drugs_use)



# DEM_AGE10 vs DAST_CAT = 1
age<-dataset$DEM_AGE10B
processed_dast_cat<-ifelse(dast_cat_score == 1, 'No involvement (DAST_SUM = 0)', 'Involvement (DAST_SUM > 0)')
age_dast_1_tb<-table(age, processed_dast_cat)
age_dast_1_tb
chisq.test(age_dast_1_tb)



# DEM_EDU
education<-dataset$DEM_EDU
education_tb<-table(education, dast_cat_score)
education_tb
chisq.test(education_tb)



# education vs DAST_SUM
education<-dataset$DEM_EDU
processed_education<-ifelse(education <= 5, 'Pre-university / currently in university',
                            'University completed and beyond')
processed_edu_tb<-table(processed_education, dast_cat_score)
processed_edu_tb<-table(dast_cat_score, processed_education)
processed_edu_tb
chisq.test(processed_edu_tb)









