#rm(list=ls())
#nstall.packages("matrixStats")

library(matrixStats)




#### to check null values

sum(is.na(MyData)) ### 0 null values

head(MyData)
colnames(MyData)



##### to check ? character
which(MyData=='?', arr.ind=TRUE)

##### to convert those ? chars to NA
MyData[MyData=="?"]<- NA

######## to  count NA values in each columns
na_count <-sapply(MyData, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)


##### to check factor/categorical variables and continuous variables
# f <- sapply(MyData, is.factor)
# which(f)   #### factor/categorical variables index




mat <- as.matrix(MyData)
mode(mat) = "numeric"
str(mat)


##### to give median values to NA entries in continuous variables





#### replace NA with column medians



mat[, "Number.of.sexual.partners"] <- ifelse(is.na(mat[, "Number.of.sexual.partners"]), median(mat[, "Number.of.sexual.partners"], 
                                                                                               na.rm=TRUE), mat[, "Number.of.sexual.partners"])

mat[, "First.sexual.intercourse"] <- ifelse(is.na(mat[, "First.sexual.intercourse"]), median(mat[, "First.sexual.intercourse"], 
                                                                                             na.rm=TRUE), mat[, "First.sexual.intercourse"])

mat[, "Num.of.pregnancies"] <- ifelse(is.na(mat[, "Num.of.pregnancies"]), median(mat[, "Num.of.pregnancies"], 
                                                                                 na.rm=TRUE), mat[, "Num.of.pregnancies"])

#### NA is replace with 0 in "smoke" column as there are 13 NA entries and most of the patients doesn't smoke
mat[, "Smokes"] <- ifelse(is.na(mat[, "Smokes"]), median(mat[, "Smokes"], 
                                                         na.rm=TRUE), mat[, "Smokes"])

######### In "Smokes..years."  NA should be replaced by 0 as the value of "smoke" column(which was NA before) for 
######### that patient , is 0 now as modified in above step
mat[, "Smokes..years."] <- ifelse(is.na(mat[, "Smokes..years."]), 0, mat[, "Smokes..years."])

###### The above argument applies for  "Smokes..packs.year." as well.
mat[, "Smokes..packs.year."] <- ifelse(is.na(mat[, "Smokes..packs.year."]), 0, mat[, "Smokes..packs.year."])

mat[, "Hormonal.Contraceptives"] <- ifelse(is.na(mat[, "Hormonal.Contraceptives"]), median(mat[, "Hormonal.Contraceptives"], 
                                                                                           na.rm=TRUE), mat[, "Hormonal.Contraceptives"])

###### took mean here..check if median is needed by finding accuracy at last
mat[, "Hormonal.Contraceptives..years."] <- ifelse(is.na(mat[, "Hormonal.Contraceptives..years."]), mean(mat[, "Hormonal.Contraceptives..years."], 
                                                                                                         na.rm=TRUE), mat[, "Hormonal.Contraceptives..years."])


mat[, "IUD"] <- ifelse(is.na(mat[, "IUD"]), median(mat[, "IUD"], na.rm=TRUE), mat[, "IUD"])

mat[, "IUD..years."] <- ifelse(is.na(mat[, "IUD..years."]), median(mat[, "IUD..years."], na.rm=TRUE), mat[, "IUD..years."])


mat[, "STDs"] <- ifelse(is.na(mat[, "STDs"]), median(mat[, "STDs"], na.rm=TRUE), mat[, "STDs"])

#### since median value   of STD is 0 and we filled 0 in place of NA in column "STDs",
####  We can replace NA in next 13 columns with 0 as it's the type of STD the person is having.

mat[, "STDs..number."] <- ifelse(is.na(mat[, "STDs..number."]), 0, mat[, "STDs..number."])
mat[, "STDs.condylomatosis"] <- ifelse(is.na(mat[, "STDs.condylomatosis"]), 0, mat[, "STDs.condylomatosis"])
mat[, "STDs.cervical.condylomatosis"] <- ifelse(is.na(mat[, "STDs.cervical.condylomatosis"]), 0, mat[, "STDs.cervical.condylomatosis"])
mat[, "STDs.vaginal.condylomatosis"] <- ifelse(is.na(mat[, "STDs.vaginal.condylomatosis"]), 0, mat[, "STDs.vaginal.condylomatosis"])
mat[, "STDs.vulvo.perineal.condylomatosis"] <- ifelse(is.na(mat[, "STDs.vulvo.perineal.condylomatosis"]), 0, mat[, "STDs.vulvo.perineal.condylomatosis"])
mat[, "STDs.syphilis"] <- ifelse(is.na(mat[, "STDs.syphilis"]), 0, mat[, "STDs.syphilis"])
mat[, "STDs.pelvic.inflammatory.disease"] <- ifelse(is.na(mat[, "STDs.pelvic.inflammatory.disease"]), 0, mat[, "STDs.pelvic.inflammatory.disease"])
mat[, "STDs.genital.herpes"] <- ifelse(is.na(mat[, "STDs.genital.herpes"]), 0, mat[, "STDs.genital.herpes"])
mat[, "STDs.molluscum.contagiosum"] <- ifelse(is.na(mat[, "STDs.molluscum.contagiosum"]), 0, mat[, "STDs.molluscum.contagiosum"])
mat[, "STDs.AIDS"] <- ifelse(is.na(mat[, "STDs.AIDS"]), 0, mat[, "STDs.AIDS"])
mat[, "STDs.HIV"] <- ifelse(is.na(mat[, "STDs.HIV"]), 0, mat[, "STDs.HIV"])
mat[, "STDs.Hepatitis.B"] <- ifelse(is.na(mat[, "STDs.Hepatitis.B"]), 0, mat[, "STDs.Hepatitis.B"])
mat[, "STDs.HPV"] <- ifelse(is.na(mat[, "STDs.HPV"]), 0, mat[, "STDs.HPV"])


##### "STDs..Time.since.first.diagnosis" column and "STDs..Time.since.last.diagnosis" have NA values
##### only where "STDs..Number.of.diagnosis" column has entries as 0. Hence we can replace NA
##### with 0 for the above two columns
mat[, "STDs..Time.since.first.diagnosis"] <- ifelse(is.na(mat[, "STDs..Time.since.first.diagnosis"]), 0, mat[, "STDs..Time.since.first.diagnosis"])
mat[, "STDs..Time.since.last.diagnosis"] <- ifelse(is.na(mat[, "STDs..Time.since.last.diagnosis"]), 0, mat[, "STDs..Time.since.last.diagnosis"])

##### All NA entries has been replaced. Below code verifies the same
any(is.na(mat))

## code to create last column

data <- as.data.frame(mat)

data$CancerRisk = data$Hinselmann + data$Schiller + data$Citology + data$Biopsy
data$CancerRisk = factor(data$CancerRisk, levels=c("0","1","2","3","4"))



#Oversampling
library(UBL)
data(data)
#dat <- iris[-c(45:75), c(1, 2, 5)]
# checking the class distribution of this artificial data set
#table(dat$Species)
newdata <- AdasynClassif(CancerRisk~., data, beta=1)
#table(newdata$Species)
beta <- list( '2'=0.2,'4'=0.6,'1'=1)
newdata <- AdasynClassif(CancerRisk~., data, beta=beta)
table(newdata$CancerRisk)

## Checking visually the created data
par(mfrow = c(1, 2))
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]),
     col = as.integer(data[,3]), main = "Original Data",
     xlim=range(newdata[,1]), ylim=range(newdata[,2]))
plot(newdata[, 1], newdata[, 2], pch = 19 + as.integer(newdata[, 3]),
     col = as.integer(newdata[,3]), main = "New Data",
     xlim=range(newdata[,1]), ylim=range(newdata[,2]))

