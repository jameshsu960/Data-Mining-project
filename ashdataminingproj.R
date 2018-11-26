#rm(list=ls())
#nstall.packages("matrixStats")

library(matrixStats)
library(fitdistrplus) #install.packages('fitdistrplus')
library(nnet)




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
data$Risk[data$CancerRisk < 1] <- 0   ##### "No risk"
data$Risk[data$CancerRisk >=1  & data$CancerRisk<= 2] <- 1  #####"medium risk"
data$Risk[data$CancerRisk >= 3 & data$CancerRisk<= 4] <- 2  ##### "high risk"







#data$CancerRisk = factor(data$CancerRisk, levels=c("0","1","2","3","4"))

#### removing the colums 
dat <- data[ -c(33,34,35,36,37) ]
#### to write updated data set to new csv
write.csv(dat, "Updated_file.csv")

#### round is rounding the output of prop.table to 2 digits
round(prop.table(table(dat$Risk)),2)





hist(dat$Risk, col = 2)

###########################################

##### check if the data is balanced

descdist(dat$Risk, discrete = FALSE)

normal_dist <- fitdist(dat$Risk, "norm")
plot(normal_dist) 

###Standardize the data
# head(dat)
# class(dat)
# Standandardize_train <- scale(dat[,-33])
# 
# var(dat[,2])
# var(dat[,3])
# 
# var(Standandardize_train[,2])
# var(Standandardize_train[,3])
# 
# var(dat[,10])
# var(Standandardize_train[,10])

# train <- sample(1:nrow(dat), nrow(dat)*.80)
# df_train <- dat[train, ]
# df_test <- dat[-train, ]
# 
# msat <- multinom(CancerRisk ~ ., data=df_train)

# library(UBL)
# 
# #sorting based in Response column
# attach(data)
# 
# # sort by response
# 
# data <-data[order(Risk),] 



data <- within(data, Risk[Risk == 0] <- "low")
data <- within(data, Risk[Risk == 1] <- "med")
data <- within(data, Risk[Risk == 2] <-  "high")
count<- table(data$Risk)

#Oversampling
library(UBL)
# Example with an imbalanced multi-class problem
data(data)
dat <- data[-c(45:75),]
# checking the class distribution of this artificial data set
table(data$Risk)
#newdata <- AdasynClassif(Risk~., data, beta=1)


library(DMwR)
data(data)
balanced<- data
# 3 class oversampling

#BALANCED OVERSAMPLE with weight for each weak classifier
C.perc = list(high = 19, med = 12 ) 
mybalanced <- RandOverClassif(Risk~., balanced, C.perc)
table(mybalanced$Risk)
# 
# #BALANCED UNBALACED OVER SAMPLE weight for each weak classifier
# C.perc = list(high = 7, med = 5 ) 
# myunbalanced <- RandOverClassif(Risk~., balanced, C.perc)
# table(myunbalanced$Risk)
# 
# #BALANCED INVERTED OVERSAMPLE
# myinverted <- RandOverClassif(Risk~., data, "extreme")
# table(myinverted$Risk)

# 5 class oversampling
# 
# 
# #BALANCED OVERSAMPLE with weight for each weak classifier
# C.perc = list('1' = 17, '2' = 33, '3'= 21,'4'=124 ) 
# mybalanced_class <- RandOverClassif(CancerRisk~., balanced, C.perc)
# table(mybalanced_class$CancerRisk)
# 
# #BALANCED UNBALACED OVER SAMPLE weight for each weak classifier
# C.perc = list('1' = 9, '2' = 20, '3'= 10,'4'=100 )
# myunbalanced_class <- RandOverClassif(CancerRisk~., balanced, C.perc)
# table(myunbalanced_class$CancerRisk)
# 
# #BALANCED INVERTED OVERSAMPLE
# myinverted_class <- RandOverClassif(CancerRisk~., data, "extreme")
# table(myinverted_class$CancerRisk)
