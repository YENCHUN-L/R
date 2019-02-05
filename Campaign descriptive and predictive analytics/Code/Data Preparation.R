if(!require("readr")) install.packages("readr"); library("readr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")


# Insert a path for the original data files
path = "C:\\Users\\vernoult\\Desktop\\Descriptive & Predictive Analysis\\DSC project\\Final code\\"

#-----------------------------------------------------------------------------------------------------------------------#
#                         CREATING VARIABLES FROM DONOR.CSV
#-----------------------------------------------------------------------------------------------------------------------#
# Importing the Donor Dataset and deriving Variables from Donor dataset

donors <- read_delim(paste0(path, "donors.csv"), ";", escape_double = FALSE, trim_ws = TRUE)


#  GENDER
#----------#

# Creating separate variables as dummy from different values of gender
donors$gender_M <- ifelse(donors$gender == "M",1,0)
donors$gender_F <- ifelse(donors$gender == "F",1,0)
donors$gender_C <- ifelse(donors$gender == "C",1,0)
donors$gender_S <- ifelse(donors$gender == "S",1,0)
donors$gender_U <- ifelse(donors$gender == "S",1,0)

# LANGUAGE
#----------#

# set language to dummy variables
donors$language_F <- ifelse(donors$language == "F" , 1,0)
donors$language <- NULL



# DERIVING REGIONS BASED ON ZIPCODE
#-----------------------------------#

donors$region <- NULL  # Remove existing region column

# If zipcode not Brussels, Wallonia or Flanders, default to Brussels
donors$region <- ifelse(donors$zipcode >= 1000 & donors$zipcode <= 1299 , "Brussels", 
                        ifelse(donors$zipcode >= 1300 & donors$zipcode <= 1499 | 
                                 (donors$zipcode >= 4000 & donors$zipcode <= 7999), "Wallonia" ,
                               ifelse((donors$zipcode >= 1500 & donors$zipcode <= 1999) |
                                        (donors$zipcode >= 2000 & donors$zipcode <= 3999) |
                                        (donors$zipcode >= 8000 & donors$zipcode <= 9999),"Flanders","Brussels")))
donors[is.na(donors$region),]$region <- "Brussels"


# set the follwing regions to dummy variables
donors$region_Brussels <- ifelse(donors$region == "Brussels",1,0)
donors$region_Wallonia <- ifelse(donors$region == "Wallonia",1,0)
donors$region_Flanders <- ifelse(donors$region == "Flanders",1,0)
# Leaving out region_Flanders on purpose

# Removing Gender, region & zipcode column
donors$region <- NULL 
donors$gender <- NULL
donors$zipcode <- NULL

#-----------------------------------------------------------------------------------------------------------------------#
#                         CREATING VARIABLES FROM GIFT.CSV 
#-----------------------------------------------------------------------------------------------------------------------#

dataset <- read_delim( paste0(path, "gifts.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
XDonateDate <- dataset[c("donorID","amount","date")]
XDonateDate$date <- as.Date(XDonateDate$date, "%d/%m/%Y") 


# VARIABLES FOR TRAINING (up to 2013-04-11)
#-------------------------------------------#

# Isolating data up to the cutoff for training
FDonateDate0_20130411 <-filter(XDonateDate, date < "2013-04-11"& amount >0)

# Donor ID, amount of last donation and date
FDonateDate0_13 <- FDonateDate0_20130411 %>% group_by(donorID) %>% slice(which.max(date)) 
# Amount of the first donation and date
a <- FDonateDate0_20130411 %>% group_by(donorID) %>% slice(which.min(date))
FDonateDate0_13[,4:5] <- a[,2:3] 
# Cutoff date for the basetable timeline
FDonateDate0_13[,6] <- "2013-04-11"
# Length of the relationship (in years, with 2 digits)
FDonateDate0_13[,7] <-as.vector(difftime(FDonateDate0_13$V6, FDonateDate0_13$date.1, units='days'))
# Length of inactivity period (in years, with 2 digits)
FDonateDate0_13[,8] <- as.vector(difftime(FDonateDate0_13$V6, FDonateDate0_13$date, units='days'))
# Length of the active period (in years, with 2 digits)
FDonateDate0_13[,9] <- as.vector(difftime(FDonateDate0_13$date, FDonateDate0_13$date.1, units='days'))
# Total amount donated
x <- FDonateDate0_20130411 %>% group_by(donorID) %>% summarise_all(funs(sum))
FDonateDate0_13[,10] <- x[2]
# N° of donations during the activity period
FDonateDate0_13[, 11] <- (FDonateDate0_20130411 %>% group_by(donorID) %>% summarize(n_donations=n()))[2]
# Frequency of donations
FDonateDate0_13[, 12] <- ifelse(FDonateDate0_13$V9==0, 0, FDonateDate0_13$n_donations / FDonateDate0_13$V9)
# Average amount
FDonateDate0_13[, 13] <- FDonateDate0_13[,10] / FDonateDate0_13[, 11]
# Increasing trend noticed or not (is last_donation > avg_donation?)
FDonateDate0_13[, 14] <- ifelse(FDonateDate0_13[,2]-FDonateDate0_13[, 13] > 0, 1, 0)

# no of donations greater than 35
FDonateDate0_13[, 15] <- (FDonateDate0_20130411 %>% group_by(donorID) %>% summarise(sum(amount > 35)))[2]


# no of donations greater than 100
FDonateDate0_13[, 16] <- (FDonateDate0_20130411 %>% group_by(donorID) %>% summarise(sum(amount > 100)))[2]

# maximum amount donated
FDonateDate0_13[, 17] <- (FDonateDate0_20130411 %>% group_by(donorID) %>% summarise(max(amount)))[2] 

# minimum amount donated
FDonateDate0_13[, 18] <- (FDonateDate0_20130411 %>% group_by(donorID) %>% summarise(min(amount)))[2] 

# median of the amount donated
FDonateDate0_13[, 19] <- (FDonateDate0_20130411 %>% group_by(donorID) %>% summarise(median(amount)))[2] 



# Assigning names to previously created variables
names(FDonateDate0_13) = c("donorID", "Last_amount",  "Last_Donate_Date", "First_amount",  "First_Donate_Date", "Cutoff_date",
                           "relationship_length2013_days", "Inactive_period_days", "Active_period_days", "TTLDonate",
                           "n_donations", "frequency", "avg_donation_amount", "increasing_trend",
                           "donation_gt35","donation_gt100","max_amount","min_amount","median_amount")

# VARIABLES FOR TESTING (up to 2014-01-15)
#-------------------------------------------#
# Isolating data up to the cutoff for testing
FDonateDate0_20140115 <-filter(XDonateDate, date < "2014-01-15"& amount >0)

# Donor ID, amount of last donation and date
FDonateDate0_14 <- FDonateDate0_20140115 %>% group_by(donorID) %>% slice(which.max(date))
b <- FDonateDate0_20140115 %>% group_by(donorID) %>% slice(which.min(date))
# Amount of the first donation and date
FDonateDate0_14[,4:5] <- b[,2:3]
# Cutoff date for the basetable timeline
FDonateDate0_14[,6] <- "2014-01-15"
# Length of the relationship (in years, with 2 digits)
FDonateDate0_14[,7] <- as.vector(difftime(FDonateDate0_14$V6, FDonateDate0_14$date.1, units='days'))
# Length of inactivity period (in years, with 2 digits)
FDonateDate0_14[,8] <- as.vector(difftime(FDonateDate0_14$V6, FDonateDate0_14$date, units='days'))
# Length of the active period (in years, with 2 digits)
FDonateDate0_14[,9] <- as.vector(difftime(FDonateDate0_14$date, FDonateDate0_14$date.1, units='days'))
# Total amount donated
y <- FDonateDate0_20140115 %>% group_by(donorID) %>% summarise_all(funs(sum))
FDonateDate0_14[,10] <- y[2]
# N° of donations during the activity period
FDonateDate0_14[, 11] <- (FDonateDate0_20140115 %>% group_by(donorID) %>% summarize(n_donations=n()))[2]
# Frequency of donations
FDonateDate0_14[, 12] <- ifelse(FDonateDate0_14$V9==0, 0, FDonateDate0_14$n_donations / FDonateDate0_14$V9)
# Average amount
FDonateDate0_14[, 13] <- FDonateDate0_14[,10] / FDonateDate0_14[, 11]
# Increasing trend noticed or not (is last_donation > avg_donation?)
FDonateDate0_14[, 14] <- ifelse(FDonateDate0_14[,2]-FDonateDate0_14[, 13] > 0, 1, 0)

# no of donations greater than 35
FDonateDate0_14[, 15] <- (FDonateDate0_20140115 %>% group_by(donorID) %>% summarise(sum(amount > 35)))[2]


# no of donations greater than 100
FDonateDate0_14[, 16] <- (FDonateDate0_20140115 %>% group_by(donorID) %>% summarise(sum(amount > 100)))[2]

# maximum amount donated
FDonateDate0_14[, 17] <- (FDonateDate0_20140115 %>% group_by(donorID) %>% summarise(max(amount)))[2] 

# minimum amount donated
FDonateDate0_14[, 18] <- (FDonateDate0_20140115 %>% group_by(donorID) %>% summarise(min(amount)))[2] 

# median of the amount donated
FDonateDate0_14[, 19] <- (FDonateDate0_20140115 %>% group_by(donorID) %>% summarise(median(amount)))[2]



# Assigning names to previously createrd variables
names(FDonateDate0_14) = c("donorID", "Last_amount",  "Last_Donate_Date", "First_amount",  "First_Donate_Date", "Cutoff_date",
                           "relationship_length2013_days", "Inactive_period_days", "Active_period_days", "TTLDonate",
                           "n_donations", "frequency", "avg_donation_amount", "increasing_trend",
                           "donation_gt35","donation_gt100","max_amount","min_amount","median_amount") 

FDonateDate0_13[, c("Last_Donate_Date", "First_Donate_Date","Cutoff_date")] = NULL
FDonateDate0_14[, c("Last_Donate_Date", "First_Donate_Date","Cutoff_date")] = NULL


#-----------------------------------------------------------------------------------------------------------------------#
#                         CREATING THE TRAIN BASETABLE
#-----------------------------------------------------------------------------------------------------------------------#

# Import the Train Dataset
train <- read_delim(paste0(path, "campaign20130411.csv"), ";", escape_double = FALSE, trim_ws = TRUE)

# DonorId was not unique , sum the duplicates with amount by grouping donorid
train <- train %>% group_by(donorID) %>% summarise(target = sum(amount))

# Set the total_amount , the target variables of train to Dummy as 0 or 1
train$target <- ifelse(train$target >= 35 , 1 , 0)

trn <- merge(donors, train, by = "donorID")


basetable_train <- merge(trn , FDonateDate0_13 , by= "donorID")


#-----------------------------------------------------------------------------------------------------------------------#
#                         CREATING THE TEST BASETABLE
#-----------------------------------------------------------------------------------------------------------------------#

# Import Test Dataset
test <- campaign20140115 <- read_delim(paste0(path, "campaign20140115.csv"), ";", escape_double = FALSE, trim_ws = TRUE)

test <- test %>% group_by(donorID) %>% summarise(target = sum(amount))

# Set the amount , the target variable of test to Dummy as 0 or 1
test$target <- ifelse(test$target >= 35 , 1 , 0)


tst <- merge(donors, test, by = "donorID")


basetable_test <- merge(tst , FDonateDate0_14 , by= "donorID")

basetable_train$donorID <- NULL
basetable_test$donorID <- NULL

#write.csv(basetable_train, "basetable_train.csv")
#write.csv(basetable_test, "basetable_test.csv")