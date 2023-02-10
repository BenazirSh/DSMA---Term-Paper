
  #Merging the SQL output csv files
  
  
  #Set-up and libraries
  rm(list=ls())
setwd("C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universit?t/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper")


Packages <- c("MLmetrics", "gridExtra", "weathermetrics", "doParallel", "smotefamily", "mice", "GGally", "ggmap", "slider", "lubridate","xgboost","SHAPforxgboost", "dplyr", "readxl","ggplot2",
              "lmtest", "Hmisc", "data.table", "caret", "pROC", "DataExplorer",
              "car","zoo","stargazer","coefplot","tseries","sandwich", "readr","tidyr","tibble", "ggcorrplot",
              "tidyverse","psych", "Matrix", "DiagrammeR", "e1071","xtable", "gmodels", "splitTools")




lapply(Packages, library, character.only = TRUE)


# #For cloud computer: Installs all packages from above which are not yet installed in the virtual machine
# #Install all missing packages
# Packages_new <- Packages[!Packages %in% installed.packages()]
# 
# # And finally we install the missing packages, including their dependency.
# for(lib in Packages_new) install.packages(lib,dependencies=TRUE)
# # After the installation process completes, we load all packages.
#sapply(Packages_new,require,character=TRUE)




#### Load R.Data image
load("DSMA.RData")






#Prerequisites Google API
register_google(key='secret') #Black out for privacy




#Data import and first preparation


#businesstable clean, use this to skip the calculations for the businesstable_infos --> DOES NOT WORK PROPOERLY, LOOSES ALL FACTOR FORMATS
#businesstable_infos_imputed <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/businesstable_infos_imputed.csv')


#businesstable
businesstable_infos <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/businesstable_infos.csv')
businesstable_infos <- read_csv('businesstable_infos.csv') #Cloud Computer




#delete the u' specifier from the alcohol and noise level column with Regex
businesstable_infos$alcohol <- sub("(u')", replacement = "", businesstable_infos$alcohol)#Replace "u'"
businesstable_infos$alcohol <- sub("(')", replacement = "", businesstable_infos$alcohol)#Replace "'"
businesstable_infos$alcohol <- sub("(')", replacement = "", businesstable_infos$alcohol)#Replace "'"


businesstable_infos$noise_level <- sub("(u')", replacement = "", businesstable_infos$noise_level)#Replace "u'"
businesstable_infos$noise_level <- sub("(')", replacement = "", businesstable_infos$noise_level)#Replace "'"
businesstable_infos$noise_level <- sub("(')", replacement = "", businesstable_infos$noise_level)#Replace "'"


#delete the " in all opening hour columns
businesstable_infos$mon_open_hours <- gsub("\"", replacement = "", businesstable_infos$mon_open_hours)#Replace """
businesstable_infos$tue_open_hours <- gsub("\"", replacement = "", businesstable_infos$tue_open_hours)#Replace """
businesstable_infos$wed_open_hours <- gsub("\"", replacement = "", businesstable_infos$wed_open_hours)#Replace """
businesstable_infos$thr_open_hours <- gsub("\"", replacement = "", businesstable_infos$thr_open_hours)#Replace """
businesstable_infos$fri_open_hours <- gsub("\"", replacement = "", businesstable_infos$fri_open_hours)#Replace """
businesstable_infos$sat_open_hours <- gsub("\"", replacement = "", businesstable_infos$sat_open_hours)#Replace """
businesstable_infos$sun_open_hours <- gsub("\"", replacement = "", businesstable_infos$sun_open_hours)#Replace """


#Extract the total weekly opening hours as a new variable and the opening hours per day


# Function that extracts time to calculate how many hours restaurants are open
extract_time_difference <- function(time_string) {
  tryCatch({
    times <- strsplit(time_string, "-")[[1]]
    start <- strsplit(times[1], ":")[[1]]
    end <- strsplit(times[2], ":")[[1]]
    start_time <- as.POSIXct(paste(start[1], start[2], sep=":"), tryFormats=c("%H:%M", "%h:%m", "%h:M", "%H:%m"))
    end_time <- as.POSIXct(paste(end[1], end[2], sep=":"), format="%H:%M")
    if (start_time == end_time) {
      return (24)
    }
    if (end_time < start_time){
      end_time <- as.POSIXct(paste(as.numeric(end[1]), end[2], sep=":"), tryFormats=c("%H:%M", "%h:%m", "%h:M", "%H:%m"))
      end_time <- end_time + 24*60*60
    }
    time_difference <- as.numeric(difftime(end_time, start_time, units="hours"))
    return (time_difference)
  }, error = function(e) {
    return(NA)
  })
}


# Example usage:
print(extract_time_difference("09:30-12:15")) # Output: 2.75
print(extract_time_difference("09:30-09:15")) # Output: "23.75"
print(extract_time_difference("09:30-09:30")) # Output: 24
print(extract_time_difference("09:30-01:30")) # Output: 16
print(extract_time_difference("11:0-2:0")) # Output: 15
print(extract_time_difference("11:0-21:30")) # Output: 10.5
print(extract_time_difference("none"))#NA


#Create new columns for each day:
businesstable_infos$open_hours_mon <- sapply(businesstable_infos$mon_open_hours, extract_time_difference)
businesstable_infos$open_hours_tue <- sapply(businesstable_infos$tue_open_hours, extract_time_difference)
businesstable_infos$open_hours_wed <- sapply(businesstable_infos$wed_open_hours, extract_time_difference)
businesstable_infos$open_hours_thu <- sapply(businesstable_infos$thr_open_hours, extract_time_difference)
businesstable_infos$open_hours_fri <- sapply(businesstable_infos$fri_open_hours, extract_time_difference)
businesstable_infos$open_hours_sat <- sapply(businesstable_infos$sat_open_hours, extract_time_difference)
businesstable_infos$open_hours_sun <- sapply(businesstable_infos$sun_open_hours, extract_time_difference)


#Convert "NULL" values to na
is.na(businesstable_infos) <- businesstable_infos == "NULL"


#Replace NA values in opening hours with 0 since it is the most plausible
businesstable_infos["open_hours_mon"][is.na(businesstable_infos["open_hours_mon"])] <- 0
businesstable_infos["open_hours_tue"][is.na(businesstable_infos["open_hours_tue"])] <- 0
businesstable_infos["open_hours_wed"][is.na(businesstable_infos["open_hours_wed"])] <- 0
businesstable_infos["open_hours_thu"][is.na(businesstable_infos["open_hours_thu"])] <- 0
businesstable_infos["open_hours_fri"][is.na(businesstable_infos["open_hours_fri"])] <- 0
businesstable_infos["open_hours_sat"][is.na(businesstable_infos["open_hours_sat"])] <- 0
businesstable_infos["open_hours_sun"][is.na(businesstable_infos["open_hours_sun"])] <- 0






#Calculate total number of opening hours per week
businesstable_infos$open_hours_week <- rowSums(businesstable_infos[, c('open_hours_mon', 'open_hours_tue',
                                                                       'open_hours_wed', 'open_hours_thu', 
                                                                       'open_hours_fri', 'open_hours_sat',
                                                                       'open_hours_sun')], na.rm = TRUE)


#Deselect opening hours string column 
businesstable_infos <- businesstable_infos %>%
  select(-c('mon_open_hours', 'tue_open_hours', 'wed_open_hours', 'thr_open_hours', 'fri_open_hours',
            'sat_open_hours', 'sun_open_hours'))


#Convert "NULL" values to na
is.na(businesstable_infos) <- businesstable_infos == "NULL"


#Rename the category Latin American into Latin_American because this would cause problems later
businesstable_infos$categories[businesstable_infos$categories == "Latin American"] <- "Latin_American"


#Convert categories column to factor  
businesstable_infos <- businesstable_infos %>%
  mutate(across(c(free_wifi, has_tv, caters, alcohol, noise_level, for_kids, outdoor_seating, has_parking, has_takeout, 
                  price_range, takes_reservations, good_for_groups, credit_card_acceptance, categories), factor))


#some quality checks
head(businesstable_infos)
levels(businesstable_infos$categories)
levels(businesstable_infos$has_tv)
levels(businesstable_infos$caters)
levels(businesstable_infos$alcohol)
levels(businesstable_infos$noise_level)
levels(businesstable_infos$price_range)




#Get a simple, quick and nice overview of the dataset --> REALLY NICE, 10/10 can recommend for EDA!
plot_intro(businesstable_infos)
plot_missing(businesstable_infos) #SHOWS THE % OF MISSINGS FOR EVERY VARIABLE




##### Data imputation!








business_data1 <- subset(businesstable_infos,select = -c(business_id)) # removed this because MICE does not like imputing factors with more than 50 levels


#inspect pattern of missings
md.pattern(business_data1)


#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column. 
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data. 
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). 
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(business_data1), ncol = ncol(business_data1)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(business_data1)
row.names(predictorMatrix)=colnames(business_data1)
predictorMatrix[c("price_range"),] <- 1 #variables "business_price" can be explained by all other variables
diag(predictorMatrix) <- 0 #diagonal must be zero


#impute data
business_data1_data_imputed <- mice(business_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)


summary(business_data1_data_imputed)


#get one of the complete data sets ( 2nd out of 5)
business_data_complete_data <- complete(business_data1_data_imputed,2)


# bring back the business_id
business_data_complete_data=cbind(business_id=businesstable_infos$business_id,business_data_complete_data)


businesstable_infos_imputed = business_data_complete_data
# businesstable_infos=businesstable_infos%>%
#   inner_join(business_data_complete_data,by="business_id") #Should be the same as before


# # make factors out of chr variables
# for(j in 1:ncol(DailyLevel_data)){
#   if(typeof(DailyLevel_data[,j])=="character")
#     DailyLevel_data[,j]=as.factor(DailyLevel_data[,j])
# }


# # limit the number of categories to Asian, American, Mexican and Others
# 
# cat_s=as.character(DailyLevel_data$business_cat)
# new_cat_s=c("Others","Asian", "American", "Mexican")
# 
# changed=0
# for(k in new_cat_s[-1]){
#   cat_s[grepl(k,cat_s)]=k
#   changed=changed+grepl(k,cat_s)
# }
# cat_s[changed==0]="Others"
# DailyLevel_data$business_cat=as.factor(cat_s)


# n_photos==NA and cum_max_u_elite==NA are actually zeros, let's replace them with 0 before imputing.


#DailyLevel_data$cum_max_u_elite[is.na(DailyLevel_data$cum_max_u_elite)]=0


# some descriptives of the data
#describe(DailyLevel_data)






#the complete data sets can be used to estimate your model of choice
#and the results of all 5 models can be combined as in the earlier example
write.csv(businesstable_infos_imputed, file="businesstable_infos_imputed.csv")


plot_intro(businesstable_infos_imputed)
plot_missing(businesstable_infos_imputed)




########################
#check-ins, cumulative number of tips, cumulative max number of elite years and tips of a user who has checked in (daily)
dcheckins_cum_infos <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/dcheckins_cum_infos.csv')
head(dcheckins_cum_infos)
dcheckins_cum_infos <- read_csv('dcheckins_cum_infos.csv') #Cloud computer




#Convert "NULL" values to NA
is.na(dcheckins_cum_infos$cum_max_u_elite) <- dcheckins_cum_infos$cum_max_u_elite == "NULL"
is.na(dcheckins_cum_infos$cum_max_us_tip) <- dcheckins_cum_infos$cum_max_us_tip == "NULL"


#class
class(dcheckins_cum_infos$cum_max_u_elite)


#Convert cum_max_u_elite column as double
dcheckins_cum_infos <- dcheckins_cum_infos %>%
  mutate(cum_max_u_elite = as.numeric(cum_max_u_elite))


#Convert NA to 0 since this is the most plausible value
dcheckins_cum_infos["cum_max_u_elite"][is.na(dcheckins_cum_infos["cum_max_u_elite"])] <- 0
dcheckins_cum_infos["cum_max_us_tip"][is.na(dcheckins_cum_infos["cum_max_us_tip"])] <- 0




#change date_tip to date format
dcheckins_cum_infos <- dcheckins_cum_infos %>%
  mutate(date_tip = as.Date(date_tip, format = "%Y-%m-%d"))


########################
#Average number of friends and fans per business_id for all users who have left a tip/review in charlotte as proxy for check-in reach 
avg_n_friends_fans_perBusID <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/Average number of friends and fans per business id.csv')
head(avg_n_friends_fans_perBusID)
avg_n_friends_fans_perBusID <- read_csv('Average number of friends and fans per business id.csv')#Cloud computer




########################
#Number of reached fans per tip/review
n_reached_fans <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/n_reached_fans.csv')
head(n_reached_fans)
n_reached_fans <- read_csv('n_reached_fans.csv')#Cloud computer




########################
#Number of reached friends per tip/review
n_reached_friends <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/n_reached_friends.csv')
head(n_reached_friends)
n_reached_friends <- read_csv('n_reached_friends.csv')#Cloud computer




########################
#Sentiment analysis for reviews in 2017
output_reviews_sentiment_BERT <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/output_reviews_sentiment_BERT.csv')
head(output_reviews_sentiment_BERT)
output_reviews_sentiment_BERT <- read_csv('output_reviews_sentiment_BERT.csv')#Cloud computer


output_reviews_sentiment_BERT <- output_reviews_sentiment_BERT %>% #Format date column
  mutate(date=as.Date(date, format = "%Y-%m-%d",),
         label = as.factor(label)) %>%
  select(-"...1") %>% #Drop first column (redundant index)
  rename(review_text = text,
         review_sent_label = label,
         review_sent_score = score
  )
levels(output_reviews_sentiment_BERT$review_sent_label)


########################
#Sentiment analysis for tips in 2017
output_tips_sentiment_BERT <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/output_tips_sentiment_BERT.csv')
head(output_tips_sentiment_BERT)
output_tips_sentiment_BERT <- read_csv('output_tips_sentiment_BERT.csv')#CLoud computer




output_tips_sentiment_BERT <- output_tips_sentiment_BERT %>% #Format date column
  mutate(date=as.Date(date, format = "%Y-%m-%d"),
         label = as.factor(label)) %>%
  select(-1) %>% #Drop first column (redundant index)
  rename(tips_text = text,
         tips_sent_label = label,
         tips_sent_score = score
  )
levels(output_tips_sentiment_BERT$tips_sent_label)
