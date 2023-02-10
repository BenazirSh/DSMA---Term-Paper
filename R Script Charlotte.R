########################
#Number of photos per business
phototable_infos <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/phototable_infos.csv')
head(phototable_infos)
phototable_infos <- read_csv('phototable_infos.csv')#Cloud computer


########################
#review information per business
reviewtable_infos <- read_csv('C:/Users/benazir/OneDrive - Johann Wolfgang Goethe Universität/Uni/Uni Frankfurt/Master/5. Semester/Data Scrience and Marketing Analytics/Paper/reviewtable_infos.csv')
head(reviewtable_infos)
reviewtable_infos <- read_csv('reviewtable_infos.csv')#Cloud computer




########################
#Add the weather data


charlotte_weather <- read_csv('weather_2010_2018.csv')#CLoud computer


charlotte_weather$date <- gsub("/", replacement = "-", charlotte_weather$date)#Replace "/" with "-"


charlotte_weather <- charlotte_weather %>%
  mutate(
    date = as.character(date),
    date = as.Date(date, format = "%Y-%m-%d")) %>%
  select(date, temp_avg, precipitation)
head(charlotte_weather)


#Merge the dcheckins_cum_infos and weather data
dcheckins_cum_infos <- inner_join(dcheckins_cum_infos, charlotte_weather, by = c('date_tip' = 'date')) #Left join photo information






#Merge all dataframes together for one "charlotte dataframe"


charlotte_df <- left_join(businesstable_infos_imputed, phototable_infos, by = c('business_id' = 'business_id')) #Left join photo information


charlotte_df <- left_join(charlotte_df, reviewtable_infos, by = c('business_id' = 'business_id')) #Left join review information


charlotte_df <- inner_join(charlotte_df, dcheckins_cum_infos, by = c('business_id' = 'business_id')) #Right join check-in information and cumulative information


charlotte_df <- left_join(charlotte_df, n_reached_fans, by = c('business_id' = 'business_id', 'date_tip' = 'date')) #Left join the number of reached fans per review/tip per day


charlotte_df <- unique(charlotte_df) #Remove weird duplicates


charlotte_df <- left_join(charlotte_df, n_reached_friends, by = c('business_id' = 'business_id', 'date_tip' = 'date')) #Left join the number of reached fans per review/tip per day


charlotte_df <- unique(charlotte_df) #Remove weird duplicates


charlotte_df <- left_join(charlotte_df, output_reviews_sentiment_BERT, by = c('business_id' = 'business_id', 'date_tip' = 'date')) #Left join the sentiment of a specific review for a day


charlotte_df <- left_join(charlotte_df, output_tips_sentiment_BERT, by = c('business_id' = 'business_id', 'date_tip' = 'date')) #Left join the sentiment of a specific tip for a day


charlotte_df <- unique(charlotte_df) #Remove weird duplicates


charlotte_df <- left_join(charlotte_df, avg_n_friends_fans_perBusID, by = c('business_id' = 'business_id')) #Left join the avg number of fans/friends of users who have written a review/tip per business




#Add a condition: If there was a check-in at a specific data for a business, keep the avg number of friends/fans, otherwise 0 --> Used to calculate reach!
charlotte_df$avg_n_friends_bus <- ifelse(charlotte_df$ch_in == 0, 0, charlotte_df$avg_n_friends_bus) #Friends
charlotte_df$avg_n_fans_bus <- ifelse(charlotte_df$ch_in == 0, 0, charlotte_df$avg_n_fans_bus) #Fans




#Calculate reach variable
charlotte_df <- charlotte_df %>%
  rename(date = date_tip) %>%
  mutate(
    n_reached_fans = replace_na(n_reached_fans, 0),#Replace NA values in n_reached_fans and n_reached_friends by 0
    n_reached_friends = replace_na(n_reached_friends, 0),
    reach = n_reached_fans + n_reached_friends + avg_n_fans_bus + avg_n_friends_bus)




#Create a rolling sum for average
rolling <- charlotte_df %>%
  select(business_id, date, reach) %>%
  group_by(business_id) %>%
  arrange(business_id, date) %>%
  mutate(
    rolling_reach_sum = slide_dbl(.x = reach, .f = sum, .before = 14,.after = 0)) %>%
  select(business_id, date, rolling_reach_sum)




#Merge reach and rolling reach sum to charlotte_df
charlotte_df <- left_join(charlotte_df, rolling, by = c('business_id' = 'business_id', 'date' = 'date'))


######Final cleanings and Feature Engineering:


#Replace n_photo NAs by 0 since this is the most plausible value
charlotte_df["n_photo"][is.na(charlotte_df["n_photo"])] <- 0


#Rename the stars.y column to stars_review (how many stars did a review get)
#Additionally, replace NAs by 0 since this is the most probable value


charlotte_df <- charlotte_df %>%
  rename(stars_review = stars.y) %>%
  mutate(stars_review = ifelse(is.na(stars_review), 0, stars_review))




#Since in 2017 only a few tips and reviews were written for our restaurant sample, I calculate the overall sentiment per restaurant
#as the share of positive to negative reviews PER RESTAURANT


df_positive_reviews <- charlotte_df %>%
  group_by(business_id) %>%
  filter(review_sent_label == "POSITIVE") %>%
  count(business_id)%>%
  rename(n_positive_reviews = n)


df_negative_reviews <- charlotte_df %>%
  group_by(business_id) %>%
  filter(review_sent_label == "NEGATIVE") %>%
  count(business_id)%>%
  rename(n_negative_reviews = n)


df_positive_tips <- charlotte_df %>%
  group_by(business_id) %>%
  filter(tips_sent_label == "POSITIVE") %>%
  count(business_id)%>%
  rename(n_positive_tips = n)


df_negative_tips <- charlotte_df %>%
  group_by(business_id) %>%
  filter(tips_sent_label == "NEGATIVE") %>%
  count(business_id)%>%
  rename(n_negative_tips = n)


df_sentiment_share <- full_join(df_positive_reviews, df_negative_reviews, by = c('business_id' = 'business_id'))


df_sentiment_share <- full_join(df_sentiment_share, df_positive_tips, by = c('business_id' = 'business_id'))


df_sentiment_share <- full_join(df_sentiment_share, df_negative_tips, by = c('business_id' = 'business_id'))


df_sentiment_share <- df_sentiment_share %>%
  group_by(business_id) %>%
  mutate(sum_positive_feedback = sum(n_positive_reviews, n_positive_tips, na.rm = T),
         sum_negative_feedback = sum(n_negative_reviews, n_negative_tips, na.rm = T),
         share_positive_feedback = round(sum_positive_feedback / (sum_positive_feedback + sum_negative_feedback),2),
         share_negative_feedback = round(sum_negative_feedback / (sum_positive_feedback + sum_negative_feedback),2))%>%
  select(c("business_id", "share_positive_feedback", "share_negative_feedback"))#Only get the "final" shares since the other columns are unimportant


charlotte_df <- left_join(charlotte_df, df_sentiment_share, by=c("business_id" = "business_id"))




# add weekends and quarters
temp=weekdays(charlotte_df$date,abbreviate = T)
charlotte_df$WE=temp=="Sa"|temp=="So"
charlotte_df$WE=as.factor(charlotte_df$WE)


# add weekends and quarters --> FOR ENGLISH CLOUD COMPUTER!
temp=weekdays(charlotte_df$date,abbreviate = T)
charlotte_df$WE=temp=="Sat"|temp=="Sun"
charlotte_df$WE=as.factor(charlotte_df$WE)


charlotte_df$Quarter=as.factor(quarters(charlotte_df$date))


plot_missing(charlotte_df) #SHOWS THE % OF MISSINGS FOR EVERY VARIABLE -> 60% of restaurants did not get a tip or review during 2017
#Replace NAs in positive and negative review share with 0 since they did not get any positive or negative review/tip
charlotte_df["share_positive_feedback"][is.na(charlotte_df["share_positive_feedback"])] <- 0
charlotte_df["share_negative_feedback"][is.na(charlotte_df["share_negative_feedback"])] <- 0
#Do the same with the compliment count that a review git 
charlotte_df["compliment_count"][is.na(charlotte_df["compliment_count"])] <- 0




# #Remove unneccessary Data from Workspace
# rm(list = c("businesstable_infos_imputed", "business_data_complete_data", "business_data1", "business_data1_data_imputed",
#             "avg_n_friends_fans_perBusID", "businesstable_infos", "charlotte_weather", "dcheckins_cum_infos", "n_reached_fans", 
#             "n_reached_friends", "output_reviews_sentiment_BERT", "predictorMatrix",
#             "output_tips_sentiment_BERT", "phototable_infos", "reviewtable_infos", "rolling", "df_negative_reviews",
#             "df_positive_reviews", "df_negative_tips", "df_positive_tips", "df_sentiment_share"))
# 






############################# DESCRIPTIVE STATISTICS ############################# 




#Get a simple, quick and nice overview of the dataset --> REALLY NICE, 10/10 can recommend for EDA!
plot_intro(charlotte_df)
plot_missing(charlotte_df) #SHOWS THE % OF MISSINGS FOR EVERY VARIABLE




# #Convert factors BACK to original form for descriptive  
# charlotte_descriptive <- charlotte_df %>%
#   mutate(across(c(free_wifi, has_tv, caters, alcohol, noise_level, for_kids, outdoor_seating, has_parking, has_takeout, 
#                   price_range, takes_reservations, good_for_groups, credit_card_acceptance, categories), factor))
# 
# charlotte_descriptive <- charlotte_descriptive %>% mutate_if(is.factor, as.numeric)
# 
# describe(charlotte_descriptive)




#Prepare a dataframe for the descriptive analysis!
charlotte_descriptive <-charlotte_df
charlotte_descriptive$price_range <- as.numeric(as.character(charlotte_descriptive$price_range))


charlotte_descriptive <-charlotte_descriptive %>% 
  mutate(free_wifi=ifelse(free_wifi=="TRUE",1,0), 
         has_tv=ifelse(has_tv=="TRUE",1,0), 
         caters=ifelse(caters=="TRUE",1,0), 
         #alcohol=ifelse(alcohol=="TRUE",1,0), 
         #noise_level=ifelse(noise_level=="TRUE",1,0),
         for_kids=ifelse(for_kids=="TRUE",1,0),
         outdoor_seating=ifelse(outdoor_seating=="TRUE",1,0),
         has_parking=ifelse(has_parking=="TRUE",1,0),
         has_takeout=ifelse(has_takeout=="TRUE",1,0),
         #price_range=ifelse(price_range=="TRUE",1,0),
         takes_reservations=ifelse(takes_reservations=="TRUE",1,0),
         good_for_groups=ifelse(good_for_groups=="TRUE",1,0),
         credit_card_acceptance=ifelse(credit_card_acceptance=="TRUE",1,0),
         #categories=ifelse(categories=="TRUE",1,0),
  ) %>%
  #select(-c(alcohol,noise_level,categories,date, temp_avg, precipitation)) %>% 
  group_by(business_id) %>% 
  dplyr::summarize(ch_in=mean(ch_in), stars=mean(stars.x), review_count=mean(review_count), 
                   free_wifi=mean(free_wifi), has_tv=mean(has_tv), 
                   caters=mean(caters), for_kids=mean(for_kids), 
                   has_parking=mean(has_parking), has_takeout=mean(has_takeout), price_range = mean(price_range), 
                   takes_reservations=mean(takes_reservations), good_for_groups=mean(good_for_groups), 
                   credit_card_acceptance=mean(credit_card_acceptance), open_hours_week = mean(open_hours_week),
                   n_photo = mean(n_photo),cum_n_tips = mean(cum_n_tips), cum_max_u_elite = mean(cum_max_u_elite),
                   cum_max_us_tip = mean(cum_max_us_tip), temp_avg = mean(temp_avg), precipitation = mean(precipitation),
                   avg_reach = mean(reach), avg_rolling_reach_sum = mean(rolling_reach_sum), 
                   share_positive_feedback = mean(share_positive_feedback),
                   share_negative_feedback = mean(share_negative_feedback)
  )


#Prepare summary statistics with prepared df
descriptive_summary <- charlotte_descriptive %>% 
  select(-c("business_id"))


statistics <- cbind(sapply(descriptive_summary,mean),sapply(descriptive_summary,median),sapply(descriptive_summary,sd),sapply(descriptive_summary,min),sapply(descriptive_summary,max)) 
statistics <- as.data.frame(round(statistics,digits=3))
colnames(statistics) <- c("Mean","Median", "Standard Deviation", "Minimum", "Maximum")


stargazer(statistics, summary = FALSE, type = "html", out="descriptive_statistics_charlotte.html")#Format for paper




#Check correlation (exclude categorical variables)
correlation_input <- charlotte_descriptive %>% select(-c("business_id", "has_takeout", "credit_card_acceptance"))#Takeout and credit card are deleted since values for all restaurants are true


correlations <- round(cor(correlation_input),2)
#print(correlations)


#Plot correlations, highlight correlation <-0,5 and >0,5
ggcorr(correlations, 
       geom = "blank",label = TRUE, hjust=0.9, size=4.9, label_size=3, layout.exp = 2) +
  geom_point(size = 10, aes(color = coefficient < 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)




# Weather graph


#easy
plot(charlotte_df$date, charlotte_df$temp_avg)
plot(charlotte_df$date, charlotte_df$precipitation)


#advanced
weather <- charlotte_df %>%
  select(date, temp_avg, precipitation)
weather$temp_avg <- fahrenheit.to.celsius(weather$temp_avg) #Fahrenheit to celsius


weather <- weather %>%
  select(date, temp_avg, precipitation) %>%
  mutate(precipitation = precipitation *25.4)#inches to mm
distinct


coeff = 80#Scale graph in any direction!
weather <- weather %>%
  select(date, temp_avg, precipitation) %>% 
  group_by(month = lubridate::floor_date(date, 'month')) %>% 
  ggplot(aes(x = date, y = precipitation)) +
  geom_col() +
  geom_line(aes(y = temp_avg*coeff), color = "orange", size=1.5) +
  geom_point(aes(y = temp_avg*coeff), color = "orange", size=2.5)+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %y")+
  scale_y_continuous("Precipitation in mm", 
                     sec.axis = sec_axis(~./coeff, name = "Temperature in Celsius")) +
  theme_bw(base_size = 14)+
  labs(x="")+
  ggtitle("Charlotte Climate")+
  theme(axis.line.y.right = element_line(color = "orange"), 
        axis.ticks.y.right = element_line(color = "orange"),
        axis.text.y.right = element_text(color = "orange"), 
        axis.title.y.right = element_text(color = "orange"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)
  ) 


plot(weather)


############################# SOME MAPS ############################# 


#Google Map of Charlotte with all Restaurant locations
charlotte_static <- get_googlemap('Charlotte', zoom=11, source='google')


gmap_charlotte <- ggmap(charlotte_static, darken = .6) + geom_point(aes(x=longitude , y=latitude , color=categories),
                                                                    data=businesstable_infos , alpha=.5, na.rm = T) +theme(legend.position="top")#+ ggtitle('Charlotte Restaurant Locations')


gmap_charlotte




#Heatmap charlotte
gmap_charlotte_heat <- ggmap(charlotte_static)+
  stat_density2d(aes(x=longitude, y=latitude,fill=..level.., alpha=..level..), data=charlotte_df , geom="polygon")+ 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position="none")
#+ ggtitle('Charlotte Restaurant Heatmap')


gmap_charlotte_heat




grid.arrange(gmap_charlotte, gmap_charlotte_heat, nrow = 1)




### Reach Plot
reach_plot <- charlotte_df %>%
  select(business_id, date, reach, rolling_reach_sum) %>%
  filter(business_id == "bzbNGyrTwWHAgg1CqkSgeg")


reach_plot %>% 
  select(-business_id)


#business_id: bzbNGyrTwWHAgg1CqkSgeg
ggplot()+
  geom_line(data=reach_plot,aes(y=reach,x= date,colour="Reach"),size=1 )+
  geom_line(data=reach_plot,aes(y=rolling_reach_sum,x= date,colour="Rolling"),size=1) +
  scale_color_manual(values = c("Reach" = "darkblue", "Rolling" = "red"))+
  labs(y = "Reached potential customers", x= "Year") +
  ggtitle("Reach and Rolling Reach Sum for Terrace Restaurants")+ 
  theme(legend.position="bottom")
########### MACHINE LEARNING! ###############






# ----
# # Importing and adjusting the yelp-data + weather data
# yelp_data_weather=read.csv(file="yelp_data_tip_weather.csv")




# some adjustments to the imported data
yelp_data=charlotte_df


#yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="Noch_in") # since the performance evaluations are mainly made
# to check for the minority class - in our case ch_in




# yelp_data$business_park=as.factor(yelp_data$business_park)
# yelp_data$business_open=as.factor(yelp_data$business_open)
# yelp_data$business_cat=as.factor(yelp_data$business_cat)
# yelp_data$WE=as.factor(yelp_data$WE)
# yelp_data$Quarter=as.factor(yelp_data$Quarter)




# A simple regression analysis ----> THROW OUT Credit Card Acceptance, compliment_count, open_hours_week, has_takeout
m1=glm(ch_in~stars.x+review_count+free_wifi+has_tv+caters+alcohol+noise_level+for_kids+outdoor_seating+has_parking+
         price_range+takes_reservations+good_for_groups+categories+open_hours_mon+
         open_hours_tue+open_hours_wed+open_hours_thu+open_hours_fri+open_hours_sat+open_hours_sun+
         n_photo+avg_stars_us+n_reviews_us+usefull+funny+cool+cum_n_tips+cum_max_u_elite+cum_max_us_tip+temp_avg+precipitation+
         stars_review+rolling_reach_sum+WE+Quarter+share_positive_feedback+share_negative_feedback, data = yelp_data, family = "binomial")
car::vif(m1)
summary(m1)


# ----


# predictive models
# Split randomly
set.seed(66)
yelp_data_na=yelp_data
# list of variables in your model -> Throw out credit_card_acceptance, has_takeout, compliment_count
varsin=c("ch_in_string","ch_in","stars.x","review_count","free_wifi",
         "has_tv","caters","alcohol","noise_level","for_kids","outdoor_seating","has_parking",
         "price_range","takes_reservations","good_for_groups",
         "categories","open_hours_mon","open_hours_tue","open_hours_wed","open_hours_thu",
         "open_hours_fri","open_hours_sat","open_hours_sun","open_hours_week","n_photo",
         "avg_stars_us","n_reviews_us","usefull","funny","cool","cum_n_tips","cum_max_u_elite",
         "cum_max_us_tip","temp_avg","precipitation","stars_review",
         "rolling_reach_sum","WE","Quarter","share_positive_feedback","share_negative_feedback")
yelp_data_na=subset(yelp_data_na,select=varsin)
datasetsize=nrow(yelp_data_na)/1 # would you like to work only  on a subset of your data? 
x <- yelp_data_na[sample(1:nrow(yelp_data_na), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]


BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))




# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))


# class imbalance check. --> 3261 / 12786 = 25,5% --> Tried SMOTE, did not improve, skip!
# temp=table(x.train[,"ch_in_string"])
# print(temp)
# # if yes, maybe you want do random over-sampling:
# if(0){
#   oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
#   minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
#   for(m in 1:(length(temp)-1)){
#     minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
#     minclassdat=x.train[x.train$ch_in_string==minchclass,]
#     minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
#     oversampled=rbind(oversampled,minclassdat)
#   }
#   x.train=oversampled
# }
# 
# # or do SMOTE:
# if(1){
#   x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
#   names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
#   x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
#   x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
#   x.traindum=x.traindum_smote
#   rm(x.traindum_smote)
# }
# temp=table(x.traindum[,"ch_in_string"])
# print(temp)




############ Data for Heuristic machine learning methods
# normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)


# adjust Baseformula to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}


# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")




BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))


# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)


# ----
# the analyses


#Hyperparameter tuning: https://rstudio-pubs-static.s3.amazonaws.com/587182_85a351763cd34130a81282495e8e3f9c.html
#Guide: https://towardsdatascience.com/a-guide-to-using-caret-in-r-71dec0bda208
#k-fold cross validation: https://towardsdatascience.com/k-fold-cross-validation-explained-in-plain-english-659e33c0bc0






##### USE the F1 score as an alternative evaltuation method (instead of accuracy)
#https://stackoverflow.com/questions/37666516/caret-package-custom-metric


#Create a custom metric -> F1
twoClassSummary2=function(data,lev=NULL, model=NULL){
  a=table(data$pred,data$obs)
  out1=c(a[1],a[2],a[3],a[4],(a[1]+a[4]))/sum(a)
  names(out1)=c("TP","FN","FP","TN","Acc")
  Prec=out1["TP"]/(out1["TP"]+out1["FP"])
  Rec=out1["TP"]/(out1["TP"]+out1["FN"])
  F1=2*(Prec*Rec)/(Prec+Rec)
  out2=c(Prec,Rec,F1)
  names(out2)=c("Prec","Rec","F1")
  out3=MLmetrics::F1_Score(y_pred=data$pred,y_true=data$obs,positive=lev[1])
  names(out3)="altF1"
  c(out1,out2,out3)
}


#Just an example
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 10,
#                            ## Estimate class probabilities
#                            classProbs = TRUE,
#                            ## Evaluate performance using
#                            ## the following function
#                            summaryFunction = twoClassSummary2)
# 
# 
# data_f_dat=data_full
# data_f_dat$is_open=as.factor(ifelse(data_f_dat$is_open==1,"open","closed"))
# m_dat=train(is_open~stars+price_level,data=data_f_dat,method="knn", trControl = fitControl,metric = "altF1")






######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"


summary(x.modelLogit)


x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"


x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))


#MAKE LIFT PLOT FUNCTION
makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  # plots the liftplot, and computes the GINI coefficient.
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$ch_in_string == "ch_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="ch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="ch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}








LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")


TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)




############ Naive Bayes




## Hyperparameter Tuning using random search  --> NAIVE BAYES


#Do this for each model!


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes",
                   tuneGrid = expand.grid(
                     adjust = c(sample(1:10, 3)),#Bandwidth adjustment
                     usekernel = c(TRUE),
                     laplace = c(sample(0:1000,3))),
                   trControl = trainControl(
                     method = "cv",
                     number = 10, #https://rpubs.com/maulikpatel/224581
                     summaryFunction = twoClassSummary2
                   ),
                   metric = "altF1")


stopCluster(cl) #


x.modelNB #adjust 9, laplace 72, True (Accuracy)
#laplace = 74, usekernel = TRUE and adjust = 10
##################################




cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes",
                   tuneGrid = data.frame(adjust = 10,
                                         usekernel = TRUE,
                                         laplace = 74),#With above found Hyperparameters!
                   trControl = trainControl(summaryFunction = twoClassSummary2),
                   metric = "altF1"
)




x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")




x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"


x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))


# the variable importance
print(varImp(x.modelNB))


# Extract the class probabilities.
x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in']#Change to ch_in, otherwise negative values???


NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")


TimeAux <- proc.time() - ptm 
NBOutput$TimeElapsed <- TimeAux[3]
NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ KNN


## Hyperparameter Tuning using random search  --> KNN


#Do this for each model!#


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelKNN <- train(BaseFormula_dum, data = x.trainnorm, method="knn",
                    tuneGrid = expand.grid(
                      k = c(sample(5:35, 10))), #which values the main parameter will take -> https://rpubs.com/Mentors_Ubiqum/tunegrid_tunelength
                    trControl = trainControl(
                      method = "cv",
                      number = 10, #https://rpubs.com/njvijay/16444
                      summaryFunction = twoClassSummary2
                    ),
                    metric = "altF1")


stopCluster(cl)


x.modelKNN #k = 30 (accuracy)
#k=34 (F1)
#############################


cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelKNN <- train(BaseFormula_dum, data = x.trainnorm, method="knn",
                    tuneGrid = expand.grid(
                      k = 34),#With above found Hyperparameter for k!
                    trControl = trainControl(summaryFunction = twoClassSummary2),
                    metric = "altF1")


x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")




x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"


x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))


# the variable importance
print(varImp(x.modelKNN))


# Extract the class probabilities.
x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in']


KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")


TimeAux <- proc.time() - ptm 
KNNOutput$TimeElapsed <- TimeAux[3]
KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)




############ SVM


## Hyperparameter Tuning using random search  --> SVM, tunelength


#Do this for each model!#


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmRadial", cachesize=12000, tolerance=.01,
                    tuneLength = 3, #https://blog.revolutionanalytics.com/2015/10/the-5th-tribe-support-vector-machines-and-caret.html
                    trControl = trainControl(method="cv",   # 10 fold cross validation
                                             number=10, 
                                             summaryFunction = twoClassSummary2
                    ),
                    metric = "altF1")
stopCluster(cl)


x.modelSVM #-> sigma = 0.01346207 and C = 1 (Accuracy)
#sigma = 0.01347207 and C = 1 (F1)


#############################


cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
# fast trainer
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmRadial", cachesize=12000, tolerance=.01,
                    tuneGrid = expand.grid(data.frame(C = c(0.01347207),
                                                      sigma = c(1))),
                    trControl = trainControl(classProbs =  TRUE,
                                             summaryFunction = twoClassSummary2),
                    metric = "altF1")#With above found Hyperparameters!




x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")




x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"


x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))


# for fast trainer you can also get the variable importance
print(varImp(x.modelSVM))


# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']#Change to ch_in, otherwise negative values???


SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")


TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


########## Neural network


## Hyperparameter Tuning using random search  --> Neural Network


#Do this for each model!#


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid= expand.grid(
  layer1 = sample(1:8, 2),
  layer2 = sample(1:8, 2),
  layer3 = sample(1:8, 2)),
  trControl = trainControl(method = "cv",
                           number = 10,
                           summaryFunction = twoClassSummary2
  ),
  metric = "altF1")#https://rpubs.com/maulikpatel/224581


stopCluster(cl)


x.modelNNet #See which layer combination performs best! -> l1 = 5, l2 = 7, l3 = 6 (Accuracy)
#layer1 = 4, layer2 = 2 and layer3 = 6 (F1)






#############################


cl <- makeCluster(detectCores())
registerDoParallel(cl)


library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 4,#Adapt with layers from above!
                       layer2 = 2,
                       layer3 = 6)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid,
                     trControl = trainControl(summaryFunction = twoClassSummary2),
                     metric = "altF1") 


x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")


x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"




x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))


print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]


NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")


TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)


stopCluster(cl)


########## TREE


## Hyperparameter Tuning using random search  --> TREE!


#Do this for each model!#
mincriterion_values = c(runif(5,0.85,0.99))


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree2', #Error: The tuning parameter grid should have columns mincriterion -> ctree2 works https://topepo.github.io/caret/available-models.html
                     tuneGrid = expand.grid(.mincriterion = c(0.9349009, 0.9224103, 0.9818578, 0.9342461, 0.8730030), 
                                            .maxdepth = as.integer(sample(2:10, 5))),
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              summaryFunction = twoClassSummary2
                     ),
                     metric = "altF1")#https://rpubs.com/maulikpatel/224581






stopCluster(cl)


x.modelTree # -> maxdepth = 8, mincriterion = 0.9745914 (Accuracy)
#maxdepth = 5 and mincriterion = 0.873003 (F1)
#############################


# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)


ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree2',
                     tuneGrid = expand.grid(
                       .mincriterion = 0.873003,
                       .maxdepth = 5),
                     trControl = trainControl(summaryFunction = twoClassSummary2),
                     metric = "altF1")#With above found Hyperparameters!




x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")


x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))


x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))


x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]


# to see the importance of the variables
print(varImp(x.modelTree))


# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}


TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")


TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)


stopCluster(cl)




############ Bagging --> Treebag method does not have any tuning parameters! https://r-forge.r-project.org/scm/viewvc.php/*checkout*/www/Tree_Based_Model.html?revision=828&root=caret
cl <- makeCluster(detectCores())
registerDoParallel(cl)


ptm <- proc.time()
# fast training using parallel computation
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)


# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")


x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))




# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))


# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]


# to see the importance of the variables
print(varImp(x.modelBagging))


BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")


TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)






############ Boosting


## Hyperparameter Tuning using random search  --> Boosting!


#Do this for each model!#


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelBoosting <- train(BaseFormula_dum, data=x.trainnorm, method='xgbTree', 
                         tuneGrid = expand.grid(
                           nrounds = c(sample(50:700,2)),
                           max_depth = c(sample(3:10,2)),
                           eta = c(runif(2,.01,.3)),
                           gamma = c(0),
                           colsample_bytree = c(runif(2,.5,.9)),
                           #early_stopping_rounds = sample(5:60,1),#range heuristic: 10% of nrounds = 60 -> not working in caret
                           min_child_weight = c(sample(0:10,2)),
                           subsample = c(runif(2,.7,1))
                         ),
                         trControl = trainControl(method = "cv",
                                                  number = 10,
                                                  summaryFunction = twoClassSummary2),
                         metric = "altF1")#https://rpubs.com/maulikpatel/224581






stopCluster(cl)


x.modelBoosting # -> nrounds = 381,max_depth = 7, eta = 0.1980942, subsample = 0.9565552, , colsample_bytree = 0.7008627, min_child_weight = 5, gamma = 0 (Accuracy)
#nrounds = 73, max_depth = 3, eta = 0.243378, gamma = 0, colsample_bytree = 0.6703593, min_child_weight = 4 and subsample = 0.9718927 (F1)


#############################






cl <- makeCluster(detectCores())
registerDoParallel(cl)


ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'xgbTree',
                          tuneGrid = expand.grid(
                            nrounds = 73,
                            max_depth = 3,
                            eta = 0.243378,
                            gamma = 0,
                            colsample_bytree = 0.6703593,
                            min_child_weight = 4,
                            subsample = 0.9718927
                          ),
                          trControl = trainControl(summaryFunction = twoClassSummary2),
                          metric = "altF1")


# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))




# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))


# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]


# to see the importance of the variables
print(varImp(x.modelBoosting))


# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")


TimeAux <- proc.time() - ptm
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)


stopCluster(cl)


#Plot VIF
plot(varImp(x.modelBoosting), top = 15)


#### USE XGBOOST to plot SHAP values! -> Code up from scratch bacause caret object does not work with SHAP for XGBoost!


# Prepare the data (sparse matrix format)
x.train_xgb <- x.train%>%
  select(-c("ch_in_string"))


x.evaluate_xgb <- x.evaluate%>%
  select(-c("ch_in_string"))




train_m <-sparse.model.matrix(BaseFormula1, data = x.train_xgb)#Create sparse model matrices 
valid_m <-sparse.model.matrix(BaseFormula1, data = x.evaluate_xgb)




train_label <-ifelse(x.train_xgb$ch_in==1,1,0)
valid_label <-ifelse(x.evaluate_xgb$ch_in==1,1,0)




## Use the previously found optimal hyperparameter combination:


#Run the hyperparameter tuned model with the best performing value from the random search (first row) -> xgb more advanced than xgboost
set.seed(12345)


xgb_hyptuned <- xgboost(data=train_m,
                        booster="gbtree",#default
                        max_depth= 3, #Granularity of the model
                        eta= 0.243378, #Learning rate
                        subsample= 0.9718927, #when <1, you’re also bagging the trees
                        #early_stopping_rounds = ,
                        colsample_bytree= 0.6703593,
                        min_child_weight= 4,
                        nrounds= 73,
                        objective="binary:logistic", #needed when target variable is binary
                        eval_metric="auc",
                        #watchlist=list(train=train_m,validation=valid_m),#Specifying the watchlist
                        #maximize=TRUE,
                        label = train_label) #when we want to see in-between-steps






print(xgb_hyptuned,verbose=TRUE)
xgb_hyptuned


#save hyperparameter-tuned xgb model
xgb.save(xgb_hyptuned, 'xgb_hyptuned.model')




# #########Hyperparameter tuning ################ --> OLD APPROACH, FULLY FUNCTIONING BUT NOT APPLICABLE HERE!
# 
# 
# 
# #Use random search for Hyperparameter tuning
# #Empty lists for for-loop
# smallest_error_list = list()
# highest_auc_list = list()
# parameters_list = list()
# 
# #Create 30 rows with random hyperparameters
# set.seed(12345)
# for(iteration in 1:30) {
#   param <- list(booster = "gbtree",#Default
#                 objective = "binary:logistic",#Check in yes/no
#                 nrounds = c(sample(50:700,5)),
#                 max_depth = sample(3:10,1),
#                 eta = runif(1,.01,.3),
#                 subsample = runif(1,.7,1),
#                 early_stopping_rounds = sample(5:60,1),#range heuristic: 10% of nrounds = 60
#                 colsample_bytree = runif(1,.5,.9),
#                 min_child_weight = sample(0:10,1)
#   )
#   parameters <-as.data.frame(param)
#   parameters_list[[iteration]] <- parameters
# }
# 
# # Create dataframe containing all randomly created hyperparameters
# parameters_df = do.call(rbind,parameters_list)
# 
# #Create 30 XGBoost models with the random hyperparameters (model cross-validation)
# for (row in 1:nrow(parameters_df)){
#   set.seed(12345)
#   modelcv<-xgboost(data=train_m,
#                    booster="gbtree",#default
#                    max_depth=parameters_df$max_depth[row],
#                    eta=parameters_df$eta[row],
#                    subsample=parameters_df$subsample[row],
#                    early_stopping_rounds = parameters_df$early_stopping_rounds[row],
#                    colsample_bytree=parameters_df$colsample_bytree[row],
#                    min_child_weight=parameters_df$min_child_weight[row],
#                    #nrounds=600, #Iterations
#                    nrounds = parameters_df$nrounds[row],
#                    objective="binary:logistic",
#                    eval_metric = "auc",
#                    eval_metric = "logloss",
#                    eval_metric = "error",
#                    maximize=TRUE, #Because of early stopping rounds
#                    label = train_label
#   )
#   
#   smallest_error<-as.data.frame(1-min(modelcv$evaluation_log$validation_error))
#   highest_auc<-as.data.frame(max(modelcv$evaluation_log$validation_auc))
#   highest_auc_list[[row]]<-highest_auc
#   smallest_error_list[[row]]<-smallest_error
# }
# 
# #Create dataframe that contains all accuracy & auc measures
# smallest_error_df = do.call(rbind,smallest_error_list)
# highest_auc_df = do.call(rbind,highest_auc_list)
# 
# #Combine accuracy, auc and random hyperparameter values into one dataframe
# randomsearch_values = cbind(smallest_error_df, highest_auc_df, parameters_df)
# 
# #Highest accuracy
# max(randomsearch_values$'1 - min(modelcv$evaluation_log$validation_error)')
# 
# #Highest AUC
# max(randomsearch_values$'max(modelcv$evaluation_log$validation_auc)')
# 
# #Collect results
# write_csv(randomsearch_values,"randomsearch_output.csv") #Write output in a csv so that calculation does not have to be repeated
# 
# #Import csv file again
# randomsearch_values<-read_csv("randomsearch_output.csv") 
# 
# 
# #Prepare table -> Accuracy
# randomsearch_values<- as.data.frame(randomsearch_values) %>%
#   rename(AUC= 'max(modelcv$evaluation_log$validation_auc)',
#          Accuracy = '1 - min(modelcv$evaluation_log$validation_error)') %>%
#   arrange(-AUC, -Accuracy)%>%
#   dplyr::select(-booster,-objective)%>%
#   mutate(AUC=round(AUC,3),#Round values for better readability
#          Accuracy=round(Accuracy,3),
#          eta=round(eta,3),
#          subsample=round(subsample,3),
#          colsample_bytree=round(colsample_bytree,3)
#   )
# 
# #Use stargazer to nicely format results for comparison
# stargazer(randomsearch_values[1:3,],summary=FALSE, covariate.labels = c("Model", "AUC", "Max depth", "Eta", "Subsample", "Early stopping rounds", "Colsample by tree", "Min child weight"), type = "html",out="Random_Search_top_3.html")
# 
# 
# #Run the hyperparameter tuned model with the best performing value from the random search (first row) -> xgb more advanced than xgboost
# set.seed(12345)
# 
# 
# ptm <- proc.time()
# xgb_hyptuned <- xgboost(data=train_m, 
#                         booster="gbtree",#default
#                         max_depth= randomsearch_values[1,]$max_depth, #Granularity of the model
#                         eta= randomsearch_values[1,]$eta, #Learning rate
#                         subsample=  randomsearch_values[1,]$subsample, #when <1, you’re also bagging the trees
#                         early_stopping_rounds = randomsearch_values[1,]$early_stopping_rounds,
#                         colsample_bytree=randomsearch_values[1,]$colsample_bytree,
#                         min_child_weight=randomsearch_values[1,]$min_child_weight,
#                         #nrounds=600, #Iterations
#                         nrounds=randomsearch_values[1,]$nrounds,
#                         objective="binary:logistic", #needed when target variable is binary
#                         eval_metric="auc",
#                         #watchlist=list(train=train_m,validation=valid_m),#Specifying the watchlist
#                         #maximize=TRUE,
#                         label = train_label) #when we want to see in-between-steps
# 
# 
# 
# print(xgb_hyptuned,verbose=TRUE)
# xgb_hyptuned
# 
# #save hyperparameter-tuned xgb model
# xgb.save(xgb_hyptuned, 'xgb_hyptuned.model')




############## Tuning End




xgb <- xgb_hyptuned


# feature performance and relevance
importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix, cex=1.2)




xgb.plot.tree(model=xgb, trees=0, show_node_id=TRUE)


# Make predictions
x.evaluate$pred_xgb <- predict(xgb, newdata = valid_m)
x.evaluate$pred_xgb_factor <- factor(ifelse(x.evaluate$pred_xgb>probthres,1,0))


summary(x.evaluate$pred_xgb_factor)




### Check accuracy with the confusion matrix ####


conf_matrix_xgb<-confusionMatrix(as.factor(x.evaluate$pred_xgb_factor),as.factor(x.evaluate$ch_in), 
                                 positive="1", # which value is what we're trying to predict? Here, 1 (ch_in)
                                 dnn = c("Prediction", "True Data"))


conf_matrix_xgb
conf_matrix_xgb$byClass["F1"]






roc <- roc(x.evaluate$ch_in,x.evaluate$pred_xgb, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)




########Evaluate which features are important for check-in
#Variable importance XGBoost
#Feature performance and relevance
importance_matrix <- xgb.importance(model = xgb)
importance_matrix = importance_matrix %>%
  select(-Cover, -Frequency)%>%
  top_n(10)
print(importance_matrix)


importance_plot <- xgb.ggplot.importance(as.data.table(importance_matrix), n_clusters=1, cex=0.8) + labs(title = "Feature importance base") + theme(legend.position = "none")




xgb.plot.tree(model=xgb, trees=0, show_node_id=TRUE)




########open the "black box" of the algorithm to determine which features were best for check in prediction


# **SHAP summary plots, use validation data set (smaller and faster)**


shap_long <- shap.prep(xgb_model = xgb, X_train = as.matrix(valid_m))
shap.plot.summary(shap_long)
#set boundaries for the x axes
shap.plot.summary(shap_long, x_bound  = 2)


# or directly
# shap.plot.summary.wrap1(xgb, X = as.matrix(valid_m)) #All
shap.plot.summary.wrap1(xgb, X = as.matrix(valid_m), top_n = 15) #Top 15












############ RANDOM FOREST




## Hyperparameter Tuning using random search  --> Random Forest!


#Do this for each model!#


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# ptm <- proc.time()
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF",
                   tuneGrid = expand.grid(
                     mtry = sample(1:25,5)),#https://stats.stackexchange.com/questions/395156/random-forest-vs-random-forest-tuned-with-caret
                   trainControl(method="cv", number = 10,
                                summaryFunction = twoClassSummary2),
                   metric = "altF1",
)




stopCluster(cl)




x.modelRF #mtry = 9 (Accuracy)
# mtry = 9 (F1)






#############################


cl <- makeCluster(detectCores())
registerDoParallel(cl)


ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF",
                   tuneGrid = expand.grid(
                     mtry = c(9)#With above found hyperparameter
                   ),
                   trainControl(summaryFunction = twoClassSummary2),
                   metric = "altF1") 


# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")


x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))




# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))


# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]


# to see the importance of the variables
print(varImp(x.modelRF))




RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")


TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)




# SOME Summarizing plots:


OverallTDL <- c(LogitOutput$TDL,NBOutput$TDL,KNNOutput$TDL,SVMOutput$TDL,
                TreeOutput$TDL, BaggingOutput$TDL,
                BoostingOutput$TDL,
                RFOutput$TDL,NNetOutput$TDL)


OverallGINI <- c(LogitOutput$GINI,NBOutput$GINI,KNNOutput$GINI,SVMOutput$GINI,
                 TreeOutput$GINI,BaggingOutput$GINI,
                 BoostingOutput$GINI,
                 RFOutput$GINI,NNetOutput$GINI)


ForGraph <- data.frame(OverallTDL,OverallGINI)


myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))


myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)


ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)


op <- par(mar = c(5,4,4,4) + 0.1)


barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), 
        beside = TRUE, yaxt = "n", 
        names.arg = c("Logit","Naive Bayes","KNN","SVM","Tree","Bagging", "Boosting","Random Forest","Neural Network")
        , ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift", legend = c("TDL","GINI"), main="Performance of the Machine Learning Algorithms",
        args.legend = list(x = "topleft",inset = c( 0,0.5), cex = 0.8))


axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)


axis(4, at = myRightAxisAt, labels = myRightAxisLabs)


mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))


mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NBOutput$TimeElapsed,digits=2),"sec"), 
        paste(round(KNNOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,
                                                                                                               14, 
                                                                                                               17,20,23,26))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(NBOutput$PercCorrect,digits=0),"%"), 
        paste(round(KNNOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,
                                                                                                             14, 
                                                                                                             17,20,23,26))


mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)


#Remove the xgb columns for the lift chart
x.evaluate.lift <- x.evaluate %>%
  select(-pred_xgb, -pred_xgb_factor)




lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit,data=x.evaluate.lift,class="ch_in")


ggplot(lift_obj)




######### Save image
save.image("DSMA.RData")
