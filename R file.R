setwd(dir = "G:/LILTHOMA/Rise_Wpu/Mentor_Mind/Identify premium pricing attributes for home insurance using R")
getwd()
library(ggplot2)
library(dplyr)
library(lubridate)
library(skimr)
home_df=read.csv("home_insurance.csv")
View(home_df)
names(home_df)
str(home_df)
nrow(home_df)

#Lot of Blank rows are present should be converted to NA
home_df[home_df==""]<-NA   #Converting Blank rows to NA

#Quote_Date is in the format m/d/y convert it to the correct format

home_df <- home_df %>%
  mutate(QUOTE_DATE = as.Date(QUOTE_DATE, format = "%m/%d/%Y")) %>%
  mutate(QUOTE_DATE = format(QUOTE_DATE, "%d/%m/%Y"))


#FINDING MISSING VALUES PERCENTAGE IN EACH COLUMNS
missing_values=data.frame(colSums(is.na(home_df)))%>%rename("nulls"="colSums.is.na.home_df..")
missing_values=missing_values%>%mutate(percentage=round(nulls/nrow(home_df)*100,2))
missing_values_final <- (subset(missing_values,percentage > 0)%>%arrange(by=percentage))
missing_values_final
rownames(missing_values_final)


#MTA_FAP ,MTA_APRP,CAMPAIGN_DESC,#PAYMENT_FREQUENCY#CLERICAL#P1_PT_EMP_STATUS has more than 50 % values missing
#RISK_RATED_AREA_C&B has 45% and 29% missing values
#QUOTE_DATE has 49.5% missing and MTA_DATE has 89.62% missing values
#others in missing_values_final has 26.20 and LAST_ANN_PREM_GROSS has 

home_df=home_df%>%select(-c(CLERICAL,P1_PT_EMP_STATUS,CAMPAIGN_DESC,MTA_DATE))

unique(home_df$PAYMENT_FREQUENCY)
home_df$PAYMENT_FREQUENCY=replace(home_df$PAYMENT_FREQUENCY,is.na(home_df$PAYMENT_FREQUENCY),0)
list_0_impute <- c("MTA_FAP","MTA_APRP","PAYMENT_FREQUENCY")

for (col in list_0_impute){
  if (any(is.na(home_df[col]))){
    home_df[[col]][is.na(home_df[[col]])] <- 0
  }
}

#Checking for Duplicate Values
sum(duplicated(home_df))



#Imputing Date column


#To see if there is any pattern between quote_date and starting date of coverage
date_cols <-home_df%>%select(QUOTE_DATE,COVER_START)
date_right <- date_cols%>%mutate(
  QUOTE_DATE=parse_date_time(date_cols$QUOTE_DATE,orders = "dmy"),
  COVER_START=parse_date_time(date_cols$COVER_START,orders = "dmy")
)%>%mutate(time_dif=difftime(COVER_START,QUOTE_DATE,units="days"))
summary(date_right)
#There is not such a pattern to be found
#To impute missing date values in R, you can utilize the na.locf() 
#function from the zoo package, which stands for "last observation carried forward."
# install.packages('zoo') :installing zoo package
library(zoo)

home_df_og=home_df #To save the current format to 

home_df$QUOTE_DATE<-na.locf(home_df$QUOTE_DATE)

home_df <-home_df%>%select(-COVER_START)

names(home_df)

#Imputing RISK_RATED_AREA_B

#RISK_RATED_AREA_B has 49% missing value we will treat this and all other missing values are
# 29% which is not significat so we can drop them so as to not indroduce
#anymore bias into the data
hist(home_df$RISK_RATED_AREA_B,col = "green",breaks = 80)
boxplot(home_df$RISK_RATED_AREA_B)
#By looking at the histogram and boxplot we can see its normally distributed with 
#some outliers We will just impute these with median
home_df$RISK_RATED_AREA_B[is.na(home_df$RISK_RATED_AREA_B)] <-median(home_df$RISK_RATED_AREA_B,na.rm = T,trim=1)



##################################################################################
##################################################################################
####################################################################################
###################################################################################
##################################################################################


#EDA

#We have handled most of missing values only 26% are now missing so we are dropping and 
#storing them and doing the analysis on the rest of the cleaned data
#

rows_with_na <- home_df %>%
  filter_all(any_vars(is.na(.)))   #Storing NA's
colSums(is.na(rows_with_na))  

rows_without_na <-home_df %>%
  filter_all(all_vars(!is.na(.)))   #Dropping NA's
colSums(is.na(rows_without_na))

cleaned_df <- rows_without_na

df=cleaned_df

#########################################################################################
#########################################################################################
#* Average permium of each in categorical features
#* Correlation among all features




#########Categorical Data Handling and Analysis



# Assuming your dataframe is named 'df' and your target variable is 'SPEC_ITEM_PREM'
selected_data <- df %>%
  select_if(is.character)              
premiums_=df%>%select(c(Police,SPEC_ITEM_PREM))
cat_data=inner_join(selected_data,premiums_,by="Police")
col_names=names(cat_data) 
col_names=col_names[-c(1,9,39,38)]
col_names=list(col_names)   #list of categories excluding date,policy,premium

looper=unlist(col_names)  #Beacause i 

looper    #This is a feature that will be used to loop over all the categorical features

#For loop showing average_premium and percenatage of values present

grouped_list <- list()

for (col in looper) {
  # Group the data and calculate the mean
  grouped_data <- cat_data %>%
    group_by_at(vars(col)) %>%
    summarise(avg_premium = mean(SPEC_ITEM_PREM), counts = n(), .groups = "drop") %>%
    mutate(percentage = counts / nrow(cat_data) * 100) %>%
    select(col, avg_premium, percentage)
  
  # Add the data to the list
  grouped_list[[col]] <-grouped_data
}
#This list contain the data derived from above loop were all categorical columns element 
#heave been compared of how much average premium are they generating and what are their 
#percentage the dataset
grouped_list


#skimr function useful to understand the data
skim_to_wide(df)

#Quote date is of no use so we extract the month from the quote to understand any relations

df$quote_month <-month(df$QUOTE_DATE)   #Quote_month                         

df$P1_DOB <-dmy(df$P1_DOB)
df$age <- round(difftime(today(),df$P1_DOB,units="days")/365)  #Age column

df <- df[-c(1,18,61)] # Removing quote date dob and policy number 
View(df)
############################################
#####Correlation analysis#####################
#################################################
###################################################
#One hot encoding binary categories and using factors for catgeories with many features
#Combine all of these with existing numerical features and find correlation with Premium
str(df)
#binary categories
binary_vars <-df%>%
  select_if(function(x) is.logical(x) | all(x %in% c("Y","N")))%>%
  mutate(across(everything(),~ifelse(.=="Y",1,0)))

#categorical not binary
categorical_vars <- df %>%
  select_if(function(x) is.character(x) && n_distinct(x) > 2) 
#mutate(across(everything(),function(x) as.factor(x)))  this not ablt to convert as numeric

str(categorical_vars)
for (col in names(categorical_vars)){
  categorical_vars[[col]] <-as.numeric(as.factor(categorical_vars[[col]]))
}
str(categorical_vars)

str(df)
numeric_vars <-df%>%
  select_if(function(x) is.numeric(x))
View(numeric_vars)
combined_df <- cbind(categorical_vars,binary_vars,numeric_vars)
names(combined_df)
str(combined_df)
correlation_matrix <- cor(combined_df)
correalation_df <-data.frame(correlation_matrix)
correalation_df%>%select(SPEC_ITEM_PREM)
#From the above correlation data we can take ones who have a strong negative or postive 
#correlation one with low positive and negatives wont be requried
#Age not select yet

###########################################################
#############################################################
###############################################################






#############################################################
#################################################################
#################################FINDINGS ...........................
#######################################################################
##########################################################################
# MTA_FAP LAST_ANNUAL_PREM SAFE_INSTALLED shows high correlataion these must 
# Influence the premium amount paid significatly

#for the different policy status the highest average premium was paid by cancelled
#policies second is lapsed which us 27% of the data 70% is live
#anova to find out if there is any significance

#For the policies that opted for MTA_FLAG are paying a higher premium but is only 29% of data
#Ttest or Ztests

#Keycare add_on pays higher but is only 4% of the population
#ttests
#Garden_add_on behaves similarly to key are add_on

#Legal_add on's post and pre also pays a higher premium

#Payment method variables anova

#Almost 100 percentage of clients are of PH occupation status

#Not flood proof property has a higher average premium

#Houses with installed Alarms pay higher average premium logically 
#it should be the opposite it maybe beacause 92% of policies not have alarms

#Houses where Bus can be used pays higher but is only 1% percentage

#Employee status Anova should be conducted before any conclusions

#Clients who made claims in the last 3 years have to pay higher premium



#Anova Tests


#Anova among st Employee status
names(df)
employee_status=df%>%select(P1_EMP_STATUS,SPEC_ITEM_PREM)

emp_values=unique(df$P1_EMP_STATUS)
employee_status_df =list()

for (i in emp_values){
  theresult <-employee_status%>%filter(P1_EMP_STATUS==i)%>%select(SPEC_ITEM_PREM)%>%
    sample_n(size = 15,replace = F)
  vectors <-unlist(theresult)
  
  employee_status_df[[i]] <-theresult
}
names(employee_status_df)

employee_aov_df <-data.frame(employee_status_df) #We have got the right format with no names
employee_stack <-stack(employee_aov_df)
names(employee_stack)
summary(aov(values~ind,data=employee_stack))
#P_val greater than 0.05 and #f _critical greater than f_value
#We accept the null and conclude there is no significant difference
#In the Premium Paid by different client from different employee_status



#Anova amogst payment methods and their premium paid
pay_methods <- df%>%select(PAYMENT_METHOD,SPEC_ITEM_PREM)
pay_values=unique(df$PAYMENT_METHOD)
pay_status =list()

for (i in pay_values){
  theresult <-pay_methods%>%filter(PAYMENT_METHOD==i)%>%select(SPEC_ITEM_PREM)%>%
    sample_n(size = 30,replace = F)
  vectors <-unlist(theresult)
  
  pay_status[[i]] <-theresult
}
names(pay_status)

pay_aov_df <-data.frame(pay_status) #We have got the right format with no names
pay_stack <-stack(pay_aov_df)
names(pay_stack)
summary(aov(values~ind,data=pay_stack))
#P value greater tha signifincae indicating accepting null p val 0.29 fval :1.277
#F critical also implying the same 
#conclusion is that there is no significant difference
#lets trying increasing sample size p val as come down to 0.17 


#Anova amogst different policy status
names(df)
pol_methods <- df%>%select(POL_STATUS,SPEC_ITEM_PREM)
pol_values=unique(df$POL_STATUS)
pol_status =list()
for (i in pol_values){
  theresult <-pol_methods%>%filter(POL_STATUS==i)%>%select(SPEC_ITEM_PREM)%>%
    sample_n(size = 15,replace = F)
  vectors <-unlist(theresult)
  
  pol_status[[i]] <-theresult
}

names(pol_status)

pol_aov_df <-data.frame(pol_status) #We have got the right format with no names
pol_stack <-stack(pol_aov_df)
names(pol_stack)
summary(aov(values~ind,data=pol_stack))
#P value less than 0.05
#conclude that there is not significant difference in means of these policy  status


#for policy status,employee_status and payment method
#There is no significant difference is means of the categories in these columns
#So these are not a strong indicator eventhough the below data tells a different story
#It is no statistically significant #These has only a small_influence


grouped_list$POL_STATUS        #    -0.05%

grouped_list$P1_EMP_STATUS     #    -0.05%

grouped_list$PAYMENT_METHOD    #    +0.01%

grouped_list$BUS_USE           #    +0.04

grouped_list$SAFE_INSTALLED    #    +0.31%

#Ttest  

#Safe installed t test

safe_df <-df%>%select(SAFE_INSTALLED,SPEC_ITEM_PREM)
safe_df_names <-unique(df$SAFE_INSTALLED)
safe_list <- list()
for (col in safe_df_names){
  safe_result <-safe_df%>%filter(SAFE_INSTALLED==col&SPEC_ITEM_PREM>0)%>%
    select(SPEC_ITEM_PREM)%>%sample_n(size=25,replace=FALSE)
  safe_list[[col]]<-safe_result
}
names(safe_list)
t_safe_df <- data.frame(safe_list)
names(t_safe_df)
t.test(t_safe_df$SPEC_ITEM_PREM,t_safe_df$SPEC_ITEM_PREM.1,mu=0)
#p val is less than signifacne so we reject the null hypothesis
#conclude that there is signifincant difference in the premium amout paid by
#safe installed and not installed

#the sample data showing houses with safety installed pays more premium which
#doesnt seem that logical so to again check for any patterns we will 

#find the mean of the premium amount which is greater than 0
#compare with the sample for houses with safe installed

the_val <-df%>%select(SPEC_ITEM_PREM)%>%filter(SPEC_ITEM_PREM >0)%>%summarise(mean=mean(SPEC_ITEM_PREM))
t.test(t_safe_df$SPEC_ITEM_PREM,mu=20.7332,alternative = ("greater"))
#P val less than 0.05 
#So we conclude that houses with safe installed pays greater premium



#Bus Opt t test

bus_df <-df%>%select(BUS_USE,SPEC_ITEM_PREM)
bus_df_names <-unique(df$BUS_USE)
bus_list <- list()
for (col in bus_df_names){
  bus_result <-bus_df%>%filter(BUS_USE==col&SPEC_ITEM_PREM>0)%>%
    select(SPEC_ITEM_PREM)%>%sample_n(size=25,replace=FALSE)
  bus_list[[col]]<-bus_result
}
t_bus_df <- data.frame(bus_list)
names(t_bus_df)
t.test(t_bus_df$SPEC_ITEM_PREM,t_bus_df$SPEC_ITEM_PREM.1,mu=0)

#P value greater than significance so we accpet the nul.
#no signifincat diiference in premium paid by homes with bus route and without bus route



#ChiSquare Test of Independence /Association Tests


#Function to find chisq test summaries for input dataframe and the two categorical columns
chi_test <-function(df,x,y){
  thedata <-df%>%select({{x}},{{y}})
  tbles <-table(thedata)
  return(chisq.test(tbles))
}
thedata <-df%>%select(CLAIM3YEARS,POL_STATUS)
tble <-table(thedata)
chisq.test(tble)
chi_test(df,CLAIM3YEARS,POL_STATUS)
#null : No Association 
#alterntive : There is Association
#Pval is less than significance so we conclude that there is association
names(df)

chi_test(df,"BUS_USE","SAFE_INSTALLED")
#P val less than 0.05 so reject the null and say that there is association

chi_test(df,"POL_STATUS","PAYMENT_METHOD")
#P val less than 0.05 so reject the null and say that there is association

chi_test(df,"POL_STATUS","SAFE_INSTALLED")
#P val less than 0.05 so reject the null and say that there is association

chi_test(df,"POL_STATUS","OCC_STATUS")
#P val is slighly greater than 0.05 so accept the null and say that there is association
#We conclude that there is slight independnce amoung these



##################################################
############################## END EDA #################
###############################################################


'''
DATA VISUALISATION IDEAS

* Client age and Policy_duration      SCATTER PLOT
* Year_Built and Premium              BARPLOT OR SCATTERPLOT
* Claim last Years and Premium        BARPLOT
* Last Claimed amount and Premium     SCATTERPLOT
* Relation between quote_month and premium and counts of Premium      SCATTERPLLOT
* All categorical features againts premium      BOXPLOTS
'''