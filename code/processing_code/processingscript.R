###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#removing unneeded variables
cleandata1 <- rawdata %>% select(-contains("Score"))
cleandata2 <- cleandata1 %>% select(-contains("FluA"))  
cleandata3 <- cleandata2 %>% select(-contains("FluB")) 
cleandata4 <- cleandata3 %>% select(-contains("Total")) 
cleandata5 <- cleandata4 %>% select(-contains("FluA")) 
cleandata6 <- cleandata5 %>% select(-contains("Dxname")) 
cleandata7 <- cleandata6 %>% select(-contains("Activity")) 
cleandata8 <- cleandata7 %>% select(-"Unique.Visit")

#After cleaning the data according to the instructions we have 32 variables, as expected. 
#We know that one variable, BodyTemp, should be continuous. Let's check the class. 
class(cleandata8$BodyTemp)
#It is numeric, so we should be good to go. 

#Remove missing values
processeddata <- na.omit(cleandata8)

#Now we have 730 observations of 32 variables, as expected. 

#11/4/21 - After learning about model evaluation and machine learning, we will
#do additional pre-processing of the data

#Feature/variable removal
#We want to remove redundant variables by removing yes/no versions of 4 variables
processeddata <- processeddata %>% select(-"CoughYN",
                                          -"WeaknessYN",
                                          -"CoughYN2", 
                                          -"MyalgiaYN")
#code three symptom severity factors as ordinal
#desired order: none < mild < moderate < severe
#checking current levels
factor(processeddata$CoughIntensity)
factor(processeddata$Weakness)
factor(processeddata$Myalgia)
#currently the levels are not ordered

#mutate variables to save factor order
processeddata <- processeddata %>% mutate(CoughIntensity = ordered(processeddata$CoughIntensity, 
                                                                   levels = c("None", "Mild", "Moderate", "Severe")),
                                          Weakness = ordered(processeddata$Weakness, 
                                                                   levels = c("None", "Mild", "Moderate", "Severe")), 
                                          Myalgia = ordered(processeddata$Myalgia, 
                                                                   levels = c("None", "Mild", "Moderate", "Severe")))
#checking factor order
factor(processeddata$CoughIntensity)
factor(processeddata$Weakness)
factor(processeddata$Myalgia)

#remove low variance predictors
#check for variance in predictors
summary(processeddata)
#we'll remove binary predictors with <50 entries in a given category
#remove: Hearing, Vision
processeddata <- processeddata %>% select(-"Hearing",
                                          -"Vision")
#now our processeddata dataframe has 730 observations or 26 variables. This is 
#the data we will use for modeling. 

# save data as RDS
# location to save file 
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processeddata, file = save_data_location)

# for the rest of the analysis, our main continuous outcome of interest is 
# BodyTemp, and our categorical outcome of interest is Nausea
