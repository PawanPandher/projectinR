install.packages("dplyr");
library(dplyr);
install.packages("PerformanceAnalytics");
library(PerformanceAnalytics);

#Step 1: Load & View the Data
burgundy<-read.csv("C:\\Users\\iamak\\Downloads\\BurgundySip.csv",na.strings =c("","NA"));
#burgundy <- read.csv(file = "C:\\Users\\pawan\\Documents\\pawandatacurrent\\module2\\Project\\BurgundySip.csv",  na.strings = c("","NA"))
burgundy;
summary(burgundy);                   
str(burgundy);
head(burgundy)

# Change the variable names
colnames(burgundy)<-c("SerialNo","WineryName","WineName","HarvestYear","Region","Variety",
                     "Rating","numberOfTesters","Price","BodyScore","AcidityScore",
                     "ResidualSugar","AlcoholPercent","Density");


#Order the Dataset
burgundy<- burgundy[order(burgundy$SerialNo),];
burgundy;

#Checking dataset for Duplicates
ifelse (!anyDuplicated(burgundy$SerialNo),
        "No duplicated observations found.",
        "Duplicated observations found.");

#Total number of duplicates according to SerialNo
sum(duplicated(burgundy$SerialNo));


# burgundycopy<- burgundy[duplicated(burgundy$SerialNo),];
# burgundycopy;

#Selecting,analyzing and display all the duplicates  
dups <-
  duplicated(burgundy$SerialNo) |
  duplicated(burgundy$SerialNo, fromLast=TRUE);
dups;
burgundaydups<-burgundy[dups,];

#Deleting the duplicates
#To remove the duplicates according the SerialNo we choose Bottom to top approach
#because when analysed the duplicated observations for each SerialNo we have found 
#that ResidualSugar level is decreasing with every next sample, according to the fact 
#the ResidualSugar level decreases during fermentation.So we assume the last observation 
#is when fermentation stops and that is the final Residual Sugar level for wine. 
burgundyclean<-burgundy[!duplicated(burgundy$SerialNo,fromLast = TRUE),];
burgundyclean;  #clean data set
summary(burgundyclean);
str(burgundyclean);


#Factorizing variables WineryName,WineName,Region and Variety
burgundyclean[,c(2,3,5,6)]<-  lapply(burgundyclean[,c(2,3,5,6)],factor)

# #converting the HarvestYear to type integer
# burgundyclean$HarvestYear<-as.integer(burgundyclean$HarvestYear)

#Converting ResidualSugar, AlcoholPercent and Density as numeric
burgundyclean[,c(12,13,14)]<-  lapply(burgundyclean[,c(12,13,14)],as.numeric)

burgundyclean;
summary(burgundyclean);
str(burgundyclean);

##variable analysis

# $ SerialNo       : chr  "N.V." "W1001-62187" "W1003-83159" "W1007-31751" ...[Not relevent]
# $ WineryName     : Factor w/ 480 levels "A Coroa","Aalto",..: 160 319 385 287 386 233 270 435 383 319 ...[INDEPENDENT]
# $ WineName       : Factor w/ 844 levels "1194","17","1730 Amontillado",..: 618 261 496 53 772 487 810 615 822 261 ...[INDEPENDENT]
# $ HarvestYear    : int  2016 2019 2011 2018 2020 NA 2016 2017 1981 2019 ...[INDEPENDENT]
# $ Region         : Factor w/ 76 levels "Abona","Alella",..: 60 70 60 9 57 33 60 16 60 70 ...[INDEPENDENT]
# $ Variety        : Factor w/ 21 levels "Albarino","Cabernet Sauvignon",..: 13 20 13 6 NA 16 13 19 13 20 ...[Dependent]
# $ Rating         : num  4.02 4.1 4.1 4.01 3.96 4.31 3.99 4.16 4.45 4.1 ...[Dependent]
# $ numberOfTesters: int  392 400 402 415 407 1452 3825 54 66 400 ...[INDEPENDENT]
# $ Price          : num  20 27.9 28.5 17.9 11.9 ...[Dependent]
# $ BodyScore      : int  4 5 4 3 NA 4 4 4 4 5 ...[Dependent]
# $ AcidityScore   : int  3 3 3 3 NA 3 3 2 3 3 ...[Dependent]
# $ ResidualSugar  : num  10.21 9.68 9.39 10.64 11.77 ...[Dependent]
# $ AlcoholPercent : num  10.8 11.7 11.3 11.5 10.5 ...[Dependent]
# $ Density        : num  0.997 0.996 0.995 0.995 0.996 ...[Dependent]


 

# •	SN: Serial number of the sample.
#We have changed the name of this variable as "SerialNo". This variable represents the 
#sample number of wine.It is character type variable. N.V. stands for "not vintage"
str(burgundyclean$SerialNo);
burgundyclean$SerialNo;
cat("The Total number of missing values in variable 'SerialNo' =",sum(is.na(burgundyclean$SerialNo)));

# •	NAME: Winery name
#We have changed the name of this variable as "WineryName". This variable represents the 
#name of the winery where the particular wine is made. It was character type variable.
#We had factorize it at line number 56.
levels(burgundyclean$WineryName);
str(burgundyclean$WineryName);
burgundyclean$WineryName;
cat("The Total number of missing values in variable 'WineryName' =",sum(is.na(burgundyclean$WineryName)));

# •	WINE: Name of the wine
#We have changed the name of this variable as "WineName". This variable represents the 
#name of. It was character type variable.We had factorize it at line number 56.
levels(burgundyclean$WineName);
str(burgundyclean$WineName);
burgundyclean$WineName;
cat("The Total number of missing values in variable 'WineName' =",sum(is.na(burgundyclean$WineName)));

# •	YR: Year in which the grapes were harvested – indicator of the AGE of the wine. 
#We have changed the name of this variable as "HarvestYear". we had changed its type to integer 
#at line number 59. 
str(burgundyclean$HarvestYear);
summary(burgundyclean$HarvestYear);
burgundyclean$HarvestYear;
cat("The Total number of missing values in variable 'HarvestYear' =",sum(is.na(burgundyclean$HarvestYear)));
#boxplot(na.omit(burgundyclean$HarvestYear),col=100);

# •	RT: Average rating given to the wine by the test users [from 1-5]
#We have changed the name of this variable as "Rating".
str(burgundyclean$Rating);
summary(burgundyclean$Rating);
burgundyclean$Rating;
cat("The Total number of missing values in variable 'Rating' =",sum(is.na(burgundyclean$Rating)));
boxplot(na.omit(burgundyclean$Rating),col=100);

# •	NUMR: Number of testers that reviewed the wine
#We have changed the name of this variable as "numberOfTesters".
summary(burgundyclean$numberOfTesters);
burgundyclean$numberOfTesters;
cat("The Total number of missing values in variable 'numberOfTesters' =",sum(is.na(burgundyclean$numberOfTesters)));
boxplot(na.omit(burgundyclean$numberOfTesters),col=100);

# •	REG: Region of the wine
#We have changed the name of this variable as "Region" and factorize it at line number 56
levels(burgundyclean$Region);
burgundyclean$Region;
cat("The Total number of missing values in variable 'Region' =",sum(is.na(burgundyclean$Region)));


# •	PR: Price in euros [€]
#We have changed the name of this variable as "Price".
summary(burgundyclean$Price);
burgundyclean$Price;
cat("The Total number of missing values in variable 'Price' =",sum(is.na(burgundyclean$Price)));
boxplot(na.omit(burgundyclean$Price),col=100);


# •	TP: Wine variety
#We have changed the name of this variable as "Variety" and factorize it at line number 56
levels(burgundyclean$Variety);
burgundyclean$Variety;
cat("The Total number of missing values in variable 'Variety' =",sum(is.na(burgundyclean$Variety)));

# •	BD: Body score, defined as the richness and weight of the wine in your mouth [from 1-5]
#We have changed the name of this variable as "Rating". 
str(burgundyclean$Rating);
summary(burgundyclean$Rating);
burgundyclean$Rating;
cat("The Total number of missing values in variable 'Rating' =",sum(is.na(burgundyclean$Rating)));
boxplot(na.omit(burgundyclean$Rating),col=100);

# •	ACD: Acidity score, defined as wine's “pucker” or tartness; it's what makes a wine refreshing 
# and your tongue salivate and want another sip [from 1-5]
#We have changed the name of this variable as "AcidityScore". 
str(burgundyclean$AcidityScore);
summary(burgundyclean$AcidityScore);
burgundyclean$AcidityScore;
cat("The Total number of missing values in variable 'AcidityScore' =",sum(is.na(burgundyclean$AcidityScore)));
boxplot(na.omit(burgundyclean$AcidityScore),col=100);

# •	RSG: residual sugar level of the wine [from 0 -16] 
# We have changed the name of this variable as "ResidualSugar". we had changed its type to numeric 
#at line number 62.
str(burgundyclean$ResidualSugar);
summary(burgundyclean$ResidualSugar);
burgundyclean$ResidualSugar;
cat("The Total number of missing values in variable 'ResidualSugar' =",sum(is.na(burgundyclean$ResidualSugar)));
boxplot(na.omit(burgundyclean$ResidualSugar),col=100);
hist(burgundyclean$ResidualSugar, col=100);

# Rating is decreasing if residual sugar increases 
#so wine with less sugar is better
plot(burgundyclean$ResidualSugar,burgundyclean$Rating,col=100);


# •	AL: Alcohol percentage of the wine. 
# We have changed the name of this variable as "AlcoholPercent". we had changed its type to numeric 
#at line number 62.
str(burgundyclean$AlcoholPercent);
summary(burgundyclean$AlcoholPercent);
burgundyclean$AlcoholPercent;
cat("The Total number of missing values in variable 'AlcoholPercent' =",sum(is.na(burgundyclean$AlcoholPercent)));
boxplot(na.omit(burgundyclean$AlcoholPercent),col=100);
hist(burgundyclean$AlcoholPercent, col=100);

# •	DN: The typical density or specific gravity of the wine is generally between 1.080 and 1.090.
#We have changed the name of this variable as "Density". we had changed its type to numeric 
#at line number 62.
str(burgundyclean$Density);
summary(burgundyclean$Density);
burgundyclean$Density;
cat("The Total number of missing values in variable 'Density' =",sum(is.na(burgundyclean$Density)));
boxplot(na.omit(burgundyclean$Density),col=100);
hist(burgundyclean$Density, col=100);


# Outlier treatment

# Outlier treatment for numberOfTesters
boxplot(burgundyclean$numberOfTesters)
summary(burgundyclean$numberOfTesters);
q1 <- summary(burgundyclean$numberOfTesters)[2];
q3 <- summary(burgundyclean$numberOfTesters)[5];
iqr <- q3-q1;
iqr;
names(iqr) <- "IQR"
c(q1, q3, iqr);
upper_bound <- q3+1.5*iqr; 
names(upper_bound) <- "upper_bound";
c(q1,  iqr, q3,upper_bound);

burgundyclean$numberOfTesters[(burgundyclean$numberOfTesters>upper_bound)& 
                                !is.na(burgundyclean$numberOfTesters)]<-upper_bound;

boxplot(burgundyclean$numberOfTesters)
summary(burgundyclean$numberOfTesters);
outlier<-boxplot.stats(burgundyclean$numberOfTesters)$out






#Alcohol percentage ou

boxplot(burgundyclean$AlcoholPercent);
boxplot.stats(burgundyclean$AlcoholPercent);
summary(burgundyclean$AlcoholPercent);  #Maximum is 14.021


burgundyclean %>% 
  na.omit()%>%
  select_if(is.numeric) %>%
  boxplot()




#Correlation Charts
install.packages("ggcorrplot");
install.packages("GGally")
library(ggplot2);
library(ggcorrplot);
library(GGally);
burgundyclean %>%
  select_if(is.numeric) %>%
  chart.Correlation();


burgundyclean %>% 
  na.omit() %>% 
  select_if(is.numeric) %>%
  cor() %>%
  round(3) %>% 
  ggcorrplot( hc.order =TRUE,type ="lower", lab =TRUE);





#boxplot.stats()
aggregate(Price ~ Region, data = burgundyclean, FUN = sum)
a<-aggregate(numberOfTesters ~ Region , data = burgundyclean, FUN = sum);
i<-order(a$numberOfTesters)
a[i,]
hist(burgundyclean$Rating);
plot(burgundyclean$AL,burgundyclean$RT,pch=22,bg=7,xlab="Alcohol Percentage",ylab="Rating",col="#00AFBB");
#using ggplot
ggplot(burgundyclean, aes(x=Price, y=Rating)) +geom_point(size=1, shape=23,bg= 7,col="#00AFBB");


rioja<-burgundyclean[burgundyclean$Region=="Rioja",]
summary(rioja)




options(repr.plot.width=7.5, repr.plot.height=6)  #Setting the plot size

theme_set(theme_minimal(10))
## Defining the variables we will explore:
variables <- c('Price', 'Rating','ResidualSugar',
               'AlcoholPercent', 'BodyScore')

## Plotting ggpairs for the selected attributed:
ggpairs(burgundyclean[variables], aes(alpha=0.3))

ggpairs(burgundyclean)+theme_bw()


#Missing value treatment


#Step 4: Calculate the probability of missingness
is.na(burgundyclean);
calculateMissProb <- function(x){
  return(sum(is.na(x))/length(x)*100);
}
apply(burgundyclean,2,calculateMissProb);


#wine name 
summary(burgundyclean);

burgundyclean$WineName;
which(is.na(burgundyclean$WineName),burgundyclean$WineName);
burgundyclean[2255,];
wineryname_Contino<-burgundyclean[c(burgundyclean$WineryName=="Contino"&burgundyclean$Price==61.94&burgundyclean$Rating==4.30&burgundyclean$HarvestYear=="2011"),]$WineName;
wineryname_Contino;
summary(wineryname_Contino[3]);
summary(wineryname_Contino);
burgundyclean[which(is.na(burgundyclean$WineName),burgundyclean$WineName),3]<-"Rioja Graciano";
summary(burgundyclean);

#sN Number
burgundyclean$SerialNo;
burgundyclean[is.na(burgundyclean$SerialNo),];
calculateMissProb(burgundyclean$SerialNo);
burgundyclean<-burgundyclean[!is.na(burgundyclean$SerialNo),];
sum(is.na(burgundyclean$SerialNo))


summary(burgundyclean);
#Region

burgundyclean$Region;
which(is.na(burgundyclean$Region),burgundyclean$Region);
which(is.na(burgundyclean$Region),burgundyclean$Region)[1];
which(is.na(burgundyclean$Region),burgundyclean$Region)[2];
#Printing the observations which is having NA in Region
burgundyclean[which(is.na(burgundyclean$Region),burgundyclean$Region),];
#Treating First Missing Value
#Comparing observations with similar Winery name 
NA_Region_1<-burgundyclean[burgundyclean$WineryName=="Bodegas La Horra",];
NA_Region_1;
summary(NA_Region_1[5]);
burgundyclean[which(is.na(burgundyclean$Region),burgundyclean$Region)[1],5]<-"Ribera del Duero";
summary(burgundyclean);
NA_Region_2<-burgundyclean[burgundyclean$WineryName=="Clos Pons",];
NA_Region_2;
summary(NA_Region_2[5]);
burgundyclean[which(is.na(burgundyclean$Region),burgundyclean$Region),5]<-"Costers del Segre";
summary(burgundyclean);


#Variety
burgundyclean[!is.na(burgundyclean$Variety),];
burgundyclean[is.na(burgundyclean$Variety),];
calculateMissProb(burgundyclean$Variety);
#missing probability is under 5%
#checked relationship of variety with other variable to do imputation
#but it dose not show any valuable findings to replace the missing values
#when checked the missing values in variety, there were NA's in body score and Acidity score on the same observations. 
burgundyclean[is.na(burgundyclean$Variety),c(3,5,6)];
#checked if there is any luck to find the missing value. But couldn't find
summary(burgundyclean[is.na(burgundyclean$Variety),c(3,5,6)])

#Removed observations with NA values in Variety
burgundyclean<-burgundyclean[!is.na(burgundyclean$Variety),];

summary(burgundyclean);

# HarvestYear

#calculating missing Probability
calculateMissProb(burgundyclean$HarvestYear);
#Checking summary for missing Values in Harvest Year 
summary(burgundyclean[is.na(burgundyclean$HarvestYear),])
#logical vector giving identifying NA's in Year column
is.na(burgundyclean$HarvestYear);
#printing missing observations
burgundyclean[is.na(burgundyclean$HarvestYear),];
#treating 1st missing value of Harvest year
#finding a substitue year value by subsetting using same variety and price
yr1<-burgundyclean[burgundyclean$Variety=="Tempranillo"&burgundyclean$Price==40.00,];
yr1;
#Printing matching year value for the replacement
yr1$HarvestYear[2];


#treating 2st missing value of Harvest year
#finding a substitue year value by subsetting using same variety and price
yr2<-burgundyclean[burgundyclean$Variety=="Ribera Del Duero Red"&burgundyclean$Price<=430 &burgundyclean$Price>=420,];
yr2;
na.omit(yr2$HarvestYear);
#Printing matching year value for the replacement
na.omit(yr2$HarvestYear)[1];


#repalcing the 2 NA year with the matching values
burgundyclean[is.na(burgundyclean$HarvestYear),4][c(1,2)]<-c(yr1$HarvestYear[2],na.omit(yr2$HarvestYear)[1]);
burgundyclean[burgundyclean$HarvestYear=="N.V.",];
burgundyclean[,4];
#Repacing NV 's with 2022
burgundyclean$HarvestYear<-gsub("N.V.","2022",burgundyclean$HarvestYear);
burgundyclean[,4];
##converting the HarvestYear to type integer
burgundyclean$HarvestYear<-as.integer(burgundyclean$HarvestYear);
summary(burgundyclean);
str(burgundyclean);


#Adding age coumn 
transform(burgundyclean,
          Age=2022-HarvestYear);

#Acidity Score
#printing NA's in acidity score in the burgundyclean data set, replacing it with the rounded mean of acidity score.  
is.na(burgundyclean$AcidityScore);
cat("The Total number of missing values in variable 'AcidityScore' =",sum(is.na(burgundyclean$AcidityScore)));
#printing all the observations with missing NA's in The dataset
burgundyclean[is.na(burgundyclean$AcidityScore),];
#mean of the Acidity score excluding NA values
mean(burgundyclean$AcidityScore,na.rm=T);
#Replacing NA values in Acidity score by Mean value after rounding it.
burgundyclean[is.na(burgundyclean$AcidityScore),]$AcidityScore<-round(mean(burgundyclean$AcidityScore,na.rm=T),0);
summary(burgundyclean);

#Body Score

#checking for NA values in body score
is.na(burgundyclean$BodyScore);
cat("The Total number of missing values in variable 'Body Score' =",sum(is.na(burgundyclean$BodyScore)));

#showing values with NA's
burgundyclean[is.na(burgundyclean$BodyScore),];

# BS_NA<-burgundyclean[is.na(burgundyclean$BodyScore),];BS_NA;
# BS_NA[,"BodyScore"]<-0;BS_NA

#aggregating to get the mean of body score comparing with variety 
burgundyclean_BS<-aggregate(BodyScore~Variety,burgundyclean, FUN=mean,na.rm=T);

#merging observations from Burgundyclean with out put from aggregation

BS1<-merge(burgundyclean,burgundyclean_BS,by="Variety");
BS1

#replaced NA's with the imputed values
BS1[is.na(BS1$BodyScore.x),"BodyScore.x"] <- BS1[is.na(BS1$BodyScore.x),"BodyScore.y"];
BS1;
summary(BS1);
#changing the column names
colnames(BS1)[10]<-"BodyScore";
colnames(BS1)
#with the subsetting by removing last column and assigning it to the original dataset.
burgundyclean<-BS1[,-15];
summary(burgundyclean);

#Alcohol Percentage
is.na(burgundyclean$AlcoholPercent);
cat("The Total number of missing values in variable 'Alcohol Percentage ' =",sum(is.na(burgundyclean$AlcoholPercent)));
burgundyclean[is.na(burgundyclean$AlcoholPercent),];
AP_NA<-burgundyclean[is.na(burgundyclean$AlcoholPercent),];AP_NA;
burgundyclean_AP<-aggregate(AlcoholPercent ~Variety + HarvestYear,burgundyclean, FUN=mean,na.rm=T);
burgundyclean_AP;
summary(burgundyclean);

AP1<-merge(burgundyclean,burgundyclean_AP,by = c("HarvestYear", "Variety"));
AP1
summary(burgundyclean);
AP1[is.na(AP1$AlcoholPercent.x),"AlcoholPercent.x"] <- AP1[is.na(AP1$AlcoholPercent.x),"AlcoholPercent.y"];
AP1
summary(AP1);
colnames(AP1)[13]<-"AlcoholPercentage";
colnames(AP1);
burgundyclean<-AP1[,-15];
summary(burgundyclean);


#Density

is.na(burgundyclean$Density);
cat("The Total number of missing values in variable 'Density' =",sum(is.na(burgundyclean$Density)));
burgundyclean[is.na(burgundyclean$Density),];
D_NA<-burgundyclean[is.na(burgundyclean$Density),];D_NA;
burgundyclean_D<-aggregate(Density~AlcoholPercentage, burgundyclean, FUN=mean,na.rm=T);
burgundyclean_D;
summary(burgundyclean);

D1<-merge(burgundyclean,burgundyclean_D,by = "AlcoholPercentage");
D1
summary(burgundyclean);
D1[is.na(D1$Density.x),"Density.x"] <- D1[is.na(D1$Density.x),"Density.y"];
D1
summary(D1);
colnames(D1)[14]<-"Density";
colnames(D1);
burgundyclean<-D1[,-15];
summary(burgundyclean);


#Treating Price
#polynomial Regression 
ggplot(burgundyclean,aes(x=Price,y=Rating))+ geom_point(size=2,shape=22,bg=7,col="#00AFBB")+geom_smooth()
boxplot.stats(na.omit(burgundyclean$Price));
boxplot(na.omit(burgundyclean$Price));
y<-na.omit(burgundyclean);
z<-y[y$Price<=1500,];
z;
boxplot(z$Price);
ggplot(z,aes(x=Price,y=Rating))+ geom_point(size=2,shape=22,bg=7,col="#00AFBB")+geom_smooth()

train<-z[1:nrow(z)*0.9,];train;
nrow(train);
nrow(z);

trainIndex <- sample(nrow(z),nrow(z)*0.9); 
trainIndex;
burgundy_train <- z[trainIndex,];
burgundy_test <- z[-trainIndex,];
nrow(burgundy_train);
nrow(burgundy_test)


burgundy_NLM <- loess(Price~Rating,data=z);
burgundy_NLM;
summary(burgundy_NLM);

predict(burgundy_NLM, burgundy_test );
burgundy_test$Price[1];
predict(burgundy_NLM, burgundy_test )[1];
(abs(burgundy_test$Price-predict(burgundy_NLM, burgundy_test ))/burgundy_test$Price)*100;
modelError<-(abs(burgundy_test$Price-predict(burgundy_NLM, burgundy_test ))/burgundy_test$Price)*100;
modelAccuracy<-round(100-mean(modelError),2);
modelAccuracy;
PredictPrice<-burgundyclean[is.na(burgundyclean$Price),];
predict(burgundy_NLM, PredictPrice );
burgundyclean[is.na(burgundyclean$Price),"Price"]<-predict(burgundy_NLM, PredictPrice );
summary(burgundyclean);

#Residual Sugar
ggplot(burgundyclean,aes(x=ResidualSugar,y=Rating))+ geom_point(size=2,shape=22,bg=7,col="#00AFBB")+geom_smooth()
