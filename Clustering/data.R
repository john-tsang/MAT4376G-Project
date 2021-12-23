#The purpose of this file is the creation of the datasets and preparing them
#for clustering

PIMENTO_CASES <- read.csv("~/Documents/MAT4376/PIMENTO_CASES.csv")
PIMENTO_PROGRAMS <- read.csv("~/Documents/MAT4376/PIMENTO_PROGRAMS.csv")

pimento.cases <- PIMENTO_CASES
pimento.programs <- PIMENTO_PROGRAMS

###create 6 different datasets

# Import libraries
library(xts)
library(dplyr)

# Import dataset that contains the total time spent each month for each employee
data <- read.csv("~/Documents/month_year_sum_time_series.csv")

# Create new dataset that combines all data

new <- data %>% group_by(mission_title,month,year) %>%
  summarise(sum = sum(sum))

data <- as.data.frame(new)


# Time series representation

repres <- matrix(nrow = length(unique(data$mission_title)), ncol = 11)

colnames(repres) <- c("ID","mean","middle 90% range", "max",
                      
                      "standard deviation","spectral","median abs % change MoM",
                      
                      "stdev of % change MoM range","lag1 autocorrelation",
                      
                      "average days between timepoints","slope of line of best fit")


# Iterate through each unique employee ID to generate a time series representation


count = 1


# Iterate through each employee

for(i in unique(data$mission_title)){
  
  # subset of the data for just the one employee
  
  mission <- data[data$mission_title==i,]
  
  # date column for time series format
  
  mission$date <- as.Date(paste(mission$year, mission$month, "01", sep="-"), "%Y-%m-%d")
  
  # Create time series
  
  ts <- xts(mission$sum, order.by=mission$date)
  
  # Create percentage change time series from original time series
  
  pc.ts <- na.omit(diff(ts)/stats::lag(ts)*100)
  
  # Ignore employees that have less than 6 months of data
  
  if(nrow(mission) >= 6){
    
    # Employee ID
    
    repres[count,1] <- i
    
    # Mean
    
    repres[count,2] <- mean(mission$sum)
    
    # Range of middle 90% of data
    
    repres[count,3] <- sort(mission$sum)[floor(length(mission$sum)*.95)]-
      
      sort(mission$sum)[ceiling(length(mission$sum)*.05)]
    
    # Maximum hours worked in a month
    
    repres[count,4] <- max(mission$sum)
    
    # Standard deviation of hours worked in a month
    
    repres[count,5] <- sd(mission$sum)
    
    # Spectral Density (frequency with highest spectral density)
    
    spect <- spectrum(ts)
    
    repres[count,6] <- 1/spect$freq[which.max(spect$spec)]
    
    # Median absolute percentage change month over month
    
    repres[count,7] <- median(abs(pc.ts))
    
    # Standard deviation percentage change month over month
    
    repres[count,8] <- sd(pc.ts[is.finite(pc.ts)])
    
    # Lag 1 autocorrelation
    
    repres[count,9] <- acf(ts,1,plot = F)$acf[2]
    
    #  Total length from first to last month worked divided by number of months with data (to detect employment gaps)
    
    repres[count,10] <- as.vector(max(mission$date)-min(mission$date))/length(mission$date)
    
    # Slope of line of best fit
    
    repres[count,11] <- lm(mission$sum~mission$date)$coefficients[2]
    
    count = count + 1
    
  } 
  
}


# Omit NAs at the end of the matrix from the employees that had under 6 months of data
# and save as dataframe

repres <- as.data.frame(na.omit(repres))

#2nd Dataset
# Number of minutes in cases and programs total

V1 <- rowSums(pimento.cases[ , c(10,12,14,16,18,20,22,24,26,28,30,32,34,36)], na.rm=TRUE)

DS1 <- data.frame(pimento.cases$MissionTitleE, V1)

DS1 <- aggregate(DS1$V1, by=list(Category=DS1$pimento.cases.MissionTitleE), FUN=sum)

V2 <- rowSums(pimento.programs[ , c(7,8,9,10,11,12,13,14,15,16,17,18,19,20)], na.rm=TRUE)

DS2 <- data.frame(pimento.programs$MissionTitleE, V2)

DS2 <- aggregate(DS2$V2, by=list(Category=DS2$pimento.programs.MissionTitleE), FUN=sum)

DS3 <- aggregate(merge(DS1, DS2, all = TRUE)$x, by=list(Category=merge(DS1, DS2, all = TRUE)$Category), FUN=sum)

#2nd Dataset Cases Total Minutes
cases.time.table <- aggregate(cbind(Disasters.Time,Death.Time,
                                    Assistance.Communications.Time,Legal.NotaryTime,
                                    Evacuation..Time,Child.Abduction.Custody.Time,
                                    Family.Distress.Time,Registration.Time,
                                    Immigration.Time,Arrest.Time,Citizenship.Time,Passport.Time,
                                    Service.Time,Financial.Assistance.Transfers.Time) ~ MissionTitleE, data = pimento.cases, FUN = sum, na.rm = TRUE)

#3rd Dataset Cases Count Total
cases.count.table <- aggregate(cbind(Other,Disasters,Death,
                                    Assistance.Communications,Legal.Notary,
                                    Evacuation,Child.Abduction.Custody,
                                    Family.Distress,Registration,
                                    Immigration,Arrest,Citizenship,Passport,
                                    Service,Financial.Assistance.Transfers) ~ MissionTitleE, data = pimento.cases, FUN = sum, na.rm = TRUE)


#4th Dataset Programs Total Count
programs.count.table <- aggregate(cbind(Other,Emergency,Program_Mgmt,Liaison,
                                        Visit_Mgmt,Pol_Econ,Comm_Trade,Development,
                                        Police,Immigration,Informatics,Program_Services,
                                        Public_Comms,Training) ~ MissionTitleE, data = pimento.programs, FUN = sum, na.rm = TRUE)

#Trend per Month - mission, month w/ highest hours, # hours that month, average hours, highest task,
#4 Cases Minutes Trend per month

#this will eliminate days variable so that totals are for months
V1 <- rowSums(pimento.cases[ , c(10,12,14,16,18,20,22,24,26,28,30,32,34,36)], na.rm=TRUE)

TM1 <- data.frame(pimento.cases$MissionTitleE, pimento.cases$Month, V1)

TM1 <- aggregate(TM1$V1, by=list(TM1$pimento.cases.MissionTitleE,TM1$pimento.cases.Month), FUN=sum)

#aggregate use FUN = max for TM1

TM3 <- aggregate(TM1$x, by=list(TM1$Group.1), FUN = max)

#merge back in months

TM3 <- merge(TM3,TM1, all.x = TRUE)

colnames(TM3) <- c("MissionTitleE", "MinutesofHighestMonth","HighestMonth")

#add mean as a column
TMmean <- aggregate(x ~ Group.1, data = TM1, FUN = mean, na.rm = TRUE)

colnames(TMmean) <- c("MissionTitleE","MeanMonthlyMinutes")

#fix decimal places
TMmean$MeanMonthlyMinutes = formatC(TMmean$MeanMonthlyMinutes, digits = 5, format = "f")

#merge mean
TM4 <- merge(TM3,TMmean, all.x = TRUE)

#Add highest task column
TMtask1 <- data.frame(pimento.cases$MissionTitleE, pimento.cases$Month,pimento.cases$Disasters.Time,
                      pimento.cases$Death.Time, pimento.cases$Assistance.Communications.Time, 
                      pimento.cases$Legal.NotaryTime, pimento.cases$Evacuation..Time,
                      pimento.cases$Child.Abduction.Custody.Time, pimento.cases$Family.Distress.Time,
                      pimento.cases$Registration.Time, pimento.cases$Immigration.Time,
                      pimento.cases$Arrest.Time, pimento.cases$Citizenship.Time,
                      pimento.cases$Passport.Time, pimento.cases$Service.Time,
                      pimento.cases$Financial.Assistance.Transfers.Time)

TMtask1 <- aggregate(cbind(pimento.cases.Disasters.Time,
                          pimento.cases.Death.Time, pimento.cases.Assistance.Communications.Time, 
                          pimento.cases.Legal.NotaryTime, pimento.cases.Evacuation..Time,
                          pimento.cases.Child.Abduction.Custody.Time, pimento.cases.Family.Distress.Time,
                          pimento.cases.Registration.Time, pimento.cases.Immigration.Time,
                          pimento.cases.Arrest.Time, pimento.cases.Citizenship.Time,
                          pimento.cases.Passport.Time, pimento.cases.Service.Time,
                          pimento.cases.Financial.Assistance.Transfers.Time) ~ pimento.cases.MissionTitleE + pimento.cases.Month, data = TMtask1, FUN = sum, na.rm = TRUE)

colnames(TMtask1) <- c("MissionTitleE", "Month","Disasters",
                       "Death", "Assistance.Communications", 
                       "Legal.Notary", "Evacuation",
                       "Child.Abduction.Custody", "Family.Distress",
                       "Registration", "Immigration",
                       "Arrest", "Citizenship",
                       "Passport", "Service",
                       "Financial.Assistance.Transfers")
#Semi transpose
library(tidyr)
TM.task.long <- pivot_longer(TMtask1, cols=3:16, names_to = "Case.Type", values_to = "Case.Minutes")

#Aggregate to highest case per month
TMtask3 <- aggregate(Case.Minutes ~ MissionTitleE + Month, data = TM.task.long, FUN = max, na.rm = TRUE)

#Add back in the case name
TMtask4 <- merge(TMtask3,TM.task.long,all.x = TRUE)
colnames(TMtask4) <- c("MissionTitleE","HighestMonth","CaseMinutes","CaseType")
TMtask4 = subset(TMtask4, select = -c(CaseMinutes) )

#The highest task
library(dplyr)
TM5= TM4 %>% left_join(TMtask4,by=c("MissionTitleE","HighestMonth"))


#6th Dataset Year Trends

#this will eliminate days variable so that totals are for years
V1 <- rowSums(pimento.cases[ , c(10,12,14,16,18,20,22,24,26,28,30,32,34,36)], na.rm=TRUE)

TY1 <- data.frame(pimento.cases$MissionTitleE, pimento.cases$Year, V1)

TY1 <- aggregate(TY1$V1, by=list(TY1$pimento.cases.MissionTitleE,TY1$pimento.cases.Year), FUN=sum)


#aggregate use FUN = max for TY1

TY3 <- aggregate(TY1$x, by=list(TY1$Group.1), FUN = max)

#merge back in years

TY3 <- merge(TY3,TY1, all.x = TRUE)

colnames(TY3) <- c("MissionTitleE", "MinutesofHighestYear","HighestYear")

#add mean as a column
TYmean <- aggregate(x ~ Group.1, data = TY1, FUN = mean, na.rm = TRUE)

colnames(TYmean) <- c("MissionTitleE","MeanYearlyMinutes")

#fix decimal places to 5 decimal places
TYmean$MeanYearlyMinutes = formatC(TYmean$MeanYearlyMinutes, digits = 5, format = "f")

#merge mean
TY4 <- merge(TY3,TYmean, all.x = TRUE)

#Add highest task column
TYtask1 <- data.frame(pimento.cases$MissionTitleE, pimento.cases$Year,pimento.cases$Disasters.Time,
                      pimento.cases$Death.Time, pimento.cases$Assistance.Communications.Time, 
                      pimento.cases$Legal.NotaryTime, pimento.cases$Evacuation..Time,
                      pimento.cases$Child.Abduction.Custody.Time, pimento.cases$Family.Distress.Time,
                      pimento.cases$Registration.Time, pimento.cases$Immigration.Time,
                      pimento.cases$Arrest.Time, pimento.cases$Citizenship.Time,
                      pimento.cases$Passport.Time, pimento.cases$Service.Time,
                      pimento.cases$Financial.Assistance.Transfers.Time)

TYtask1 <- aggregate(cbind(pimento.cases.Disasters.Time,
                           pimento.cases.Death.Time, pimento.cases.Assistance.Communications.Time, 
                           pimento.cases.Legal.NotaryTime, pimento.cases.Evacuation..Time,
                           pimento.cases.Child.Abduction.Custody.Time, pimento.cases.Family.Distress.Time,
                           pimento.cases.Registration.Time, pimento.cases.Immigration.Time,
                           pimento.cases.Arrest.Time, pimento.cases.Citizenship.Time,
                           pimento.cases.Passport.Time, pimento.cases.Service.Time,
                           pimento.cases.Financial.Assistance.Transfers.Time) ~ pimento.cases.MissionTitleE + pimento.cases.Year, data = TYtask1, FUN = sum, na.rm = TRUE)

colnames(TYtask1) <- c("MissionTitleE", "Year","Disasters",
                       "Death", "Assistance.Communications", 
                       "Legal.Notary", "Evacuation",
                       "Child.Abduction.Custody", "Family.Distress",
                       "Registration", "Immigration",
                       "Arrest", "Citizenship",
                       "Passport", "Service",
                       "Financial.Assistance.Transfers")
#Semi transpose
library(tidyr)
TY.task.long <- pivot_longer(TYtask1, cols=3:16, names_to = "Case.Type", values_to = "Case.Minutes")

#Aggregate to highest case per year
TYtask3 <- aggregate(Case.Minutes ~ MissionTitleE + Year, data = TY.task.long, FUN = max, na.rm = TRUE)

#Add back in the case name
TYtask4 <- merge(TYtask3,TY.task.long,all.x = TRUE)
colnames(TYtask4) <- c("MissionTitleE","HighestYear","CaseMinutes","CaseType")
TYtask4 = subset(TYtask4, select = -c(CaseMinutes) )

#The highest task
library(dplyr)
TY5= TY4 %>% left_join(TYtask4,by=c("MissionTitleE","HighestYear"))


#Final Datasets
#repres
#cases.count.table
#cases.time.table
#programs.count.table
#TM5
#TY5

#Preparing the data for clustering
cases.time.table$MissionTitleE <- trimws(cases.time.table$MissionTitleE, which=c("left"))
cases.time.table <- cases.time.table %>% arrange(MissionTitleE)
cases.time.table$MissionTitleE <- as.factor(cases.time.table$MissionTitleE)
cases.time.table <- cases.time.table[apply(cases.time.table[,-1], 1, function(x) !all(x==0)),]
cases.time.table<-subset(cases.time.table, MissionTitleE!="Jackson" &
                           MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                         & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                         & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                         & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                         & MissionTitleE!="Bosphorus")

cases.count.table$MissionTitleE <- trimws(cases.count.table$MissionTitleE, which=c("left"))
cases.count.table <- cases.count.table %>% arrange(MissionTitleE)
cases.count.table$MissionTitleE <- as.factor(cases.count.table$MissionTitleE)
cases.count.table <- cases.count.table[apply(cases.count.table[,-1], 1, function(x) !all(x==0)),]
cases.count.table<-subset(cases.count.table, MissionTitleE!="Jackson" &
                           MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                         & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                         & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                         & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                         & MissionTitleE!="Bosphorus")

programs.count.table$MissionTitleE <- trimws(programs.count.table$MissionTitleE, which=c("left"))
programs.count.table <- programs.count.table %>% arrange(MissionTitleE)
programs.count.table$MissionTitleE <- as.factor(programs.count.table$MissionTitleE)
programs.count.table <- programs.count.table[apply(programs.count.table[,-1], 1, function(x) !all(x==0)),]
programs.count.table<-subset(programs.count.table, MissionTitleE!="Jackson" &
                            MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                          & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                          & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                          & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                          & MissionTitleE!="Bosphorus")

TM5$MissionTitleE <- trimws(TM5$MissionTitleE, which=c("left"))
TM5 <- TM5 %>% arrange(MissionTitleE)
TM5$MissionTitleE <- as.factor(TM5$MissionTitleE)
TM5$CaseType <- as.factor(TM5$CaseType)
TM5$MeanMonthlyMinutes <- as.numeric(TM5$MeanMonthlyMinutes)
TM5 <- TM5[apply(TM5[,-1], 1, function(x) !all(x==0)),]
TM5 <- filter(TM5, MinutesofHighestMonth != 0)
TM5 <- TM5[!duplicated(TM5$MissionTitleE), ]
TM5<-subset(TM5, MissionTitleE!="Jackson" &
                            MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                          & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                          & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                          & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                          & MissionTitleE!="Bosphorus")

TY5$MissionTitleE <- trimws(TY5$MissionTitleE, which=c("left"))
TY5 <- TY5 %>% arrange(MissionTitleE)
TY5$MissionTitleE <- as.factor(TY5$MissionTitleE)
TY5$CaseType <- as.factor(TY5$CaseType)
TY5$MeanYearlyMinutes <- as.numeric(TY5$MeanYearlyMinutes)
TY5 <- TY5[apply(TY5[,-1], 1, function(x) !all(x==0)),]
TY5 <- filter(TY5, MinutesofHighestYear != 0)
TY5 <- TY5[!duplicated(TY5$MissionTitleE), ]
TY5<-subset(TY5, MissionTitleE!="Jackson" &
                            MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                          & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                          & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                          & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                          & MissionTitleE!="Bosphorus")


repres$MissionTitleE <- as.factor(repres$ID)
repres = subset(repres, select = -c(ID) )
repres <- repres[apply(repres[,-1], 1, function(x) !all(x==0)),]
repres <- repres %>%
  select(MissionTitleE, everything())
repres<-subset(repres, MissionTitleE!="Jackson" &
                            MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                          & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                          & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                          & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                          & MissionTitleE!="Bosphorus")


#########done
