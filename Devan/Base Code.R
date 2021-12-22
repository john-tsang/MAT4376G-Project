#Author: Devan Scholefield

library(readxl)
library(data.table)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(scales)
PIMENTO_CASES <- read_excel("PIMENTO_CASES.xlsx")
PIMENTO_PROGRAMS <- read_excel("PIMENTO_PROGRAMS.xlsx")

PIMENTO_CASES_NA_OMITTED <- na.omit(PIMENTO_CASES)
PIMENTO_PROGRAMS_NA_OMITTED <- na.omit(PIMENTO_PROGRAMS)




col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x=cor(PIMENTO_CASES_NA_OMITTED[,c(7:35)]), col = col, symm = TRUE)
heatmap(x=cor(PIMENTO_PROGRAMS_NA_OMITTED[,c(7:20)]), col = col, symm = TRUE)


setDT(melt(cor(PIMENTO_CASES_NA_OMITTED[,c(7:35)])))[Var1 != Var2, .SD[which.max(value)], keyby=Var1]
setDT(melt(cor(PIMENTO_PROGRAMS_NA_OMITTED[,c(7:20)])))[Var1 != Var2, .SD[which.max(value)], keyby=Var1]

MissionSumCases <- PIMENTO_CASES_NA_OMITTED %>% 
  group_by(MissionTitleE) %>%
  summarise(Num_Entries = length(MissionTitleE))

MissionSumPrograms <- PIMENTO_PROGRAMS_NA_OMITTED %>% 
  group_by(MissionTitleE) %>%
  summarise(Num_Entries = length(MissionTitleE))

write.csv(MissionSumCases,"MissionSumCases.csv", row.names = FALSE)
write.csv(MissionSumPrograms,"MissionSumPrograms.csv", row.names = FALSE)


EmployeeSumPrograms <- PIMENTO_PROGRAMS_NA_OMITTED %>% 
  group_by(MissionTitleE) %>%
  summarise(Num_Entries = unique(EmployeeCode)) %>%
  summarise(Num_Entries = length(Num_Entries))

EmployeeSumCases <- PIMENTO_CASES_NA_OMITTED %>% 
  group_by(MissionTitleE) %>%
  summarise(Num_Entries = unique(EmployeeID)) %>%
  summarise(Num_Entries = length(Num_Entries))

hist(EmployeeSumPrograms$Num_Entries, main = "Programs", xlab = "Employees per Mission")
hist(EmployeeSumCases$Num_Entries, main = "Cases", xlab = "Employees per Mission")


PIMENTO_PROGRAMS_NA_OMITTED$date <- as.Date(with(PIMENTO_PROGRAMS_NA_OMITTED, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
PIMENTO_CASES_NA_OMITTED$date <- as.Date(with(PIMENTO_CASES_NA_OMITTED, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")


#Time series plots

CaseCounts <- subset(PIMENTO_CASES_NA_OMITTED,select=-c(GeoRegionNameE,EmployeeID,Month,Day,Year,`Disasters Time`,`Death Time`,
                             `Assistance Communications Time`,`Legal/NotaryTime`,`Evacuation  Time`,
                             `Child Abduction/Custody Time`, `Family Distress Time`, `Registration Time`,
                             `Immigration Time`, `Arrest Time`, `Citizenship Time`, `Passport Time`,
                             `Service Time`, `Financial Assistance/Transfers Time`))






AggCases <- aggregate(CaseCounts[,c(2:16)], by=list(CaseCounts$MissionTitleE, CaseCounts$date), FUN=sum)




AggCases$DayCountCases <- rowSums(AggCases[,c(3:17)])
colnames(AggCases)[1] <- "Mission"
colnames(AggCases)[2] <- "date"
AggCases <- subset(AggCases,select=c(Mission,date,DayCountCases))

AggCases$Mission <- factor(AggCases$Mission)
AggCases <- AggCases[-c(218272), ]
AggCases1 <- AggCases





#Old method since replaced to account for number of employees
#AggCases <- AggCases[AggCases$DayCountCases < 1400, ]

      
MeanAggCases <- data.frame(AggCases$Mission,AggCases$DayCountCases)
MeanAggCases <- aggregate(MeanAggCases$AggCases.DayCountCases, by=list(AggCases$Mission), FUN=mean)
colnames(MeanAggCases)[1] <- "Mission"
colnames(MeanAggCases)[2] <- "Average"


SpaghettiCases <- ggplot(data = AggCases, aes(x = date, y = DayCountCases, group = Mission))
SpaghettiCases + geom_line() + xlab("Date") + ylab("Cases Opened Per Day")


hist(MeanAggCases$Average, breaks = 20, main = "Histogram of Mean Cases", xlab = "Average Number of Cases Opened Per Day Per Mission")


###Case Time

CaseTime <- subset(PIMENTO_CASES_NA_OMITTED, select=c(MissionTitleE,`Disasters Time`,`Death Time`,
                                                      `Assistance Communications Time`,`Legal/NotaryTime`,`Evacuation  Time`,
                                                      `Child Abduction/Custody Time`, `Family Distress Time`, `Registration Time`,
                                                      `Immigration Time`, `Arrest Time`, `Citizenship Time`, `Passport Time`,
                                                      `Service Time`, `Financial Assistance/Transfers Time`,date))

AggCaseTime <- aggregate(CaseTime[,c(2:15)], by=list(CaseTime$MissionTitleE, CaseTime$date), FUN=sum)
colnames(AggCaseTime)[1] <- "Mission"
colnames(AggCaseTime)[2] <- "date"

AggCaseTime$DayTimeCases <- rowSums(AggCaseTime[,c(3:16)])


EmployeeSumCases$MaxTime <- EmployeeSumCases$Num_Entries*900


for(i in 1:nrow(EmployeeSumCases)){
  
  AggCaseTime$MaxTime[AggCaseTime$Mission==EmployeeSumCases$MissionTitleE[i]] <- EmployeeSumCases$MaxTime[i]
  
}


AggCaseTime <- AggCaseTime[AggCaseTime$DayTimeCases < AggCaseTime$MaxTime,]

#Aggregate for all missions

AggMonthCase <- aggregate(AggCaseTime$DayTimeCases, by=list(AggCaseTime$MonthYear),FUN=sum)
colnames(AggMonthCase) <- c("Month", "Time")


PastaMonth <- ggplot(data = AggMonthCase, aes(x = Month, y = Time, group=1))+
  geom_line() + xlab("Date") + ylab("Total Time Spent on Cases")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(AggMonthCase$Month))[c(T, rep(F, 11))])+
  scale_y_continuous(labels = comma)
PastaMonth


#Add month and year column to aggregate by month mean
AggCaseTime$MonthYear <- format(AggCaseTime$date, "%Y-%m")

MonthAggCaseTime <- aggregate(AggCaseTime$DayTimeCases, by =list(AggCaseTime$Mission,AggCaseTime$MonthYear), FUN=mean)
colnames(MonthAggCaseTime) <- c("Mission", "Month", "TimePerMonth")


### Spaghetti plots for case time
SpaghettiCaseTime <- ggplot(data = MonthAggCaseTime, aes(x = Month, y = TimePerMonth, group = Mission))+
  geom_line() + xlab("Date") + ylab("Average Time Spent on Cases per Month")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(MonthAggCaseTime$Month))[c(T, rep(F, 11))])

SpaghettiCaseTime
#ggplotly(SpaghettiCaseTime)


MeanTimeCases <- data.frame(AggCaseTime$Mission,AggCaseTime$DayTimeCases)
MeanTimeCases <- aggregate(MeanTimeCases$AggCaseTime.DayTimeCases, by=list(MeanTimeCases$AggCaseTime.Mission), FUN=mean)
colnames(MeanTimeCases)[1] <- "Mission"
colnames(MeanTimeCases)[2] <- "Average Time"


###Program Time


ProgramTime <- subset(PIMENTO_PROGRAMS_NA_OMITTED, select=-c(GeoRegionNameE,EmployeeCode,Month,Day,Year))
AggProgramTime <- aggregate(ProgramTime[,c(2:15)], by=list(ProgramTime$MissionTitleE,ProgramTime$date), FUN=sum)

AggProgramTime$DayTimeCases <- rowSums(AggProgramTime[,c(3:16)])

colnames(AggProgramTime)[1] <- "Mission"
colnames(AggProgramTime)[2] <- "date"


# MeanTimePrograms <- data.frame(AggProgramTime$Group.1,AggProgramTime$DayTimeCases)
# MeanTimePrograms <- aggregate(MeanTimePrograms$AggProgramTime.DayTimeCases, by=list(MeanTimePrograms$AggProgramTime.Group.1), FUN=mean)
# colnames(MeanTimePrograms)[1] <- "Mission"
# colnames(MeanTimePrograms)[2] <- "Average Time"



EmployeeSumPrograms$MaxTime <- EmployeeSumPrograms$Num_Entries*900

for(i in 1:nrow(EmployeeSumPrograms)){
  
  AggProgramTime$MaxTime[AggProgramTime$Mission==EmployeeSumPrograms$MissionTitleE[i]] <- EmployeeSumPrograms$MaxTime[i]
  
}


AggProgramTime <- AggProgramTime[AggProgramTime$DayTimeCases < AggProgramTime$MaxTime,]

#Add month and year column to aggregate by month mean
AggProgramTime$MonthYear <- format(AggProgramTime$date, "%Y-%m")

MonthAggProgramTime <- aggregate(AggProgramTime$DayTimeCases, by =list(AggProgramTime$Mission,AggProgramTime$MonthYear), FUN=mean)
colnames(MonthAggProgramTime) <- c("Mission", "Month", "TimePerMonth")


### Spaghetti plots for program time
SpaghettiProgramTime <- ggplot(data = MonthAggProgramTime, aes(x = Month, y = TimePerMonth, group = Mission))+
  geom_line() + xlab("Date") + ylab("Average Time Spent on Programs per Month")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(MonthAggProgramTime$Month))[c(T, rep(F, 11))])

SpaghettiProgramTime
ggplotly(SpaghettiProgramTime)


#Aggregate for all missions

AggMonthPrograms <- aggregate(AggProgramTime$DayTimeCases, by=list(AggProgramTime$MonthYear),FUN=sum)
colnames(AggMonthPrograms) <- c("Month", "Time")


PastaMonthProg <- ggplot(data = AggMonthPrograms, aes(x = Month, y = Time, group=1))+
  geom_line() + xlab("Date") + ylab("Total Time Spent on Cases")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(AggMonthPrograms$Month))[c(T, rep(F, 11))])+
  scale_y_continuous(labels = comma)
PastaMonthProg

#Combined Case and Program on one Graph
CombMonthCP <- merge(AggMonthCase,AggMonthPrograms,by="Month")
colnames(CombMonthCP) <- c("Month","Cases","Programs")
MeltCombMonthCP <- melt(CombMonthCP, id.vars="Month")

MeltCombMonthCP$variable <- as.factor(MeltCombMonthCP$variable)
colnames(MeltCombMonthCP) <- c("Month", "Type", "value")

PastaCombMonthCP <- ggplot(MeltCombMonthCP, aes(Month,value,group=Type,col=Type))+
  geom_line()+
  geom_point()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(MeltCombMonthCP$Month))[c(T, rep(F, 11))])+
  scale_y_continuous(labels = comma)+
  ylab("Average Monthly Time (Minutes)")

PastaCombMonthCP

###Total Mission Time

MissionTimes <- merge(MeanTimeCases,MeanTimePrograms, by  = "Mission")
colnames(MissionTimes)[2] <- "Case Time"
colnames(MissionTimes)[3] <- "Program Time"
MissionTimes$Mission <- factor(MissionTimes$Mission)


#Comparison of a few cases and programs of representative missions

CompMissions <- MissionTimes[MissionTimes$Mission == "Gangkou"|MissionTimes$Mission == "Lynden"|MissionTimes$Mission == "Tenoch"
                             |MissionTimes$Mission == "Bonaira" |MissionTimes$Mission == "Sao Manuel"| MissionTimes$Mission == "Barcino",]

CompMissions$Mission <- factor(CompMissions$Mission)
MeltCompMission <- melt(CompMissions, id.vars="Mission")
colnames(MeltCompMission)[2] <- "Type"


CompMissionTimeDifference <- ggplot(MeltCompMission, aes(Mission,value, col=Type)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top")+
  ylab("Average Time (Minutes)") +
  scale_x_discrete(limits = rev(levels(MeltCompMission$Mission))) +
  coord_flip() 

CompMissionTimeDifference




MissionTime1 <- MissionTimes[1:55,]
MissionTime1$Mission <- factor(MissionTime1$Mission)
MissionTime2 <- MissionTimes[56:110,]
MissionTime2$Mission <- factor(MissionTime2$Mission)
MissionTime3 <- MissionTimes[111:165,]
MissionTime3$Mission <- factor(MissionTime3$Mission)
MissionTime4 <- MissionTimes[166:219,]
MissionTime4$Mission <- factor(MissionTime4$Mission)



MeltMissionTime1 <- melt(MissionTime1, id.vars="Mission")
colnames(MeltMissionTime1)[2] <- "Type"

MeltMissionTime2 <- melt(MissionTime2, id.vars="Mission")
colnames(MeltMissionTime2)[2] <- "Type"

MeltMissionTime3 <- melt(MissionTime3, id.vars="Mission")
colnames(MeltMissionTime3)[2] <- "Type"

MeltMissionTime4 <- melt(MissionTime4, id.vars="Mission")
colnames(MeltMissionTime4)[2] <- "Type"


MissionTimeDifference1 <- ggplot(MeltMissionTime1, aes(Mission,value, col=Type)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top")+
  ylab("Average Time") +
  scale_x_discrete(limits = rev(levels(MeltMissionTime1$Mission))) +
  coord_flip() 
  
MissionTimeDifference1

MissionTimeDifference2 <- ggplot(MeltMissionTime2, aes(Mission,value, col=Type)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top")+
  ylab("Average Time") +
  scale_x_discrete(limits = rev(levels(MeltMissionTime2$Mission))) +
  coord_flip() 

MissionTimeDifference2

MissionTimeDifference3 <- ggplot(MeltMissionTime3, aes(Mission,value, col=Type)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top")+
  ylab("Average Time") +
  scale_x_discrete(limits = rev(levels(MeltMissionTime3$Mission))) +
  coord_flip() 

MissionTimeDifference3

MissionTimeDifference4 <- ggplot(MeltMissionTime4, aes(Mission,value, col=Type)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top")+
  ylab("Average Time") +
  scale_x_discrete(limits = rev(levels(MeltMissionTime4$Mission))) +
  coord_flip() 

MissionTimeDifference4


mean(MissionTimes$`Case Time`>MissionTimes$`Program Time`)



# number of employees multiplied by feasible minutes per employee = total possible time


#Boxplots of the total time spent on programs and cases by mission

MeltPrograms <- melt(programs.count.table,id.vars='MissionTitleE', measure.vars=c('Other','Emergency','Program_Mgmt',
                                                                                  "Liaison", "Visit_Mgmt", "Pol_Econ",
                                                                                  "Comm_Trade", "Development", "Police",
                                                                                  "Immigration", "Informatics", "Program_Services",
                                                                                  "Public_Comms", "Training"))

MeltPrograms$variable<-gsub(".", " ", MeltPrograms$variable, fixed=TRUE)
MeltPrograms$variable<-gsub("_", " ", MeltPrograms$variable, fixed=TRUE)
colnames(MeltPrograms) <- c("Mission", "Type", "Time")





Program.Time.Boxplot <- ggplot(MeltPrograms) +
  geom_boxplot(aes(x=Type, y=Time, color=Type))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Time (Minutes)")+
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  scale_y_continuous(labels = comma)
Program.Time.Boxplot

MeltCaseTime <- melt(cases.time.table, id.vars="MissionTitleE", measure.vars=c("Disasters.Time", "Death.Time","Assistance.Communications.Time",
                                                                               "Legal.NotaryTime", "Evacuation..Time", "Child.Abduction.Custody.Time",
                                                                               "Family.Distress.Time", "Registration.Time", "Immigration.Time",
                                                                               "Arrest.Time", "Citizenship.Time", "Passport.Time", "Service.Time",
                                                                               "Financial.Assistance.Transfers.Time"))
MeltCaseTime$variable<-gsub(".", " ", MeltCaseTime$variable, fixed=TRUE)
colnames(MeltCaseTime) <- c("Mission", "Type", "Time")
MeltCaseTime$Type<-gsub("Time", " ", MeltCaseTime$Type, fixed=TRUE)
MeltCaseTime$Type<-gsub("Assistance Communications", "Assistance Comms ", MeltCaseTime$Type, fixed=TRUE)
MeltCaseTime$Type<-gsub("Financial Assistance Transfers", "Fin Assist Trans ", MeltCaseTime$Type, fixed=TRUE)
MeltCaseTime$Type<-gsub("Child Abduction Custody", "Child Ab Custody ", MeltCaseTime$Type, fixed=TRUE)


Cases.Time.Boxplot <- ggplot(MeltCaseTime) +
  geom_boxplot(aes(x=Type, y=Time, color=Type))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Time (Minutes)")+
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  scale_y_continuous(labels = comma)
Cases.Time.Boxplot



hist(TM5$CaseType)


#Time series comparison of cases and programs for certain missions

GangkouComp <- data.frame(MonthAggCaseTime$TimePerMonth[MonthAggCaseTime$Mission=="Gangkou"],MonthAggProgramTime$TimePerMonth[MonthAggProgramTime$Mission=="Gangkou"],MonthAggCaseTime$Month[MonthAggCaseTime$Mission=="Gangkou"])
colnames(GangkouComp) <- c("Case Time", "Program Time", "Month")

MeltGangkouComp <- melt(GangkouComp, id.vars='Month')

GangkouCompChart <- ggplot(MeltGangkouComp, aes(Month,value, group=variable,color=variable)) + 
  geom_point() + 
  geom_line()+
  ylab("Average Time Spent per Month")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(MeltGangkouComp$Month))[c(T, rep(F, 11))])

GangkouCompChart
#ggplotly(GangkouComp)



BarcinoComp <- data.frame(MonthAggCaseTime$TimePerMonth[MonthAggCaseTime$Mission=="Barcino"],MonthAggProgramTime$TimePerMonth[MonthAggProgramTime$Mission=="Barcino"],MonthAggCaseTime$Month[MonthAggCaseTime$Mission=="Barcino"])
colnames(BarcinoComp) <- c("Case Time", "Program Time", "Month")

MeltBarcinoComp <- melt(BarcinoComp, id.vars='Month')

BarcinoCompChart <- ggplot(MeltBarcinoComp, aes(Month,value, group=variable,color=variable)) + 
  geom_point() + 
  geom_line()+
  ylab("Average Time Spent per Month")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_discrete(breaks = levels(as.factor(MeltBarcinoComp$Month))[c(T, rep(F, 11))])

BarcinoCompChart



