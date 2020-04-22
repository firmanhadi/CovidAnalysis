install.packages("ggplot2")
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
options(scipen=999)  #Disable scientific notation in R
setwd('C:/Users/sashg/Desktop/Covid_Data')

#Import John Hopkins Database (For Deaths, Recoveries, and Calculated Active cases)
df<-read.csv('Cases.csv')
names(df)[names(df)=="ï..DateTime"]<-"DateTime"
df$DateTime <- as.Date(df$DateTime, format = "%d-%m-%Y")

#Import Oxford Covid Response Tracker database
df2<-read.csv('Database2.csv')
df2[is.na(df2)]<-0                                                      #Replaces all NAs to 0
df2 <- transform(df2, Date = as.Date(as.character(Date), "%Y%m%d"))     #Transforms the numeric type date entries to the R-recognised Date type
df2<-gather(df2,Case_Type,Cases,ConfirmedCases:ConfirmedDeaths) #Combine Deaths and Confirmed cases into rows
df2<-df2[order(df2$Date,df2$CountryName),]  #Order according to Country instead of case_type
df2<-df2[,c(2,1,4,5,3)] #Change the order of columns
rownames(df2)<-NULL #resetting the rownames after all the gather and order activities


#Basic Plot : Total Confirmed Cases
df9<-df2[df2$Case_Type == "ConfirmedCases", ]
dfsumcases<-df9%>% group_by(Date)%>%
  summarise(ConfirmedCases = sum(Cases)/1000)     #Total confirmed cases from every country on a daily basis
plotcases <- ggplot(data=dfsumcases, aes(x = Date, y = ConfirmedCases))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.title.x=element_blank())
print(plotcases)

#Basic Plot : Total Confirmed Cases
df9<-df2[df2$Case_Type == "ConfirmedDeaths", ]
dfsumdeaths<-df9%>% group_by(Date)%>%
  summarise(Deaths = sum(Cases)/1000)            #Total confirmed deaths from every country on a daily basis
plotdeaths <- ggplot(data=dfsumdeaths,aes(x = Date, y = Deaths))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45))+
  theme(axis.title.x=element_blank())
print(plotdeaths)



#PLOT 1: Plot of Combined Global cumulative Number of Active, Deaths, and Recovered Cases_______________________________________
df %>%filter(!grepl('Confirmed', Case_Type)) %>% group_by(DateTime, Case_Type) %>% 
  summarise(Cases = Total/1000)%>% ggplot(aes(x = DateTime, y = Cases, fill = Case_Type))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45))+
  theme(axis.title.x=element_blank())+
  scale_fill_manual("legend", values = c("Active" = "steelblue4", "Deaths" = "black", "Recovered" = "goldenrod3"))


#PLOT 2: Plot of Separated Global cumulative Number of Active, Deaths, and Recovered Cases______________________________________
df %>%
  filter(!grepl('Confirmed', Case_Type))%>%
  group_by(DateTime, Case_Type)%>%
  summarise(Cases = Total/1000)%>%
  ggplot(aes(x = DateTime, y = Cases, fill = Case_Type))+
  geom_bar(stat = 'identity', position = 'dodge')+
  theme(axis.text.x = element_text(angle = 45))+
  theme(axis.title.x=element_blank())+
  scale_fill_manual("legend", values = c("Active" = "steelblue4", "Deaths" = "black", "Recovered" = "goldenrod3"))


#PLOT 3: Multifaceted plot (use of subplots) to visualise cases and deaths in individual countries______________________________

#Finding the global Max in confirmed cases and deaths (Moving to Oxford Database from John Hopkins one) 
maxconfirmed <- df2 %>% filter(Case_Type=="ConfirmedCases") %>% group_by(CountryName) %>% arrange(-(Cases)) %>% distinct(CountryName, .keep_all = TRUE)
maxdeaths <- df2 %>% filter(Case_Type=="ConfirmedDeaths") %>% group_by(CountryName) %>% arrange(-(Cases)) %>% distinct(CountryName, .keep_all = TRUE)
head(maxconfirmed)
head(maxdeaths)

countryplot <- function(x) {
  df2 %>%
    filter(CountryName == x & Date > '2019-12-31')%>%
    group_by(Date, Case_Type)%>%              #CASES in 1000s
    ggplot(aes(x = Date, y = Cases/1000, fill = Case_Type))+
    geom_bar(stat = 'identity', position = 'dodge')+ggtitle(x)+
    theme(plot.title = element_text(hjust = 0.5),plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 45))+
    scale_fill_manual("legend", values = c("ConfirmedCases" = "steelblue4", "ConfirmedDeaths" = "black"))
}

#The countries are chosen according to the results from maxconfirmed and maxdeaths calculated above
pus<-countryplot('United States')
pit<-countryplot('Italy')+ coord_cartesian(ylim = c(0, 180))
psp<-countryplot('Spain')+ coord_cartesian(ylim = c(0, 180))
pge<-countryplot('Germany')+ coord_cartesian(ylim = c(0, 180))
pfr<-countryplot('France')+ coord_cartesian(ylim = c(0, 180))
puk<-countryplot('United Kingdom')+ coord_cartesian(ylim = c(0, 180))
pch<-countryplot('China')+ coord_cartesian(ylim = c(0, 180))
pin<-countryplot('India')


plc1<-ggarrange(psp,pit,pge,ncol = 3,legend = "none")
plc2<-ggarrange(pfr,puk,pch,ncol = 3, legend = "none")
plc3<-ggarrange(plc1,plc2,nrow = 2, legend = "none")
plc4<-ggarrange(pus,plc3,widths = c(1,3),common.legend = TRUE, legend = "bottom")
annotate_figure(plc4,
                bottom = text_grob("Data source: The Oxford COVID-19 Government Response Tracker (OxCGRT)", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                                   left = text_grob("Cases (in thousands)", color = "black", rot = 90))



#PLOT 4: Expansion profile of total confirmed deaths for all countries, highlighting certain countries of interest_________________
#CALCULATE ROLLING AVERAGE FOR CONFIRMED DEATHS
df3<-df2[df2$Case_Type == "ConfirmedDeaths", ]
df3<-df3[order(df3$CountryName,df3$Date),] %>% 
  group_by(CountryName) %>% 
  mutate(roll_mean = rollmean(Cases, 5, na.pad = T))
df3<-df3[!(df3$Cases<10),]
df3<-df3 %>% group_by(CountryName) %>% mutate(dc = row_number())
df3<-df3 %>% 
  group_by(CountryName) %>% 
  slice(-n())

#CONVERT THE ROLLING AVERAGE PLOT TO LOGARITHMIC SCALE (DEATHS)
breaksc <- 10^(-10:10)
minor_breaksc <- rep(1:9, 21)*(10^rep(-10:10, each=9))
logplotc<-ggplot() +
  geom_line(data=df3,aes(x=dc, y=roll_mean, group=CountryName),color="gray68") +
  geom_line(data=df3 %>% filter(CountryName =='United States'),aes(x=dc, y=roll_mean),color="steelblue4",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='Italy'),aes(x=dc, y=roll_mean),color="darkseagreen4",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='Spain'),aes(x=dc, y=roll_mean),color="green4",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='China'),aes(x=dc, y=roll_mean),color="honeydew4",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='South Korea'),aes(x=dc, y=roll_mean),color="navy",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='United Kingdom'),aes(x=dc, y=roll_mean),color="royalblue4",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='Lebanon'),aes(x=dc, y=roll_mean),color="steelblue",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='Iran'),aes(x=dc, y=roll_mean),color="goldenrod",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='Japan'),aes(x=dc, y=roll_mean),color="lavenderblush4",size=1) +
  geom_line(data=df3 %>% filter(CountryName =='India'),aes(x=dc, y=roll_mean),color="black",size=1.5) +
  scale_y_log10(breaks = breaksc, minor_breaks = minor_breaksc) +
  annotation_logticks(sides=c("l")) +
  coord_cartesian(xlim = c(0, 55)) +
  theme_bw()
print(logplotc)

yyy<-logplotc+
  geom_segment(aes(x = 1, y = 10, xend = 13, yend = 81920), color = "black", linetype='dashed')+                 #Doubles every day
  geom_segment(aes(x = 1, y = 10, xend = 26, yend = 81920), color = "black", linetype='dashed')+                 #Doubles every 2 days
  geom_segment(aes(x = 1, y = 10, xend = 39, yend = 81920), color = "black", linetype='dashed')+                 #Doubles every 3 days
  geom_segment(aes(x = 1, y = 10, xend = 50, yend = 1413.23), color = "black", linetype='dashed')+               #Doubles every week
  geom_segment(aes(x = 1, y = 10, xend = 50, yend = 31.25), color = "black", linetype='dashed')                  #Doubles every month
print(yyy)



#PLOT 5: Expansion profile of total confirmed cases for all countries, highlighting certain countries of interest_________________
#CALCULATE ROLLING AVERAGE FOR CONFIRMED CASES
df4<-df2[df2$Case_Type == "ConfirmedCases", ]
df4<-df4[order(df4$CountryName),] %>% 
  group_by(CountryName) %>% 
  mutate(roll_mean = rollmean(Cases, 7, na.pad = T))
df4<-df4[!(df4$Cases<100),]
df4<-df4 %>% group_by(CountryName) %>% mutate(dc = row_number())
df4<-df4 %>% 
  group_by(CountryName) %>% 
  slice(-n())

#CONVERT THE ROLLING AVERAGE PLOT TO LOGARITHMIC SCALE (CONFIRMED CASES)
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
dfcd<- df4 %>% filter(CountryName =='United States')
logplot<-ggplot() +
  geom_line(data=df4,aes(x=dc, y=roll_mean, group=CountryName),color="gray68") +
  geom_line(data=df4 %>% filter(CountryName =='United States'),aes(x=dc, y=roll_mean),color="steelblue4",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='Italy'),aes(x=dc, y=roll_mean),color="darkseagreen4",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='Spain'),aes(x=dc, y=roll_mean),color="green4",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='China'),aes(x=dc, y=roll_mean),color="honeydew4",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='South Korea'),aes(x=dc, y=roll_mean),color="navy",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='United Kingdom'),aes(x=dc, y=roll_mean),color="royalblue4",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='Lebanon'),aes(x=dc, y=roll_mean),color="steelblue",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='Japan'),aes(x=dc, y=roll_mean),color="lavenderblush4",size=1) +
  geom_line(data=df4 %>% filter(CountryName =='India'),aes(x=dc, y=roll_mean),color="black",size=1.5) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  annotation_logticks(sides=c("l")) +
  coord_cartesian(xlim = c(0, 60)) +
  theme_bw()
print(logplot)

xxx<-logplot+
  geom_segment(aes(x = 1, y = 100, xend = 15, yend = 1000000), color = "black", linetype='dashed')+                 #Doubles every day
  geom_segment(aes(x = 1, y = 100, xend = 30, yend = 1000000), color = "black", linetype='dashed')+                 #Doubles every 2 days
  geom_segment(aes(x = 1, y = 100, xend = 46, yend = 1000000), color = "black", linetype='dashed')+                 #Doubles every 3 days
  geom_segment(aes(x = 1, y = 100, xend = 55, yend = 15000), color = "black", linetype='dashed')+                   #Doubles every week
  geom_segment(aes(x = 1, y = 100, xend = 55, yend = 200), color = "black", linetype='dashed')                      #Doubles every month
print(xxx)




#PLOT 6: Percentage Growth Rate (Deaths) visualisation of all countries, with certain highlighted countries of interest_________________
df5<-df2[df2$Case_Type == "ConfirmedDeaths", ]
df5<-df5[order(df5$CountryName,df5$Date),]
growth_rate = df5 %>% group_by(CountryName)%>%
  mutate(Cases_1 = lag(Cases),
         Cases_2 = lag(Cases_1),
         Cases_3 = lag(Cases_2),
         Cases_4 = lag(Cases_3),
         Cases_5 = lag(Cases_4),
         Diff_growth = Cases - Cases_5,
         Rate_percent = ((Diff_growth/5)/Cases * 100))

df6<-growth_rate
df6<-df6 %>% group_by(CountryName) %>% 
  mutate(roll_mean = rollmean(Rate_percent, 3, na.pad = T))
df6<-df6[!(df6$Cases<10),]
df6<-df6 %>% group_by(CountryName) %>% mutate(dc = row_number())
df6<-df6 %>% 
  group_by(CountryName) %>% 
  slice(-n())          

gplot<-ggplot() +
  geom_line(data=df6,aes(x=dc, y=roll_mean, group=CountryName),color="gray68") +
  #geom_line(data=df6 %>% filter(CountryName =='United States'),aes(x=dc, y=roll_mean),color="steelblue4",size=1) +
  geom_line(data=df6 %>% filter(CountryName =='Italy'),aes(x=dc, y=roll_mean),color="darkseagreen4",size=1) +
  geom_line(data=df6 %>% filter(CountryName =='Spain'),aes(x=dc, y=roll_mean),color="green4",size=1) +
  #geom_line(data=df6 %>% filter(CountryName =='China'),aes(x=dc, y=roll_mean),color="honeydew4",size=1) +
  geom_line(data=df6 %>% filter(CountryName =='South Korea'),aes(x=dc, y=roll_mean),color="navy",size=1) +
  geom_line(data=df6 %>% filter(CountryName =='United Kingdom'),aes(x=dc, y=roll_mean),color="royalblue4",size=1) +
  #geom_line(data=df6 %>% filter(CountryName =='Lebanon'),aes(x=dc, y=roll_mean),color="steelblue",size=1) +
  geom_line(data=df6 %>% filter(CountryName =='Japan'),aes(x=dc, y=roll_mean),color="lavenderblush4",size=1) +
  #geom_line(data=df6 %>% filter(CountryName =='India'),aes(x=dc, y=roll_mean),color="black",size=1.5)  +
  coord_cartesian(xlim = c(0, 60))  +
  theme_bw()
print(gplot)



#PLOT 7: Percentage Growth Rate (Deaths) visualisation of all countries, with certain highlighted countries of interest_________________
df7<-df2[df2$Case_Type == "ConfirmedCases", ]
df7<-df7[order(df7$CountryName,df7$Date),]
growth_rate = df7 %>% group_by(CountryName)%>%
  mutate(Cases_1 = lag(Cases),
         Cases_2 = lag(Cases_1),
         Cases_3 = lag(Cases_2),
         Cases_4 = lag(Cases_3),
         Cases_5 = lag(Cases_4),
         Diff_growth = Cases - Cases_5,
         Rate_percent = ((Diff_growth/5)/Cases * 100))

df8<-growth_rate
df8<-df8 %>% group_by(CountryName) %>% 
  mutate(roll_mean = rollmean(Rate_percent, 3, na.pad = T))
df8<-df8[!(df8$Cases<100),]
df8<-df8 %>% group_by(CountryName) %>% mutate(dc = row_number())
df8<-df8 %>% 
  group_by(CountryName) %>% 
  slice(-n())          

gplot<-ggplot() +
  geom_line(data=df8,aes(x=dc, y=roll_mean, group=CountryName),color="gray68") +
  geom_line(data=df8 %>% filter(CountryName =='United States'),aes(x=dc, y=roll_mean),color="steelblue4",size=1) +
  geom_line(data=df8 %>% filter(CountryName =='Italy'),aes(x=dc, y=roll_mean),color="darkseagreen4",size=1) +
  geom_line(data=df8 %>% filter(CountryName =='China'),aes(x=dc, y=roll_mean),color="honeydew4",size=1) +
  geom_line(data=df8 %>% filter(CountryName =='South Korea'),aes(x=dc, y=roll_mean),color="navy",size=1) +
  geom_line(data=df8 %>% filter(CountryName =='Lebanon'),aes(x=dc, y=roll_mean),color="steelblue",size=1) +
  geom_line(data=df8 %>% filter(CountryName =='Japan'),aes(x=dc, y=roll_mean),color="lavenderblush4",size=1) +
  geom_line(data=df8 %>% filter(CountryName =='India'),aes(x=dc, y=roll_mean),color="black",size=1.5)  +
  coord_cartesian(xlim = c(0, 60))  +
  theme_bw()
print(gplot)