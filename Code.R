#setting working directory
setwd('C:/Users/toami/OneDrive/Desktop/Data Science/Data Visualisation/assignments/assignment2/dataset')

#loading library
#install.packages("ggrepel")
library(ggrepel)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sqldf)
library(reshape)
library(scales)
#install.packages('rgdal')
library(rgdal)
#install.packages('maptools')
library(maptools)
#install.packages('raster')
library(raster)
#install.packages('sf')
library(sf)
#install.packages('cartography')
library(cartography)
#install.packages('ggthemes')
library(ggthemes)

#clearing environment
rm(list = ls())

#CLEANING
#STEP1
#unzipping file
unzip(zipfile = "crime.zip")

#STEP2
#Loading two files
cases=read.csv('42_Cases_under_crime_against_women.csv',header = TRUE)
arrest=read.csv('43_Arrests_under_crime_against_women.csv',header = TRUE)

#STEP3
#changing column names
colnames(cases)[which(names(cases) == "ï..Area_Name")] <- 'State'
colnames(cases)[which(names(cases) == "Sub_Group_Name")] <- 'Crime'

colnames(arrest)[which(names(arrest) == "ï..Area_Name")] <- 'State'
colnames(arrest)[which(names(arrest) == "Sub_Group_Name")] <- 'Crime'

#STEP4
#cleaning crime column
cases$Crime=str_sub(cases$Crime, 5,)
arrest$Crime=str_sub(arrest$Crime, 5,)

#STEP5
#selecting useful columns
cases = subset( cases, select = c(State, Year, Crime, Cases_Chargesheeted, Cases_Reported, Cases_Trials_Completed))
arrest = subset( arrest, select = c(State, Year, Crime, Persons_Arrested, Persons_Chargesheeted, Persons_Trial_Completed))

#STEP6
#removing all row from cases and arrest dataframes where value is NA
#(these rows were not there in the original files, they got added when data is loaded to R studio)
cases=na.omit(cases)
arrest=na.omit(arrest)

#STEP7
#Modifying values of crime column to more appropriate and short values
cases$Crime[cases$Crime=='Kidnapping & Abduction of Women & Girls']=
  'Kidnapping & Abduction'
cases$Crime[cases$Crime=='Cruelty by Husband and Relatives']=
  'Cruelty by Relatives'
cases$Crime[cases$Crime=='Importation of Girls']=
  'Importation'
cases$Crime[cases$Crime=='Immoral Traffic Prevention Act']=
  'Immoral Trafficing'
cases$Crime[cases$Crime=='Dowry Prohibition Act']=
  'Dowry cases'
cases$Crime[cases$Crime=='Indecent Representation of Women(Prohibition) Act']=
  'Indecent Representation'
cases$Crime[cases$Crime=='Sati Prevention Act']=
  'Sati'
cases$Crime[cases$Crime=='Total Crimes Against Women']=
  'Total Crimes'
#-------------------------------------------------------------------------
arrest$Crime[arrest$Crime=='Kidnapping & Abduction of Women & Girls']=
  'Kidnapping & Abduction'
arrest$Crime[arrest$Crime=='Cruelty by Husband and Relatives']=
  'Cruelty by Relatives'
arrest$Crime[arrest$Crime=='Importation of Girls']=
  'Importation'
arrest$Crime[arrest$Crime=='Immoral Traffic Prevention Act']=
  'Immoral Trafficing'
arrest$Crime[arrest$Crime=='Dowry Prohibition']=
  'Dowry cases'
arrest$Crime[arrest$Crime=='Indecent Representation of Women(Prohibition) Act']=
  'Indecent Representation'
arrest$Crime[arrest$Crime=='Sati Prevention Act']=
  'Sati'
arrest$Crime[arrest$Crime=='Total Crimes Against Women']=
  'Total Crimes'
#----------------------------------------------------------------------------------------
#DATA WRANGLING
#STEP1
#creating primary key in both cases and arrest dataframes for merge
cases$primary_key= paste(as.character(cases$State), 
                         as.character(cases$Year), as.character(cases$Crime),sep = "_")
arrest$primary_key= paste(as.character(arrest$State), 
                          as.character(arrest$Year), as.character(arrest$Crime),sep = "_")

#STEP2
#merging both datasets on primary key
record=merge(x=cases, y=arrest, by="primary_key", all = TRUE)

#STEP3
#dropping the duplicate columns from new data frame
record = subset( record, select = c("State.x", "Year.x", "Crime.x", "Cases_Chargesheeted", "Cases_Reported", "Cases_Trials_Completed", "Persons_Arrested", "Persons_Chargesheeted", "Persons_Trial_Completed"))

#STEP4
#changing column names of new dataframe to more appropriate
colnames(record)[which(names(record) == "State.x")] <- 'State'
colnames(record)[which(names(record) == "Year.x")] <- 'Year'
colnames(record)[which(names(record) == "Crime.x")] <- 'Crime'

#-----------------------------------------------------------------------------
#STEP5
#creating dataframe PLOT1
Plot1=sqldf('select year, sum(cases_reported) as Cases_Reported, 
            sum(cases_chargesheeted) as Cases_Chargesheeted, 
            sum(cases_trials_completed) as Trial_completed  
            from record group by year')

#STEP6
plot1_melt=melt(Plot1,id.vars = c('Year'))
colnames(plot1_melt)=c('year','cases','numbers')

#STEP7
plot1_melt$year=ymd(plot1_melt$year, truncated = 2L)

#STEP8
g1=ggplot(data = plot1_melt, aes(x = year, y = numbers, color = cases, face="bold")) + 
  geom_line()+
  scale_y_continuous(labels = comma, n.breaks = 7)+
  scale_x_date(breaks=date_breaks('1 year'), labels=date_format('%Y'))+
  ggtitle("Justice to Women in Indian Judicial system")+xlab("Year")+ylab("Number of Cases")+
  theme(panel.background = element_rect(fill='lightyellow', size=0.5,
                                        linetype=0, colour='lightblue'),
        plot.title = element_text(color="black", size=16, hjust=0.5, face="bold"),
        axis.title.x = element_text(color="blue", size=10, face="bold"),
        axis.title.y = element_text(color="blue", size=10, face="bold"),
        panel.grid = element_line(color = 'grey'))
#-------------------------------------------------------------------------------------

#STEP9
#creating dataframe PLOT2
Plot2=sqldf('select UPPER(state) as STATE, sum(cases_reported) as Cases_Reported 
            from record group by state order by Cases_Reported')

#STEP10
#reading the shape file for India
data=st_read('india_st.shp')

#STEP11
Plot2$STATE[Plot2$STATE=='ANDAMAN & NICOBAR ISLANDS']=
  'ANDAMAN AND NICOBAR ISLANDS'
Plot2$STATE[Plot2$STATE=='DADRA & NAGAR HAVELI']=
  'DADRA AND NAGAR HAVELI'
Plot2$STATE[Plot2$STATE=='DAMAN & DIU']=
  'DAMAN AND DIU'
Plot2$STATE[Plot2$STATE=='JAMMU & KASHMIR']=
  'JAMMU AND KASHMIR'
Plot2$STATE[Plot2$STATE=='ODISHA']=
  'ORISSA'
Plot2$STATE[Plot2$STATE=='PUDUCHERRY']=
  'PONDICHERRY'

#STEP12
#merging Plot2 and data on state
record2=merge(x=data, y=Plot2, by='STATE', all = TRUE)

#STEP13
point=cbind(record2, st_coordinates(st_centroid(record2$geometry)))

#STEP14
g2=ggplot(data=record2)+
  geom_sf(aes(fill=Cases_Reported), color='white',size=0.1)+
  scale_fill_viridis_c(option='viridis', trans='reverse',
                       labels = comma, n.breaks = 11, guide = "colourbar")+
  geom_text(data = point, aes(x=X,y=Y, label=paste(STATE))
            ,color='white', size=2,fontface='bold', angle=0, 
            vjust=-1, check_overlap = TRUE)+
  geom_text(data= point,aes(x=X, y=Y, label=paste(Cases_Reported)),
            color = "red", size=2.0, fontface = "plain", angle=0, 
            vjust=+1, check_overlap = TRUE) +
  ggtitle("Crime against women across states of India")+xlab("Longitude")+ylab("Latitude")+
  theme(panel.background = element_rect(fill='lightblue', size=0.5,
                                        linetype=0, colour='lightblue'),
        plot.title = element_text(color="black", size=16, hjust=0.5, face="bold"),
        axis.title.x = element_text(color="blue", size=10, face="bold"),
        axis.title.y = element_text(color="blue", size=10, face="bold"))
#geom_label_repel
#-------------------------------------------------------------------------------------
#STEP15
#creating dataframe Plot3
plot3=sqldf('select crime, sum(Cases_Reported) as cases 
             from record where crime<>"Total Crimes" group by crime order by sum(Cases_Reported)')

#STEP16
plot3$Crime[plot3$Crime=='Sati']='Others'
plot3$Crime[plot3$Crime=='Importation']='Others'
plot3$Crime[plot3$Crime=='Dowry cases']='Others'
plot3$Crime[plot3$Crime=='Indecent Representation']='Others'

plot3=sqldf('select crime, sum(Cases) as cases 
             from plot3 group by crime order by sum(Cases)')

#STEP17
plot3$per=round(plot3$cases/sum(plot3$cases)*100,2)

#STEP18
g3=ggplot(plot3, aes(x=2, y=per, fill=Crime)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(x=2, label = paste0(per, "%")), 
            position = position_stack(vjust=0.7),check_overlap = TRUE) +
  labs(x = NULL, y = NULL, fill = NULL)+
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color="black", size=16,  face="bold"))+
  xlim(0.2, 2.5)+ggtitle('Distribution of types of crimes')
#----------------------------------------------------------------------------------
g1
ggsave("plot1.png", width=6, height=3, dpi=1000)

g2
ggsave("plot2.png", width=6, height=6, dpi=1000)

g3
ggsave("plot3.png", width=6, height=6, dpi=1000)

