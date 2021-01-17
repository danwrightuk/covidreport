library(httr)
library(data.table)  
library(dplyr)       
library(purrr)      
library(tidyr)       
library(ggplot2)     
library(scales)      
library(gridExtra)  
library(viridis)    
library(knitr)
library(tidyverse)
library(plotly)
library(sf)
library(tmap)
library(gifski)
library(stplanr)

#retrieve MSOA data from the UK government

x <- GET("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&areaCode=E06000054&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv")
bin <- content(x, "raw")
writeBin(bin, "msoadata.csv")
msoa = read.csv("msoadata.csv", header = TRUE, dec = ",")

#retrieve Salisbury NHS Foundation Trust data

y <- GET("https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&areaCode=RNZ&metric=covidOccupiedMVBeds&metric=cumAdmissions&metric=newAdmissions&metric=hospitalCases&format=csv")
bin <- content(y, "raw")
writeBin(bin, "nhsdata.csv")
nhs  = read.csv("nhsdata.csv", header = TRUE, dec = ",")

#formatting the data, removing NA etc

msoa$newCasesBySpecimenDateRollingSum <- as.integer(msoa$newCasesBySpecimenDateRollingSum)
msoa$newCasesBySpecimenDateRollingSum[is.na(msoa$newCasesBySpecimenDateRollingSum)] <- 0

msoa$newCasesBySpecimenDateRollingRate <- as.integer(msoa$newCasesBySpecimenDateRollingRate)
msoa$newCasesBySpecimenDateRollingRate[is.na(msoa$newCasesBySpecimenDateRollingRate)]<- 0

msoa$date <- as.Date(msoa$date)

salisburymsoa <- dplyr::filter(msoa, str_detect(areaName, "\\Salisbury|\\Laverstock|Wilton, Nadder & Ebble|\\Great Wishford|\\Downton|Amesbury|Durrington & Bulford|Whaddon, Whiteparish & Winterslow|Larkhill, Shrewton & Bulford Camp|Great Wishford, Woodford Valley & Porton"))

nhs$date <- as.Date(nhs$date)

#plotting heatmap

hmap <- ggplot(salisburymsoa, aes(x=date, y=areaName, fill=newCasesBySpecimenDateRollingRate,
                                  text = paste('</br> Date: ', date,
                                               '</br> Area: ', areaName,
                                               '</br> 7-Day Rolling Rate'))) +
  geom_tile(color="white", size=0.1) +
  coord_fixed(ratio = 8) + 
  scale_fill_viridis(name="Cases per\n100,000 people", label=comma) +
  labs(x=NULL, y=NULL, title="Rolling Seven-Day COVID-19 Rate by MSOA Area", subtitle="Most recent complete data from gov.uk website") +
  theme_bw() +
  theme(panel.border = element_blank()) + 
  theme(plot.title=element_text(hjust=0, face="bold", size=14)) + 
  theme(plot.subtitle=element_text(hjust=0, size=12)) + 
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=12)) + 
  theme(legend.title=element_text(size=12)) +
  theme(legend.text=element_text(size=9)) +
  scale_x_date(expand = c(0, 0),date_breaks = "1 month", date_labels="%b")
hmap

heatmapplotly <- ggplotly(hmap, tooltip="text")%>%
  style(hoverlabel = list(bgcolor = "white"))
heatmapplotly

#read in MSOA shapefile and merge into Salisbury MSOA data

shapes <- st_read(dsn ="C:/Users/Danie/Desktop/MSOA2011/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp")
colnames(salisburymsoa)[which(names(salisburymsoa) == "areaCode")] <- "msoa11cd"
last7days <- dplyr::filter(salisburymsoa, date >= (Sys.Date()-7))

interactive_map_salisbury <- merge(shapes, last7days, by="msoa11cd")

#plot interactive map

interactive_map <- tm_shape(interactive_map_salisbury) +
  tm_polygons("newCasesBySpecimenDateRollingRate", id="areaName",
  palette = "-viridis", alpha=0.7, title = "Rolling Rate",
  breaks=c(0,10,50,100,200,400,800,1200),
  labels=c("0 to 9", "10 to 49", "50 to 99", "100 to 199", "200 to 399", "400 to 799",
           "800 to 1200")) +
  tm_basemap(leaflet::providers$Stamen.Toner)

tmap_mode("view")
interactive_map

# plot NHS admissions

admissions <- ggplot(nhs, aes(date, newAdmissions,
                              text = paste('</br> Date: ', date,
                                           '</br> Patients Admitted: ', newAdmissions))) +
  geom_bar(stat="identity", na.rm=TRUE, color="darkgreen", width=0.7, position=position_dodge(width=0.8)) + 
  scale_x_date(labels=date_format("%b %d"), breaks=date_breaks(width="1 month")) +
  ggtitle("COVID-19 Patients Admitted to Hospital ") +
  xlab("Date") + ylab("Admissions") +
  theme_light()

adm <- ggplotly(admissions, tooltip="text")%>%
  style(hoverlabel = list(bgcolor = "white"))
adm

ventilators <- ggplot(nhs, aes(date, covidOccupiedMVBeds,
                               text = paste('</br> Date: ', date,
                                            '</br> Occupied Ventilated Beds: ', covidOccupiedMVBeds))) +
  geom_bar(stat="identity", na.rm=TRUE, color="darkgreen", width=0.7, position=position_dodge(width=0.8)) + 
  scale_x_date(labels=date_format("%b %d"), breaks=date_breaks(width="1 month")) +
  ggtitle("Patients in Mechanical Ventilation Beds") +
xlab("Date") + ylab("Occupied Ventilator Beds") +
  theme_light()

vent <- ggplotly(ventilators, tooltip="text")%>%
  style(hoverlabel = list(bgcolor = "white"))
vent

hospitalpatients <- ggplot(nhs, aes(date, hospitalCases, 
                                    text = paste('</br> Date: ', date,
                                                 '</br> Hospital Cases: ', hospitalCases))) +
  geom_bar(stat="identity", na.rm=TRUE, color="#7fd686", width=0.7, position=position_dodge(width=0.8)) + 
  scale_x_date(labels=date_format("%b %d"), breaks=date_breaks(width="1 month")) +
  ggtitle("COVID-19 Patients in Hospital") +
  xlab("Date") + ylab("Patients") +
  theme_light()

ggg <- ggplotly(hospitalpatients, tooltip = 'text') %>%
  style(hoverlabel = list(bgcolor = "white"))

ggg