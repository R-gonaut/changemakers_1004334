
getData <- function(CovidDataPLOT){

# UK Data Wrangling -------------------------

ukData <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=cumPeopleVaccinatedFirstDoseByPublishDate&metric=cumPeopleVaccinatedSecondDoseByPublishDate&format=csv", na.strings = "", fileEncoding = "UTF-8-BOM")

ukData$Population <- 68200000

ukData <- ukData %>%
  rename(
    Date = date,
    Cumulative_First_Dose = cumPeopleVaccinatedFirstDoseByPublishDate,
    Cumulative_Second_Dose = cumPeopleVaccinatedSecondDoseByPublishDate,
    Country = areaName 
  )

ukData <- ukData %>% 
  mutate(Date = as.Date(Date, "%Y-%m-%d"))

LookupDate2 <- read.csv("Date_Lookup.csv", sep = ",")

LookupDate2 <- LookupDate2 %>% 
  mutate(Date = as.Date(Date, "%d/%m/%Y"))

names(LookupDate2) <- c("YearWeekISO", "Date")

ukData <- merge(x=ukData,y=LookupDate2,by="Date",all.x=TRUE)

ukData <- ukData %>% drop_na()

ukData$First_Dose_Percent <- ukData$Cumulative_First_Dose/ukData$Population
ukData$Second_Dose_Percent <- ukData$Cumulative_Second_Dose/ukData$Population

ukData <- ukData %>% select(-one_of('areaCode', 'areaType', 'YearWeekISO'))


# EUROPE Data Wrangling --------------------------------

LookupRegion <- read.csv("Countries_Lookup.csv", sep = ",")

LookupDate <- read.csv("Date_Lookup.csv", sep = ",")

SourceDataTemp <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

names(LookupRegion)[1:2] <- c("ReportingCountry", "Country")

names(LookupDate)[1:2] <- c("YearWeekISO", "Date")

SourceData <- merge(x=SourceDataTemp,y=LookupRegion,by="ReportingCountry",all.x=TRUE)

SourceData <- merge(x=SourceData,y=LookupDate,by="YearWeekISO",all.x=TRUE)

SourceData <- SourceData[!nchar(as.character(SourceData$Region)) > 2, ]

SourceData <- subset(SourceData, SourceData$TargetGroup=="ALL")

SourceData <- SourceData %>% select(-one_of('ReportingCountry', 'Denominator', 'FirstDoseRefused', 'UnknownDose', 
                                            'NumberDosesReceived', 'Region', 'TargetGroup', 'Vaccine'))

SourceData <- SourceData %>% 
  mutate(Date = as.Date(Date, "%d/%m/%Y"))

SourceData <- SourceData %>% drop_na()

CovidData <- SourceData %>% 
  group_by(Date, Country, Population) %>% 
  summarise(FirstDose = sum(FirstDose), SecondDose = sum(SecondDose)) 

CovidData <- CovidData %>% 
  arrange(Date) %>% 
  group_by(Country) %>%
  mutate(Cumulative_First_Dose = cumsum(FirstDose)) %>%
  mutate(Cumulative_Second_Dose = cumsum(SecondDose))

CovidData$First_Dose_Percent <- CovidData$Cumulative_First_Dose/CovidData$Population
CovidData$Second_Dose_Percent <- CovidData$Cumulative_Second_Dose/CovidData$Population

CovidData <- CovidData %>% select(-one_of('FirstDose', 'SecondDose'))



# PLOT Data Wrangling -----------------------------------

CovidDataPLOT <- CovidData

CovidDataPLOT <- CovidDataPLOT %>%
  group_by(Country) %>%
  arrange(Date) %>% 
  mutate(First_Dose_Given = Cumulative_First_Dose/Population)

CovidDataPLOT <- CovidDataPLOT %>%
  group_by(Country) %>%
  arrange(Date) %>% 
  mutate(Second_Dose_Given = Cumulative_Second_Dose/Population)


# Final Wrangling ----------

CovidDataPLOT <- rbind(CovidData, ukData)

CovidDataPLOT <-  CovidDataPLOT[CovidDataPLOT$Date>'2021-01-11',]

CovidDataPLOT$Year <- year(ymd(CovidDataPLOT$Date))
CovidDataPLOT$Month <- month(ymd(CovidDataPLOT$Date))
CovidDataPLOT$Day <- day(ymd(CovidDataPLOT$Date))

CovidDataPLOT$First_Dose_Given <- CovidDataPLOT$First_Dose_Percent*100
CovidDataPLOT$Second_Dose_Given <- CovidDataPLOT$Second_Dose_Percent*100

CovidDataPLOT <- CovidDataPLOT %>%
  arrange(Country)

}
