#load dataset from John Hopkins university, with cumulative confirmed cases, deaths, confirmed ####
jhudf <-  read.csv("data_main/JHUdata.csv") %>%
  #format the date column
  mutate(date=as.Date(date))  %>% 
  #rename country to country2, country contains abbreviations like us, fr, aut etc in our dataset
  rename(country2 = country) %>% 
  #chage country so it corresponds to the country names in our data
  mutate(country2 = as.character(dplyr::recode(country2, "United Kingdom"="UK", US = "USA", Belgium="BelgiumDutch", "New Zealand"="NewZealand",  Canada='CanadaEnglish'))) %>% 
  #select the countries we analyse
  filter(country2 %in%  c("Australia", "Austria", "BelgiumDutch", "CanadaEnglish", "Chile", "Ecuador", "France", "Germany", "Ireland", "Italy", "Mexico", "Netherlands", "NewZealand", "Peru", "Spain", "Switzerland", "UK", "USA")) %>% 
  #delete the other levels in the country factor
  droplevels() %>% arrange(country2) %>%
  #select only the variables we need: confirmed cumulative cases, recovered and deaths per country
  select(date, confirmed, recovered, deaths, country2)

# for each country, combine datafiles for each emotion and total tweets into one long datafile ####
for (country in c("Austria", "France", "Belgium", "Germany", "Italy", "Spain", "UK", "USA", "BelgiumDutch", "CanadaEnglish", "Ireland", "Netherlands", "Switzerland",
                  "Australia", "NewZealand", "Chile", "Ecuador", "Peru", "Mexico"))

{
  df <- prepareCountry(country)
  #this function calculates percentages per emotion, the weekday baseline, produces long dataframe per country, calculates confidence intervals per day
  write.csv(df, file=paste0("data_twitter_volumes/",country,"SocEmoLong.csv"), row.names=F)
}

# create the Countries.csv: load long version of data from each country, add column for country, and add cases data ####
aut <- read.csv("data_twitter_volumes/AustriaSocEmoLong.csv")
aut <- aut %>% mutate(country = factor("aut"), country2=("Austria"), 
                      date = as.Date(date))
aut <- addCaseData(aut, countryName="Austria", jhudf)

ita <-read.csv("data_twitter_volumes/ItalySocEmoLong.csv") %>% 
  droplevels() %>% 
  mutate(country = factor("ita"), country2="Italy", 
         date = as.Date(date))
ita <- addCaseData(ita, countryName="Italy", jhudf)

fra <-read.csv("data_twitter_volumes/FranceSocEmoLong.csv")%>% 
  droplevels() %>% 
  mutate(country = factor("fra"), country2="France", 
         date = as.Date(date))
fra <- addCaseData(fra, countryName="France", jhudf)

uk  <-read.csv("data_twitter_volumes/UKSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("uk"), country2="UK", 
         date = as.Date(date))
uk <- addCaseData(uk, countryName="United Kingdom", jhudf)

usa <-read.csv("data_twitter_volumes/USASocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("usa"), country2="USA", 
         date = as.Date(date))
usa <- addCaseData(usa, countryName="US", jhudf)

ger <-read.csv("data_twitter_volumes/GermanySocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("ger"), country2="Germany", 
         date = as.Date(date))
ger <- addCaseData(ger, countryName="Germany", jhudf)

esp <-read.csv("data_twitter_volumes/SpainSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("esp"), country2="Spain", 
         date = as.Date(date))
esp <- addCaseData(esp, countryName="Spain", jhudf)

beldutch <-read.csv("data_twitter_volumes/BelgiumDutchSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("beldutch"), country2="BelgiumDutch", 
         date = as.Date(date))
beldutch <- addCaseData(beldutch, countryName="Belgium", jhudf)

can <-read.csv("data_twitter_volumes/CanadaEnglishSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("can"), country2="CanadaEnglish", 
         date = as.Date(date))
can <- addCaseData(can, countryName="Canada", jhudf)

ire <-read.csv("data_twitter_volumes/IrelandSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("ire"), country2="Ireland", 
         date = as.Date(date))
ire<- addCaseData(ire, countryName="Ireland", jhudf)

net <-read.csv("data_twitter_volumes/NetherlandsSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("net"), country2="Netherlands", 
         date = as.Date(date))
net <- addCaseData(net, countryName="Netherlands", jhudf)

che <-read.csv("data_twitter_volumes/SwitzerlandSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("che"), country2="Switzerland", 
         date = as.Date(date))
che <- addCaseData(che, countryName="Switzerland", jhudf)

aus <-read.csv("data_twitter_volumes/AustraliaSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("aus"), country2="Australia", 
         date = as.Date(date))
aus <- addCaseData(aus, countryName="Australia", jhudf)

nzl <-read.csv("data_twitter_volumes/NewZealandSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("nzl"), country2="NewZealand", 
         date = as.Date(date))
nzl <- addCaseData(nzl, countryName="New Zealand", jhudf)

chl <-read.csv("data_twitter_volumes/ChileSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("chl"), country2="Chile", 
         date = as.Date(date))
chl <- addCaseData(chl, countryName="Chile", jhudf)

ecu <-read.csv("data_twitter_volumes/EcuadorSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("ecu"), country2="Ecuador", 
         date = as.Date(date))
ecu <- addCaseData(ecu, countryName="Ecuador", jhudf)

per <-read.csv("data_twitter_volumes/PeruSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("per"), country2="Peru", 
         date = as.Date(date))
per <- addCaseData(per, countryName="Peru", jhudf)

mex <-read.csv("data_twitter_volumes/MexicoSocEmoLong.csv")%>% 
  droplevels()%>% 
  mutate(country = factor("mex"), country2="Mexico", 
         date = as.Date(date))
mex <- addCaseData(mex, countryName="Mexico", jhudf)

#combine all countries into one data frame
all <- rbind(aut, ita, esp, fra, uk, usa, ger,beldutch, can, ire, net, che, aus, nzl, chl, ecu, per, mex) 

#add the start of social distancing date and the outbreak date to the dataframe, for each country
#load dates of nationwide broad social distancing measures in each country
measures <- read.csv2('data_main/SocialDistancingMeasures.csv') %>% 
  mutate(social_distancing_start = as.Date(social_distancing, format = '%d/%m/%y')) %>% 
  select(country, social_distancing_start)

# add the outbreak date (30 cases) and the social distancing start date per country to the dataframe
all = all %>% 
  group_by(country2) %>% 
  filter(confirmed>=30) %>% 
  slice(1) %>% 
  select(date, country2) %>% 
  ungroup() %>% 
  rename(outbreak_date = date) %>% 
  inner_join(all) %>% 
  inner_join(measures) %>% 
  #move the outbreak and lockdown dates after the column date
  relocate(outbreak_date, .after=date) %>%
  relocate(social_distancing_start, .after=outbreak_date)

#save datafile including all countries
write.csv(all, file="data_main/CountriesData.csv", row.names=F)

# create dataset with average since day with 30 cases ####
print("Make dataset with means since day with 30 cases: Outbreak-30-Levels dataset")
cdata <- read.csv("data_main/CountriesData.csv") 
#cases threshold, first day included in this dataset
threshold<-30
all <- NULL

for (country in c("Austria", "France", "Germany", "Italy", "Spain", "UK", "USA", "BelgiumDutch", "CanadaEnglish", 
                  "Ireland", "Netherlands", "Switzerland", 
                  "Australia", "NewZealand", "Chile", "Ecuador", "Peru", "Mexico"))
  
{
  datefrom <- cdata$date[cdata$confirmed>=threshold & cdata$country2==country][1]
  datelast <- as.Date(datefrom) + 5*7-1 #5 weeks, add 34 days to starting date
  df <- countryPeriodAverage(country, datefrom=as.character(datefrom), datelast=as.character(datelast))
  df$country <- country
  all <- rbind(all, df)
}
write.csv(all, file="data_main/Outbreak-30-LevelsWithCases.csv", row.names=F)

# after outbreak period split into 5 weeks ####

all <- NULL
for (i in seq(1, length(countries)))
{
  country <- countries[i]
  print(country)
  date1 <- as.Date(cdata$date[cdata$confirmed>=threshold & cdata$country2==country][1])
  print(date1)
  df <- countryWeeks(country, outbreakDate = date1) 
  df$country <- country
  all <- rbind(all, df)
}
write.csv(all, file="data_main/AfterOutbreak-Weeks-5.csv", row.names=F)


# create data set of US states to compare daily volumes of US aggregate with separate states ####
for (country in c("California", "NewYork"))
  
{
  df <- prepareCountry(country)
  #this function calculates percentages per emotion, the weekday baseline, produces long dataframe per country, calculates confidence intervals per day
  write.csv(df, file=paste0("data_twitter_volumes/",country,"SocEmoLong.csv"), row.names=F)
}

# create the Countries.csv: load long version of data from each country, add column for country, and add cases data ####
ny <- read.csv("data_twitter_volumes/NewYorkSocEmoLong.csv") %>% droplevels()%>% 
  mutate(country = factor("ny"), country2="NewYork")
ca <- read.csv("data_twitter_volumes/CaliforniaSocEmoLong.csv") %>% droplevels()%>% 
  mutate(country = factor("ca"), country2="California")
usa <-read.csv("data_twitter_volumes/USASocEmoLong.csv")%>% droplevels()%>% 
  mutate(country = factor("usa"), country2="USA")
#connect the three state dataframes
usstates <- rbind(ny, ca, usa) %>% 
  mutate(date = as.Date(date))

#save datafile including all us states
write.csv(usstates, file="data_main/USStatesData.csv", row.names=F)

# create TwitterChange.csv: total tweets, followers, authors

df_tw = NULL
aut_tw = NULL
for(country in c('Austria', 'Peru','Chile','USA','UK', 'Spain', 'Italy', 'Ireland', 'Netherlands', 'France', 'CanadaEnglish', 'Germany', 'NewZealand', 'Mexico', 'Ecuador' ,'Switzerland','BelgiumDutch', "Australia")){ #
  aut_tw = prepareTwitterChange(country=country)
  df_tw = rbind(df_tw, aut_tw)
}
df_tw = df_tw %>% 
  mutate(country = recode(country, BelgiumDutch = 'Belgium', CanadaEnglish = 'Canada'))
write.csv(df_tw, 'data_main/TwitterChange.csv', row.names=F)

