#quick data processing steps - can be run every time 01_EmotionTimelines is run. 

# calculate length of sustained periods above/below baseline ####

threshold <- 1 #threshold number of confirmed cases that we define as start date of covid-19 in each country

#first COVID case in any of our countires

firstcaseall <- all %>%
  group_by(country) %>% 
  filter(confirmed > 0) %>% 
  slice(1) %>% #select first line per country
  select(date, country) %>%
  arrange(date) %>% 
  ungroup() %>% 
  slice(1) #select only the first line per date and country

#threshold for level below/above baseline - sd in 2019
mad <- all %>% 
  filter(date > as.Date("2020-01-01")) %>% 
  group_by(wordlist, country) %>% 
  summarise(mad2019 = mad((pr-bl)/bl)) %>% 
  ungroup()

#per country, emotion, and date, code if value was above 0
sustained <- all %>% 
  inner_join(mad) %>% 
  mutate(country = as.factor(country)) %>% 
  arrange(country, wordlist, date) %>% 

  ##if above 0, code 1, otherwise 0: lots of FALSE or lots of TRUE in a row mean long periods above or below the baseline
  mutate(above0 = (pr-bl)/bl> 0) %>%   #baseline corrected percent bigger than 0 = TRUE, or bigger than 1 SD in 2019
  #above or below a certain value
  mutate(notzero = if_else((pr-bl)/bl> mad2019, "positive",
                   if_else((pr-bl)/bl< -mad2019, "negative", "zero"))) %>% 
  
  ##apply a threshold of cases per country
  mutate(separdate = factor(ifelse(confirmed >= threshold, "After", "Before")))
  
  ##apply a cases threshold across all countries, starting on the same date, since countries are not independent (news from italy spread on Twitter)
  #mutate(separdate = factor(ifelse(date > firstcaseall$date, "After", "Before")))


durationlist <- list()
for (iwordlist in unique(sustained$wordlist)){
  for (icountry in unique(sustained$country)){
    for(isepardate in unique(sustained$separdate)){
      idf <- filter(sustained, country == icountry  &separdate == isepardate & wordlist == iwordlist) 
      #Compute the lengths and values of runs of equal values in the vector "above 0) with rle() Rule Length Encoding
       xconsec <-   (rle(idf$notzero))
     #  xconsec <-   (rle(idf$above0)) #with(rle(idf$above0), length) #or values
      
      #if extracting only lenghth, not values from rle(), add NA to vectors that only have one entry (e.g. After in Austria anxiety, only one long period, only one value):
      #if (length(xconsec) ==1) {xconsec <- append(xconsec, NA)} 
      durationlist[[iwordlist]][[isepardate]][[icountry]] <- xconsec
      
      #below[[iwordlist]][[isepardate]][[icountry]]<- with(rle(idf$above0), lengths[!values]) #only number of FALSE values in a row
    }
  }
}

#transform the list to a dataframe
duration <- durationlist %>% 
  unlist(recursive = FALSE) %>% 
  unlist(recursive=FALSE) %>%
  unlist(recursive=FALSE) %>% 
  #transpose: name as one variable, values/length one row each per country/emotion/period
  tibble::enframe()%>% 
  #unlist the value column: one row for each entry
  #separate the column name into 3 columns
  separate(name, into = c("wordlist", "separdate", "country", "length_value")) %>% 
  #one separate column for length and value
  pivot_wider(id_cols = c("wordlist","separdate", "country"), names_from = length_value, values_from=value, values_fn = list(value = list)) %>% 
  # #unlist the value and length columns to one list per line
  unnest(cols = c("values", "lengths")) %>% 
  #unlist the value and length columns to one value per line
  unnest(cols = c("values", "lengths")) %>% 
  #rename lenghts to number of days and values to above/below zero
  rename(ndays = lengths, 
         notzero = values) %>% 
  #rename country labels
  mutate(country =  recode(country, aut = "Austria", fra=  "France", ger = "Germany", ita = "Italy",
                           esp = "Spain",uk = "UK",  usa= "USA",  beldutch = "Belgium", can="Canada", ire = "Ireland", 
                           net = "Netherlands", che= "Switzerland", aus  = "Australia", nzl= "New Zealand", chl="Chile", ecu="Ecuador", per="Peru", mex="Mexico"))


#max number of days after covid-19 start, used for ordering countries in plots
maxdays <- duration %>% 
  filter(!notzero=="zero") %>% #keep only time periods above or below zero, exclude those around zero
  group_by(wordlist, country,separdate) %>% 
  mutate(maxdays =max(ndays)) %>% 
  select(-c(ndays, notzero)) %>% 
  slice(n()) %>% #select only one row (last per group)
  #calculate ratio of maxdays pre-post
  group_by(country, wordlist) %>% 
  mutate(maxratio = maxdays[separdate=="Before"]/maxdays[separdate=="After"]) %>% 
  ungroup()

#add maxdays to duration data frame
duration <- inner_join(duration, maxdays)

confirmed_lastdate <- all %>% 
  group_by(country) %>% 
  arrange(date) %>% 
  slice(n()) %>% 
  ungroup()

#rm(sustained)
#rm(durationlist)

write.csv(duration, file="data_main/Duration.csv", row.names=F)
