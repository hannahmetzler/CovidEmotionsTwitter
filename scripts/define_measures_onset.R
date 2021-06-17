library(dplyr)

#onset of nationwide, broad social distancing measures, based on covid19-interventions.com
d <- read.csv2('data_main/COVID19_non-pharmaceutical-interventions-semicolonsep.csv')
unique(d$Country)
df <- d %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter( Measure_L1 =="Social distancing" &
         Country %in% c("Austria", "Australia", "Belgium","Canada","Switzerland","Ecuador","Spain","France",
                        "Germany","Italy","Mexico","Netherlands","New Zealand", "United Kingdom")) %>%  #missing: Peru, USA, Australia, Ireland,"Chile",
  group_by(Country) %>%
  arrange(Date) %>%
  #select the country to look at
  filter(Country=="Mexico" & Region =="Mexico")
View(df[,c(3, 5:11)])