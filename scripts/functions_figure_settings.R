# functions and settings ####

#note: clean the code for added Cases in functions and process data!!!!! --------------------

#colours and other figure settings ####

collockdown <- "black" #turquois1
colsepardate <- "grey20"
fillperiods <- scales::alpha("grey90",0.2)
colterror <- "red2" #needed for Germany
colnormal <- "dodgerblue" #normal large public events (sports, elections, ...)
colpublicneg <- "green" #(negative) public events (military actions, political scandals, ...)
colcatast <- "purple" #tragic events (death of public figures), natural catastrophies (Notre Dame), ...
colpress <- "dodgerblue"

#font size labels of lines
labelsize = 11
axisfontsize = 17

#dates for lines in timeline figures:
controlstart <- as.Date("2020-01-15")
controlend <- as.Date("2020-02-15")

# #functions to read, organise, preprocess data ####

#change date format, remove last date
prepdata <- function(dfdatevol) {
  dfdatevol <- dfdatevol %>%
    mutate(date = as.Date(date)) %>%
    select(-end) %>%  #remove the column with ending date, end is always 24h after start
    arrange(date)
  return(dfdatevol)
}

#prepare long data frame and baseline for each country, and calculate confidence intervals
prepareCountry <- function (country = "Austria")
{
  #anxiety
  aut_anx <- read.csv(paste0('data_twitter_volumes/',country,'AnxVol.csv'))
  names(aut_anx) <- c("date","end","anx_n") #rename columns
  aut_anx <- prepdata(aut_anx) #apply function defined above to format date and remove last date
  
  #anger
  aut_ang <- read.csv(paste0('data_twitter_volumes/',country,'AngerVol.csv'))
  names(aut_ang) <- c("date","end", "ang_n")
  aut_ang <- prepdata(aut_ang)
  
  #sadness
  aut_sad <- read.csv(paste0('data_twitter_volumes/',country,'SadVol.csv'))
  names(aut_sad) <- c("date","end", "sad_n")
  aut_sad <- prepdata(aut_sad)
  
  #positive emotions
  aut_pos <- read.csv(paste0('data_twitter_volumes/',country,'PosemoVol.csv'))
  names(aut_pos) <- c("date","end", "pos_n")
  aut_pos<- prepdata(aut_pos)
  
  #total number of tweets
  aut_tot <- read.csv(paste0('data_twitter_volumes/',country,'TotVol.csv'))
  names(aut_tot) <- c("date","end", "tot_n")
  aut_tot <- prepdata(aut_tot)
  
  #proportions with terms per category 
  aut_all <- aut_tot%>%
    left_join(aut_anx)%>%
    left_join(aut_ang)%>%
    left_join(aut_sad) %>% 
    left_join(aut_pos) %>% 
    #calculate proportions
    mutate(anx_pr = anx_n/tot_n*100,
           ang_pr = ang_n/tot_n*100,
           sad_pr = sad_n/tot_n*100, 
           pos_pr = pos_n/tot_n*100) #%>%
  #slice(-n()) #delete last row
  
  aut_all %>% filter(date>="2019-01-01") -> aut_all
  
  #set language to produce English weekdays
  Sys.setlocale("LC_TIME", 'en_US.UTF-8')
  #baseline over the year 2019 per weekday
  baseline <- aut_all %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))%>% #transform date to day of the week, first day: Tuesday
    filter(date < as.Date("2020-01-01"))%>%
    group_by(weekday) %>% 
    summarise(anx_bl = mean(anx_pr), 
              ang_bl = mean(ang_pr), 
              sad_bl = mean(sad_pr), 
              pos_bl = mean(pos_pr),
              anx_sd = sd(anx_pr), 
              ang_sd = sd(ang_pr), 
              sad_sd = sd(sad_pr), 
              pos_sd = sd(pos_pr), .groups = 'drop')
  
  #filling corresponding baseline values
  aut_all <- aut_all %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))  #add weekday labels
  
  aut_all <- inner_join(aut_all, baseline)
  
  #produce long version of the data
  aut_long <- aut_all%>%
    pivot_longer(
      -c(weekday, date, tot_n), 
      names_to = c("wordlist", ".value"), 
      names_sep = "_" 
      #values_drop_na = TRUE
    ) %>%
    mutate(wordlist = dplyr::recode(wordlist, anx = "anxiety", ang = "anger", sad = "sadness", pos = "positive")) %>% 
    mutate(wordlist = factor(wordlist))%>%
    rename(wordlist_n = n)
  
  aut_long$prlow <- NA
  aut_long$prhigh <- NA
  
  #confidence intervals - this takes a while
  for (i in seq(1, nrow(aut_long)))
  {
    if (!is.na(aut_long$wordlist_n[i]) &  !is.na(aut_long$tot_n[i]))
    {
      btest <- binom.test(x= aut_long$wordlist_n[i], n =  aut_long$tot_n[i])
      aut_long$prlow[i] <- 100*btest$conf.int[1]
      aut_long$prhigh[i] <- 100*btest$conf.int[2]
      # 95% CI computed with the Clopper and Pearson (1934) method
    }
    
  }
  return(aut_long)
  
}


addCaseData <- function(df, countryName=NULL, jhudf)
{
  if("country2" %in% names(df)) { # if the variable country 2 exists, filter for the current country, otherwise take the entire dataframe
  countrydf = jhudf %>%
    filter(country2 == countryName) %>% droplevels()
  rdf <- left_join(df, countrydf, by=c("date", "country2"))
  }
  else{
    countrydf = jhudf
    rdf <- left_join(df, countrydf, by=c("date"))
    rdf$country2 = NULL
  }
  
  rdf$confirmed[is.na(rdf$confirmed)] <- 0
  rdf$recovered[is.na(rdf$recovered)] <- 0
  rdf$deaths[is.na(rdf$deaths)] <- 0
  rdf$activecases <- rdf$confirmed-rdf$recovered #calculate active
  rdf$date <- as.Date(rdf$date)
  return(rdf)
}

#means and CIs for one month
countryPeriodAverage <- function (country = "Austria", datefrom="2020-03-01", datelast="2020-03-31") #default input Austria and Month of March
{
  print(paste("calculate period means for", country))
  #anxiety
  aut_anx <- read.csv(paste0('data_twitter_volumes/',country,'AnxVol.csv'))
  names(aut_anx) <- c("date","end","anx_n")
  aut_anx <- prepdata(aut_anx)

  aut_ang <- read.csv(paste0('data_twitter_volumes/',country,'AngerVol.csv'))
  names(aut_ang) <- c("date","end", "ang_n")
  aut_ang <- prepdata(aut_ang)
  
  #sadness
  aut_sad <- read.csv(paste0('data_twitter_volumes/',country,'SadVol.csv'))
  names(aut_sad) <- c("date","end", "sad_n")
  aut_sad <- prepdata(aut_sad)
  
  #positive emotions
  aut_pos <- read.csv(paste0('data_twitter_volumes/',country,'PosemoVol.csv'))
  names(aut_pos) <- c("date","end", "pos_n")
  aut_pos<- prepdata(aut_pos)
  
  #total number of tweets
  aut_tot <- read.csv(paste0('data_twitter_volumes/',country,'TotVol.csv'))
  names(aut_tot) <- c("date","end", "tot_n")
  aut_tot <- prepdata(aut_tot)
  
  #proportions with terms per category 
  aut_all <- aut_tot%>%
    left_join(aut_anx)%>%
    left_join(aut_ang)%>%
    left_join(aut_sad) %>% 
    left_join(aut_pos) %>% 
    #calculate proportions
    mutate(anx_pr = anx_n/tot_n*100,
           ang_pr = ang_n/tot_n*100,
           sad_pr = sad_n/tot_n*100, 
           pos_pr = pos_n/tot_n*100) %>%
    slice(-n()) #delete last row
  
  aut_all = aut_all %>% filter(date>="2019-01-01")
  #aut_all = addCaseData(aut_all,  countryName=country2, jhudf)
  
  #baseline over the year 2019 (day-wise)
  baseline <- aut_all %>% 
    filter(date < as.Date("2020-01-01"))%>%
    summarize(anx_bl = mean(anx_pr), 
              ang_bl = mean(ang_pr), 
              sad_bl = mean(sad_pr), 
              pos_bl = mean(pos_pr),
              anx_sd = sd(anx_pr), 
              ang_sd = sd(ang_pr), 
              sad_sd = sd(sad_pr), 
              pos_sd = sd(pos_pr))
    #filter(!row_number()==nrow(aut_all)) #line not necessary
  
  aut_all %>% filter(date>=as.character(datefrom) & date <= as.character(datelast)) -> aut_sel
  aut_sel %>% filter(complete.cases(aut_sel)) -> aut_sel  
  aut_res <- NULL
  
  #anxiety
  btest <- binom.test(x=sum(aut_sel$anx_n), n = sum(aut_sel$tot_n))
  aut_res <- rbind(aut_res, data.frame(hits=sum(aut_sel$anx_n), total=sum(aut_sel$tot_n),
                                       # activecases=sum(aut_sel$activecases), deaths=sum(aut_sel$deaths),
                                       # confirmed=sum(aut_sel$confirmed), recovered=sum(aut_sel$recovered),
                                       low=100*btest$conf.int[1],
                                       high=100*btest$conf.int[2],
                                       pr=100*sum(aut_sel$anx_n)/sum(aut_sel$tot_n),
                                       bl=baseline$anx_bl, sd=baseline$anx_sd,
                                       wordlist="anxiety"))

  #anger
  btest <- binom.test(x=sum(aut_sel$ang_n), n = sum(aut_sel$tot_n))
  aut_res <- rbind(aut_res, data.frame(hits=sum(aut_sel$ang_n), total=sum(aut_sel$tot_n),
                                       # activecases=sum(aut_sel$activecases), deaths=sum(aut_sel$deaths),
                                       # confirmed=sum(aut_sel$confirmed), recovered=sum(aut_sel$recovered),
                                       low=100*btest$conf.int[1],
                                       high=100*btest$conf.int[2],
                                       pr=100*sum(aut_sel$ang_n)/sum(aut_sel$tot_n),
                                       bl=baseline$ang_bl, sd=baseline$ang_sd,
                                       wordlist="anger"))
  #sadness
  btest <- binom.test(x=sum(aut_sel$sad_n), n = sum(aut_sel$tot_n))
  aut_res <- rbind(aut_res, data.frame(hits=sum(aut_sel$sad_n), total=sum(aut_sel$tot_n),
                                       # activecases=sum(aut_sel$activecases), deaths=sum(aut_sel$deaths),
                                       # confirmed=sum(aut_sel$confirmed), recovered=sum(aut_sel$recovered),
                                       low=100*btest$conf.int[1],
                                       high=100*btest$conf.int[2],
                                       pr=100*sum(aut_sel$sad_n)/sum(aut_sel$tot_n),
                                       bl=baseline$sad_bl, sd=baseline$sad_sd, 
                                       wordlist="sadness"))
  #positive
  btest <- binom.test(x=sum(aut_sel$pos_n), n = sum(aut_sel$tot_n))
  aut_res <- rbind(aut_res, data.frame(hits=sum(aut_sel$pos_n), total=sum(aut_sel$tot_n),
                                       # activecases=sum(aut_sel$activecases), deaths=sum(aut_sel$deaths),
                                       # confirmed=sum(aut_sel$confirmed), recovered=sum(aut_sel$recovered),
                                       low=100*btest$conf.int[1],
                                       high=100*btest$conf.int[2],
                                       pr=100*sum(aut_sel$pos_n)/sum(aut_sel$tot_n),
                                       bl=baseline$pos_bl, sd=baseline$pos_sd, 
                                       wordlist="positive"))

  return(aut_res)
  
}

corr_emo_casediff <- function(df, casediff, iemo, iperiod)#casediff is either relative or absolute difference of a case variable (death, confirmed, etc)
{
  df$prcorr <- (df$pr-df$bl)/df$bl
  
  emo <- df$prcorr[df$wordlist==iemo & df$period==iperiod]
  cases <- df[df$wordlist==iemo & df$period==iperiod, casediff]
  print(paste("Correlations of", iemo, "with", casediff, "in", iperiod))
  return(cor.test(emo, cases, method="spearman"))
}

# functions for plots ####

plotTS <- function(df, mode="percent", title="Country TS", limits=NULL, factor=15)
{
  
  plotdf <- data.frame(date=rep(df$date,2), y=c(df$activecases, df$deaths), group=rep(c("Active activecases", "Cumulative deaths"), each=nrow(df)))
  plotdf %>% arrange(date) %>% arrange(rev(group)) %>% distinct(date, group, .keep_all=T) -> plotdf
  
  plotdf$group <- factor(plotdf$group, sort(unique(plotdf$group), decreasing = TRUE))
  plotdf %>% filter(y>0) -> plotdf
  
  cuts <- 10^seq(0,floor(log10(max(df$activecases))))
  tcuts <- 1+factor*log10(cuts)
  
  #stringency data frame, cut date where emotion date ends
  
  plotds = ds %>% 
    filter(date <= as.Date(last(df$date)))
  
    #make plot
    plt <- ggplot() +
      geom_point(data=df[df$activecases>0,], aes(x=date, y=1+factor*log10(activecases)), color="grey", size=1.5) +
      geom_point(data=df[df$deaths>0,], aes(x=date, y=1+factor*log10(deaths)), color="black", size=1.5, pch=15) +
      scale_y_continuous(sec.axis = sec_axis(~., name = "Number of activecases/deaths (log)", breaks = tcuts, labels=cuts))
  
  if (mode == "percent")
    plt <- plt +
    geom_ribbon(data=df, aes(x=date, ymin=100*(prlow-bl)/bl, ymax=100*(prhigh-bl)/bl, fill=wordlist, colour=wordlist), alpha=0.2, size=0) +
    geom_line(data=df, aes(x=date, y=100*(pr-bl)/bl, colour=wordlist))+
    #   #add stringency
    # geom_line(data=filter(plotds, country2==as.character(unique(df$country2))), aes(y=stringency, x=date))+
    ylab("% Difference over 2019 baseline")  
  if (mode == "Z")
    plt <- plt +
    geom_ribbon(data=df, aes(x=date, y=(pr-bl)/sd, ymin=(prlow-bl)/sd, ymax=(prhigh-bl)/sd, fill=wordlist, colour=wordlist), alpha=0.2, size=0) +
    geom_line(data=df, aes(x=date, y=(pr-bl)/sd, colour=wordlist))+
    ylab("Z-score") #y axis label
  
  plt <- plt +  ggtitle(title)+ #plot title
    scale_fill_manual(values = c("anxiety"="orange", "anger"="red", "sadness"="blue", "positive"="cyan"))+
    scale_colour_manual(values = c("anxiety"="orange", "anger"="red", "sadness"="blue", "positive"="cyan"))+
    theme_bw()+ theme(text=element_text(size=axisfontsize), axis.text=element_text(size=labelsize), 
                      axis.title.x = element_blank(),
                      legend.title=element_blank(), legend.position="bottom",
                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
    #xlab("Date") + 
    scale_x_date(date_breaks="1 month", date_labels = "%b")+
    geom_hline(yintercept = 0, colour = "grey50") #mean line
  
  
  if (!is.null(limits))
    plt <- plt + 
    scale_y_continuous(limits = limits, sec.axis = sec_axis(~., name = "Number of activecases/deaths (log)", breaks = tcuts, labels=cuts))
  
  
  return(plt)
}

addperiods <- function(plotlines, controlstart, date1, date2, date3, date4, date5, enddate, yb, yt){
  plotlines + geom_rect(aes(xmin = controlstart, xmax = controlend, ymin=yb, ymax=yt), colour="grey50",fill=fillperiods)+
    geom_rect(aes(xmin = date1, xmax = date2, ymin=yb, ymax=yt), colour="grey50", fill=fillperiods)+   
    geom_rect(aes(xmin = date2, xmax = date3, ymin=yb, ymax=yt), colour="grey50",fill=fillperiods)+   
    geom_rect(aes(xmin = date3, xmax = date4, ymin=yb, ymax=yt), colour="grey50",fill=fillperiods)+   
    geom_rect(aes(xmin = date4, xmax = date5, ymin=yb, ymax=yt), colour="grey50",fill=fillperiods)+   
    geom_rect(aes(xmin = date5, xmax = enddate, ymin=yb, ymax=yt), colour="grey50",fill=fillperiods)
}


# boxplots n days above zero: duration plots

plotndays <- function(df, title="Country TS", limits=NULL)
{

  ggplot(df, aes(y=ndays, colour=separdate, x=0.5, fill=separdate))+
    facet_wrap(~reorder(country, maxdays), ncol=6)+
    geom_boxplot(position="identity")+
    geom_point(aes(y=maxdays, x=0.5))+
    #geom_dotplot(binwidth = 3, dotsize=0.8, binaxis = "y", stackdir = "center")+
    ylab("N days in a row")+
    scale_colour_manual(values=c("Before"="grey70", "After"="purple" ))+
    scale_fill_manual(values=c("Before"=scales::alpha("grey70", 0.2), "After"=scales::alpha("purple", 0.2)))+
    theme_bw()+ theme(text=element_text(size=14), axis.text=element_text(size=9),
                      axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
                      legend.title=element_blank(), legend.position="bottom",
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(title)
}



# calculate the average per week after the outbreak

countryWeeks <- function (country = "Austria", outbreakDate="2020-03-01", blStart="2019-01-01", blEnd="2019-12-31", controlStart="2020-01-14", controlEnd="2020-02-14", nWeeks=5)
{
  #anxiety
  aut_anx <- read.csv(paste0('data_twitter_volumes/',country,'AnxVol.csv'))
  names(aut_anx) <- c("date","end","anx_n")
  aut_anx <- prepdata(aut_anx)
  
  #anger
  aut_ang <- read.csv(paste0('data_twitter_volumes/',country,'AngerVol.csv'))
  names(aut_ang) <- c("date","end", "ang_n")
  aut_ang <- prepdata(aut_ang)
  
  #sadness
  aut_sad <- read.csv(paste0('data_twitter_volumes/',country,'SadVol.csv'))
  names(aut_sad) <- c("date","end", "sad_n")
  aut_sad <- prepdata(aut_sad)
  
  #positive emotions
  aut_pos <- read.csv(paste0('data_twitter_volumes/',country,'PosemoVol.csv'))
  names(aut_pos) <- c("date","end", "pos_n")
  aut_pos<- prepdata(aut_pos)
  
  #total number of tweets
  aut_tot <- read.csv(paste0('data_twitter_volumes/',country,'TotVol.csv'))
  names(aut_tot) <- c("date","end", "tot_n")
  aut_tot <- prepdata(aut_tot)
  
  #proportions with terms per category 
  aut_all <- aut_tot%>%
    left_join(aut_anx)%>%
    left_join(aut_ang)%>%
    left_join(aut_sad) %>% 
    left_join(aut_pos) %>% 
    slice(-n()) #delete last row
  
  #aut_all %>% filter(date>="2019-01-01") -> aut_all
  
  #baseline over the year 2019 (day-wise)
  baseline <- aut_all %>% 
    filter(date >= blStart & date <= blEnd)%>%
    summarize(anx_n = sum(as.numeric(anx_n)), 
              ang_n = sum(as.numeric(ang_n)), 
              sad_n = sum(as.numeric(sad_n)), 
              pos_n = sum(as.numeric(pos_n)),
              tot = sum(as.numeric(tot_n))) %>%
    filter(!row_number()==nrow(aut_all))
  
  #control period (day-wise)
  controlperiod <- aut_all %>% 
    filter(date >= controlStart & date <= controlEnd)%>%
    summarize(anx_n = sum(as.numeric(anx_n)), 
              ang_n = sum(as.numeric(ang_n)), 
              sad_n = sum(as.numeric(sad_n)), 
              pos_n = sum(as.numeric(pos_n)),
              tot = sum(as.numeric(tot_n))) %>%
    filter(!row_number()==nrow(aut_all))
  
  aut_res <- NULL
  
  aut_res <- rbind(aut_res, data.frame(emo_n = baseline$anx_n, tot_n = baseline$tot, wordlist="anxiety",period="baseline"))
  aut_res <- rbind(aut_res, data.frame(emo_n = baseline$sad_n, tot_n = baseline$tot, wordlist="sadness",period="baseline"))
  aut_res <- rbind(aut_res, data.frame(emo_n = baseline$ang_n, tot_n = baseline$tot, wordlist="anger",period="baseline"))
  aut_res <- rbind(aut_res, data.frame(emo_n = baseline$pos_n, tot_n = baseline$tot, wordlist="positive",period="baseline"))
  
  aut_res <- rbind(aut_res, data.frame(emo_n = controlperiod$anx_n, tot_n = controlperiod$tot, wordlist="anxiety",period="control"))
  aut_res <- rbind(aut_res, data.frame(emo_n = controlperiod$sad_n, tot_n = controlperiod$tot, wordlist="sadness",period="control"))
  aut_res <- rbind(aut_res, data.frame(emo_n = controlperiod$ang_n, tot_n = controlperiod$tot, wordlist="anger",period="control"))
  aut_res <- rbind(aut_res, data.frame(emo_n = controlperiod$pos_n, tot_n = controlperiod$tot, wordlist="positive",period="control"))
  
  #date of outbreak
  date1 <- outbreakDate
  
  for (i in seq(1, nWeeks))
  {
    aut_all %>% filter(as.Date(date)>=as.Date(date1) & as.Date(date) <= as.Date(date1)+6) -> aut_sel
    aut_sel %>% filter(complete.cases(aut_sel)) -> aut_sel  
    
    aut_res <- rbind(aut_res, data.frame(emo_n = sum(aut_sel$anx_n), tot_n = sum(aut_sel$tot_n), wordlist="anxiety",period=paste("Week", i)))
    aut_res <- rbind(aut_res, data.frame(emo_n = sum(aut_sel$sad_n), tot_n = sum(aut_sel$tot_n), wordlist="sadness",period=paste("Week", i)))
    aut_res <- rbind(aut_res, data.frame(emo_n = sum(aut_sel$ang_n), tot_n = sum(aut_sel$tot_n), wordlist="anger",period=paste("Week", i)))
    aut_res <- rbind(aut_res, data.frame(emo_n = sum(aut_sel$pos_n), tot_n = sum(aut_sel$tot_n), wordlist="positive",period=paste("Week", i)))
    
    date1 <- as.Date(date1) + 7      
  }
  
  return(aut_res)
}

#function to read in total tweet and author data for some example countries
prepareTwitterChange = function(country='Austria'){

  aut_tw = read.csv(paste0('data_twitter_volumes/', country, 'TotVol.csv'))
  aut_auth = read.csv(paste0('data_twitter_volumes/', country, '_total_unique_authors.csv'))

  #delete the column with row numbers if one exists
    if(ncol(aut_auth) == 3){
            aut_auth$X = NULL
  }
  
  aut_tw = aut_tw %>% 
    rename(date = 'startDate', ntweets='numberOfDocuments') %>% 
    select(-endDate) %>% 
    mutate(date = as.Date(date)) %>% 
    filter(date > as.Date('2018-12-31') & date < as.Date('2020-04-21')) %>% 
    mutate(date = as.factor(date)) %>% 
    inner_join(aut_auth) %>% 
    mutate(date=as.Date(date)) %>% 
    filter(date < as.Date('2020-04-20')) %>%
    pivot_longer(names_to = 'Twitter', cols=c("unique_authors", "ntweets")) %>% 
    mutate(Twitter = recode(Twitter,  
                            unique_authors = 'Unique authors', 
                            ntweets = 'Total tweets')) %>% 
    mutate(weekday = lubridate::wday(date, label=T, abbr=F))  #add weekday labels
    
  
  #set language to produce English weekdays
  Sys.setlocale("LC_TIME", 'en_US.UTF-8')
  
  #baseline over the year 2019 per weekday
  baseline <- aut_tw %>% 
     filter(date < as.Date("2020-01-01"))%>%
    group_by(Twitter, weekday) %>% 
    summarise(bl= mean(value), 
              sd = sd(value)) %>% 
    ungroup()

  aut_tw <- inner_join(aut_tw, baseline, by=c('Twitter', 'weekday'))
  aut_tw$country = rep(country, nrow(aut_tw))
  return(aut_tw)
}
