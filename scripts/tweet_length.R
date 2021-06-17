
#To get the datasets that are read below, first write tweet text to files from the .json files in TweetTextData/,
# using zcat US.json.gz | jq -r '.full_text | gsub("[\\n\\t]";"")' > US_text & in the terminal (see script extract_tweets)


# calculate tweet_length
  us = read.csv("./TweetTextData/US_tweet_length", header=F)
  summary(us)
  hist(us$V1)
  #mean US: 99.40607  
  
  uk = read.csv("./TweetTextData/UK_tweet_length", header=F)
  summary(uk)
 #mean 114.8393
  # median 92
  hist(uk$V1)

  spain = read.csv("./TweetTextData/Spain_tweet_length", header=F)
  summary(spain)
 # median 88
  hist(spain$V1)
  