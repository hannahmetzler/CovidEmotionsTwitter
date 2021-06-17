library(dplyr)

df <- read.table("USroberta.dat", stringsAsFactors=F, colClasses=c("character","character", rep("numeric", 11)), header=T)
df %>% group_by(date) %>% summarize(anger=mean(anger>0.9), fear=mean(fear>0.9), positive=mean(joy>0.9 | love>0.9 | optimism>0.9), sadness=mean(sadness>0.9)) -> ddf
write.csv(ddf, "USRoberta.csv", row.names=F)

fdf <- read.csv("USfoll.csv", stringsAsFactors = F, colClasses=c("character","character", "integer"), header=F)
df$nfoll <- fdf$V3

df %>% filter(nfoll<5000) %>% group_by(date) %>% summarize(anger=mean(anger>0.9), fear=mean(fear>0.9), positive=mean(joy>0.9 | love>0.9 | optimism>0.9), sadness=mean(sadness>0.9)) -> ddf
write.csv(ddf, "USRoberta-5K.csv", row.names=F)

df %>% filter(nfoll<2000) %>% group_by(date) %>% summarize(anger=mean(anger>0.9), fear=mean(fear>0.9), positive=mean(joy>0.9 | love>0.9 | optimism>0.9), sadness=mean(sadness>0.9)) -> ddf
write.csv(ddf, "USRoberta-2K.csv", row.names=F)

df2 <- read.table("ESroberta.dat", stringsAsFactors=F, colClasses=c("character","character", rep("numeric", 11)), header=T)
df2 %>% group_by(date) %>% summarize(anger=mean(anger>0.9), fear=mean(fear>0.9), positive=mean(joy>0.9 | love>0.9 | optimism>0.9), sadness=mean(sadness>0.9)) -> ddf2
write.csv(ddf2, "ESRoberta.csv", row.names=F)

fdf <- read.csv("ESfoll.csv", stringsAsFactors = F, colClasses=c("character","character", "integer"), header=F)
df2$nfoll <- fdf$V3

df2 %>% filter(nfoll<5000) %>% group_by(date) %>% summarize(anger=mean(anger>0.9), fear=mean(fear>0.9), positive=mean(joy>0.9 | love>0.9 | optimism>0.9), sadness=mean(sadness>0.9)) -> ddf
write.csv(ddf, "ESRoberta-5K.csv", row.names=F)

df2 %>% filter(nfoll<2000) %>% group_by(date) %>% summarize(anger=mean(anger>0.9), fear=mean(fear>0.9), positive=mean(joy>0.9 | love>0.9 | optimism>0.9), sadness=mean(sadness>0.9)) -> ddf
write.csv(ddf, "ESRoberta-2K.csv", row.names=F)
