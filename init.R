install.packages("syuzhet") #For NLP Analysis
install.packages("tm") #To remove stopwords
install.packages("snowballc") #To reduce words to roots
install.packages("wordcloud") #To make a word cloud
install.packages("RColorBrewer")
install.packages("devtools")
install_github("ropensci/cld2")
install_github("ropensci/cld3")
install.packages(tidytext)
library(devtools)
library(cld2)
library(cld3)
library(syuzhet)
library(dplyr)
library(stringi)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(data.table) # Replace NAs
library(tidytext)

colnames(full_data)
data[1,]


head(meanchar)


data <-full_data %>% mutate(noofwords = stri_count_fixed(text, " "))
#data to avoid messing up original dataset
head(data)


#Plot of average Tweet Length
meanchar  <- data %>% group_by(username) %>% summarise(avg=mean(noofwords))
head(meanchar)

meanchar_plot <- meanchar %>% ggplot(aes(x=reorder(username,avg), y=avg, fill = username))+
  geom_bar(stat="identity")+
  coord_flip()+ 
  theme(legend.position = "none")+
  ylab("Average Tweet length")+
  xlab("Tweeter")
  

meanchar_plot


#Who tweets the most?

tweetstats <- data%>%
  group_by(username)%>%
  summarise(nooftweets = n())%>%
  arrange_at("nooftweets",desc)
  
tweetstats %>% ggplot(aes(x=reorder(username,nooftweets),y=nooftweets,fill=username))+
  geom_bar(stat="identity")+
  coord_flip()+ 
  theme(legend.position = "none")+
  ylab("Number of tweets from 2014-2020")+
  xlab("Tweeter")#+
 # scale_y_log10()



#Finding Language of all text filter for only eng
data_eng <- data.frame(lapply(data$text,as.character),stringsAsFactors = FALSE)
detect_language(data_eng)

data_eng <- data %>% mutate(cld2 = cld2::detect_language(text = text, plain_text = FALSE,lang_code=TRUE), cld3 = cld3::detect_language(text = text))

head(data_eng)
data_eng%>%group_by(cld2)%>%summarise(n())
data_eng <- data_eng %>% mutate_if(is.character, ~replace(., is.na(.), 0))
mean(data_eng$cld2=="en")

data_only_eng <- data_eng %>% filter(cld2 == "en" & cld3 == "en")








