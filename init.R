install.packages("syuzhet") #For NLP Analysis
install.packages("tm") #To remove stopwords
install.packages("snowballc") #To reduce words to roots
install.packages("wordcloud") #To make a word cloud
install.packages("RColorBrewer")

library(syuzhet)
library(dplyr)
library(stringi)
library(ggplot2)
library(tm)
library(snowballc)
library(wordcloud)
library(RColorBrewer)
colnames(full_data)

meanchar <- full_data %>% group_by(username) %>% summarise(mean(stri_count_words(username)))
meanchar

stri_count_words("Does this work")

full_data %>% group_by(username)%>% summarise(stri_count_fixed(text, " "))


head(full_data %>% mutate(noofwords = stri_count_fixed(text, " ")))

data <-full_data %>% mutate(noofwords = stri_count_fixed(text, " "))
meanchar  <- data %>% group_by(username) %>% summarise(mean(noofwords))


#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
str(data)

data <- data %>% mutate_at("text",as.character) #Converting text from factor to character

data<- data %>% mutate(cleantext = tm_map(text, toSpace, "/") ) #Unsure of how this works but it does

TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
