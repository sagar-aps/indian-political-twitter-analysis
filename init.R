install.packages("syuzhet") #For NLP Analysis
install.packages("tm") #To remove stopwords
install.packages("snowballc") #To reduce words to roots
install.packages("wordcloud") #To make a word cloud
install.packages("RColorBrewer")
install.packages("devtools")
install_github("ropensci/cld2")
install_github("ropensci/cld3")
library(devtools)
library(cld2)
library(cld3)
library(syuzhet)
library(dplyr)
library(stringi)
library(ggplot2)
library(tm)
library(snowballc)
library(wordcloud)
library(RColorBrewer)


colnames(full_data)
data[1,]

meanchar <- full_data %>% group_by(username) %>% summarise(mean(stri_count_words(username)))

full_data %>% group_by(username)%>% summarise(stri_count_fixed(text, " "))

head(full_data %>% mutate(noofwords = stri_count_fixed(text, " ")))

data <-full_data %>% mutate(noofwords = stri_count_fixed(text, " "))
#data to avoid messing up original dataset

meanchar  <- data %>% group_by(username) %>% summarise(mean(noofwords))

#Text Cleaning Section
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
str(data)

data <- data %>% mutate_at("text",as.character) #Converting text from factor to character

#data<- data %>% mutate(cleantext = tm_map(text, toSpace, "/") ) #Unsure of how this works but it doesn't
corp <- Corpus(VectorSource(data$text))

# Convert the text to lower case
corp <- tm_map(corp, content_transformer(tolower))

# remove punctuation
corp <- tm_map(corp, removePunctuation)

# Remove english common stopwords
corp <- tm_map(corp, removeWords, stopwords("english"))

# Eliminate extra white spaces
corp <- tm_map(corp, stripWhitespace)

# Text stemming - which reduces words to their root form
corp <- tm_map(corp, stemDocument)

corp <- tm_map(corp, toSpace, "/")
corp <- tm_map(corp, toSpace, "@")
corp <- tm_map(corp, toSpace, "\\|")

#specify your custom stopwords as a character vector and remove your own stop word
corp <- tm_map(corp, removeWords, c("hrs", "govt", "days")) 


# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(corp[[i]]))
}

#Build Term Document Matrix

corp_dtm <- TermDocumentMatrix(corp)
inspect(corp_dtm)

dtm_m <- as.matrix(corp_dtm)

# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

#Making Word clouds




#Finding Language of all text
data_eng <- data.frame(lapply(data$text,as.character),stringsAsFactors = FALSE)
detect_language(data_eng)

data_eng <- data %>% mutate(cld2 = cld2::detect_language(text = text, plain_text = FALSE,lang_code=FALSE), cld3 = cld3::detect_language(text = text))

head(data_eng)

mean(data_eng$cld2=='en')

data_eng%>%group_by(cld2)%>%summarise(n())
