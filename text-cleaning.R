#Text Cleaning Section

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
str(data)

gsub("[^[:alnum:]]", " ", data_eng$text)

data_eng <- data_eng %>% mutate_at("text",as.character) #Converting text from factor to character

#data<- data %>% mutate(cleantext = tm_map(text, toSpace, "/") ) #Unsure of how this works but it doesn't
corp <- Corpus(VectorSource(data_eng$text))

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
dtm_m <- tidy(corp_dtm)
head(Terms(corp_dtm))

head(dtm_m)
str(dtm_m)
head(dtm_m[order(-count)])

dtm_sorted <- dtm_m[order(dtm_m$count ),]

tail(dtm_sorted)
# dtm_m <- as.matrix(corp_dtm) Coercing this DTM needs enourmous memeory space

# Sort by descearing value of frequency
#dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
#dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words

dtm_m <- sort(dtm_m)

pakCases <- tm_map(corp$content,grep, pattern = "\\<pak")
sum(unlist(pakCases))

## count frequency of "miners"
minerCases <- tm_map(myCorpusCopy, grep, pattern = "\\<miners")
sum(unlist(minerCases))


# # replace "miners" with "mining"
 myCorpus <- tm_map(myCorpus, gsub, pattern = "miners", replacement = "mining")


#Making Word clouds