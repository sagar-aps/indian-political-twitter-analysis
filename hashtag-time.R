#To make an animatation of hashtag across time (days)

install.packages("dplyr")
library(dplyr)

hashed_data <- full_data %>% filter(nchar(hashtags)>0)

#Filter tweets with hashtags
hashed_data <-hashed_data %>% mutate(noofhashes = stri_count_fixed(hashtags, " "))

#Find tweets with multiple hashtags or tokenize hastags

hashs <- unlist(strsplit(hashed_data$hashtags, "[#^[:alnum:]]"))
# [1] "flu"      "fever"    "feverish" "fever"    "cold"


hash_unique<-as.vector(unique(hashs))

table(hash_unique)
length(hash_unique)

#Extract day from timestamp

#Make table hashtag
