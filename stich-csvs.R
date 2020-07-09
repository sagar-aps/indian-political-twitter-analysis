multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, function(x){read.csv(x, stringsAsFactors = F, sep=',')}))
}


full_data = multmerge("D:/G Drive Data/Projects/indian-political-twitter-analysis/tweet-data/raw")

write.csv(full_data,file = "FullData.csv")
