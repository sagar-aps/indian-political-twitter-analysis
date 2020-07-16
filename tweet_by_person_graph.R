ggplot(top_n(data,5), aes(x=date))+geom_density()

uniquedata<- unique(data)

usndat<- uniquedata %>% select(date,username) %>% group_by(username,date = as.Date(substr(date,1,10), c("%Y-%m-%d")))%>% summarize(n=n())

usndat %>% filter(year(date)==2020 & username != 'BJP4India') %>% ggplot(aes(x=date,y=n, color=username)) +
  geom_line()


usndat %>% filter(n>100)        

#Wtf was Kejriwal tweeting about 5000 + times in the same day??
uniquedata %>% mutate(date = as.Date(substr(date,1,10), c("%Y-%m-%d"))) %>% filter(date==as.Date('2015-02-04'),username=='ArvindKejriwal')
# Ah... Bot tweet. Remove to:

uniquedata %>% select()