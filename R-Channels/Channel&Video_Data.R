

ChannelURL<-function(URL)
{
  library(stringr)
  library(XML)
  library(RCurl)
  #sample URL taken for testing
  #URL<-"http://www.youtube.com/user/Switchfoot/videos"
  Channel_mainpage <- getURL(URL,verbose=T, header=F) 
  Channel_mainpagecount <- readLines(tc <- textConnection(Channel_mainpage)); close(tc)  
  
  #Channel_maintree <- htmlTreeParse(Channel_mainpagecount, error=function(...){}, useInternalNodes = T)

  return ( htmlTreeParse(Channel_mainpagecount, error=function(...){}, useInternalNodes = T) )
}

######
#   Channel_link<-function(Channel_maintree)
#   {
#     Channel_link<- xpathSApply(Channel_maintree, "//*/span[@class='about-channel-link-text']", xmlValue)
#     Channel_link<-gsub("*\\n|^\\s+|\\s+$", '', Channel_link)
#     return(Channel_link)
#   }


Channel_subscribers<-function(Channel_maintree)
{

  Channel_subscribers<- xpathSApply(Channel_maintree, "//*/span[@class='yt-subscription-button-subscriber-count-branded-horizontal']", xmlValue)
  Channel_subscribers<-str_replace_all(Channel_subscribers,",","")
  return(Channel_subscribers)

}

Channel_Links<-function(Channel_maintree)
{
  Channel_Links<-xpathSApply(Channel_maintree, "//*/a[@class='about-channel-link yt-uix-redirect-link about-channel-link-with-icon']", xmlGetAttr,"href")
  return(Channel_Links)
}
 
Channel_Buttons_info<-function(Channel_maintree)
{
  Channel_Buttons_info<-xpathSApply(Channel_maintree, "//*/span[@class='yt-uix-button-content']", xmlValue)
  Channel_Buttons_info<-gsub("*\\n|^\\s+|\\s+$", '', Channel_Buttons)
  return(Channel_Buttons_info)
}


Videos_data <-function (Channel_maintree)
{
  Name <- xpathSApply(Channel_maintree, "//*/a[@class='yt-uix-sessionlink yt-uix-tile-link  spf-link  yt-ui-ellipsis yt-ui-ellipsis-2']", xmlValue)
  Views<- xpathSApply(Channel_maintree, "//*/ul[@class='yt-lockup-meta-info']", xmlValue)
  # vedio time for which it runs
  Duration<-xpathSApply(Channel_maintree, "//*/span[@class='video-time']", xmlValue)
  Duration<-as.numeric(sub(":",".",Duration))
  
  Vedio_link<-xpathSApply(Channel_maintree, "//*/a[@class='ux-thumb-wrap yt-uix-sessionlink contains-addto  spf-link ']", xmlGetAttr,"href")
  Vedio_link<-paste("http://www.youtube.com",Vedio_link,sep="")

  Videos<-data.frame(Name=Name,Url=Vedio_link,Views=Views,Duration=Duration)
  
  tmp<-str_replace_all(Videos$Views,",","")
  #number of views
  Views_no<-sub(" views.*","",tmp)
  
  # time scraping and manipulation 
  # Views_time e.g. 11 months
  Views_time<-sub(".*views","",tmp)
  Views_time<-sub(" ago","",Views_time)
  #tells which vector variables have months in it
  day_no<-grep("day",Views_time)
  months_no<-grep("months",Views_time)
  year_no<-grep("year",Views_time)
  
  Views_time_months<-sub(" day","",Views_time)
  #Views_time_months<-sub(" days","",Views_time_months)
  Views_time_months<-sub(" month","",Views_time_months)
  #Views_time_months<-sub(" months","",Views_time_months)
  #Views_time_months<-sub(" years","",Views_time_months)
  Views_time_months<-sub(" year","",Views_time_months)
  Views_time_months<-as.numeric(sub("s","",Views_time_months))
  
  #considering each month has 30 days
  Views_time_months[day_no]<-Views_time_months[day_no]/30
  Views_time_months[year_no]<-Views_time_months[year_no]*12
  
  #data frame Videos manipulation :: adding new columns
  Videos$Views_no<-as.numeric(Views_no)
  Videos$Views_time_months<-Views_time_months
  
  Videos_orignal<-Videos
  
  # removing Na values introduced in the character to numeric conversion of Views_no
  Videos<-Videos[!is.na(Videos$Views_no),]
  
  # might cause problem if a vedio is removed
  rownames(Videos)<-1:nrow(Videos)
  return(Videos)
}