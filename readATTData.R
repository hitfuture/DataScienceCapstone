library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
require(lubridate)

library(ggplot2)
library(ggthemes)
library(wordcloud)

cleanData <- function (.data) {
        
        data <- .data%>%filter(`Usage Type`=="Web")
        data <- data%>%
                mutate(dateTime = as.POSIXct(paste(Date,"/2015", " ",Time,sep = ""),format="%m/%d/%Y %I:%M %p"),
                       dt = as.Date(dateTime),
                       hr= hour(dateTime))
        data[which(month(data$dt) == 1 &(year(data$dt)) == 2015),]$dt <-(data[which(month(data$dt) == 1 &(year(data$dt))== 2015),]$dt) + 365
        year(data[which(month(data$dateTime) == 1 &(year(data$dateTime)) == 2015),]$dateTime) <-year(data[which(month(data$dateTime) == 1 &(year(data$dateTime))== 2015),]$dateTime) + 1
        
         data
}

kevin <- fread("./data/9162232602_TextDataDetails.csv",header=TRUE)
kevin.web <- cleanData(kevin)
 
 
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]

p <- ggplot( kevin.web, aes( x = dt, y = hr, fill = Usage)) 
p + geom_raster()+
        theme_bw()
 


ggplot( kevin.web, aes( x = dateTime, y = Usage )) + 
        geom_bar( size = 3,stat="identity",color="red") + 
        theme_bw() + 
        theme( panel.grid.major.x = element_blank(), 
               panel.grid.minor.x =element_blank(), 
               panel.grid.major.y = element_line( color ="grey60", linetype ="dashed"))


ggplot( kevin.web, aes( x = dt, y = hr ,size = Usage,color=Usage)) + 
        geom_point(  ) + 
        theme_bw() + 
        theme( panel.grid.major.x = element_blank(), 
               panel.grid.minor.x =element_blank(), 
               panel.grid.major.y = element_line( color ="grey60", linetype ="dashed"))

shannon <- fread("./data/2089578950_TextDataDetails.csv",header=TRUE)
shannon.web <- cleanData(shannon)



pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]

p <- ggplot( data, aes( x = dt, y = hr, fill = Usage)) 
p + geom_raster() +
theme_bw()



ggplot( shannon.web, aes( x = dateTime, y = Usage )) + 
        geom_bar( size = 3,stat="identity") + 
        theme_bw() + 
        theme( panel.grid.major.x = element_blank(), 
               panel.grid.minor.x =element_blank(), 
               panel.grid.major.y = element_line( color ="grey60", linetype ="dashed"))


ggplot( shannon.web, aes( x = dt, y = hr ,size = Usage,color=Usage)) + 
        geom_point(  ) + 
        theme_bw() + 
        theme( panel.grid.major.x = element_blank(), 
               panel.grid.minor.x =element_blank(), 
               panel.grid.major.y = element_line( color ="grey60", linetype ="dashed"))