#Indian Start-up funding

# Libraries used
library(dplyr)
library(ggplot2)
library(plyr)
library(tidyr)
library(splitstackshape)
library(ggmap)
library(gridExtra)

#input all data
alldata <- read.csv("data/startup_funding.csv", stringsAsFactors = FALSE)
#explore data
str(alldata) 
head(alldata)


# Data Cleaning
# removing all blanks as NA
alldata[alldata==""] <- NA 

#removing .com, .in etc from data
alldata$StartupName <- gsub(".com", "", alldata$StartupName)
alldata$StartupName <- gsub(".co", "", alldata$StartupName)
alldata$StartupName <- gsub(".in", "", alldata$StartupName)
alldata$StartupName <- gsub(".io", "", alldata$StartupName)
alldata$StartupName <- gsub(".ai", "", alldata$StartupName)

# AmountInUSD was input as factor-  need to convert to integer
#this step removes comma from the digits
alldata$AmountInUSD <- gsub(",","", as.character(alldata$AmountInUSD))

#this step converts character to integer
alldata$AmountInUSD <- as.numeric(alldata$AmountInUSD)
class(alldata$AmountInUSD)


# Top 15 startup - based on investment
summary1 = alldata %>% select(StartupName,InvestorsName,AmountInUSD) %>% ddply(.(StartupName),summarise,sum=sum(AmountInUSD)) %>% arrange(desc(sum))
#plotting
ggplot(head(summary1,15),aes(x = reorder(StartupName,sum), y=sum, fill = 'StartupName', color = 'coral3')) + 
geom_bar(stat = "identity" ) +
theme(legend.position="none",axis.text.x = element_text(angle=90,vjust=0.5), title=element_text(color="coral3",size=10,face="bold")) +
labs(x="",y="Total Investment",title="Investors High Valued Startups" , size=10,face="bold") +
coord_flip() + 
scale_y_continuous(labels=scales::comma)

# Top 15 startup - based on number of investment
summary2 = alldata %>% select(StartupName) %>%  group_by(StartupName) %>% count() %>% arrange(desc(freq))
str(summary2)
head(summary2,15)
#plotting
ggplot(head(summary2,15),aes(x = reorder(StartupName,freq), y=freq, fill = 'StartupName')) + 
  geom_bar(stat = "identity", fill = "goldenrod1" ) +
  theme(legend.position="none",axis.text.x = element_text(angle=0,vjust=0.5), title=element_text(color="coral3",size=10,face="bold")) +
  labs(x="",y="Total Numbers",title="#Times funded" , size=10,face="bold") +
  coord_flip() 

# Hottest Industry
# by investment
summary3 = alldata %>% select(IndustryVertical,AmountInUSD) %>% ddply(.(IndustryVertical),summarise,sum=sum(AmountInUSD)) %>% arrange(desc(sum))
#plotting
ggplot(head(summary3,15),aes(x = reorder(IndustryVertical,sum), y=sum, fill = 'IndustryVertical')) + 
  geom_bar(stat = "identity" , fill = "chocolate1" ) +
  theme(legend.position="none",axis.text.x = element_text(angle=90,vjust=0.5), title=element_text(color="coral3",size=10,face="bold")) +
  labs(x="",y="Total Investment",title="Investors High Valued Industries" , size=10,face="bold") +
  #coord_flip() + 
  scale_y_continuous(labels=scales::comma)
#maximum number of start-Ups - donut-chart
alldata$IndustryVertical=tolower(alldata$IndustryVertical)
summary4 = alldata %>% select(IndustryVertical) %>%
group_by(IndustryVertical) %>% drop_na(IndustryVertical) %>% 
count() %>% 
mutate(perc=(freq/sum(freq))*100) %>%  #finding percentage and count 
arrange(desc(freq))

head(summary4) # this table shows that there are only three prominant categories
# if you see from the above more than 99% starups are related to three categories only
# like...
datadonut <- summary4[1:3,]
datadonut[4,1] <- "other"  
datadonut[4,2] <- as.integer(1)  
datadonut[4,3] <- as.numeric(36.0)  
str(datadonut)

datadonut$fraction = datadonut$perc / sum(datadonut$perc)
datadonut$ymax = cumsum(datadonut$fraction)
datadonut$ymin = c(0, head(datadonut$ymax, n=-1))

#plotting donut-chart using geom_rect
ggplot(datadonut, aes(fill=IndustryVertical, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="grey30") +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Hot Industry Veriticals")

# Favourite verticle of the investors
summary5 = alldata %>% select(SubVertical,AmountInUSD) %>% ddply(.(SubVertical),summarise,sum=sum(AmountInUSD)) %>% arrange(desc(sum))
#plotting
ggplot(head(summary5,15),aes(x = reorder(SubVertical,sum), y=sum, fill = 'SubVertical')) + 
  geom_bar(stat = "identity", fill = "aquamarine4" ) +
  theme(legend.position="none",axis.text.x = element_text(angle=90,vjust=0.5), title=element_text(color="coral3",size=10,face="bold")) +
  labs(x="",y="Total Investment",title="Investors High Valued Verticles" , size=10,face="bold") +
  coord_flip() + 
  scale_y_continuous(labels=scales::comma)

# Top 20 investors
alldata1=cSplit(alldata,"InvestorsName",sep=",",direction="long",drop=FALSE)
head(alldata1)
alldata1$InvestorsName = gsub("SoftBank Group", "Softbank", alldata1$InvestorsName)
alldata1$InvestorsName = gsub("SoftBank Group Corp", "Softbank", alldata1$InvestorsName)
alldata1$InvestorsName = gsub("SoftBank Vision Fund", "Softbank", alldata1$InvestorsName)
alldata1$InvestorsName = gsub("SoftBank", "Softbank", alldata1$InvestorsName)
alldata1$InvestorsName = gsub("SoftBank Corp*", "Softbank", alldata1$InvestorsName)


str(alldata1)
summary6 = alldata1 %>% select(InvestorsName,AmountInUSD) %>% ddply(.(InvestorsName),summarise,sum=sum(AmountInUSD)) %>% arrange(desc(sum))
#plotting
ggplot(head(summary6,20),aes(x = reorder(InvestorsName,sum), y=sum, fill = 'InvestorsName')) + 
  geom_bar(stat = "identity", fill = "cadetblue4") +
  theme(legend.position="none",axis.text.x = element_text(angle=90,vjust=0.5), title=element_text(color="coral3",size=10,face="bold")) +
  labs(x="",y="Total Investment",title="Top 15 Investors" , size=10,face="bold") +
  coord_flip() + 
  scale_y_continuous(labels=scales::comma)

# Hottest investment locations
  summary7 = alldata %>% 
  filter(!is.na(AmountInUSD)) %>% 
  filter(!is.na(CityLocation))%>%
  select(CityLocation,AmountInUSD) %>% 
  ddply(.(CityLocation),summarise,sum1 = sum(AmountInUSD)) %>%
  mutate(perc=round((sum1/sum(sum1))*100)) %>%
  arrange(desc(sum1))

summary7$perc=paste(summary7$perc,"%") # add % to the column
head(summary7)
ggplot(head(summary7,10),aes(x = reorder(CityLocation,sum1), y=sum1, fill = CityLocation)) + 
  geom_bar(stat = "identity" ) +
  theme(legend.position="none",axis.text.x = element_text(angle=90,vjust=0.5), title=element_text(color="coral3",size=10,face="bold")) +
  labs(x="",y="Total Investment",title="15 Hot Locations" , size=10,face="bold") +
  geom_label(aes(label=perc)) +
    coord_flip() + 
  scale_y_continuous(labels=scales::comma)

#Locations on Map
alldata1=cSplit(alldata,"CityLocation",sep="/",direction="long",drop=FALSE)

summary11 = alldata1 %>% 
  filter(!is.na(AmountInUSD)) %>% 
  filter(!is.na(CityLocation))%>%
  select(CityLocation,AmountInUSD) %>% 
  ddply(.(CityLocation),summarise,sum1 = sum(AmountInUSD)) %>%
  arrange(desc(sum1))


#converting states/provinces into character
summary11$CityLocation <- as.character(summary11$CityLocation)

for (j in 1:nrow(summary11)) {
  latlon = geocode(summary11$CityLocation[j])
  summary11$lon[j] = as.numeric(latlon[1])
  summary11$lat[j] = as.numeric(latlon[2])
}

write.csv(summary11, "startuplocs.csv")
x <- c(19,23,24,27,28,29,30,31,33,37,39,41,42,43,45,46,51)


for (j in 39) {
  latlon = geocode(summary11$CityLocation[j])
  summary11$lon[j] = as.numeric(latlon[1])
  summary11$lat[j] = as.numeric(latlon[2])
}


India_center = geocode("India")
India <-ggmap(get_map(location=India_center,zoom=5), extent="panel")

circle_scale <- .0000000015

#par(mfrow=c(2,1)) - Not working

India + geom_point(aes(x=lon, y=lat), data=summary11, col="red",
                   alpha=0.4, size=summary11$sum1*circle_scale)
India + geom_point(aes(x=lon, y=lat), data=summary11, col="blue",
                   alpha=0.4, size=6)
#summary11[39,]
summary11
#write.csv(summary11,"loc2.csv")
# Investment Trend over time.

#Date Conversion
# As the date is given in char format, we need to convert it in data
alldata$Date= as.Date(alldata$Date,"%d-%m-%Y")
str(alldata$Date)

alldata$Month=as.factor(format(alldata$Date,"%m"))
alldata$Year=as.factor(format(alldata$Date,"%Y"))
alldata$Monyr=paste(alldata$Year,alldata$Month,sep="-")

summary8 <- alldata %>%  filter(!is.na(AmountInUSD)) %>%
  select(Year, AmountInUSD) %>% group_by(Year) %>%
  ddply(.(Year), summarise, sum = sum(AmountInUSD)) %>%
  arrange(desc(sum))

ggplot(data= summary8, aes(x=Year, y=sum/1000000000)) +
  geom_bar(stat="identity" , fill = "goldenrod1") +
  labs(x="Year", y="Investment (Billion USD)", colour="Year", 
       title="Investment per Year") +
  theme_bw(base_size = 10)

# Month specific
summary9 <- alldata %>%  filter(!is.na(AmountInUSD)) %>%
  select(Month, AmountInUSD) %>% group_by(Month) %>%
  ddply(.(Month), summarise, sum = sum(AmountInUSD)) %>%
  arrange(desc(sum))

ggplot(data= summary9, aes(x=Month, y=sum/1000000000)) +
  geom_bar(stat="identity" , fill = "darkorange1") +
  labs(x="Month", y="Investment (Billion USD)", colour="Month", 
       title="Month-Wise Investment") +
  theme_bw(base_size = 10)

# Yearly - Quarter-wise ( using Facet)
# is there any favourite quarter?
alldata$qtr <- "NA"

for (i in 1:nrow(alldata) )
{
  #q1
  if (alldata$Month[i] == "01")
  { alldata$qtr[i] <- "Q1" }
  if (alldata$Month[i] == "02")
  { alldata$qtr[i] <- "Q1" }
  if (alldata$Month[i] == "03")
  { alldata$qtr[i] <- "Q1" }
  #Q2
  if (alldata$Month[i] == "04")
  { alldata$qtr[i] <- "Q2" }
  if (alldata$Month[i] == "05")
  { alldata$qtr[i] <- "Q2" }
  if (alldata$Month[i] == "06")
  { alldata$qtr[i] <- "Q2" }
  
  #Q3
  if (alldata$Month[i] == "07")
  { alldata$qtr[i] <- "Q3" }
  if (alldata$Month[i] == "08")
  { alldata$qtr[i] <- "Q3" }
  if (alldata$Month[i] == "09")
  { alldata$qtr[i] <- "Q3" }
  
  #q4
  if (alldata$Month[i] == "10")
  { alldata$qtr[i] <- "Q4" }
  if (alldata$Month[i] == "11")
  { alldata$qtr[i] <- "Q4" }
  if (alldata$Month[i] == "12")
  { alldata$qtr[i] <- "Q4" }
}

summary10 <- alldata %>%  filter(!is.na(AmountInUSD)) %>%
  select(qtr, Year, AmountInUSD) %>% group_by(qtr) %>%
  #ddply(.(qtr), summarise, sum = sum(AmountInUSD)) %>%
  arrange(desc(sum))

ggplot(data= summary10, aes(x=qtr, y=AmountInUSD/1000000)) +
  geom_bar(stat="identity" , fill = "darkorchid4") +
  facet_grid(. ~Year) +
  theme_bw(base_size = 10) +
labs(x="Quarter", y="Investment (million USD)", colour="Month", 
     title="Year-Quarter-Wise Investment") +
  theme_bw(base_size = 10)








