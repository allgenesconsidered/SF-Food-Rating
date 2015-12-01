#Created By Michael Olvera
#2015

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(data.table)
library(XML)

###########
#PREPING!!#
###########

#Generate Maps of SF and Downtown
sfMap <- ggmap(get_stamenmap( bbox = 
              c(left = -122.531067, bottom = 37.701973, right = -122.352711, top = 37.816651),
              zoom = 14, maptype = "toner-lite"))
downtownMap <- ggmap(get_stamenmap( bbox = 
              c(left = -122.4267, bottom = 37.7850, right = -122.3917, top = 37.8050),
              zoom = 16, maptype = "toner-lite"))

#Load in the list of businesses and health ratings
sfMapbuisnessListP <- read.csv("./SFFoodProgram_complete/businesses_plus.csv")
ratings <- read.csv("./SFFoodProgram_complete/inspections_plus.csv")
violations <- read.csv('./SFFoodProgram_complete/violations_plus.csv')
#Remove NA ratings and combine repetative scores as an average
ratings <- ratings[complete.cases(ratings),1:2]
ratings <- aggregate(Score ~ business_id, FUN = mean, data = ratings)

#Merge the two datasets adn remove useless information
dat <- merge(sfMapbuisnessListP, ratings, by = "business_id", all.y = TRUE)
dat <- dat[, c(1,2,3,6,7,17)]

#Sum total number of violations as a data frame
vTotal <- as.data.frame(table(violations[,1]))
colnames(vTotal) <- c('business_id', 'violations')
#Change from factor to integer
vTotal <- data.frame(lapply(vTotal, as.integer), stringsAsFactors=FALSE)
#Merge with dat
healthscores <- merge(dat, vTotal, by = 'business_id', all.x = TRUE)
healthscores$violations[is.na(healthscores$violations)] <- 0

#Also generate datasets with high and low health scores to plot independantly
highscores <- healthscores[which(healthscores$Score > 90),]
lowscores <- healthscores[which(healthscores$Score <= 80),]

#Also generate a bonus dataset with places label 'High risk of vermin infestation' 
verm<- violations[which(violations$description == 'High risk vermin infestation'),]
highverm <- merge(dat, verm, by = 'business_id', all.y = TRUE)
highverm <- highverm[complete.cases(highverm),]

###########
#PLOTTING!#
###########

maptitle = c("Restaurants in SF by Health Score", "Restaurants With Health Score > 90",
             "Restaurants With Health Score =< 80")
lowcolor = c('firebrick','greenyellow','firebrick')
highcolor = c('palegreen3','palegreen3' ,'orange')

datamap = healthscores
i = 1

sfMap + geom_point(data = datamap, aes(x = longitude, y = latitude, colour = Score ),
                   alpha = 0.8, size = 1.7) +
#   geom_text(data = datamap, colour = 'grey0', aes(x = longitude, y = latitude , label = name, textface = 'bold'),
#             size = 0.5, vjust = -2) +
  theme_bw() +
  scale_color_gradient(name = "Health\nScore",low = lowcolor[i], high = highcolor[i], guide = 'legend') +
  labs(title = maptitle[i]) +
  theme(
    text = element_text(face = 'bold', size = 12),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave(filename = 'Text_sfhealthscoremap.png', dpi = 720)


i = 2
downtownMap + geom_point(data = highscores, aes(x = longitude, y = latitude, colour = Score, text = name ),
                   alpha = 0.9, size = 2.5) +
  geom_text(data = highscores, aes(x = longitude, y = latitude , label = name, fontface = 'bold'),
            size = 0.6, vjust = -2) +
  theme_bw() +
  scale_color_gradient(name = "Health\nScore",low = lowcolor[i], high = highcolor[i], guide = 'legend') +
  labs(title = maptitle[i]) +
  theme(
    text = element_text(face = 'bold', size = 12),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave(filename = 'Text_dthighscoremap.png', dpi = 900)


sfMap + geom_point(data = highverm, aes(x = longitude, y = latitude, colour = 'magenta', text = name),
                   alpha = 0.7, size = 1.7) +
  geom_text(data = highverm, colour ='black', aes(x = longitude, y = latitude , label = name, fontface = 'bold'),
            size = 0.4, vjust = -2.5) +
  theme_bw() +
  labs(title = "Restaurants With 'High Risk Vermin Infestation'") +
  theme(
    text = element_text(face = 'bold', size = 12),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave(filename = 'sfhighvermmap.png', dpi = 720)





#Generate an ECDF plot of the scores
ggplot(data = dat, aes(x = Score)) +
  stat_ecdf(geom = 'step', size = 1.3, colour = 'firebrick3') +
  theme_bw() +
  scale_x_continuous('Health score',expand = c(0,0)) +
  scale_y_continuous('Cumulative Probability') +
  ggtitle('ECDF of Health Scores') +
  theme(
    text = element_text(face = 'bold', size = 12),
    rect = element_rect(size = 1.3, colour = 'black'),
    legend.position = 'none',
    panel.border = element_rect(colour = 'grey0'),
    panel.grid.major = element_line(colour = 'grey', linetype = 2),
    panel.grid.minor = element_line(colour = 'grey', linetype = 2),
    panel.background = element_blank()
  )

ggsave('SFecdf.png', dpi = 720)

ggplot() +
  geom_bar(data = healthscores, aes(x = violations), colour = 'black', fill = 'firebrick', binwidth = 1) +
  scale_x_continuous('Number of Health Violations',expand = c(0,0)) +
  scale_y_continuous('Number of Restaurants',expand = c(0,0), limits = c(0,150)) +
  theme_bw() +
  ggtitle('Health Code Violations (Violations > 0)') +
  theme(
    text = element_text(face = 'bold', size = 12),
    rect = element_rect(size = 1.3, colour = 'black'),
    legend.position = 'none',
    panel.border = element_rect(colour = 'grey0'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

ggsave('SFviolations.png', dpi = 720)
##################################################################################


testdat <- as.data.frame(tail(healthscores))
testdat$yelpreviews <- 0
testdat$yelpcat <- 0
testdat$yelpprice <- 0
testdat$yelpscore <- 0

CURRENT_URL <- 'http://www.yelp.com/biz/india-clay-oven-san-francisco-3'
resI <- 4
data <- htmlParse(CURRENT_URL)


xpathreviews <- '//*[@id="wrap"]/div[3]/div/div[1]/div/div[3]/div[1]/div/div[1]/div[1]/span/span'
testdat[resI,10] <- xpathSApply(data, xpathreviews, xmlValue)
xpathcat <- '//*[@id="wrap"]/div[3]/div/div[1]/div/div[3]/div[1]/div/div[2]/span[2]/a'
testdat[resI,11] <- xpathSApply(data, xpathcat, xmlValue)
xpathprice <- '//*[@id="super-container"]/div/div/div[2]/div[1]/div[2]/ul/li[3]/div[2]/dl/dd'
testdat[resI,12] <- xpathSApply(data, xpathprice, xmlValue)
xpathscore <- '//*[@id="wrap"]/div[3]/div/div[1]/div/div[3]/div[1]/div/div[1]/div[1]/div/meta'
testdat[resI,13] <- xpathSApply(data, xpathscore, xmlGetAttr, 'content')


consumerKey = "xxxx"
consumerSecret = "xxxx"
token = "xxxx"
token_secret = "xxxx"

require(httr)
require(httpuv)
require(jsonlite)
# authorization
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)
limit <- 10

# 10 bars in Chicago
yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit,"&location=Chicago%20IL&term=bar")
locationdata=GET(yelpurl, sig)
locationdataContent = content(locationdata)
locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
head(data.frame(locationdataList))
