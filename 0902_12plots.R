rm(list=ls())
library("data.table")
library("ggplot2")
library("dplyr")
library("purrr")
library("reshape")
library("openxlsx")
library("BBmisc")

# 
setwd("/Users/alice/Desktop/RA/Air_bnb_YuFeng_Project")

# load data
city <- c("austin","boston","chicago","denver","nashville","new-orleans","oakland",
          "portland","san-diego","san-francisco","santa-cruz-county","washington-dc")

df <- c("df.aus","df.bos","df.chi","df.den","df.nash","df.new","df.oak","df.por",
        "df.dieg","df.fran","df.santa","df.wash")

sum <- c("sum.aus","sum.bos","sum.chi","sum.den","sum.nash","sum.new","sum.oak","sum.por",
         "sum.dieg","sum.fran","sum.santa","sum.wash")

for (i in 1:length(city)){
  f <- df[i]
  fc <- city[i]
  filename <- paste0("/Users/alice/Desktop/RA/Air_bnb_YuFeng_Project/airbnb_data/",city[i],"/listings.csv")
  assign(f,fread(filename))
}

# year-month
for (i in 1:length(city)){
  f <- df[i]
  fc <- get(f)$last_scraped
  s <- sum[i]
  count <- rep(1, len = length(fc))
  ym <- format(as.Date(fc), "%y-%m")
  ymd <- format(as.Date(fc), "%y-%m-%d")
  dat <- data.frame("ym" = ym,
                    "ymd" = ymd,
                    "count"=count)
  count.day <- aggregate(count ~ ymd + ym, dat, FUN = "sum") # sum counts for date
  assign(s,aggregate(count ~ ym, count.day, FUN = "mean")) # sum counts for month
}

# plot year-month for each city

par(mfrow=c(2,2))
for (i in 1:length(city)){
  count <- get(sum[i])$count
  ym <- get(sum[i])$ym
  plot(x = ym, y = count, col = "green",
       main = paste0("Monthly listings ", city[i]))
}

# merge 12 cities

tplot <- list(sum.aus,sum.bos,sum.chi,sum.den,sum.nash,sum.new,sum.oak,sum.por,
     sum.dieg,sum.fran,sum.santa,sum.wash) %>% reduce(full_join, by = "ym")
colnames(tplot) <- c("ym", print(city))

# 4 plots for 3 cities, respectively

tdata <- na.omit(melt(tplot, id="ym"))

# glimpse(tdata)

for (i in c(1,4,7,10)){
  f <- tdata[as.character(tdata$variable) %in% c(city[i],city[i+1],city[i+2]), ]
  g <- ggplot(aes(x = ym, y = value, color = variable), data = f) +
    geom_point() +
    geom_point(data = f, aes(y = value), size = 2) +
    xlab("year-month") + ylab("listings") + ggtitle("Airbnb Listings in US Cities")
  print(g)
}


# read xlsx

code <- read.xlsx("Lisence-Tax-Airbnb.xlsx", sheet = 3, detectDates = T)
code$date_changed <- format(as.Date(code$date_changed), "%y-%m")

cdata <- merge(tdata, code, by.x = c("variable", "ym"), by.y = c("city","date_changed"), all = TRUE)

cdata$is.license[is.na(cdata$is.license)] <- 0
cdata$is.tax[is.na(cdata$is.tax)] <- 0

for (i in 2:nrow(cdata)){
  if (cdata$variable[i] == cdata$variable[i-1] & cdata$is.license[i-1] == 1){
    cdata$is.license[i] = 1
  } else {
    cdata$is.license[i] = cdata$is.license[i]
    }
}

for (i in 2:nrow(cdata)){
  if (cdata$variable[i] == cdata$variable[i-1] & cdata$is.tax[i-1] == 1){
    cdata$is.tax[i] = 1
  } else {
    cdata$is.tax[i] = cdata$is.tax[i]
  }
}

# remove data without listings records
cdata <- cdata[-which(is.na(cdata$value)),]
# season
cdata$month <- as.numeric(substr(cdata$ym, 4, 5))

# run regression + year*month; package: lfe felm() function for large data 

summary(lm(normalize(value) ~ variable + factor(ym) + is.tax, data = cdata))

summary(lm(normalize(value) ~ variable + factor(month) + is.tax, data = cdata))

summary(lm(normalize(value) ~ variable + factor(month) + is.license, data = cdata))

summary(lm(normalize(value) ~ variable * (factor(month) + is.tax), data = cdata))

summary(lm(normalize(value) ~ variable * (factor(month) + is.license), data = cdata))

normalize(cdata$value)

hist(cdata$value)
hist(normalize(cdata$value))
