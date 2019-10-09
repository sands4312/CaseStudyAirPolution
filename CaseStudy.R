#Reading the 1999 data
#Reading the text file into R
pm0 <- read.table("RD_501_88101_1999-0.txt",
                  comment.char = "#",     #ignoring the records/lines starting with #
                  header = FALSE,         #header is set to true if the first line of the file does not contain header name 
                  sep = "|",              #how the records are separated in the files that you are reading
                  na.strings = "")        #how the missing values are indicated in the file. In this file they are just left blank
dim(pm0)  #to find the total no. of rows and columns in the given table

#reading the names of the column which are in the first line of the file
#aregument 1 specifies the no. of the lines to read fromt he given file
cnames <- readLines("RD_501_88101_1999-0.txt", 1)

#the cnames we got from aboce are just a continuous string.. we want to separate them into a list
cnames <- strsplit(cnames, "|", fixed = TRUE)

#assigning the cnames to the columns in the file
names(pm0) <- cnames[[1]]

#the column names we assigned are not valid as they contain space in it
#this function takes the column names and makes it valid automatically
#by getting rid of unnecessary spaces, hyphens, underscores, capital letters, etc
names(pm0) <- make.names(cnames[[1]])


#pulling out the Sample.Value column from the table asis is the PM2.5 column
x0 <- pm0$Sample.Value


#to find the dimensions of x0
#dim() funcyion wont work here as x0 is not a table
str(x0)



#to find min, max. median, mean of x0
summary(x0)


#Reading the 2012 data into R
pm1 <- read.table("RD_501_88101_2012-0.txt",
                  header = FALSE, 
                  sep = "|",
                  na.strings = "")

dim(pm1)

cnames <- readLines("RD_501_88101_2012-0.txt", 1)


cnames <- strsplit(cnames, "|", fixed = TRUE)

names(pm1) <- cnames[[1]]


names(pm1) <- make.names(cnames[[1]])

x1 <- pm1$Sample.Value


#Since we will be comparing two years of data, it makes sense to combine them
library(dplyr)

pm <- rbind(pm0, pm1)  #combine the dataframes by rows


#create a factor variable(column) = Year, indicating which year the data comes from
pm <- mutate(pm, year = factor(rep(c(1999, 2012), c(nrow(pm0), nrow(pm1))))) %>%
rename(PM = Sample.Value)  #rename the Sample.Value column to PM

#set.seed. Set the seed of R's random number generator,
#which is useful for creating simulations or random objects that can be reproduced. 
set.seed(2015)
#Take the random sample because it is faster
idx <- sample(nrow(pm), 1000)
qplot(year, log2(PM), data = pm[idx, ], geom = "boxplot")

#looking at the plot it shows that year 2012 has some negative values
#lets investigate the negative values
filter(pm, year == "2012") %>% summarize(negative = mean(PM < 0, na.rm = TRUE))

#The pecentage of negative values is very less so we ignore them


#We have concluded that the PM levels have decreased in the period between 1999 - 2012
#However this was for the entire country, we need to see this for a specific state
sites <- filter(pm, State.Code == 36) %>% select(County.Code, Site.ID, year) %>% unique


#crreate a new variabke that combines County.code and site.ID into a single string
sites <- mutate(sites, site.code = paste(County.Code, Site.ID, sep = "."))


#Finally, we want the intersection between the sites present in 1999 and 2012 so that we
#might choose a monitor that has data in both periods.
site.year <- with(sites, split(site.code, year))
both <- intersect(site.year[[1]], site.year[[2]])
print(both)

#there are 10 monitors that were operating in both time
#periods. However, rather than choose one at random, it might best to choose one that
#had a reasonable amount of data in each year.
count <- mutate(pm, site.code = paste(County.Code, Site.ID, sep = ".")) %>% 
          filter(site.code %in% both)

#Now that we have subsetted the original data frames to only include the data from the
#monitors that overlap between 1999 and 2012, we can count the number of observations
#at each monitor to see which ones have the most observations.

group_by(count, site.code) %>% summarize(n = n())

pmsub <- filter(pm, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

#Now we plot the time series data of PM for the monitor in both years.

pmsub <- mutate(pmsub, date = as.Date(as.character(Date), "%Y%m%d"))
 rng <- range(pmsub$PM, na.rm = TRUE)

   par(mfrow = c(1, 2), mar = c(4, 5, 2, 1))
 with(filter(pmsub, year == "1999"), {
   plot(date, PM, ylim = rng)
   abline(h = median(PM, na.rm = TRUE))
   })
 with(filter(pmsub, year == "2012"), {
   plot(date, PM, ylim = rng)
   abline(h = median(PM, na.rm = TRUE))
   })

#We calculate the mean of PM for each state in 1999 and 2012
 mn <- group_by(pm, year, State.Code) %>% summarize(PM = mean(PM, na.rm = TRUE))

 
#Now make a plot that shows the 1999 state-wide means in one “column” and the 2012
#state-wide means in another columns. We then draw a line connecting the means for
#each year in the same state to highlight the trend.
 
 qplot(xyear, PM, data = mutate(mn, xyear = as.numeric(as.character(year))),
       color = factor(State.Code),
       geom = c("point", "line"))
 























