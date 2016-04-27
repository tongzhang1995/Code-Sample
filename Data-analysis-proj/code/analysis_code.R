# =============================================================================
# Stat 133: Final Project
# Part2 Data Analysis
# Description: work with cleaned-precoessed csvs (storms.csv; tracks.csv) to
# produce analysis of storms from 1980 to 2010
# Data: International Best Track Archive for Climate Stewardship (IBTrACS)
# ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/
# Basin.NA.ibtracs_hurdat.v03r06.hdat

# =============================================================================

# Name:Tong Zhang 24540972, Zhiqiang Liao 24229382

# =============================================================================
# Load csv
# =============================================================================
library(stringr)
storms<-read.csv('data/storms.csv', stringsAsFactors = FALSE)
tracks<-read.csv('data/tracks.csv', stringsAsFactors = FALSE)


# =============================================================================
# Number of storms per year
# =============================================================================

# Extract years from date
date = as.Date(storms$date, '%m/%d/%Y')
year = format(date, '%Y')
# Extract years that are from 1980 to 2010
year = year[which(year >= 1980 & year <= 2010)]

# Find the frequency of storms in each year
stormPerYear<-table(year)

# Plot a bar graph
barplot(as.numeric(stormPerYear),
                           names.arg = unique(year),
        main = 'number of storms per year', 
        xlab = 'Year',ylab='number of storms' )

# Open device
pdf("images/stormPerYearPlot.pdf") 
barplot(as.numeric(stormPerYear),
        names.arg = unique(year),
        main = 'number of storms per year', 
        xlab = 'Year',ylab='number of storms' )
dev.off() # close device

# Open device
png("images/stormPerYearPlot.png", width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerYear),
        names.arg = unique(year),
        main = 'number of storms per year', 
        xlab = 'Year',ylab='number of storms' )
dev.off() # close device

# =============================================================================
# Number of storms per year with wind >=35
# =============================================================================
# As date the date column
date = as.Date(tracks$date, '%m/%d/%Y')
# Extract years 
year = format(date, '%Y')
# Extract years that are from 1980 to 2010
tmpTracks = tracks[which(year >= 1980 & year <= 2010),]
year35Date = as.Date(storms$date[unique(tmpTracks$id[tmpTracks$wind>=35])], 
                     '%m/%d/%Y')
year35 = format(year35Date, "%Y")
# Using table to find frequency of storms in each year
stormPerYear35<-table(year35)
# Plot a bar graph
barplot(as.numeric(stormPerYear35), 
                             names.arg=unique(year35), 
        main='number of storms per year with wind >=35',
        xlab = 'Year', ylab='number of storms')

# Open device
pdf("images/stormPerYear35Plot.pdf") 
barplot(as.numeric(stormPerYear35), 
        names.arg=unique(year35), 
        main='number of storms per year with wind >=35',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Open device
png("images/stormPerYear35Plot.png", width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerYear35), 
        names.arg=unique(year35), 
        main='number of storms per year with wind >=35',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# =============================================================================
# Number of storms per year with wind >=64
# =============================================================================
# Years of which the storm's speed exceed 64

year64Date = as.Date(storms$date[unique(tmpTracks$id[tmpTracks$wind>=64])], 
                     '%m/%d/%Y')
# Extract years
year64 = format(year64Date, "%Y")
# Using table to find frequency of storms in each year
stormPerYear64<-table(year64)

# Plot a bar graph
stormPerYear64Plot = barplot(as.numeric(stormPerYear64), 
                             names.arg=unique(year64),
        main='number of storms per year with wind >= 64',
        xlab = 'Year', ylab='number of storms')

# Open device
pdf("images/stormPerYear64Plot.pdf") 
stormPerYear64Plot = barplot(as.numeric(stormPerYear64), 
                             names.arg=unique(year64),
                             main='number of storms per year with wind >= 64',
                             xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Open device
png("images/stormPerYear64Plot.png", width = 1024, height = 900, res= 120) 
stormPerYear64Plot = barplot(as.numeric(stormPerYear64), 
                             names.arg=unique(year64),
                             main='number of storms per year with wind >= 64',
                             xlab = 'Year', ylab='number of storms')
dev.off() # close device


# =============================================================================
# Number of storms per year with wind >=96
# =============================================================================
# Extract years of which the storm's speed exceed 96
year96Date = as.Date(storms$date[unique(tmpTracks$id[tmpTracks$wind>=96])], 
                     '%m/%d/%Y')
# Extract years
year96 = format(year96Date, "%Y")
# Using table to find frequency of storms in each year
stormPerYear96<-table(year96)
# Plot a bar graph
barplot(as.numeric(stormPerYear96), 
                             names.arg=unique(year96),
        main='number of storms per year with wind >= 96',
        xlab = 'Year', ylab='number of storms')

# Open device
pdf("images/stormPerYear96Plot.pdf") 
barplot(as.numeric(stormPerYear96), 
        names.arg=unique(year96),
        main='number of storms per year with wind >= 96',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Open device
png("images/stormPerYear96Plot.png", 
    width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerYear96), 
        names.arg=unique(year96),
        main='number of storms per year with wind >= 96',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Extract according to Month
# =============================================================================
# Number of storms per month
# =============================================================================
# As date the date column
date = as.Date(storms$date, '%m/%d/%Y')
# Extract month and year from the date column
monthWithYear = format(date, '%m/%Y')
# Extract year
year = format(date, '%Y')
# Extract month and year with year from 1980 to 2010
monthWithYear = monthWithYear[year >= 1980 & year <= 2010]

# Find the frequency of storms that happened in each month
month = substring(text = monthWithYear, first = 1, last = 2)
stormPerMonth<-table(month)
# Plot
monthName = month.name[sort(as.numeric(unique(month)))]
barplot(as.numeric(stormPerMonth), 
        names.arg = monthName,
        main='number of storms per Month',
        xlab = 'Year', ylab='number of storms' )

# Open device
pdf("images/stormPerMonthPlot.pdf") 
barplot(as.numeric(stormPerMonth), 
        names.arg = monthName,
        main='number of storms per Month',
        xlab = 'Year', ylab='number of storms' )
dev.off() # close device

# Open device
png("images/stormPerMonthPlot.png", width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerMonth), 
        names.arg = monthName,
        main='number of storms per Month',
        xlab = 'Year', ylab='number of storms' )
dev.off() # close device

# =============================================================================
# Number of storms per month with wind >=35
# =============================================================================
# As date the date column
date = as.Date(tracks$date, '%m/%d/%Y')
# Extract month and Year from the date column
monthWithYear = format(date, '%m/%Y')
# Extract year
year = format(date, '%Y')
# Extract storms that are from 1980 to 2010
tmpTracks = tracks[which(year >= 1980 & year <= 2010),]

# Find the frequency of storms that happened in each month
month35<-storms$date[unique(tmpTracks$id[tmpTracks$wind>=35])]
month35 = as.Date(month35, '%m/%d/%Y')
month35 = format(month35, '%m')
stormPerMonth35<-table(month35)

# Plot
monthName = month.name[sort(as.numeric(unique(month35)))]
barplot(as.numeric(stormPerMonth35),
        names.arg = monthName,
        main='number of storms per Month with wind >= 35 knots',
        xlab = 'Year', ylab='number of storms')

# Open device
pdf("images/stormPerMonth35Plot.pdf") 
barplot(as.numeric(stormPerMonth35),
        names.arg = monthName,
        main='number of storms per Month with wind >= 35 knots',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Open device
png("images/stormPerMonth35Plot.png", width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerMonth35),
        names.arg = monthName,
        main='number of storms per Month with wind >= 35 knots',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# =============================================================================
# Number of storms per month with wind >=64
# =============================================================================
# Find the frequency of storms that happened in each month
month64<-storms$date[unique(tmpTracks$id[tmpTracks$wind>=64])]
month64 = as.Date(month64, '%m/%d/%Y')
month64 = format(month64, '%m')
stormPerMonth64<-table(month64)

# Plot
monthName = month.name[sort(as.numeric(unique(month64)))]
barplot(as.numeric(stormPerMonth64), 
        names.arg = monthName,
        main='number of storms per Month with wind >= 64 knots',
        xlab = 'Year', ylab='number of storms')

pdf("images/stormPerMonth64Plot.pdf") # open device
barplot(as.numeric(stormPerMonth64), 
        names.arg = monthName,
        main='number of storms per Month with wind >= 64 knots',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Open device
png("images/stormPerMonth64Plot.png", width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerMonth64), 
        names.arg = monthName,
        main='number of storms per Month with wind >= 64 knots',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# =============================================================================
# Number of storms per month with wind >=96
# =============================================================================
# Find the frequency of storms that happened in each month
month96<-storms$date[unique(tmpTracks$id[tmpTracks$wind>=96])]
month96 = as.Date(month96, '%m/%d/%Y')
month96 = format(month96, '%m')
stormPerMonth96<-table(month96)

# Plot
monthName = month.name[sort(as.numeric(unique(month96)))]
barplot(as.numeric(stormPerMonth96), 
        names.arg = monthName,
        main='number of storms per Month with wind >= 96 knots',
        xlab = 'Year', ylab='number of storms')

# Open device
pdf("images/stormPerMonth96Plot.pdf") 
barplot(as.numeric(stormPerMonth96), 
        names.arg = monthName,
        main='number of storms per Month with wind >= 96 knots',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# Open device
png("images/stormPerMonth96Plot.png", width = 1024, height = 900, res= 120) 
barplot(as.numeric(stormPerMonth96), 
        names.arg = monthName,
        main='number of storms per Month with wind >= 96 knots',
        xlab = 'Year', ylab='number of storms')
dev.off() # close device

# =============================================================================
# Annual numbers of storms >= 35/64/96
# =============================================================================
## Average 
mean35<-mean(stormPerYear35)
mean64<-mean(stormPerYear64)
mean96<-mean(stormPerYear96)
## std
std35<-sd(stormPerYear35)
std64<-sd(stormPerYear64)
std96<-sd(stormPerYear96)
## 25 percentile
Q35<-summary(as.numeric(stormPerYear35))[2]
Q64<-summary(as.numeric(stormPerYear64))[2]
Q96<-summary(as.numeric(stormPerYear96))[2]
## median
median35<-summary(as.numeric(stormPerYear35))[3]
median64<-summary(as.numeric(stormPerYear64))[3]
median96<-summary(as.numeric(stormPerYear96))[3]
## 75 percentile
q35<-summary(as.numeric(stormPerYear35))[5]
q64<-summary(as.numeric(stormPerYear64))[5]
q96<-summary(as.numeric(stormPerYear96))[5]

## table
annual<-matrix(c(mean35,std35,Q35, median35,q35,
                 mean64,std64,Q64, median64,q64,
                 mean96,std96,Q96, median96,q96), ncol=5, byrow=TRUE)
colnames(annual)<-c('Avg','Std.Dev','25th','50th','75th')
rownames(annual)<-c('35 knots','64 knots', '96 knots')
annual = signif(annual, 4)
annual<-as.table(annual)

# =============================================================================
# Regression Line
# =============================================================================
# Mean pressure and mean wind speed for each storm
vec<-c(1:1777)
pressMean1<-sapply(vec, function(x) 
  mean(tmpTracks$press[tmpTracks$id==x]))
# Remove zeros
pressMean<-pressMean1[pressMean1!=0]
pressMean = na.exclude(pressMean)
# Mean wind speed
windMean1<-sapply(vec,function(x) 
  mean(tmpTracks$wind[tmpTracks$id==x]))
# Remove zeros
windMean<- windMean1[pressMean1!=0]
windMean = na.exclude(windMean)

# Find regression 
reg = lm(pressMean ~ windMean)

# Plot regression
plot(windMean, pressMean,xlab = 'mean wind speed', ylab = 'mean pressure')
title('Mean Wind VS Mean Wind Speed for Each Storm ')
abline(reg)

# Open device
pdf("images/meanWindPressureReg.pdf") 
reg = lm(pressMean ~ windMean)
plot(windMean, pressMean,xlab = 'mean wind speed', ylab = 'mean pressure')
title('Mean Wind VS Mean Wind Speed for Each Storm ')
abline(reg)
dev.off() # close device

# Open device
png("images/meanWindPressureReg.png", width = 1024, height = 900, res= 120) 
reg = lm(pressMean ~ windMean)
plot(windMean, pressMean,xlab = 'mean wind speed', ylab = 'mean pressure')
title('Mean Wind VS Mean Wind Speed for Each Storm')
abline(reg)
dev.off() # close device


# =============================================================================
# Regression analysis 2
# =============================================================================
# Median pressure and median wind speed for each storm
vec<-c(1:1777)
pressMedian1<-sapply(vec,function(x) 
  median(tmpTracks$press[tmpTracks$id==x]))
# Mean wind speed
windMedian1<-sapply(vec,function(x) 
  median(tmpTracks$wind[tmpTracks$id==x]))
# Remove zeros
pressMedian<-pressMedian1[pressMedian1!=0]
pressMedian = na.exclude(pressMedian)
windMedian<-windMedian1[pressMedian1!=0]
windMedian = na.exclude(windMedian)

# Find Regression
reg1 = lm(pressMedian ~ windMedian)
plot(windMedian, pressMedian, xlab = 'Median wind speed', 
     ylab = 'Median pressure')
title('Median Wind Speed VS Median Pressure For Each Storm ')
abline(reg1)

# open device
pdf("images/medianWindPressureReg.pdf") 
reg1 = lm(pressMedian ~ windMedian)
plot(windMedian, pressMedian, xlab = 'Median wind speed', 
     ylab = 'Median pressure')
title('Median Wind Speed VS Median Pressure For Each Storm ')
abline(reg1)
dev.off() # close device

# open device
png("images/medianWindPressureReg.png", width = 1024, height = 900, res= 120) 
reg1 = lm(pressMedian ~ windMedian)
plot(windMedian, pressMedian, xlab = 'Median wind speed', 
     ylab = 'Median pressure')
title('Median Wind Speed VS Median Pressure For Each Storm ')
abline(reg1)
dev.off() # close device








