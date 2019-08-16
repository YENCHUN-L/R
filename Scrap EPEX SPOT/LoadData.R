# https://github.com/wagnertimo/emarketcrawlR

##########IntradayContinuousEPEXSPOT#############

# Set Logging to print out the state of process including a progress bar
setLogging(TRUE)

# Get the 15min (default: hour data) trading price data in the given time period of the german cont. intra. at EPEX SPOT
IntradayContinuousEPEXSPOT <- getIntradayContinuousEPEXSPOT("2014-01-01", "2018-12-31", "60", "FR")

head(IntradayContinuousEPEXSPOT)

write.csv(IntradayContinuousEPEXSPOT, file = "IntradayContinuousEPEXSPOT.csv")


##########DayAheadAuctionEPEXSPOT############

# Set Logging to print out the state of process including a progress bar
setLogging(TRUE)

# Get the hourly day-ahead auction price data in the given time period of the french auction market at EPEX SPOT. 
DayAheadAuctionEPEXSPOT <- getDayAheadAuctionEPEXSPOT("2014-01-01", "2018-12-31", 'FR')

head(DayAheadAuctionEPEXSPOT)

write.csv(DayAheadAuctionEPEXSPOT, file = "DayAheadAuctionEPEXSPOT.csv")



##########IntradayContinuousEPEXSPOT#############

# Set Logging to print out the state of process including a progress bar
setLogging(TRUE)

# Get the 15min (default: hour data) trading price data in the given time period of the german cont. intra. at EPEX SPOT
IntradayContinuousEPEXSPOT <- getIntradayContinuousEPEXSPOT("2014-01-01", "2018-12-31", "60", "DE")

head(IntradayContinuousEPEXSPOT)

write.csv(IntradayContinuousEPEXSPOT, file = "IntradayContinuousEPEXSPOT.csv")


##########DayAheadAuctionEPEXSPOT############

# Set Logging to print out the state of process including a progress bar
setLogging(TRUE)

# Get the hourly day-ahead auction price data in the given time period of the french auction market at EPEX SPOT. 
DayAheadAuctionEPEXSPOT <- getDayAheadAuctionEPEXSPOT("2014-01-01", "2018-12-31", "DE")

head(DayAheadAuctionEPEXSPOT)

write.csv(DayAheadAuctionEPEXSPOT, file = "DayAheadAuctionEPEXSPOT_DE.csv")


##########DayAheadAuctionEPEXSPOT############

# Set Logging to print out the state of process including a progress bar
setLogging(TRUE)

# Get the hourly day-ahead auction price data in the given time period of the french auction market at EPEX SPOT. 
DayAheadAuctionEPEXSPOT <- getDayAheadAuctionEPEXSPOT("2018-10-01", "2018-12-31", "DE_LU")

head(DayAheadAuctionEPEXSPOT)

write.csv(DayAheadAuctionEPEXSPOT, file = "DayAheadAuctionEPEXSPOT_DE_LU.csv")


