# Assumes valid token has been set up and saved in the workspace, else throws an error message

#set colour blind palette
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#set libraries
require("RCurl")
require("rjson")
require("ggplot2")
require("plyr")
require("gridExtra")
require("reshape")
require("RGoogleAnalytics")
require("lubridate")
require("dplyr")
require("gtable")
require("shiny")
require("shinyapps")
require("shinyBS")

test_load <- try(load("./token_file"))
if("try-error" %in% class(test_load)) {
  print("Token File not available, check credentials to access token needed by Google Analytics.")
}

# Validate and refresh the token
ValidateToken(token)

#require dynamic start_date and end_date for modifying the graph
#absolute start date for the series is 9 march
start_date <- "2015-03-09"
end_date <- as.character(today())
query.list1 <- Init(start.date = start_date,
                    end.date = end_date,
                    dimensions = "ga:year,ga:month,ga:day",
                    metrics = "ga:users",
                    #filters = "ga:eventLabel=~TCP-StarterKit.pdf",
                    max.results = 10000,
                    #sort = "-ga:eventLabel",
                    table.id = "ga:63264147")

query.list3 <- Init(start.date = start_date,
                    end.date = end_date,
                    dimensions = "ga:pagePathLevel4,
                                    ga:year,ga:month,ga:day",
                    #dimensions = "ga:month,ga:day",
                    metrics = "ga:uniquePageviews",
                    # filters = "ga:pagePathLevel1=~/wp-content/",
                    max.results = 10000,
                    #sort = "-ga:eventLabel",
                    table.id = "ga:63264147")

ga.query1 <- QueryBuilder(query.list1)
#  downloads of TCP-StarterKit, English adult version


#test of query3
ga.query3 <- QueryBuilder(query.list3)

# Extract the data and store it in a data-frame
ga.data1 <- GetReportData(ga.query1, token)

ga.data3 <- GetReportData(ga.query3, token)

#select only pagePath records with   /03/TCP_StarterKit_2015_Final_Writeable.pdf item

ga.data3.1 <- ga.data3[ga.data3$pagePathLevel4 == "/03/TCP_StarterKit_2015_Final_Writeable.pdf",]

ga.data1$Date <- paste0(ga.data1$year,"-",ga.data1$month,"-",ga.data1$day)
#ga.data2.1$Date <- paste0(ga.data2.1$year,"-",ga.data2.1$month,"-",ga.data2.1$day)
ga.data3.1$Date <- paste0(ga.data3.1$year,"-",ga.data3.1$month,"-",ga.data3.1$day)

#allow for mismatch in downloads from user data.   should add a check for the number of rows vs dates in 
#ga.data1 file--to assure that there are not data missing.
ga.data0 <- left_join(ga.data1, ga.data3.1,by="Date")
ga.dataA <- ga.data0[which(complete.cases(ga.data0)),]

ga.dataA$Day.of.Week <- wday(ga.dataA$Date,label=TRUE)
ga.dataA$Date <- as.Date(ga.dataA$Date)

ga.dataA$pct_download <- 100*ga.dataA$uniquePageviews/ga.dataA$users

names(ga.dataA)[10] <- "SK_downloads"
names(ga.dataA)[4] <- "TotVis_NU"

#Baseline index start  Need to make the choice of indices for median an input  Needs to be in ui and server code
m_idx <- 1

#order the factor in calendar order M-Su not alphabetical:  done by wday function
#ga.data$Day.of.Week <- factor(ga.data$Day.of.Week,levels(ga.data$Day.of.Week)[c(2,6,7,5,1,3,4)])



#create reactive table based on chosen data range.