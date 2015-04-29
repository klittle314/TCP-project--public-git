shinyUI(fluidPage(
  titlePanel("TCP Starter Kit data"),
  
   
  
  sidebarLayout(
    sidebarPanel(
      h3 ("App Purpose"),
      helpText("This app interacts automatically with Google Analytics to summarize daily TCP website data  ",
               "on visits and downloads of The Starter Kit, English adult version."," Default view is the past 30 days.",
               " We count downloads as unique pageViews of /03/TCP_StarterKit_2015_Final_Writeable.pdf ."),
      h3 ("Contact"),
      helpText("Kevin Little, Ph.D., Informing Ecological Design, LLC, klittle@iecodesign.com.  Last update 24 April 2015",
               ""),
      h3 (""),
      dateRangeInput('dateRange',
                     label = 'Date range for charts, start to end',
                     start = max(ga.dataA$Date)-30, end = max(ga.dataA$Date)
                    ),
      
      dateRangeInput('dateRangeM',
                    label = 'Date range for reference medians, start to end',
                    start = max(ga.dataA$Date)-30, end = max(ga.dataA$Date)
                    ),
           
      fileInput("files", "Upload Annotation CSV File", multiple=FALSE),
      helpText("Click on Choose Files to add annotations in CSV format:  start date, end date, test description and notes"),
      
      downloadButton('downloadData', 'Download CSV'),
      helpText("Click on download to get a CSV file of the TCP web data defined by the date range for charts. ",
               "To get the complete data set, enter 2015-03-09 as the start date and today's date as the end date.")
    ),
    mainPanel(
      tabsetPanel(type="tabs",
              tabPanel("Main Plot", plotOutput("main_plot",height="800px")),
              tabPanel("Table of Data",dataTableOutput("table")),
              tabPanel("Table of Annotations",dataTableOutput("file_notes"))
      
    )
   )
#       mainPanel(
#         textOutput("start"),
#         textOutput("end"),
#         tableOutput("table")
#       )
  )
 )
)