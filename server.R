#server.R
source("helper functions.R")
source("global.R")

shinyServer(function(input, output) {

#create values for start and end dates of the data range and the median reference 
ga.start <- reactive({
   input$dateRange[1] 
 })

ga.end <- reactive({
  input$dateRange[2]
})

med.start <- reactive({
  ms <- input$dateRangeM[1]
})

med.end <- reactive({
  me <- input$dateRangeM[2]
})


#create a reactive dataframe using the input date range
ga.data <- reactive({
  start1 <- as.Date(ga.start())
  end1 <- as.Date(ga.end())
  data_out <- ga.dataA[ga.dataA$Date >= start1 & ga.dataA$Date <= end1,]
})


#create titles for the plots
Titles <- c("N SK downloads","N Total Visitors, Non-Unique","Percent Downloads")
 
#create reactive reference medians.  It appears that function calls cannot have more than one "direct" reactive argument.
ref_medPct <- reactive({
  d10 <- as.Date(med.start())
  d20 <- as.Date(med.end())
  ref1 <- make_med(df=ga.dataA,y_idx=12,d1=d10,d2=d20)
})

ref_medSK <- reactive({
  d10 <- as.Date(med.start())
  d20 <- as.Date(med.end())
  ref2 <- make_med(df=ga.dataA,y_idx=10,d1=d10,d2=d20)
})

ref_medTV <- reactive({
  d10 <- as.Date(med.start())
  d20 <- as.Date(med.end())
  ref3 <- make_med(df=ga.dataA,y_idx=4,d1=d10,d2=d20)
})



#define the upload file and extract the annotations from the annotation file to add to the plot
df_notes <- reactive({
  inFile <- input$files
  if (is.null(inFile)) {
    # User has not uploaded a file yet
    return(NULL)
  }
  data_notes <- read.csv(inFile$datapath, header=T, stringsAsFactors =F)
  data_notes$Start_Date <- as.Date(data_notes$Start_Date,format="%m/%d/%Y")
  data_notes$End_Date <- as.Date(data_notes$End_Date,format="%m/%d/%Y")
  return(data_notes)
})

#define the table for output verification
output$file_notes <- renderDataTable({
  df_out <- df_notes()
})


 #set up overlay rectangles, used to show annotation periods, for the plots as a reactive dataframe
dfrect <- reactive({
  df_out <- data.frame(
    xmin = df_notes()$Start_Date,
    xmax = df_notes()$End_Date,
    ymin = -Inf,
    ymax = Inf)
  
  return(df_out)
})

#set up annotations for the plots as a reactive dataframe
dftext <- reactive({
  df_text <- data.frame(
    xtext = df_notes()$Start_Date + floor((df_notes()$End_Date-df_notes()$Start_Date)/2),
    ytext1 = 1.1*min(ga.data()[,12]),
    ytext2 = 1.1*min(ga.data()[,10]),
    ytext3 = 1.1*min(ga.data()[,4]),
    label = df_notes()$Test_description)
  return(df_text)
})

#create reactive plot objects, conditional on the presence of the annotation file
p0p <- reactive({
  if(is.null(df_notes())){
    pz <- pmed1(df=ga.data(),y_idx=12,title2=Titles[3]) #+ geom_text(x=x1,y=25,label="Jan Death over Dinner",size=3)
    pz1 <- pz + geom_hline(yintercept=ref_medPct(),lty=2) 
  } else {
    pz <- pmed1(df=ga.data(),y_idx=12,title2=Titles[3]) 
    pz1 <- pz + geom_hline(yintercept=ref_medPct(),lty=2) 
    pz1.1 <- pz1 + geom_rect(data=dfrect(),aes(NULL,NULL,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="gray80",alpha=0.5)
    pz1.2 <- pz1.1 + geom_text(data=dftext(),aes(x=xtext,y=ytext1,label=label),size=4)
  }
  })

p0SK <- reactive({
  if(is.null(df_notes())) {
    py <- pmed1(df=ga.data(),y_idx=10,title2=Titles[1]) 
    py1 <- py + geom_hline(yintercept=ref_medSK(),lty=2)
  } else {
    py <- pmed1(df=ga.data(),y_idx=10,title2=Titles[1]) 
    py1 <- py + geom_hline(yintercept=ref_medSK(),lty=2)
    py1.1 <- py1 + geom_rect(data=dfrect(),aes(NULL,NULL,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="gray80",alpha=0.5)
    py1.2 <- py1.1 + geom_text(data=dftext(),aes(x=xtext,y=ytext2,label=label),size=4)
  }
    
})  

p0TNU <- reactive({
  if(is.null(df_notes())) {
    px <- pmed1(df=ga.data(),y_idx=4,title2=Titles[2])
    px1 <- px + geom_hline(yintercept=ref_medTV(),lty=2)
  } else {
    px <- pmed1(df=ga.data(),y_idx=4,title2=Titles[2])
    px1 <- px + geom_hline(yintercept=ref_medTV(),lty=2)
    px1.1 <- px1 + geom_rect(data=dfrect(),aes(NULL,NULL,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="gray80",alpha=0.5)
    px2.2 <- px1.1 + geom_text(data=dftext(),aes(x=xtext,y=ytext3,label=label),size=4)
  }
})


#render the plot for display, conditional on the annotation file being loaded.
output$main_plot <- renderPlot({
    pall0 <- grid.arrange(p0p(),p0SK(),p0TNU(), 
                      main="Summary of Download Activity, TCP website" , 
                      ncol=1)
   print(pall0)
  })

 
#create table object for display
table_out <- reactive({
  #tdate <- as.character(as.Date(ga.data()$Date))
  tdata <- ga.data()[,c(5,11,4,10,12)]
  tdata[,5] <- formatC(tdata[,5],format="f",digits=1)
  names(tdata) <- c("Date","Day_of_Week","Non-unique_visitors","SK_downloads","Pct_downloads")
  return(tdata)
})

 
#render the Data Table for display (note that the function renderTable is plain HTML and doesn't handle Date objects directly)
output$table <- renderDataTable({
  table_out()
})

#create the file for download
output$downloadData <- downloadHandler(
  filename = function() {
    paste('TCP_downloads_', Sys.Date(), '.csv', sep='')
  },
    content = function(file) {
      write.csv(ga.data()[,c(5,11,4,10,12)], file, row.names=FALSE)
    }
  )

})

