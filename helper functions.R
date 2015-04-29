#function to create median daily web stats
make_ref <- function(x,df){
  df_out <- data.frame(levels(df$Day.of.Week),x)
  names(df_out)[1] <- "Day.of.Week"
  names(df_out)[2] <- "median_val"
  return(df_out)
}

#function to create median for the reference medians on run charts
make_med <- function(df,y_idx,d1,d2){
  med_out <- median(df[df$Date >= d1 & df$Date <= d2, y_idx],na.rm=TRUE)
}

#plot function with median overlay
#y_idx is the index of column of the dataframe to be used

pmed1 <- function(df,y_idx,title2){
  y1 <- names(df)[y_idx]
  p1 <- ggplot(data=df,aes_string(x="Date",y=y1))+
    theme_bw() +
    geom_point(aes(colour=Day.of.Week),data=df,size=3.5) +
    #geom_point(size=2.5) +
    geom_line() +
    xlab("")+
    ylab("")+
    theme(axis.text.x=element_text(angle=30,hjust=1))+
    #geom_vline(xintercept=as.numeric(as.Date("2015-04-20")),lty-2,colour="green",size=1)+
    scale_colour_manual(values = cb_palette)+
    ggtitle(title2)
      
}

#trap an error if ref_med is not contained in the range of the original data?
pmed2 <- function(px,ref_med) {
  p1.1 <- px + geom_hline(yintercept=ref_med,lty=2)
}
  
#function to modify a run chart with annotation and shading to indicate intervals under test conditions
#takes a ggplot2 object pp built from a dataframe df and overlays a shaded rectangle to denote time period of a test.  The 
#rectangle is defined by dates extracted from a dataframe of annotations that has four columns:  
#Start_Date, End_Date, Test_description, and Notes.   
#We also append a note on the chart for each rectangle.   Function allows multiple annotated rectangles, defined by the number of rows of the annotation df.
#alpha1 is the transparency paramters, default = 0.5
# pnote <- function(pp,df,dfnotes,alpha1=0.5) {
#   yval <- df[,which(names(df)==as.character(pp$mapping[2]))]
#   dfrect <- data.frame(
#     xmin = dfnotes$Start_Date,
#     xmax = dfnotes$End_Date,
#     ymin = -Inf,
#     ymax = Inf
#   )
#   
#   df_text <- data.frame(
#     xtext = dfnotes$Start_Date,
#     ytext = 0.9*max(yval,na.rm=TRUE),
#     label = dfnotes$Test_description
#   )
#   
#   pout <- pp +geom_rect(data=dfrect,
#                         aes(NULL,NULL,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="gray80",alpha=alpha1) +
#     geom_text(data=df_text,
#               aes(x=xtext,y=ytext,label=label))
#   return(pout)
# }