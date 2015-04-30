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
  
