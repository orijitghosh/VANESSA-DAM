#' @export
clocklab_to_DAM <- function(bin = 5, file) {
  #' clocklab format to DAM2 converter
  #' 
  #' Function to convert a mouse locomotor activity data acquired through a Trikinetics system to a DAM file
  #' for using with VANESSA-DAM. Outputs a dataframe which can be written to a tab separatedtext file using write.table() command
  #' @param bin The binning value of the raw data in minutes
  #' @param file The .csv data file with path from trikentics systems within double quotes (must be comma separated)
  #' 
  
  bin = bin
  df_raw <- read.delim(file, sep = ",",  header = F)
  df_raw_cut <- df_raw[-c(1:4),-6]
  df_raw_cut <- df_raw_cut[!apply(df_raw_cut == "", 1, any),]
  df1 <- matrix(0, nrow = nrow(df_raw_cut), ncol = 42)
  df1[,1] <- seq(1, (nrow(df_raw_cut)), by = 1)
  df1[,11] <- df_raw_cut[,4]
  date_time_start <- dmy_hm(paste(df_raw[3,1], df_raw[3,2]))
  date_time_end <- date_time_start + days(as.numeric((df_raw_cut[nrow(df_raw_cut),1]))-1) + hours(df_raw_cut[nrow(df_raw_cut),2]) +
    minutes(df_raw_cut[nrow(df_raw_cut),3])
  ag <- data.frame(timestamp = seq(as.POSIXct(date_time_start), as.POSIXct(date_time_end), by = bin*60))
  df1 <- cbind(df1, ag)
  df1[,2] <- (df1$timestamp)
  df1 <- df1[,-43]
  df1 <- df1 %>% separate(2, c("a", "b"), sep = " ")
  df1 <- df1[,-4]
  df1$a <- ymd(df1$a)
  df1$a <- format(df1$a, format = "%d %b %y")
  df1 <- df1[complete.cases(df1),]
  df1[,4] <- 1
  df1[,8] <- "Ct"
  data <- df1
  return(data)
}