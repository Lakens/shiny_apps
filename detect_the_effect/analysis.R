#Set working directory
setwd("C:/Users/Daniel/surfdrive/R/shiny_apps/detect_the_effect/online_responses")

#Read excel files in folder
files <- list.files(pattern = "\\.csv$")

#read in all seperate data files into a single list
#gives warning that can be ignored (last line not empty)
datalist = lapply(files, function(x) read.table(x, header = F)) 

#determine max length to fill dataframe
max_length <- max(unlist(lapply(datalist,length)))

#fill dataframe
df_filled <- lapply(datalist,function(x) {
  ans <- rep(NA,length=max_length);
  ans[1:length(x)]<- x;
  return(ans)
  })
#combine lists into a dataframe
all_data <- as.data.frame(do.call(rbind, df_filled))