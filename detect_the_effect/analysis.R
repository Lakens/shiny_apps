#Set working directory
setwd("data")

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


###ANALYSIS----
str(all_data)
mean(as.numeric(all_data$V2))

mean(as.numeric(all_data$V2))

#create variable for correct/incorrect
all_data$correct <- ifelse(as.numeric(all_data$V3) == 0 & (as.numeric(all_data$V4) == 0) | as.numeric(all_data$V3) == 1 & (as.numeric(all_data$V4) > 0),
                  1,
                  0)
