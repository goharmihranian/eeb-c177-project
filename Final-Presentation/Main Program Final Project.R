#please keep in mind that you will need to pigfolder on your desktop for this to run
setwd("~/Desktop") #setting the working directory to the desktop where my files are located
pigfolder = "Pig109011RAcontrolARI" #this is the name of the folder I want it to extract data from (each pig has its own folder so I would just change this name to "Pig818" and it would do it for a different folder as well)
setwd(paste0("~/Desktop/",pigfolder))
getwd()
library(ggplot2)

read_data <- function(pigfiles) {
  data <- read.csv(pigfiles, sep= "\t", header = F)
}
#this read_data function allows fort he data to be more readable initally and tab delimited

build_AT <- function(data) {
  data.new <- data[seq(3, nrow(data), 3), ]
  return(data.new)
}
#this function takes the raw data and starting from the 3rd element of each column and every 3rd entry thereafter in order to make a list of the Activation Recovery times 

build_RT <- function(data) {
  data.new <- data[seq(4, nrow(data), 3), ]
  return(data.new)
}
#this function does the same thing as the build_AT function but starts from the 4th entry and every 3rd one thereafter in order to gather a list of the recovery times

split_data <- function(data, numleads=64) {
  data.new <- as.data.frame(data)
  split.data <- strsplit(as.character(data.new$data), split = "  ")
  data.list <- list()
  for(ii in 1:numleads) {
    data.list[[ii]] <- as.numeric(split.data[[ii]])
    data.list[[ii]] <- na.omit(data.list[[ii]])
    attributes(data.list[[ii]])$na.action <- NULL
  }
  return(data.list[sapply(data.list, length) >1])
}
#this function splits my data, as it was giving it to me as one giant string. I used a for loop to help identify each number as an integer and delete the NA's and save them to a new list

build_ARI <- function(RT,AT, numleads=64) {
  ARI <- list()
  for(ii in 1:numleads) {
    ARI[[ii]]<- RT[[ii]] - AT[[ii]]
  }
  return(ARI)
}
#This function essentailly takes those values we found through the build_RT and build_AT functions and subtracts them from each other
#As a point of reference, there are 64 leads on the heart of the pig during the experiments, which is why I keep referring to the number 64 in certain places
build_plot <- function(ARI_means, filename){
  pdf(paste0("figure_", filename, ".pdf"))
  plot(seq(1:length(ARI_means)), ARI_means, xlab = "Seconds", ylab = "Average ARI", main = filename)
  dev.off()
}
#my data wouldn't work through ggplot because I needed to convert it to a dataframe and that wasn't working well for the data and it wasn't plotting nicely
#however, I didnt need fancy graphs, I just needed simple scatterplots to see if the HR went up, down, or stayed the same
#so I took the ARI_means calculates below and plotted them. However many means we were able to get, is how long the recording ran, which is why my x-axis is 1:length(ARI_means) and my y-axis is just the actual means

compute_means <- function(ARI) {
  numleads <- length(ARI)
  numtimeslices <- length(ARI[[1]])
  means <- rep(NA, numtimeslices)
  for (ii in 1:numtimeslices) {
    temp_values <- rep(NA, numleads)
    for(jj in 1:numleads) {
      temp_values[jj] <- ARI[[jj]][ii]
    }
    means[ii] <- mean(temp_values)
  }
  return(means)
}
#this function allowed me to take the means of the ARI we calculated in build_ARI
# we defined the number of leads as the length of ARI and the timeslices are however many means we had

execute_mean_calculation <- function(pigfolder) {
  pigfiles = list.files(path=pigfolder, pattern="*.txt", full.names=TRUE)
  runs <- vector("list", length(pigfiles))
  for (ii in 1:length(pigfiles)){
    runs[[ii]]<- read_data(pigfiles[ii])
    AT <- build_AT(runs[[ii]])
    AT <- split_data(AT)
    
    RT <- build_RT(runs[[ii]])
    RT <- split_data(RT)
    
    ARI <- build_ARI(RT, AT)
    ARI_means <- compute_means(ARI)
    
    temp_filename <- str_split(pigfiles[ii],"/")
    split_name <- temp_filename[[1]][6]
    build_plot(ARI_means,split_name)
  }
}
#this is my main function, which takes all of the functions above and puts them in a for loop to make it happen for each file I have in my folder


execute_mean_calculation(getwd())

