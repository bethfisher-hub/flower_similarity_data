# Code from Ariel and Yusuke 

# live dangerously, get rid of pesky warnings
oldw <- getOption("warn")
options(warn = -1)

shhh <- suppressPackageStartupMessages # stops annoying warnings when loading libraries
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(Matrix)
library(reshape2)
library(ape) # stats
library(vegan) # stats
library(RColorBrewer)
library(cocor)
library(DescTools)
library(reshape2)
library(grid)
library(ggplotify)
library(stringr)



# read the csv data files into a dataframe
files = list.files(pattern="*.csv")
data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id")

colnames(data)

# Select variables we need for analysis 
trial_vars<- c( "participant",
                "Flower1", "Flower2",
                "similarity", "response_time", "catchnumber", "Ecc", "catchnumberprac", "catchresponse", "catchtrialorder", "screen_size_x","screen_size_y","viewerdistancecm", 'viewer_distance',"trialnumber","Ecc")

data <- subset(data, select = trial_vars)

# Catch Trial Check

get.catchtrial.info <- function(df.catchtrialorder){
  info <- (unique(df.catchtrialorder)[2])
  info <- as.character(info) # convert to string
  info <- str_sub(info, 2, -2) # remove the square brackets
  info <- str_split(info, pattern = fixed(',')) # get a vector of the catch trials in string format
  info <- info[[1]]
  #print(info) # testing
  info <- as.numeric(info) # convert to numeric
  return(info)
}


add.catchtrial.info <- function(df){
  IDs <- unique(df$participant)
  colnames <- colnames(df)
  output.df <- df[FALSE,]
  for(ID in IDs){
    tempdf <- subset(df, participant == ID)
    catch.trials <- get.catchtrial.info(tempdf$catchtrialorder)
    tempdf$catch.trial <- ifelse(is.element(tempdf$trialnumber,catch.trials),TRUE,FALSE)
    #print(colnames(tempdf)) #testing
    output.df <- rbind(output.df,tempdf)
  }
  return(output.df)
  
}
data$catch.trials <- NA # need to add this here to make stuff work nicely later
test <- add.catchtrial.info(data)
  

# Check catch scores 
catch_trial_checker <- function(datadf){
  
  subjectlist <- sort(unique(test$participant))
  print("Catch scores")
  for (participant in subjectlist){
    subjectdf <- test[which(test$participant == participant),] 
    catch_trials <- subset(subjectdf, catch.trial == TRUE)
    catch_num = nrow(catch_trials)
    catch_correct = nrow(subset(catch_trials, catchnumber == catchresponse))
    
    print(paste("Subject",participant,":",catch_correct,"/",catch_num))
  }
}


# Screen parameters 

# Screen size function 

screen_size <- function(dftrials){
  
  dftrials<- subset(dftrials, !is.na(screen_size_x), !is.na(screen_size_y))

  width <- as.numeric(substr(as.character(dftrials$screen_size_x)[1],1,6))
  height <- as.numeric(substr(as.character(dftrials$screen_size_y)[1],1,6))
  
  # use pythagoras to just get the hypotenuse. Subjects have a fixed 16/9 aspect ratio so these are all comparable
  return(sqrt(width*width + height*height))
}

# View distance function 

view_distance <- function(data){
  dataview <- subset(data,!is.na(viewer_distance))
  return(median(dataview$viewer_distance))
}

# Calculate screen parameters for each participant 

screen_parameters <- function(dftrials,individual=FALSE){
  
  subjectlist <- sort(unique(dftrials$participant))
  print("Screen Parameters")
  screen_fail = 0
  viewing_fail = 0
  for (participant in subjectlist){

    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    
    
    screen_size <- round(screen_size(subjectdf)/10,1)
    viewing_distance <- round(view_distance(subjectdf)/10,1)
    
    if(screen_size < 20){screen_fail = screen_fail + 1}
    if(viewing_distance < 30){viewing_fail = viewing_fail + 1}
    
    if(individual){
      print(paste("Subject",participant,":"))
      print(paste("Screen size:",screen_size,"cm"))
      print(paste("Viewing distance:",viewing_distance,"cm"))
      print("")
    }
    
    
  }
  print("")
  print(paste("Screen size issues:",screen_fail,"/",length(subjectlist)))
  print(paste("Viewing distance issues:",viewing_fail,"/",length(subjectlist)))
}  

screen_parameters(data,individual=TRUE)

# Create data frame for trials 
dftrials <- subset(data, !is.na(Flower2))

# Label participant number from 1 - 15 
dftrials$ID <- NA
subjectlist <- unique(dftrials$participant)
k= 0
for (participant in subjectlist){
  k = k + 1
  dftrials$ID[dftrials$participant == participant] <- k
}


# Check average Response Time

rt_avg <- function(data){
  return(mean(data$response_time))
}

rt_avg_check <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$participant))
  print("RT avg")
  for (participant in subjectlist){
    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    rt = rt_avg(subjectdf)
    print(paste("Subject:",participant,"mean rt",rt))
  }
}

# For graphing 
dftrials$Flower1 <- as.character(dftrials$Flower1)
dftrials$Flower1 <- revalue(dftrials$Flower1, 
                            c(  "1" = 'flower1',
                                "2" = 'flower2',
                                "3" = 'flower3',
                                "4" = 'flower4',
                                "5" = 'flower5',
                                "6" = 'flower6',
                                "7" = 'flower7',
                                "8" = 'flower8',
                                "9" = 'flower9'))
dftrials$Flower2 <- as.character(dftrials$Flower2)
dftrials$Flower2 <- revalue(dftrials$Flower2, 
                            c(  "1" = 'flower1',
                                "2" = 'flower2',
                                "3" = 'flower3',
                                "4" = 'flower4',
                                "5" = 'flower5',
                                "6" = 'flower6',
                                "7" = 'flower7',
                                "8" = 'flower8',
                                "9" = 'flower9'))



flowers <- c('flower1', 'flower2','flower3','flower4','flower5','flower6','flower7','flower8','flower9')

# changing from int indicators in the .csv file to more readable labels for eccentricity
foveal = -1
peripheral = 1

# set the maximum and minimum dissimilarity values for later analysis
min_val = 0
max_val = 6


# Similarity judgment histogram
simhistplot <- function(datadf){
    
   plot <- ggplot(dftrials, aes(x = similarity)) + geom_bar(aes(y = ..prop..)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
    return(plot)
}


simhistplot_summary <- function(datadf){
    
    datadf$ID <- as.character(datadf$ID) # necessary for visualisation
    
    plot <- ggplot(datadf, aes(x = similarity)) + 
    geom_line(stat='count',aes(y = ..prop..,group = ID),color='#CC9933') +
    geom_line(stat='count',aes(y = ..prop..),size=1.5) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
    return(plot)
    
}


# reaction time for each similarity

rsplot <- function(datadf){
    
    plot <- ggplot(dftrials, aes(x= similarity, y=response_time)) + 
    stat_summary(fun.y = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", size =0.5, aes(width=0.5)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') + ylab('Reaction Time (s)') +
    theme(legend.position = "none") +
    ylim(0,4) # anyone taking more than 4 seconds has probably mindwandered
    
    return(plot)
}


rsplot_all <- function(data){
  subjectlist <- sort(unique(dftrials$ID))
  par(mfrow=c(3,5))
  for (ID in subjectlist){
    subjectdf <- dftrials[which(dftrials$ID==ID),]
    plot <- rsplot(subjectdf)
    return(plot)
  }
}


# factor the dataframes for the plot function
dissimdata2 <- function(dftrials, flowers){
    
    # refactor the levels so they can be plotted properly later if need be
    dftrials$Flower1 <- with(dftrials, factor(Flower1, levels = flowers))
    dftrials$Flower2 <- with(dftrials, factor(Flower2, levels = flowers))
    
    return(dftrials)
}

dissimdata2(dftrials, flowers)


quantify_asymmetry <- function(dftrials){
    
    data <- dissimdata2(dftrials, flowers)
    
    # aggregate over the remaining columns of interest
    nmdsdata <- aggregate(data, by = list(data$Flower1, data$Flower2),FUN=mean)
    nmdsdata$Flower1 <- nmdsdata$Group.1
    nmdsdata$Flower2 <- nmdsdata$Group.2

    nmdsdata = subset(nmdsdata, select = c("Flower1","Flower2","similarity"))  # get rid of unnecessary columns
    
    nmdsmatrix <- spread(nmdsdata, Flower1, similarity) # convert the dataframe to a matrix
    nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number (just some label stuff) 
    nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
    nmdsmatrix[is.na(nmdsmatrix)] <- 0  # change NA to 0 so sum can be calculated.
    
    matdf <- as.data.frame(as.vector(abs(nmdsmatrix - t(nmdsmatrix)))) # calculate the asymmetry
    asymmery_value <- sum(matdf)/2 # need to divide by 2 to get rid of the duplicates

    return(asymmery_value)
}

quantify_asymmetry(dftrials)



# return a list of the asymmetrical values for each subject
asymValues_list2 <- function(datadf){
    
    subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
    
    asymValues_list <- vector() # array to store the values in
    
    for (ID in subjectlist){ # go through subject by subject
        subjectdf <-  dftrials[which(dftrials$ID == ID),] 
        # select the ID for subject of interest
        asymValues_list <- c(asymValues_list, quantify_asymmetry(subjectdf))
    }
    return(asymValues_list)
}


df2mat.full <- function(dftrials){
  
  
  # aggregate over the remaining columns of interest
  datadf <- aggregate(dftrials, by = list(dftrials$Flower1, dftrials$Flower2),FUN=mean)
  datadf$Flower1 <- datadf$Group.1
  datadf$Flower2 <- datadf$Group.2
  
  datadf = subset(datadf, select = c("Flower1","Flower2","similarity"))  # get rid of unnecessary columns
  datadf <- spread(datadf, Flower1, similarity)
  
  # convert the dataframe to a matrix
  datamatrix <- data.matrix(datadf)
  datamatrix <- datamatrix[,-1] # get rid of the labels in the first column, it messes up the code
  rownames(datamatrix) <- colnames(datamatrix)
  return(datamatrix)
  
}


# Dissimplot for all data

dissimplot_temporal <- function(subjectdf,flowers,dependent='color'){
    
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(subjectdf, flowers)
    datatemp <- aggregate(datatemp, by = list(datatemp$Flower1, datatemp$Flower2),FUN=mean)
    
    plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
    theme(axis.text.x = element_text(), axis.text.y = element_text(),
                      axis.title.x = element_blank(), axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
    
    # stuff that's standard across plot types
        plot <- plot + geom_raster(aes(fill = similarity)) +
                labs(title = 'Presented - Response Screen') +
                scale_fill_gradientn(colours = c("white","black")) +
                guides(fill=guide_legend(title="Dissimilarity"))
    return(plot)
}

dissimplot_temporal(dftrials, flowers)

# Plot a dissmiliarity matrix for all subjects 
dissimplot_temporal_subject <- function(dftrials, flowers){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
    #Subset data for the subject
    
    subjectdf = dftrials[which(dftrials$ID == ID),] 
    
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(subjectdf, flowers)
    datatemp <- aggregate(datatemp, by = list(datatemp$Flower1, datatemp$Flower2),FUN=mean)
    
    
    plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
      theme(axis.text.x = element_text(), axis.text.y = element_text(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      ggtitle(paste("Subject ID:", ID))
    
    
    # stuff that's standard across plot types
    plot <- plot + geom_raster(aes(fill = similarity)) +
      scale_fill_gradientn(colours = c("white","black")) +
      guides(fill=guide_legend(title="Dissimilarity"))
    
    plot_list[[ID]] <- plot
    
  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}




# CORRELATION BETWEEN PASSES 


matrixcor_pear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=162),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=163),])
  return(cor(c(matrix1), c(matrix2), method = "pearson"))
}

matrixcor_spear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=162),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=163),])
  return(cor(c(matrix1), c(matrix2), method = "spearman"))
}

matrixcor_spear(subjectdf)

pass_compare_list_plot <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  correlation_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
    correlation_list <- c(correlation_list, (matrixcor_pear(subjectdf)))
    
    plot <- plot(correlation_list, main = '1st and 2nd pass Pearson correlation - r',
                 xlab='Participant',ylab='r',xlim=c(1,14),pch = 21, col="black")
    axis <- axis(1,seq(1,14,1))
  }
  return(correlation_list)
  return(plot)
  return(axis)
}




library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


# Asymmtery matrix temporal

df2mat_asymmetry_temporal <- function(dftrials){
  
  datatemp <- dissimdata2(dftrials, flowers)
  
  # aggregate over the remaining columns of interest
  nmdsdata <- aggregate(datatemp, by = list(datatemp$Flower1, datatemp$Flower2),FUN=mean)
  nmdsdata$Flower1 <- nmdsdata$Group.1
  nmdsdata$Flower2 <- nmdsdata$Group.2
  
  nmdsdata = subset(nmdsdata, select = c("Flower1","Flower2","similarity"))  # get rid of unnecessary columns
  nmdsmatrix <- spread(nmdsdata, Flower1, similarity) # convert the dataframe to a matrix
  nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
  nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
  
  matdf<-  as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
  matdf$flowersset <- c(flowers) # adding additional column "colorset"
  num_flowers <- length(flowers)
  matdf <- matdf %>% gather(otherflowers,asymmetry ,1:num_flowers) # convert the matrix back to the data frame which has the 
  return(matdf)
}

df2mat_asymmetry_temporal(dftrials)

dftrials <- dftrials[which(dftrials$ID!=7),]
# plot an asymmetry matrix for all data
asymmetry_plot_temporal <- function(subjectdf, flowers){

  datatemp <- df2mat_asymmetry_temporal(subjectdf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$flowersset <- with(datatemp, factor(flowersset, levels = flowers))
  datatemp$otherflowers <- with(datatemp, factor(otherflowers, levels = flowers))
  
  plot <- ggplot(datatemp, aes(x = flowersset, y = otherflowers)) +
    theme(axis.text.x = element_text(), axis.text.y = element_text(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          #axis.title.x = element_text("left"), axis.title.y = element_text("right"),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    labs(title = 'Presented - Response Screen') +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))
  return(plot)
}
asymmetry_plot_temporal(dftrials, flowers)


# Plot an asymmetry matrix for all subjects 
asymmetry_plot_temporal_subject <- function(dftrials, flowers){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
    #Subset data for the subject
    
  subjectdf = dftrials[which(dftrials$ID == ID),] 
  datatemp <- df2mat_asymmetry_temporal(subjectdf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$flowersset <- with(datatemp, factor(flowersset, levels = flowers))
  datatemp$flowerscolor <- with(datatemp, factor(otherflowers, levels = flowers))
  
  plot <- ggplot(datatemp, aes(x = flowersset, y = otherflowers)) +
    theme(axis.text.x = element_text(), axis.text.y = element_text(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    ggtitle(paste("Subject ID:", ID))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))

  plot_list[[ID]] <- plot
  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}
  

asymmetry_plot_temporal_subject(dftrials, flowers)





