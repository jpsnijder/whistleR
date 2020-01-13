# Whistle.R -- Cleaning Functions
## Version: .200
## - from dropbox to github hosting.
## - change of functions names
## - added standard deviation functionality
## - added a help function

# Depedencies
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}


#################################################
### RT ##########################################
#################################################

####
# Winsorized RT 
####
RT.win <- function(data, subjVar = "", vars = "", value, new_df = winsorized, st.d = 3, minRT = 250){
  print("Reminder:")
  print("For RT data, only include correct trials!")
  
  temp <- ddply(data, c(subjVar, vars), function(x){
    
    cMax <- (mean(x[[value]])+st.d*sd(x[[value]]))
    cMax <- ifelse(is.na(cMax), x[[value]], cMax)
    mutate(x, RTwin = ifelse(x[[value]] >= cMax, cMax, 
                             ifelse(x[[value]] <= minRT, minRT, x[[value]])))
    
  })
  
  df_name <- deparse(substitute(new_df))
  df_name <- gsub('[\"]', '', df_name)
  colnames(temp)[colnames(temp)=="V1"] <- value
  assign(df_name, temp, envir = .GlobalEnv)
}

# RT.win(data = datRT,
#             subjVar = "ID",
#             vars = c("phase", "session", "trialType"), 
#             value = "RT", 
#             new_df = "datRT",
#             st.d = 3,
#             minRT = 250)

####
# RT Standard Cutoff 
####
RT.cut <- function(data, subjVar, vars, value, new_df = cut, st.d = 3, minRT = 250){
  print("Reminder:")
  print("For RT data, only include correct trials!")
  
  temp <- ddply(data, c(subjVar, vars), function(x){
    
    cMax <- (mean(x[[value]])+st.d*sd(x[[value]]))
    cMax <- ifelse(is.na(cMax), x[[value]], cMax)
    
    x <- x[which(x[[value]] <= cMax),]
    
    
    
  })
  
  df_name <- deparse(substitute(new_df))
  df_name <- gsub('[\"]', '', df_name)
  colnames(temp)[colnames(temp)=="V1"] <- value
  assign(df_name, temp, envir = .GlobalEnv)
}

# RT.cut(data = datRT,
#             subjVar = "ID",
#             vars = c("phase", "session", "trialType"), 
#             value = "RT", 
#             new_df = "datRT",
#             st.d = 3,
#             minRT = 250)



###########
### ACC ###
###########

### ACC winsorize ###

ACC.win <- function(data, subjVar, vars, value, new_df = cleanedACC, minACC = .2){

  temp <- ddply(data, c(subjVar, vars), function(x){

    mean(x[[value]])

  })

  df_name <- deparse(substitute(new_df))
  df_name <- gsub('[\"]', '', df_name)

  temp <- mutate(temp, ACCwin = ifelse(temp$V1 <= minACC, minACC, temp$V1))

  colnames(temp)[colnames(temp)=="V1"] <- value
  assign(df_name, temp, envir = .GlobalEnv)

}


### ACC cutoff ###

ACC.cut <- function(data, subjVar, vars, value, new_df = cleanedACC, minACC = .2){
  
  temp <- ddply(data, c(subjVar, vars), function(x){
    
    mean(x[[value]])
    
  })
  
  df_name <- deparse(substitute(new_df))
  df_name <- gsub('[\"]', '', df_name)
  
  temp <- subset(temp, V1 >= minACC)
  
  colnames(temp)[colnames(temp)=="V1"] <- value 
  assign(df_name, temp, envir = .GlobalEnv)
  
}


# /Cleaning Functions

# ACC.win(data = datACC,
#              subjVar = "ID",
#              vars = c("session", "trialType", "block", "parity"),
#              value = "probeCorrect",
#              new_df = datACC.c,
#              minACC = .2)

# ACC.cut(data = datACC,
#              subjVar = "ID",
#              vars = c("phase", "session", "trialType", "list"),
#              value = "probeCorrect",
#              new_df = datACCwin,
#              minACC = .2)


### Improvised Help File ###
whistleR.help <- function(){
  download.file("https://raw.githubusercontent.com/jpsnijder/whistleR/master/whistleRhelp.txt", "whistleRhelp.txt")
  file.show("whistleRhelp.txt")
}








