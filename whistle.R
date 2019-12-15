# Cleaning Functions
print("Did you update the cleaning function link?")
print("Did you update the cleaning function link?")
print("Did you update the cleaning function link?")
print("Did you update the cleaning function link?")
print("Did you update the cleaning function link?")

### RT ### 

cleanRT.win <- function(data, vars, value, new_df = cleaned, minRT = 250){
  print("Reminder:")
  print("For RT data, only include correct trials!")
  
  temp <- ddply(data, c(vars), function(x){
    
    cMax <- (mean(x[[value]])+3*sd(x[[value]]))
    cMax <- ifelse(is.na(cMax), x[[value]], cMax)
    mutate(x, RTwin = ifelse(x[[value]] >= cMax, cMax, 
                             ifelse(x[[value]] <= minRT, minRT, x[[value]])))
    
  })
  
  df_name <- deparse(substitute(new_df))
  df_name <- gsub('[\"]', '', df_name)
  colnames(temp)[colnames(temp)=="V1"] <- value
  assign(df_name, temp, envir = .GlobalEnv)
}


### ACC winsorize ###

cleanACC.win <- function(data, subjVar, vars, value, new_df = cleanedACC, minACC = .2){
  
  temp <- ddply(data, c(subjVar, vars), function(x){
    
    mean(x[[value]])
    
  })
  
  df_name <- deparse(substitute(new_df))
  df_name <- gsub('[\"]', '', df_name)
  
  temp <- mutate(temp, ACCwin = ifelse(temp$V1 <= minACC, minACC, temp$V1))
  
  colnames(temp)[colnames(temp)=="V1"] <- value 
  assign(df_name, temp, envir = .GlobalEnv)
  
}


### ACC cut off ###

cleanACC.cut <- function(data, subjVar, vars, value, new_df = cleanedACC, minACC = .2){
  
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

# cleanRT.win(data = datRT,
#             vars = c("ID", "phase", "session", "trialType"), 
#             value = "RT", 
#             new_df = "datRT",
#             minRT = 250)

# cleanACC.win(data = datACC,
#              subjVar = "ID",
#              vars = c("session", "trialType", "block", "parity"),
#              value = "probeCorrect",
#              new_df = datACC.c,
#              minACC = .2)

# cleanACC.cut(data = datACC,
#              subjVar = "ID",
#              vars = c("phase", "session", "trialType", "list"),
#              value = "probeCorrect",
#              new_df = datACCwin,
#              minACC = .2)