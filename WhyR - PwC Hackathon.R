# ------------------------------------------------------------------------------------------------------------
# Jan 27, 2021
# Script of whyR-PwC Hackathon:
# The challenge is matching name of academic papers in different format.
# ------------------------------------------------------------------------------------------------------------
# Verify the packages
library(readr)
library(stringr)
library(qdap)
library(tm)
library(stringdist)
library(magrittr)
library(hashr)

# importing data from csv files
tableA <- read.csv("tableA.csv")
tableB <- read.csv("tableB.csv")
train  <- read.csv("train.csv")
valid  <- read.csv("valid.csv")

###############################################################################################################
# preprocessing of textB
# extract the ids of the papers in tableB to "doc_id" vector
doc_id <- tableB[,1]

# merge the title, authors, venue and year column in "textB" vector
textB <- NULL
for(i in 1:2294){
  textB[i] <- paste(tableB[i, c(2,3,4,5)], collapse = " ")
}

# replacing NAs resulting from merging in line 27 with space.
textB  <- str_replace(textB, "NA", " ") 

# replacing meaningless letter and short texts with space.
textB  <- gsub('\\b\\w{1}\\b', '', textB) 

# replacing the term of "approximate", it may cause mismatching because of being in many texts.
textB  <- str_replace(textB, "approximate"," ")

# replacing the similar terms with unique terms.
textB  <- str_replace(textB, "acm transactions on database systems ( tods )", "acmsigmodrectrans")
textB  <- str_replace(textB, "international conference on management of data", "sigmodconference")
textB  <- str_replace(textB, "acm sigmod record"  , "sigmodrecord")
textB  <- str_replace(textB, "the vldb journal -- the international journal on very large data bases", "vldbj")
textB  <- str_replace(textB, "very large data bases", "vldb")

# removing punctuations
textB  <- removePunctuation(textB) 

# converting strings to lowercase in "textB"
textB  <- tolower(textB)

# removing prefix and suffix in "textB"
textB  <- removeWords(textB, stopwords("en"))

# removing white spaces
textB  <- stripWhitespace(textB) 

# extracting the years in "textB" to "yearB"
yearB  <- gsub(".*(199[0-9]|20[01][0-9]).*","\\1", textB)

# removing numbers
textB  <- removeNumbers(textB)

# creating dfB data.frame consists id, text and year.
dfB    <- data.frame(doc_id = doc_id, text = textB, year = yearB)

###############################################################################################################
# preprocessing of textA
# extract the ids of the papers in tableA to "doc_id" vector
doc_id <- tableA[,1]
textA <- NULL
for(i in 1:2616){
  textA[i] <- paste(tableA[i, c(2,3,4,5)], collapse = " ")
}

# replacing NAs resulting from merging in line 73 with space.
textA <- str_replace(textA, "NA", " ")

# replacing meaningless letter and short texts with space.
textA <- gsub('\\b\\w{1}\\b', '', textA) 

# replacing the term of "approximate", it may cause mismatching because of being in many texts.
textA <- str_replace(textA, "approximate", " ")

# replacing the similar terms with unique terms.
textA <- str_replace(textA, "acm trans . database syst .", "acmsigmodrectrans")
textA <- str_replace(textA, "sigmod conference", "sigmodconference")
textA <- str_replace(textA, "sigmod record", "sigmodrecord")
textA <- str_replace(textA, "vldb j.", "vldbj")

# removing punctuations
textA <- removePunctuation(textA) 

# converting strings to lowercase in "textA"
textA <- tolower(textA)

# removing prefix and suffix in "textA"
textA <- removeWords(textA, stopwords("en"))

# removing white spaces in "textA"
textA <- stripWhitespace(textA) #büyük boşlukları siliyor.

# extracting the years in "textA" to "yearA"
yearA <- gsub(".*(199[0-9]|20[01][0-9]).*","\\1", textA)

# removing numbers in "textA"
textA <- removeNumbers(textA)

# creating dfA data.frame consists id, text and year.
dfA   <- data.frame(doc_id = doc_id, text = textA, year = yearA)

###############################################################################################################
# optimal_cut function returns the optimal cut-off value to maximize the accuracy.
# It searches the optimal cut-off value in the neighborhood of the specified value 
# obtained from X.

#max ve 3rd quantile orta noktası

optimal_cut <- function(cut, step = 0.01){
  options(warn = -1)
  acc_opt <- NULL
  cut <- c(cut - 3 * step,
           cut - 2 * step, 
           cut - step, 
           cut, 
           cut + step, 
           cut + 2 * step,
           cut + 3 * step)
  
  for(j in 1:length(cut)){
    k <- cut[j]
    n_train <- dim(train)[1]
    for(i in 1:n_train){
      sim_mat <-  data.frame(text_sim = stringsim(dfA[train$ltable_id[i]+1,2],
                                                  dfB[train$rtable_id[i]+1,2],
                                                  method = 'jw'))
      sor <- (dfA[train$ltable_id[i] + 1, "year"]) == (dfB[train$rtable_id[i] + 1, "year"])
      if(sim_mat < k){
        train$predicted[i] <- 0
      }else
        train$predicted[i] <- 1 * sor
    }
    acc <- NULL
    for(i in 1:n_train){
      acc[i] <- train$label[i] == train$predicted[i]
    }
    acc_opt[j] <- mean(acc)
  }
  opt_cuts <- data.frame(Cutoff = cut, Accuracy = acc_opt)
  return(opt_cuts$Cutoff[opt_cuts$Accuracy == max(opt_cuts$Accuracy)])
 #return(max(opt_cuts$Accuracy))
}
#acc <- sapply(c(0.75,0.8),optimal_cut)

###############################################################################################################
# choosing initial cut value
# the initial cut value is obtained by using the summary statistics of the similarity values of 
# true matched texts. It is the midpoint of 3rd quantile and maximum value.
sim_vec <- NULL
for(i in 1:dim(dfB)[1]){
    sim_vec[i] <- stringsim(dfA[train[which(train$label == 1),1][i], 2], 
                            dfB[train[which(train$label == 1),1][i], 2], 
                            method ='jw')
}
initial_cut <- mean(c(max(sim_vec,na.rm =TRUE) ,quantile(sim_vec, 0.75, na.rm = TRUE)))

# obtaining optimal cut value assigned to "opt_cut_value" is 0.79.
opt_cut_value <- optimal_cut(initial_cut)
# opt_cut_value  
# [1] 0.79

###############################################################################################################
# This is our novel approach based on the optimal cut value given in above. It classifies the texts are
# matched or not matched according to the cut-off value.
proposed_matcher <- function(train, dfA, dfB, cut_value){
  n_train <- dim(train)[1]
  for(i in 1:n_train){
    # creating similarity matrix named as "sim_mat" show the calculated similarity values by using 
    # Jaro-Jinkler distance of the matched texts. 
    sim_mat <-  data.frame(text_sim = stringsim(dfA[train$ltable_id[i] + 1,2 ],
                                                dfB[train$rtable_id[i] + 1, 2],
                                                method = 'jw'))
    
    # there are some similar titles with different years. sor is a vector indicates that the papers 
    # titled similar but in different years.
    sor <- (dfA[train$ltable_id[i] + 1, "year"]) == (dfB[train$rtable_id[i] + 1, "year"])
    
    # If the similarity value of a matched text is lower than 0.79, it is labeled as 0: not matched 
    # or else in the following if condition. The cut-value is 0.79 in the trained model. 
    # It is calculated 
    if(sim_mat < cut_value){
      train$predicted[i] <- 0 # not matched
    }else
      train$predicted[i] <- 1 * sor # matched. Not matched when sor is 0.
  }
    return(train$predicted)
}

# predicted labels from the proposed_matcher() function assigns to "train$predicted"
train$predicted <- proposed_matcher(train = train, dfA = dfA, dfB = dfB, cut_value = opt_cut_value)

###############################################################################################################
# calculating the accuracy of the trained model
acc <- NULL
n_train <- dim(train)[1]
for(i in 1:n_train){
  acc[i] <- train$label[i] == train$predicted[i]
}
# paste("Accuracy on Train Set: ", mean(acc))
# [1] "Accuracy on Train Set:  0.991101523527033"

###############################################################################################################
# predicting the labels of the valid set
n_valid <- dim(valid)[1]
for(i in 1:n_valid){
  sim_mat <-  data.frame(text_sim = stringsim(dfA[valid$ltable_id[i] + 1,2],
                                              dfA[valid$rtable_id[i] + 1,2],
                                              method = 'jw'))
  sor <- (dfA[valid$ltable_id[i] + 1, "year"]) == (dfB[valid$rtable_id[i] + 1, "year"])
  if(sim_mat < opt_cut_value){
    valid$predicted[i] <- 0
  }else
    valid$predicted[i] <- 1 * sor
}

#write.csv(valid,"valid-submission.csv",row.names = FALSE)
