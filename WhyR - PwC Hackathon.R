# -------------------------------------------------------------------------------------------------
# Jan 27, 2021
# Script of whyR-PwC Hackathon:
# The challenge is matching name of academic papers in different format.
# -------------------------------------------------------------------------------------------------
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

# extract the ids of papers in tableB to "doc_id" vector
doc_id <- tableB[,1]

# merge the title, authors, venue and year column in "textB" vector
textB <- NULL
for(i in 1:2294){
  textB[i] <- paste(tableB[i, c(2,3,4,5)], collapse = " ")
}

# preprocessing of textB
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



# term_count_a <- freq_terms(texta, 10)
# term_count_b <- freq_terms(textb, 10)
# plot(term_count_a)
# plot(term_count_b)


n_train <- dim(train)[1]
for(i in 1:n_train){
  sim_mat <-  data.frame(text_sim = stringsim(dfA[train$ltable_id[i] + 1, ],
                                              dfB[train$rtable_id[i] + 1, ],
                                              method = 'jw'))
  
  # yılları farklı olan makaleleri ayırmaya yarıyor
  # sor: eşit mi değil mi
  sor <- (dfA[train$ltable_id[i] + 1, "year"]) == (dfB[train$rtable_id[i] + 1, "year"])
  # den: deneme
  if(sim_mat[2,] < 0.79){
    train$predicted[i] <- 0 # benzer değildir.
  }else
    train$predicted[i] <- 1 * sor # benzerdir.
}



acc <- NULL
for(i in 1:n_train){
  acc[i] <- train$label[i] == train$predicted[i]
}

#paste("Train set accuracy :",mean(acc))



# This part more suitable for the data but accuracy is less than stringsim()


# n_train <- dim(train)[1]
# for(i in 1:n_train){
#   sim_mat <-  seq_dist(hash(strsplit(df_a[i,"text"], "\\s+")), hash(strsplit(df_b[i,"text"], "\\s+")), method = "jaccard", q = 2)
#   sor <- (df_a[train$ltable_id[i]+1,"year"]) == (df_b[train$rtable_id[i]+1,"year"])
#   if(sim_mat <0.90){
#     train$den[i] <- 0
#   }else
#     train$den[i] <-1*sor
# }
# 
# acc <- NULL
# for(i in 1:n_train){
#   acc[i] <- train$label[i] == train$den[i]
# }
# 
# paste("Train set accuracy :",mean(acc))



n_valid <- dim(valid)[1]
for(i in 1:n_valid){
  sim_mat <-  data.frame(text_sim = stringsim(dfA[valid$ltable_id[i] + 1,],
                                              dfA[valid$rtable_id[i] + 1,],
                                              method = 'jw'))
  sor <- (dfA[valid$ltable_id[i] + 1, "year"]) == (dfB[valid$rtable_id[i] + 1, "year"])
  if(sim_mat[2,] < 0.79){
    valid$predicted[i] <- 0
  }else
    valid$predicted[i] <- 1 * sor
}

#head(valid)

#write.csv(valid,"valid-submission.csv",row.names = FALSE)
