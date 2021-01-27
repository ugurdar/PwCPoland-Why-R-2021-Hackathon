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
tablea <- read.csv("tableA.csv")
tableb <- read.csv("tableB.csv")
train  <- read.csv("train.csv")
valid  <- read.csv("valid.csv")

# extract the ids of papers in tableB to "doc_id" vector
doc_id <- tableb[,1]

# merge the title, authors, venue and year column in "textb" vector
textb <- NULL
for(i in 1:2294){
  textb[i] <- paste(tableb[i, c(2,3,4,5)], collapse = " ")
}

# 
textb  <- str_replace(textb, "NA", " ")
textb  <- gsub('\\b\\w{1}\\b', '', textb)
textb  <- str_replace(textb, "approximate"," ")
textb  <- str_replace(textb, "acm transactions on database systems ( tods )", "acmsigmodrectrans")
textb  <- str_replace(textb, "international conference on management of data", "sigmodconference")
textb  <- str_replace(textb, "acm sigmod record"  , "sigmodrecord")
textb  <- str_replace(textb, "the vldb journal -- the international journal on very large data bases", "vldbj")
textb  <- str_replace(textb, "very large data bases", "vldb")
textb  <- removePunctuation(textb) # nokta ünlem gibi işaretleri siliyor.
textb  <- tolower(textb)
textb  <- removeWords(textb, stopwords("en"))#ekleri siliyor.
textb  <- stripWhitespace(textb) #büyük boşlukları siliyor.
year_b <- gsub(".*(199[0-9]|20[01][0-9]).*","\\1", textb)
textb  <- removeNumbers(textb)
df_b   <- data.frame(doc_id = doc_id, text = textb, year = year_b)
#head(df_b)


doc_id <- tablea[,1]
texta <- NULL
for(i in 1:2616){
  texta[i] <- paste(tablea[i,c(2,3,4,5)], collapse = " ")
}

texta <- str_replace(texta, "NA", " ")
texta <- gsub('\\b\\w{1}\\b', '', texta) 
texta <- str_replace(texta, "approximate", " ")
texta <- str_replace(texta, "acm trans . database syst .", "acmsigmodrectrans")
texta <- str_replace(texta, "sigmod conference", "sigmodconference")
texta <- str_replace(texta, "sigmod record", "sigmodrecord")
texta <- str_replace(texta, "vldb j.", "vldbj")
texta <- removePunctuation(texta) # nokta ünlem gibi işaretleri siliyor.
texta <- tolower(texta)
texta <- removeWords(texta, stopwords("en"))#ekleri siliyor.
texta <- stripWhitespace(texta) #büyük boşlukları siliyor.
year_a <- gsub(".*(199[0-9]|20[01][0-9]).*","\\1", texta)
texta <- removeNumbers(texta)
df_a <- data.frame(doc_id = doc_id, text = texta, year = year_a)
#head(df_a)


# term_count_a <- freq_terms(texta, 10)
# term_count_b <- freq_terms(textb, 10)
# plot(term_count_a)
# plot(term_count_b)


n_train <- dim(train)[1]
for(i in 1:n_train){
  sim_mat <-  data.frame(text_sim = stringsim(df_a[train$ltable_id[i]+1, ],
                                              df_b[train$rtable_id[i]+1, ],
                                              method = 'jw'))
  sor <- (df_a[train$ltable_id[i] + 1, "year"]) == (df_b[train$rtable_id[i] + 1, "year"])
  if(sim_mat[2,] < 0.79){
    train$den[i] <- 0
  }else
    train$den[i] <- 1 * sor
}



acc <- NULL
for(i in 1:n_train){
  acc[i] <- train$label[i] == train$den[i]
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
  sim_mat <-  data.frame(text_sim = stringsim(df_a[valid$ltable_id[i]+1,],
                                              df_b[valid$rtable_id[i]+1,],
                                              method = 'jw'))
  sor <- (df_a[valid$ltable_id[i] + 1, "year"]) == (df_b[valid$rtable_id[i] + 1, "year"])
  if(sim_mat[2,] < 0.79){
    valid$label[i] <- 0
  }else
    valid$label[i] <- 1 * sor
}

#head(valid)

#write.csv(valid,"valid-submission.csv",row.names = FALSE)
