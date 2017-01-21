###By Tianlin Duan 11/26/2016 ###

#Goal: Remove stop words, stemming, n-gramming
#Note that this script uses outputs from autoGetText_v3_random.R, so run the latter first.

### Improvement in this version: p-value + count cutoff for N-gramming

library(SnowballC)
library(tm)
library(textreg)
setwd("C:/Users/Tianlin/Desktop/Duke/2016 fall/STA 393")
source("NGramTesting.R") #so that the textToBigrams function can be used directly

# [1] Clean corpus ------------------------------------------------------------
corp.all<-Corpus(VectorSource(append(simList,farList))) #from autoGetText output
## 1.1 Only keep alphabetic characters
for(j in seq(corp.all))
  {   
    corp.all[[j]] <- gsub("[^A-Za-z ]", "", corp.all[[j]])  
}

## 1.2 Removing stop words
docs<-tm_map(corp.all,tolower)
docs<-tm_map(docs, removeWords, stopwords("english"))

## 1.3 Other clean-up
docs<-tm_map(docs, stripWhitespace)
docs<-tm_map(docs,trimws)

inspect(docs[1]) # test and check


# [2] Stemming ------------------------------------------------------------

#docs <- tm_map(docs, stemDocument) # Only working for the last word, reason & solution see:
                                    # http://stackoverflow.com/questions/7263478/snowball-stemmer-only-stems-last-word
for(j in seq(docs))   
{   
  docs[[j]] <- paste(wordStem(unlist(strsplit(docs[[j]], " "))),collapse = " ") 
}

## test and check
inspect(docs[1])
class(docs)

## Now docs (or docs2) is a corpus, but one last IMPORTANT step of preprocessing!
docs<-tm_map(docs,PlainTextDocument)
# The above step tells R to treat preprocessed documents as text documents


# [3] N-gramming -----------------------------------------------------------
## 3.1 Prep

#The parameter for textToBigrams, TextList, is a character vector of documents, 
#where each element is a document with tokens separated by a single white space

#So, transform our corpus: (and write the preprocessed dataframe to csv)
docList.all<-data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=FALSE)
write.csv(docList.all,file="cleanCorp/all_preNgram.csv")

## 3.2 Get N-grams
# Reminder: Don't print the function result directly, or the program will crash!
corpus=docList.all$text

# 3.2.1 Bigrams only
# Using all documents as one corpus for identifying bigrams
bi=textToBigrams(corpus,p.value.test = T,p.value.cut = 0.01,return.word.list = T)
# Only keep ngrams with no less than 10 appearances
bi=bi[bi$bigramCounts>=10,]
write.csv(bi,file="cleanCorp/Ngram/bi_combined_withPval.csv")
# Change all identified (unprocessed) bigrams in corpus to "_" concatenated bigrams
  # the original auto-concatenate function in NGramTesting source code was not working correctly
corpus.bi=corpus
bigrams=paste(bi[[1]],"_",bi[[2]],sep = "")
for (abigram in bigrams){
  raw=paste(unlist(strsplit(abigram,"_")),collapse = " ")
  corpus.bi=gsub(raw,abigram,corpus.bi)
}

#Write post-bigram-processing textList to csv files for future reference
write.csv(corpus.bi,"cleanCorp/all_bi.csv")

## 3.2.2 Both bigrams and trigrams: run test one more time on the bigrammed corpus
# remember to use the original corpus (see how NGramTesting work)
bi_tri=textToBigramsTrigrams(corpus,p.value.test = T,p.value.cut = 0.01,return.word.list=T)
#write.csv(bi_tri,file="cleanCorp/Ngram/bi_tri_withPval.csv")
#corpus.bi_tri=textToBigramsTrigrams(corpus,p.value.test = T,p.value.cut = 0.01)
#corpus.bi_tri=corpus.bi_tri[[1]]
#write.csv(corpus.bi_tri,file="cleanCorp/all_bi_tri.csv")

# Experiment: apply bi-test again
# result: no more ngram with more than 10 appearances
bi2=textToBigrams(corpus.bi,p.value.test = T,p.value.cut = 0.01,return.word.list = T)