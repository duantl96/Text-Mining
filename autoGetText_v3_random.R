### By Tianlin Duan 11/4/2016 ###

###General Goal: 
#Automate the process of scraping and synthesizing ~100 documents of 500-word length 
#with 1/2 or 1/4 or 1/8 of content from topic A, and the rest from topic B

###Problem of v1: 
#for documents generated from the same subtopic (eg. 'Modern Dance' under 'Dance'), 
#all text started with the same paragraph, which means we will not be modeling on a topic but rather on a pre-defined text

###Updated goal & implementation: 
#we want 'randomly selected text from a pre-defined topic'
#implement it by scraping from different Wiki sub-pages of different length for each pre-defined topic

library(rvest)
library(tm)
library(stringr)
#library(magrittr)


# Functions and methods ---------------------------------------------------

### function: script_urls(String name)
# Takes a topic string 'name', and return a list of sublinks on the Wiki page for that topic
root  <- "https://en.wikipedia.org/wiki/"
script_urls <- function(name){
  file <- read_html(paste0(root,name)) %>% html_nodes('.mw-redirect') %>% html_attr("href")
  return (file)
}

### function: topicText(String[] t.list)
# Takes a list of (partial) sublinks on the Wikipage for some topic, 't.list',
# returns a list of lists of text on that topic (each element of output is a list of text on that sublink page)  
topicText<-function(t.list){
  txlist<-vector("list",length(t.list))
  for (i in 1:length(t.list)){
    subtopic=t.list[i]
    wbpg=paste("https://en.wikipedia.org",subtopic,sep="")
    xml<-read_html(wbpg)
    cast<-html_nodes(xml,"p")
    #mathImage=html_nodes(xml,".mwe-math-fallback-image-inline");
    text<-html_text(cast)
    #remove citation brackets so that don't add to word counts later eg. [1][2][3], [citation needed]
    text<-gsub("\\[[[:alnum:]]*|\\[|\\]]*","",text)
    #remove formating context, i.e. anything inside {}
    text<-gsub("\\{.*\\}","",text)
    #every element in the txlist is a list of strings (the content of 'wbpg'), 
    #where each string is a paragraph scraped from 'wbpg'
    txlist[[i]]=text
  }
  return(txlist)
}

### function: getNwords
# Given a word count limit, 'N.num', and a list of lists of text, 'xTxt', 
# 1) sample one list of text (content of a sublink page)
# 2) return one paragraph starting from somewhere in the middle of the sublink page of length 'N.num'
getNwords<-function(N.num, txlist){
  fulldoc=""
  wcount=0
  # RANDOM SAMPLE
  n=0
  avaiLength=0
  while (avaiLength<N.num|n<4){ # make sure it at least starts from the 2nd paragraph
    # check if the length of i-th to last paragaphs are larger than N.num (s.t. no NA will be generated)
    # if not, go back and sample again
    m=sample(txlist,1) 
    m=m[[1]] # sample one sublink page's content
    #the [[1]] above is necessary because we sampled a list of 1 list,but need a single-layer list to generate text
    n=length(m)
    i=sample((1+round(n/4)):round(n/2),1) # start from somewhere in the middle
    # also, do not use sample.int: it always starts from 1
    avaiSub=m[i:n]
    avaiLength=cumsum(str_count(avaiSub,"\\S+"))[n-i+1]
  }
  while (i<n){
    paragraph=gsub("[^A-Za-z ]", "", m[i])
    pcount=str_count(paragraph,"\\S+")
    if (wcount+pcount>=N.num){
      break
    }
    fulldoc=paste(fulldoc,paragraph,sep=" ")
    wcount=wcount+pcount
    i=i+1
  }
  #slice the last needed-paragraph to a string of (N.num-wcount) words length
  last=gsub("[^A-Za-z ]", "", m[i])
  split=strsplit(last," ")
  fulldoc=paste(fulldoc,paste(split[[1]][1:(N.num-wcount)],collapse = " "))
  fulldoc=trimws(fulldoc)
  return(fulldoc)
}

### function: genDocN(txl1,txl2)
# Given two list-of-lists-of-text, 'txl1' for topic1 and 'txl2' for topic2
# get 10*5 randomly-generated (in terms of content) synthesized documents, 
# write out each document and save all to a list 
genDocN <- function(txl1,txl2){
  # set length for each synthesized document
  N=500
  # get potential lengths for each (2 in total) text component
  posLen=c(round(N/2),round(N/4),round(N/8),round(3*N/4),round(7*N/8))
  #initiate a list containing all output documents
  lout <- list()
  k=1 #help populate 'lout'
  for (x in 1:10) {
    for (p in posLen){ #length of text on topic1
      t1=getNwords(p,txl1)
      q=N-p #length of text on topic2
      t2=getNwords(q,txl2)
      out=paste(t1,t2,sep=" ")
      lout[k]<-out
      k=k+1
      #write out document and include composition type in naming
      write(out,file=paste("TestWiki/",x,"_",p,"_",q,".txt",sep=""))
    }
  }
  return(lout)
}



# Main --------------------------------------------------------------------
### get a list of sublinks for each topic: Calculus, Dance, Statistics
sList=unique(script_urls('Statistics'))
mList=unique(script_urls('Calculus'))
dList=unique(script_urls('Dance'))

### get the full text on each of the three topics
sTxt=unique(topicText(sList))
mTxt=unique(topicText(mList))
dTxt=unique(topicText(dList))

### get the synthesized documents
simList=genDocN(sTxt,mTxt) # similar topics (Statistics + Calculus)
farList=genDocN(sTxt,dTxt) # far apart topics (Statistics + Dance)
### save so that do not need to run everytime to scrape, also more easily to inspect
simSave=data.frame(text=unlist(simList),stringsAsFactors = FALSE)
#write.csv(simSave,file="TestWiki/simSave.csv")
farSave=data.frame(text=unlist(farList),stringsAsFactors = FALSE)
#write.csv(farSave,file="TestWiki/farSave.csv")

# Diagnosis and testing ---------------------------------------------------
# diagnose the NA problem
lapply(sTxt, length)
sTxt[[93]]
cumsum(str_count(sTxt[[93]],"\\S+"))[length(sTxt[[93]])]
#getNwords(375,sTxt)
