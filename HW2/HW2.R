library(quanteda)
setwd("/Users/apple/Desktop/Text_HW2/")
reviews <- read.csv("p4k_reviews.csv")
reviews$text<-as.character(reviews$text)
med<-median(reviews$score)
ninety<-quantile(reviews$score, .9)
ten<-quantile(reviews$score, .1)
reviews$positive<-reviews$score>med
reviews$anchor_positive<-reviews$score>ninety
reviews$anchor_negative<-reviews$score<ten
table(reviews$anchor_negative)
#read in terms
pos<-read.table("positive-words.txt", as.is=F)
pos<-as.character(pos$V1)
neg<-read.table("negative-words.txt", as.is=F)
neg<-as.character(neg$V1)
##create dict
mydict <- list(pos=pos, neg=neg)
##pre-process
head(reviews$text)
mydfm<-dfm(reviews$text, removePunct=TRUE, stem=F, dictionary=mydict)
##calculate sentiment
reviews$sent<-as.numeric(mydfm[,'pos'])-as.numeric(mydfm[,'neg'])
table(reviews$sent)
reviews$sent_pos<-reviews$sent>-1
##find median sentiment
med_sent<-median(reviews$sent)
##find number of positive sentiment reviews
pos_sent<-length(which(reviews$sent_pos==TRUE))

table(reviews$positive, reviews$sent_pos)
acc<-(4413 + 174) /10000
mydict <- list(pos=pos, neg=neg)
precision<-4413/(4413+5219)
acc
precision
recall
recall<-4413/(4413+194)
recall
pos_sent
reviews$rank_scores<-rank(reviews$score, ties.method="average")
reviews$rank_sent<-rank(reviews$sent, ties.method="average")
rank_gap<-sum(abs(reviews$rank_scores - reviews$rank_sent))
rank_gap

reviews_NB <-reviews[1:250,]
reviews_NB
reviews_NB$label_NB<-reviews_NB$positive
reviews_NB$label_NB[1:50]<-NA
NBdfm<-dfm(reviews_NB$text, stem=T,  removePunct=T, ignoredFeatures=stopwords("english"))
p4kNBmodel <- textmodel(NBdfm, reviews_NB$label_NB,  model="NB", smooth=0, priors="uniform")
table(reviews_NB$label_NB)
acc_NB <-(86+4)/200
acc_NB
precision_NB <-86/(109+86)
precision_NB
recall_NB <-86/(86+1)
recall_NB
install.packages("RTextTools")
library(RTextTools)
reviews_SVM <-reviews[1:1000,]

dtm  <- create_matrix(reviews_SVM$text, language="english", stemWords = FALSE, removePunctuation = FALSE)

container <- create_container(dtm, t(reviews_SVM$positive), trainSize=1:700, testSize=700:1000, virgin=FALSE)
cv.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = 'linear')

cv.svm$meanAccuracy
cv1.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = 'radial')

cv1.svm$meanAccuracy

extreme_pos <-list()
anchor_positive<-reviews$anchor_positive
for (i in 1:length(anchor_positive)){
  if (anchor_positive[i]=='TRUE'){
     extreme_pos <-c(extreme_pos,i)
  }
}

extreme_pos<-unlist(extreme_pos)
extreme_pos
extreme_neg <-list()
anchor_negative<-reviews$anchor_negative
for (i in 1:length(anchor_negative)){
  if (anchor_negative[i]=='TRUE'){
    extreme_neg <-c(extreme_neg,i)
  }
}
extreme_neg<-unlist(extreme_neg)

text_pos <-tokenize(reviews$text[41],removePunct=TRUE)
text_pos <-unlist(text_pos)
text_pos
for (i in 2:length(extreme_pos)){
  text_ex<-tokenize(reviews$text[i],removePunct=TRUE)
  
  text_ex<-unlist(text_ex)
  text_ex <-unique(text_ex)
  text_pos<-c(text_pos,text_ex)
 
}
text_pos
wordscore_pos<-dfm(text_pos,ignoredFeatures = stopwords("english"))
topfeatures(wordscore_pos)
text_neg <-tokenize(reviews$text[extreme_neg[1]],removePunct=TRUE)
text_neg <-unlist(text_neg)

for (i in 2:length(extreme_neg)){
  text_ex<-tokenize(reviews$text[i],removePunct=TRUE)
  text_ex<-unique(text_ex)
  text_ex<-unlist(text_ex)
  text_neg<-c(text_neg,text_ex)
  
}

wordscore_neg<-dfm(text_neg, removePunct=TRUE, stem = F,ignoredFeatures = stopwords("english"))
topfeatures(wordscore_neg)
text_pos = toString(text_pos)
pos_token<-tokenize(text_pos,removePunct=TRUE)
pos_count <- ntype(pos_token)
pos_count
text_neg = toString(text_neg)
neg_token<-tokenize(text_neg,removePunct=TRUE)
neg_count <- ntype(neg_token)
neg_count

s = "s"
like = "like"
album = "album"
music = "music"
one = "one"
t = "t"
songs = "songs"
can = "can"
even = "even"

a1<- (7693/pos_count)/(7693/pos_count+8497/neg_count)-(8497/neg_count)/(7693/pos_count+8497/neg_count)
a2<- (3515/pos_count)/(3515/pos_count+3823/neg_count)-(3823/neg_count)/(3515/pos_count+3823/neg_count)
a3<- (2279/pos_count)/(2279/pos_count+2518/neg_count)-(2518/neg_count)/(2279/pos_count+2518/neg_count)
a4<- (1957/pos_count)/(1957/pos_count+2200/neg_count)-(2200/neg_count)/(1957/pos_count+2200/neg_count)
a5<- (1751/pos_count)/(1751/pos_count+1942/neg_count)-(1942/neg_count)/(1751/pos_count+1942/neg_count)
a6<- (1407/pos_count)/(1407/pos_count+1571/neg_count)-(1571/neg_count)/(1407/pos_count+1571/neg_count)
a7<- (1387/pos_count)/(1387/pos_count+1522/neg_count)-(1522/neg_count)/(1387/pos_count+1522/neg_count)
a8<- (1214/pos_count)/(1214/pos_count+1345/neg_count)-(1345/neg_count)/(1214/pos_count+1345/neg_count)
a9<- (1187/pos_count)/(1187/pos_count+1326/neg_count)-(1326/neg_count)/(1187/pos_count+1326/neg_count)
##create dict
mydict <- list(s=s, like = like,album = album,music=music,one=one,t=t,songs=songs,can=can,even=even)
mydict
ntype(tokenize(reviews$text, removePunct=TRUE))
##pre-process
head(reviews$text)
mydfm<-dfm(reviews$text, removePunct=TRUE, stem=F, dictionary=mydict)

##calculate sentiment
reviews$sent<-as.numeric(mydfm[,'s']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a1+as.numeric(mydfm[,'like']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a2+as.numeric(mydfm[,'album']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a3+as.numeric(mydfm[,'music']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a4+as.numeric(mydfm[,'one']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a5+as.numeric(mydfm[,'t']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a6+as.numeric(mydfm[,'songs']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a7+as.numeric(mydfm[,'can']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a8+as.numeric(mydfm[,'even']/ntype(tokenize(reviews$text, removePunct=TRUE)))*a9
reviews$sent
table(reviews$sent)

reviews$rank_scores<-rank(reviews$score, ties.method="average")
reviews$rank_sent<-rank(reviews$sent, ties.method="average")
rank_gap<-sum(abs(reviews$rank_scores - reviews$rank_sent))
rank_gap
