install.packages("topicmodels")
install.packages("lda")
install.packages("stm")
install.packages("LDAvis")
install.packages("ggplot2")
library(ggplot2)
require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
data(immigNewsCorpus, package = "quantedaData")
summary(immigNewsCorpus, 5)
topPapers <- sort(table(immigNewsCorpus[["paperName"]]), decreasing = TRUE)
reducedCorpus <- subset(immigNewsCorpus, paperName %in% names(topPapers)[1:4])
table(reducedCorpus[["paperName"]])
reducedCorpus[["paperName"]]

load("/Users/apple/Desktop/custom_stopwords.RData")
custom_stopwords[573:588] 
news_dfm <-dfm(reducedCorpus)
news_dfm <-removeFeatures(news_dfm,custom_stopwords)
news_select <-trim(news_dfm, minCount = 30, minDoc = 20) 

require(topicmodels)
require(LDAvis)
K <- 30
SEED<-2000
TM<-list(Gibbs = LDA(news_select, k = K, method = "Gibbs",  control = list(seed = SEED,iter = 5000)))
lda_terms <- get_terms(TM[["Gibbs"]], 10)

lda_terms[, 1:5]
lda_terms
doc_topics<-TM[["Gibbs"]]@gamma
words_topics<-TM[["Gibbs"]]@beta

words_topics
#1-g
doc_topics
doc_topics<-t(doc_topics)
doc_topics
topic_guar_1 <-0
topic_guar_2 <-0
topic_guar_3 <-0
topic_guar_4 <-0
topic_guar_5 <-0
for (i in 1316:1667){
  topic_guar_1 = topic_guar_1+doc_topics[,i][1]
  topic_guar_2 = topic_guar_2+doc_topics[,i][2]
  topic_guar_3 = topic_guar_3+doc_topics[,i][3]
  topic_guar_4 = topic_guar_4+doc_topics[,i][4]
  topic_guar_5 = topic_guar_5+doc_topics[,i][5]
}
topic_guar_1 <- topic_guar_1/352
topic_guar_1
topic_guar_2 <- topic_guar_2/352
topic_guar_2
topic_guar_3 <- topic_guar_3/352
topic_guar_3
topic_guar_4 <- topic_guar_4/352
topic_guar_4
topic_guar_5 <- topic_guar_5/352
topic_guar_5

#1-f
#arrange topics
max<-apply(doc_topics, 1, which.max)
max
##write a function that finds the second max
which.max2<-function(x){
  which(x == sort(x,partial=(K-1))[K-1])
  
}

max222<- apply(doc_topics, 1, which.max2)
max222<-sapply(max222, max)

max222

index<-seq(1:392)
index2<-seq(1:412)
guardian2<-data.frame(max[1:392], max222[1:392])
mail2<-data.frame(max[393:804], max222[393:804])
mail2
####plot
z<-ggplot(guardian2, aes(x=index, y=max.1.392., pch="First")) 

z + geom_point(aes(x=index, y=max222.1.392., pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("guardian")  + 
  xlab(NULL) + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point() + 
  geom_vline(xintercept=57) +
  geom_vline(xintercept=143)  +
  geom_vline(xintercept=114, linetype=2) +
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

z<-ggplot(mail2, aes(x= index2, y=max.393.804., pch="First")) 

z + geom_point(aes(x=index2, y=max222.393.804., pch="Second") ) + ylab("Topic Number")+theme_bw()   + ggtitle("mail")  +
  xlab(NULL) + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point() + 
  geom_vline(xintercept=57) +
  geom_vline(xintercept=143)  +
  geom_vline(xintercept=114, linetype=2) +  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

#1-h
require(LDAvis)
install.packages('servr') 
LDApost <- posterior(TM[["Gibbs"]])
jsonLDA <- createJSON(phi = LDApost$terms, 
                      theta = LDApost$topics, 
                      doc.length = ntoken(news_select), 
                      vocab = features(news_select), 
                      term.frequency = colSums(news_select))
serVis(jsonLDA, out.dir = "visCollLDA", open.browser = TRUE)

#T2
K <- 30
SEED<-1000
TM1<-list(Gibbs = LDA(news_select, k = K, method = "Gibbs",  control = list(seed = SEED,iter = 5000)))
lda_terms1 <- get_terms(TM1[["Gibbs"]], 10)

lda_terms1[, 1]
lda_terms1
doc_topics1<-TM1[["Gibbs"]]@gamma
words_topics1<-TM1[["Gibbs"]]@beta
install.packages("clue")
#2-b
align <- clue::solve_LSAP(words_topics1%*%t(words_topics), maximum=TRUE)
align
#2-c
intersect(lda_terms1[, 30],lda_terms[, 8])
#2-d
K <- 10
SEED<-2000
TM3<-list(Gibbs = LDA(news_select, k = K, method = "Gibbs",  control = list(seed = SEED,iter = 5000)))
lda_terms3 <- get_terms(TM3[["Gibbs"]], 10)

lda_terms3
doc_topics3<-TM3[["Gibbs"]]@gamma
words_topics3<-TM3[["Gibbs"]]@beta

K <- 10
SEED<-1000
TM4<-list(Gibbs = LDA(news_select, k = K, method = "Gibbs",  control = list(seed = SEED,iter = 5000)))
lda_terms4 <- get_terms(TM4[["Gibbs"]], 10)

lda_terms4
doc_topics4<-TM4[["Gibbs"]]@gamma
words_topics4<-TM4[["Gibbs"]]@beta
align2 <- clue::solve_LSAP(words_topics4%*%t(words_topics3), maximum=TRUE)
align2
intersect(lda_terms4[, 10],lda_terms3[, 10])

#T3
data_stm <- subset(immigNewsCorpus, paperName %in% c("guardian","mail"))
table(data_stm[["paperName"]])
days<-data_stm$documents$day
papername<-data_stm[["paperName"]]
txt<-data_stm$documents$texts
library(stm)
data<-data.frame(papername, txt, days)
processed <- textProcessor(data$txt, metadata=data,stem=TRUE)
out<- prepDocuments(processed$documents, processed$vocab, processed$meta)

install.packages("Rtsne")
install.packages("geometry")
fitSpec <- stm(out$documents,out$vocab,K=0, init.type="Spectral", 
                 content=~papername, prevalence = ~papername + as.numeric(days),data=out$meta, seed=5926696)

plot.STM(fitSpec, type="summary")
big_25<-c(1)
labelTopics(fitSpec, big_25)

out$meta$papername<-as.factor(out$meta$papername)
out$meta$days<-as.numeric(out$meta$days)
prep<-estimateEffect(big_25 ~ papername , fitSpec, meta=out$meta)
plot.estimateEffect(prep, covariate="papername", topics=big_25, model=out, method="difference",  cov.value1 = "guardian", cov.value2 = "mail",
                    xlab = "More mail......More Guardian", xlim=c(-.1, .1))

prep<-estimateEffect(big_25 ~ s(days) , fitSpec, meta=out$meta)

##plot effects
plot.estimateEffect(prep, covariate="days", topics=big_25, model=out, method="continuous")
