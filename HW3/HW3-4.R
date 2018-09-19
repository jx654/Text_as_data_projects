mydfm <- dfm(subset(inaugCorpus, Year>1900))
summary(subset(inaugCorpus, Year>1900))

df_fit<-textmodel_wordfish(mydfm, c(28,18))
df_fit@theta
words<-df_fit@beta
names(words) <- df_fit@features

sort(words)[1:50]

sort(words, decreasing=T)[1:50]

##guitar plot


weights<-df_fit@beta
words<-df_fit@psi
plot(weights, words)


