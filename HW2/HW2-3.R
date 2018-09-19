setwd("/Users/apple/Desktop/Text_HW2/")
rankings <- read.csv("CF_rate_trustworthiness.csv")
rankings <- data.frame(rankings)
country <-unique(rankings$X_country)
photo <-rankings$image_name
rating <-rankings$rating
for (i in 1:length(country)){
  list_rate <-rankings$rating[rankings$X_country ==country[i]]
  if (length(list_rate) >1) {
    if(t.test(list_rate,rating)['p.value']<=0.05) {
      print(country[i])
    }
  }
  
  
}

photo
list_photo_ww <-rankings$rating[substr(rankings$image_name,1,10)== 'whitewoman']
list_photo_wm <-rankings$rating[substr(rankings$image_name,1,8)== 'whiteman']
list_photo_bm <-rankings$rating[substr(rankings$image_name,1,8)== 'blackman']

t.test(list_photo_bm,list_photo_ww)['p.value']
t.test(list_photo_bm,list_photo_wm)['p.value']
t.test(list_photo_ww,list_photo_wm)['p.value']
