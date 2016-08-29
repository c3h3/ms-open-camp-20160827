

library(dplyr)
library(tidyr)
library(ggplot2)


lottery_df <- read.csv("lottery_utf8.csv")
lottery_df <- lottery_df[c(2,6:12)] 
names(lottery_df) <- c("Date", paste0("X",1:6), "S")

lottery_df %>% View


lottery_df %>% gather(key = Ball, value = X, X1:S) %>% arrange(Date) %>% 
  group_by(X) %>% summarise(count=n()) %>% ggplot(.) + geom_bar(mapping = aes(x=X,y=count),stat="identity")


lottery_df %>% gather(key = Ball, value = X, X1:S) %>% arrange(Date) %>% 
  group_by(X) %>% summarise(count=n()) %>% ggplot(.) + geom_bar(mapping = aes(x=reorder(X,-count),y=count),stat="identity")


long.format.df = lottery_df %>% gather(key = Ball, value = X, X1:S) %>% arrange(Date)

wide.format.df = long.format.df %>% select(Date,X) %>% mutate(N = 1) %>% spread(key = X, value = N, fill=0) 

wide.format.df %>% View
wide.format.df %>% select(-Date) %>% View

# write.csv(x = long.format.df,file = "long_format_df.csv")
# write.csv(x = wide.format.df,file = "wide_format_df.csv")

Xlist.df = lottery_df %>% select(X1:S) %>% as.matrix

################################################################
## Conditional Prob
################################################################

events = expand.grid(Z2=Xlist.df[1,],Z1=Xlist.df[2,])

for (i in 2:(NROW(Xlist.df)-2)){
  events = rbind(events,expand.grid(Z2=Xlist.df[i,],Z1=Xlist.df[i+1,]))
}

events.df = data.frame(events)
events.df %>% group_by(Z1,Z2) %>% summarise(N=n()) %>% spread(key = Z1,value = N,fill = 0) %>% View

events.wide.df = events.df %>% group_by(Z1,Z2) %>% summarise(N=n()) %>% spread(key = Z1,value = N,fill = 0) 

events.wide.df %>% select(15, 37, 35, 17, 45, 30, 41) %>% t %>% colSums %>% sort(decreasing = T,index.return=T) %>% .[["ix"]] %>% .[1:7]
events.wide.df %>% select(27, 40,  2, 35, 22,  1, 33) %>% t %>% colSums %>% sort(decreasing = T,index.return=T) %>% .[["ix"]] %>% .[1:7]
events.wide.df %>% select(15,  4, 20, 22, 28, 29,  1) %>% t %>% colSums %>% sort(decreasing = T,index.return=T) %>% .[["ix"]] %>% .[1:7]

predict.by.cond.prob = function(...){
  last.numbers = list(...)
  events.wide.df %>% select(last.numbers[[1]], last.numbers[[2]], last.numbers[[2]], last.numbers[[4]], last.numbers[[5]], last.numbers[[6]], last.numbers[[7]]) %>% 
    t %>% 
    colSums %>% 
    sort(decreasing = T,index.return=T) %>% 
    .[["ix"]] %>% 
    .[1:7]
}


results = c()
for (i in 1:(NROW(Xlist.df)-1)){
  predictions = do.call(predict.by.cond.prob,as.list(Xlist.df[i,]))
  results = c(results,length(which(predictions %in% Xlist.df[i-1,])))
}
length(which(results>2)) / length(results)
1 / (350 / 50 + 1)


################################################################
## Jump Sampling
################################################################


jump.sampling = function(k){
  events = expand.grid(Z2=Xlist.df[1,],Z1=Xlist.df[k+1,])
  
  for (i in 2:(NROW(Xlist.df)-k)){
    events = rbind(events,expand.grid(Z2=Xlist.df[i,],Z1=Xlist.df[i+k,]))
  }
  
  events.df = data.frame(events)
  
  events.wide.df = events.df %>% group_by(Z1,Z2) %>% summarise(N=n()) %>% spread(key = Z1,value = N,fill = 0) #%>% View
  events.wide.df
}

predict.by.cond.prob = function(prob.df,...){
  last.numbers = list(...)
  prob.df %>% select(last.numbers[[1]], last.numbers[[2]], last.numbers[[2]], last.numbers[[4]], last.numbers[[5]], last.numbers[[6]], last.numbers[[7]]) %>% 
    t %>% 
    colSums %>% 
    sort(decreasing = T,index.return=T) %>% 
    .[["ix"]] %>% 
    .[1:7]
}


jump_k = 3
prob.df=jump.sampling(jump_k)
results = c()
for (i in 1:(NROW(Xlist.df)-1)){
  predictions = do.call(predict.by.cond.prob,
                        c(list(prob.df=prob.df),as.list(Xlist.df[i,])))
  results = c(results,length(which(predictions %in% Xlist.df[i-1,])))
}
length(which(results>2)) / length(results)
1 / (350 / 50 + 1)


jump_results = c()
for (jump_k in 1:20){
  prob.df=jump.sampling(jump_k)
  results = c()
  for (i in 1:(NROW(Xlist.df)-20)){
    predictions = do.call(predict.by.cond.prob,
                          c(list(prob.df=prob.df),as.list(Xlist.df[i,])))
    results = c(results,length(which(predictions %in% Xlist.df[i-1,])))
  }
  jump_results = rbind(jump_results,c(jump=jump_k,wp=length(which(results>2)) / length(results)))
}

jump_results %>% head

jump_results %>% as.data.frame %>% ggplot(.) + geom_bar(mapping = aes(x=jump,y=wp),stat="identity")

################################################################
## coverage testing
################################################################

wide.format.mat = wide.format.df %>% select(-Date) %>% as.matrix

first_converage_results = c()
for (i in (1:NROW(wide.format.mat))){
  # print("~~~~~~~~~~~~~~~~~~~")
  # print(i)
  j = i
  covered = F
  while (!covered & j<NROW(wide.format.mat)){
    j = j+1
    # print(j)
    if ((wide.format.mat[i:j,] %>% colSums %>% min) > 0){
      first_converage_results = rbind(first_converage_results,c(i=i,j=j,diff=j-i))
      covered = T
    }
  }
}


first_converage_results_df = data.frame (first_converage_results)
first_converage_results_df %>% group_by(diff) %>% summarise(n=n()) %>% ggplot(.) + geom_bar(mapping = aes(x=diff,y=n),stat="identity")


################################################################
## coverage testing
################################################################

wide.format.mat = wide.format.df %>% select(-Date) %>% as.matrix

all_ngram_sum = c()
for (k in 2:50){
  for (i in (1:(NROW(wide.format.mat)-k+1))){
    # print("~~~~~~~~~~~~~~~~~~~")
    # print(i)
    j = i+k-1
    ngram_sum = wide.format.mat[i:j,] %>% colSums %>% sort(decreasing = T) %>% paste0(collapse = "")
    all_ngram_sum =rbind(all_ngram_sum,c(k=k,i=i,j=j,ngram_sum=ngram_sum))
  }
}


all_ngram_sum_df = all_ngram_sum %>% data.frame(stringsAsFactors = F)
all_ngram_sum_df$ngram_sum
all_ngram_sum_df$k = as.integer(all_ngram_sum_df$k)
all_ngram_sum_df$i = as.integer(all_ngram_sum_df$i)
all_ngram_sum_df$j = as.integer(all_ngram_sum_df$j)

write.csv(all_ngram_sum_df,"all_ngram_sum_df.csv")

all_ngram_sum_df %>% group_by(k,ngram_sum) %>% summarise(n=n()) %>% arrange(-n) %>% View


gb_df = all_ngram_sum_df %>% group_by(k,ngram_sum) %>% summarise(n=n()) %>% arrange(-n)


write.csv(gb_df,"gb_all_ngram_sum_df.csv")
