# analyze jc_nojc data using xgboost

library(xgboost)
library(Matrix)
library(ggplot2)
library(plotly)
library(pROC)

# import biblical and extrabiblical datasets
setwd("~/SynVar/Proefschrift/JC_AJN")
dat <- read.csv("jc_nojc_bib.csv")
datx <- read.csv("jc_nojc_xbib.csv")
dath <- rbind(dat, datx)
dath <- dath[dath$language %in% c('Hebrew','hbo'),]

#remove PP-subject
dath <- dath[dath$subj_type != 'PP',]

dat6 <- dath

#label ABH as 'poetry' in genre
dat6$ebh_lbh <- factor(dat6$ebh_lbh)
dat6$book.ch <- paste(dat6$book, dat6$chapter, sep = '_')
dat6$ebh_lbh <- as.character(dat6$ebh_lbh)

# change some genre labels
dat6$ebh_lbh <- as.factor(dat6$ebh_lbh)

dat6$genre <- as.character(dat6$genre)
dat6$genre[dat6$book.ch == 'Genesis_49' & as.numeric(dat6$verse) > 1 & as.numeric(dat6$verse) < 28] <- 'poetry'
dat6$genre[dat6$book.ch == 'Exodus_15' & as.numeric(dat6$verse) > 1 & as.numeric(dat6$verse) < 19] <- 'poetry'
dat6$genre[dat6$book.ch == 'Numbers_6' & as.numeric(dat6$verse) > 23 & as.numeric(dat6$verse) < 27] <- 'poetry'
dat6$genre[dat6$book.ch == 'Numbers_21' & as.numeric(dat6$verse) > 27 & as.numeric(dat6$verse) < 31] <- 'poetry'
dat6$genre[dat6$book.ch == 'Numbers_23' & as.numeric(dat6$verse) > 7 & as.numeric(dat6$verse) < 11] <- 'poetry'
dat6$genre[dat6$book.ch == 'Numbers_23' & as.numeric(dat6$verse) > 18 & as.numeric(dat6$verse) < 25] <- 'poetry'
dat6$genre[dat6$book.ch == 'Numbers_24' & as.numeric(dat6$verse) > 3 & as.numeric(dat6$verse) < 10] <- 'poetry'
dat6$genre[dat6$book.ch == 'Numbers_24' & as.numeric(dat6$verse) > 15 & as.numeric(dat6$verse) < 25] <- 'poetry'
dat6$genre[dat6$book.ch == 'Deuteronomy_32' & as.numeric(dat6$verse) < 44] <- 'poetry'
dat6$genre[dat6$book.ch == 'Deuteronomy_33' & as.numeric(dat6$verse) > 2] <- 'poetry'
dat6$genre[dat6$book.ch == 'Judges_5' & as.numeric(dat6$verse) > 1 & as.numeric(dat6$verse) < 31] <- 'poetry'
dat6$genre[dat6$book.ch == 'Judges_9' & as.numeric(dat6$verse) > 7 & as.numeric(dat6$verse) < 16] <- 'poetry'
dat6$genre[dat6$book.ch == '1_Samuel_2' & as.numeric(dat6$verse) < 11] <- 'poetry'
dat6$genre[dat6$book.ch == '2_Samuel_22' & as.numeric(dat6$verse) > 2] <- 'poetry'
dat6$genre[dat6$book.ch == '2_Samuel_23' & as.numeric(dat6$verse) > 2 & as.numeric(dat6$verse) < 8] <- 'poetry'
dat6$genre[dat6$book.ch == '2_Samuel_1' & as.numeric(dat6$verse) > 18 & as.numeric(dat6$verse) < 28] <- 'poetry'
dat6$genre <- as.factor(dat6$genre)

dat6$genre[dat6$book.ch == 'Jonah_2' & as.numeric(dat6$verse) > 3 & as.numeric(dat6$verse) < 11] <- 'poetry'

dat6$genre[dat6$book.ch == 'Daniel_2' & as.numeric(dat6$verse) > 19 & as.numeric(dat6$verse) < 24] <- 'poetry'
dat6$genre[dat6$book.ch == 'Daniel_8' & as.numeric(dat6$verse) > 22 & as.numeric(dat6$verse) < 27] <- 'poetry'
dat6$genre[dat6$book.ch == 'Daniel_12' & as.numeric(dat6$verse) < 4] <- 'poetry'
dat6$genre[dat6$book.ch == 'Nehemiah_9' & as.numeric(dat6$verse) > 5 & as.numeric(dat6$verse) < 38] <- 'poetry'
dat6$genre[dat6$book.ch == '1_Chronicles_16' & as.numeric(dat6$verse) > 7 & as.numeric(dat6$verse) < 37] <- 'poetry'

dat6$genre[dat6$book.ch == 'Job_1' & as.numeric(dat6$verse) < 21] <- 'prose'
dat6$genre[dat6$book.ch == 'Job_2' & as.numeric(dat6$verse) < 14] <- 'prose'
dat6$genre[dat6$book.ch == 'Job_42' & as.numeric(dat6$verse) > 6 & as.numeric(dat6$verse) < 18] <- 'prose'
dat6$genre[dat6$book.ch == 'Isaiah_36' & as.numeric(dat6$verse) < 23] <- 'prose'
dat6$genre[dat6$book.ch == 'Isaiah_37' & as.numeric(dat6$verse) < 22] <- 'prose'
dat6$genre[dat6$book.ch == 'Isaiah_38' & as.numeric(dat6$verse) < 10] <- 'prose'
dat6$genre[dat6$book.ch == 'Isaiah_38' & as.numeric(dat6$verse) > 20] <- 'prose'
dat6$genre[dat6$book.ch == 'Isaiah_39'] <- 'prose'
dat6$genre[dat6$book.ch == 'Jeremiah_52'] <- 'prose'
dat6$genre[dat6$book.ch == 'Jeremiah_39' | dat6$book.ch == 'Jeremiah_40'] <- 'prose'

dath2 <- dat6

table(dath2$cl_type)

dat11 <- dath2[, c(1, 9, 10, 11, 12,13, 14,15, 17, 20, 21,25, 26, 27, 33, 34, 36, 46, 47, 49, 52)]
dat11 <- dat11[sample(1:nrow(dat11)),]

# creation of lab (label or output) and sp.m (the predictors)
lab <- ifelse(dat11$cl_type == 'jc', 1, 0)
sp.m <- sparse.model.matrix(cl_type ~ .-1, data = dat11)
head(sp.m)

# 5-fold CV is applied
nfolds = 5

set.seed(3)
folds <- sample(rep(1:nfolds, length.out = nrow(sp.m)), size = nrow(sp.m), replace = F)

grid <- matrix(rep(0, 20*30), ncol = 30)
m <- 1
n <- 1


#################################################################################

CV.xgb <- lapply(1:nfolds, function(x){ 
  
  fold_x <- which(folds != x)
  train_nojc <- which(lab[fold_x] == 0)
  train_jc <- which(lab[fold_x] == 1)
  
  
  nojc <- sp.m[fold_x,][train_nojc,]
  jc <- sp.m[fold_x,][train_jc,]
  
  samp_ind <- sample(1:nrow(jc), nrow(nojc), replace = T)
  jc2 <- jc[samp_ind,]
  train <- rbind(nojc, jc2)
  label_train <- c(rep(0, nrow(nojc)), rep(1, nrow(jc2)))
  
  # train the model
  model_xg <- xgboost(data = train, eta = 0.1, label = label_train, maxdepth=4, nrounds=300, objective='binary:logistic')
  
  # make predictions on test set
  preds <- predict(model_xg,  sp.m[folds == x,])
  preds.resp <- ifelse(preds > 0.02, 1, 0)
  importance <- xgb.importance(colnames(train), model_xg)
  return(list(preds=preds.resp, true.vals = lab[folds == x], importance, preds, model_xg))
})

for (i in 1:nfolds){ 
  print(table(CV.xgb[[i]]$preds, CV.xgb[[i]]$true.vals))
}

for (i in 1:nfolds){
  print(CV.xgb[[i]][3]) 
}


############################################################

tot.acc <- numeric(nfolds)

corr.trip <- numeric(nfolds)
tot.trip <- numeric(nfolds)

corr.bip <- numeric(nfolds)
tot.bip <- numeric(nfolds)

rocs <- list()

for (i in 1:nfolds) {
  rf.tab <- table(CV.xgb[[i]][[2]], CV.xgb[[i]][[1]])
  tot.acc[i] <- sum(diag(rf.tab))/sum(rf.tab)
  corr.trip[i] <- rf.tab[1,1]
  tot.trip[i] <- sum(rf.tab[1,])
  corr.bip[i] <- rf.tab[2,2]
  tot.bip[i] <- sum(rf.tab[2,])
  print(rf.tab)
  print(rf.tab[1,])
  print(sum(rf.tab[1,]))
  #print(specificity(rf.tab))
  #print(sensitivity(rf.tab))
  true.v <- CV.xgb[[i]][[2]]
  print(true.v)
  true.v.num <- ifelse(true.v == 'jc', 1,0)
  pred.v <- CV.xgb[[i]][[1]]
  pred.v.num <- ifelse(pred.v == 'jc', 1,0)
  #print(CV.xgb[[i]][[4]])
  #print(true.v.num)
  print(roc(true.v, CV.xgb[[i]][[4]]), auc=T)
  rocs[[i]] <- roc(true.v, CV.xgb[[i]][[4]])
}

###############################################################

# make roc plot

plot(rocs[[1]], lty=3,add=FALSE,asp=NA,main='Figure 4.35. ROC curves of 5-Fold Cross Validation of ???? vs no-???? using XGBoost')
for (i in 2:nfolds) {
  lines(rocs[[i]], lty=3)
}

#################################################################

# calculate average of AUC's

sum_rocs = 0

for (i in 1:nfolds) {
  sum_rocs <- sum_rocs + rocs[[i]]$auc[1]
  print(rocs[[i]]$auc[1])
}

average_roc <- sum_rocs/nfolds
average_roc




###########################################################################
tot.acc <- numeric(nfolds)

corr.trip <- numeric(nfolds)
tot.trip <- numeric(nfolds)

corr.bip <- numeric(nfolds)
tot.bip <- numeric(nfolds)

for (i in 1:nfolds) {
  rf.tab <- table(CV.xgb[[i]][[1]], CV.xgb[[i]][[2]])
  tot.acc[i] <- sum(diag(rf.tab))/sum(rf.tab)
  corr.trip[i] <- rf.tab[2,2]
  tot.trip[i] <- sum(rf.tab[,2])
  corr.bip[i] <- rf.tab[1,1]
  tot.bip[i] <- sum(rf.tab[,1])
  print(rf.tab)
  print(rf.tab[1,])
  print(sum(rf.tab[1,]))
}

mean.acc <- mean(tot.acc)
sd.acc <- sd(tot.acc)
min.acc <- mean.acc - 1.96*(sd.acc/sqrt(nfolds))
max.acc <- mean.acc + 1.96*(sd.acc/sqrt(nfolds))
tot.data <- c('All data',mean.acc,sd.acc,min.acc, max.acc)

frac.corr.bip <- corr.bip/tot.bip
frac.corr.trip <- corr.trip/tot.trip

frac.corr.bip
frac.corr.trip

mean.bip <- mean(frac.corr.bip)
sd.bip <- sd(frac.corr.bip)
min.bip <- mean.bip - 1.96*(sd.bip/sqrt(nfolds))
max.bip <- mean.bip + 1.96*(sd.bip/sqrt(nfolds))
tot.bipart <- c('no_jc',mean.bip,sd.bip,min.bip, max.bip)

mean.trip <- mean(frac.corr.trip)
sd.trip <- sd(frac.corr.trip)
min.trip <- mean.trip - 1.96*(sd.trip/sqrt(nfolds))
max.trip <- mean.trip + 1.96*(sd.trip/sqrt(nfolds))
tot.tripart <- c('jc',mean.trip,sd.trip,min.trip, max.trip)

overall <- rbind(tot.data,tot.bipart,tot.tripart)
overall
overall <- data.frame(overall)
colnames(overall) <- c('data','means','sds','mins','maxs')
overall$means <- as.numeric(as.character(overall$means))
overall$sds <- as.numeric(as.character(overall$sds))
overall$mins <- as.numeric(as.character(overall$mins))
overall$maxs <- as.numeric(as.character(overall$maxs))

overall <- overall[2:3,]
overall

axis_labels <- c("????", "no-????")

p <- ggplot(overall, aes(x = data, y = means, ymin = mins, ymax = maxs)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_x_discrete(breaks=c("jc", "no_jc"),
                   labels=axis_labels) +
  labs(x = "Class", y = "Accuracy") +
  ggtitle("Figure 4.36. Correct predictions using XGBoost")

p

##############################################################################

# make stripchart with probabilities
df <- data.frame(CV.xgb[[4]][[2]],CV.xgb[[4]][[4]])
colnames(df) <- c('true_val', 'prob')
df$true_val <- as.factor(df$true_val)
ggp <- ggplot(df, aes(true_val,prob))+ geom_jitter(aes(colour = true_val))
ggp
#######################################################################

# make variable importance plot

library(ggplot2)

vals  <- data.frame(CV.xgb[[1]][[3]][,2])
name_list <- lapply(CV.xgb[[1]][[3]][,1], function(x){strsplit(x,':') })

chars <- NULL
for (i in 1:length(name_list$Feature)){
  chars[i] <- name_list$Feature[[i]]
}
df1 <- data.frame(chars, vals)

df1
########################

vals2 <- data.frame(CV.xgb[[2]][[3]][,2])
name_list2 <- lapply(CV.xgb[[2]][[3]][,1], function(x){strsplit(x,':') })

chars <- NULL
for (i in 1:length(name_list2$Feature)){
  chars[i] <- name_list2$Feature[[i]]
}
df2 <- data.frame(chars,vals2)

#########################################

vals3 <- data.frame(CV.xgb[[3]][[3]][,2])
name_list3 <- lapply(CV.xgb[[3]][[3]][,1], function(x){strsplit(x,':') })

chars <- NULL
for (i in 1:length(name_list3$Feature)){
  chars[i] <- name_list3$Feature[[i]]
}
df3 <- data.frame(chars,vals3)

###############################################################

vals4 <- data.frame(CV.xgb[[4]][[3]][,2])
name_list4 <- lapply(CV.xgb[[4]][[3]][,1], function(x){strsplit(x,':') })

chars <- NULL
for (i in 1:length(name_list4$Feature)){
  chars[i] <- name_list4$Feature[[i]]
}

df4 <- data.frame(chars,vals4)
####################################################

vals5 <- data.frame(CV.xgb[[5]][[3]][,2])
name_list5 <- lapply(CV.xgb[[5]][[3]][,1], function(x){strsplit(x,':') })

chars <- NULL
for (i in 1:length(name_list5$Feature)){
  chars[i] <- name_list5$Feature[[i]]
}

df5 <- data.frame(chars,vals5)
########################################################

new.df <- Reduce(function(x, y) merge(x, y, by='chars',all=TRUE), list(df1, df2, df3, df4,df5))
rownames(new.df) <- new.df$chars
new.df$chars <- NULL
new.df[is.na(new.df)] <- 0
means <- rowSums(new.df)/5
sds <- apply(new.df, 1, sd)

means.sds <- data.frame(means,sds)
#######################################################################
means.sds <- means.sds[order(-means.sds$means),]
means.sds$feature <- rownames(means.sds)
means.sds$feature <- factor(means.sds$feature, levels = unique(means.sds$feature))
means.sds$mins <- means.sds$means - 1.96*(means.sds$sds/sqrt(5))
means.sds$mins <- unlist(lapply(means.sds$mins, function(x){max(0,x)})) 
means.sds$maxs <- means.sds$means + 1.96*(means.sds$sds/sqrt(5)) 

p <- ggplot(means.sds, aes(x = feature, y = means, ymin = mins, ymax = maxs)) +
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1,vjust=-0.1)) +
  ggtitle("Figure 4.37. Variable importance in the XGBoost model")

p