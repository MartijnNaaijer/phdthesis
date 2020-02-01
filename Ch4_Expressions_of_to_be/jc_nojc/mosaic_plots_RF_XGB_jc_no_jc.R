library(randomForest)
library(vcd)
library(plotly)
library(pROC)
library(xgboost)
library(Matrix)

# import data
setwd("C:/Users/geitb/Documents/SynVar/Proefschrift/JC_AJN")

# biblical data
dat <- read.csv("jc_nojc_bib.csv")
# extrabiblical data
datx <- read.csv("jc_nojc_xbib.csv")
dath <- rbind(dat, datx)
# select hebrew data
dath <- dath[dath$language %in% c('Hebrew','hbo'),]

#remove PP-subject
dath <- dath[dath$subj_type != 'PP',]
head(dath)
dim(dath)
table(dath$cl_type)

# select relevant columns
colnames(dath)
selected_cols <- c("cl_type", "cl_id", "book", "chapter", "verse", "ebh_lbh", "genre",
                   "txt_type", "s_p_order", "CP", "cl_len", "subj_len", "pc_len",
                   "main_sub", "Loca", "Ques", "Rela", "Time", "mother")
dat.jc.nojc <- dath[, selected_cols]


dat.jc.nojc$ebh_lbh <- factor(dat.jc.nojc$ebh_lbh)
dat.jc.nojc$book.ch <- paste(dat.jc.nojc$book, dat.jc.nojc$chapter, sep = '_')
dat.jc.nojc$ebh_lbh <- as.factor(dat.jc.nojc$ebh_lbh)
dat.jc.nojc$genre <- as.character(dat.jc.nojc$genre)

# import adapt_genre(), file can be found in folder "various"
source("adapt_genre.R")

# adapt genre for specific verses
dat.jc.nojc <- adapt_genre(dat.jc.nojc)
dat.jc.nojc$genre <- as.factor(dat.jc.nojc$genre)

################################################################################

# make explorative plots

# language phase
phase <- table(dat.jc.nojc$ebh_lbh, dat.jc.nojc$cl_type)
colnames(phase) <- c('JC', 'no-JC')
mosaicplot(phase, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# main and subordinate clauses
m.s <- table(dat.jc.nojc$main_sub, dat.jc.nojc$cl_type)
colnames(m.s) <- c('JC', 'no-JC')
mosaicplot(m.s, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# discourse
genre <- table(dat.jc.nojc$txt_type, dat.jc.nojc$cl_type)
colnames(genre) <- c('JC', 'no-JC')
mosaicplot(genre, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# genre
genre <- table(dat.jc.nojc$genre, dat.jc.nojc$cl_type)
colnames(genre) <- c('JC', 'no-JC')
mosaicplot(genre, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# make spineplot of clause length versus clause type
len_table <- table(dat.jc.nojc$cl_len, dat.jc.nojc$cl_type)
colnames(len_table) <- c('JC', 'no-JC')
spineplot(len_table, main = '')

# make spineplot of subject length versus clause type
len_table <- table(dat.jc.nojc$subj_len, dat.jc.nojc$cl_type)
colnames(len_table) <- c('JC', 'no-JC')
spineplot(len_table, main = '')

# genre and discourse together
mosaic(structable(dat.jc.nojc$genre, dat.jc.nojc$txt_type, dat.jc.nojc$cl_type), rot_labels = c(right = -45))

mother_table <- table(dat.jc.nojc$mother, dat.jc.nojc$cl_type)
colnames(mother_table) <- c('JC', 'no-JC')
mother_table <- mother_table[1:10,]
mosaicplot(mother_table, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

################################################################################################################

# do RF with 5-fold CV

head(dat.jc.nojc)
table(dat.jc.nojc$ebh_lbh, dat.jc.nojc$cl_type)

nfolds = 5

set.seed(4)
folds <- sample(rep(1:nfolds, length.out = nrow(dat.jc.nojc)), size = nrow(dat.jc.nojc), replace = F)

CV.rf <- lapply(1:nfolds, function(x){ 
  
  # split in train/test
  dat.train <- dat.jc.nojc[folds != x,]
  dat.test <- dat.jc.nojc[folds == x,]
  print(length(dat.test$cl_type))
  
  # do oversampling on train data
  dat.jc.train <- dat.train[dat.train$cl_type=='jc',]
  dat.nojc.train <- dat.train[dat.train$cl_type=='no_jc',]
  
  set.seed(5)
  dat.jc.oversamp <- dat.jc.train[sample(1:nrow(dat.jc.train),nrow(dat.nojc.train),replace = T),]
  
  dat.oversamp <- rbind(dat.jc.oversamp, dat.nojc.train)
  
  # train the  on selection of variables that contribute positively to prediction accuracy
  # hyperparameters: ntree=5000, mtry=6
  rf.model <- randomForest(cl_type ~ mother+main_sub+CP+ebh_lbh+genre+s_p_order+cl_len+txt_type+Ques,ntree=5000,mtry=6,data = dat.oversamp)
  
  ########################################
  
  # make predictions
  preds <- predict(rf.model,  dat.test, type="prob")
  # cut-off point is 0.05, if prob > 0.05, prediction is "jc", else "no-jc"
  preds.resp <- ifelse(preds[,1] > 0.05, "jc", "no_jc")
  
  #print(preds)
  print(table(preds.resp, dat.test$cl_type))
  result_tab <- table(preds.resp, dat.test$cl_type)
  
  precision <- result_tab[1,1]/sum(result_tab[1,1:2])
  print(paste("precision", precision))
  recall <- result_tab[1,1]/sum(result_tab[1:2,1])
  print(paste("recall", recall))
  f_score <- 2 * precision * recall /(precision + recall)
  print(paste("f", f_score))
  return(list(preds.resp, true.vals = dat.test$cl_type, importance(rf.model), preds[,2], rf.model, dat.oversamp, precision=precision, recall=recall, f_score=f_score))
  
})

########################################################################

# calculate ROC 

tot.acc <- numeric(nfolds)

corr.jc <- numeric(nfolds)
tot.jc <- numeric(nfolds)

corr.nojc <- numeric(nfolds)
tot.nojc <- numeric(nfolds)

rocs <- list()

for (i in 1:nfolds) {
  rf.tab <- table(CV.rf[[i]][[2]], CV.rf[[i]][[1]])
  tot.acc[i] <- sum(diag(rf.tab))/sum(rf.tab)
  corr.jc[i] <- rf.tab[1,1]
  tot.jc[i] <- sum(rf.tab[1,])
  corr.nojc[i] <- rf.tab[2,2]
  tot.nojc[i] <- sum(rf.tab[2,])
  print(rf.tab)
  print(rf.tab[1,])
  print(sum(rf.tab[1,]))

  true.v <- CV.rf[[i]][[2]]
  true.v.num <- ifelse(true.v == 'jc', 1,0)
  pred.v <- CV.rf[[i]][[1]]
  pred.v.num <- ifelse(pred.v == 'jc', 1,0)  
  
  rocs[[i]] <- roc(true.v.num, CV.rf[[i]][[4]])
}
rocs

###############################################################

# make ROC plot

plot(rocs[[1]], lty=3,add=FALSE,asp=NA,main='')
for (i in 2:nfolds) {
  lines(rocs[[i]], lty=3)
}

###############################################################

# precision

all_precisions <- numeric(5)

for (i in 1:nfolds) {
  print(CV.rf[[i]]["precision"][[1]])
  all_precisions[i] <- CV.rf[[i]]["precision"][[1]]
}

average_precision <- mean(all_precisions)
average_precision

################################################################

# recall

all_recalls <- numeric(5)

for (i in 1:nfolds) {
  print(CV.rf[[i]]["recall"])
  all_recalls[i] <- CV.rf[[i]]["recall"][[1]]
}

average_recall <- mean(all_recalls)
average_recall

################################################################

# F-score

for (i in 1:nfolds) {
  print(CV.rf[[i]]["f_score"])
}

################################################################

# make plot of average variable importance with confidence interval

all_imports <- list()

for (i in 1:nfolds) {
  import <- as.data.frame(CV.rf[[i]][[3]])
  sort_import <- import[ order(row.names(import)), ]
  print(sort_import)
  all_imports[[i]] <- sort_import
  
}

all_imports_df <- as.data.frame(do.call(cbind, all_imports))
row.names(all_imports_df) <- row.names(import)[order(row.names(import))]

means <- apply(all_imports_df, 1, mean)
sds <- apply(all_imports_df, 1, sd)
mins <- means - 1.96*(sds/sqrt(5))
maxs <- means + 1.96*(sds/sqrt(5)) 

means.sds <- cbind(means, sds, mins, maxs)
means.sds <- as.data.frame(means.sds)
means.sds$feature <- row.names(means.sds)

means.sds <- means.sds[order(-means.sds$means),]

means.sds$feature <- factor(means.sds$feature, levels = means.sds$feature)
p <- ggplot(means.sds, aes(x = feature, y = means, ymin = mins, ymax = maxs)) +
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1,vjust=-0.1)) +
  ggtitle("")
p

####################################################################

# make plot with accuracy for both classes

mean.acc <- mean(tot.acc)
sd.acc <- sd(tot.acc)
min.acc <- mean.acc - 1.96*(sd.acc/sqrt(nfolds))
max.acc <- mean.acc + 1.96*(sd.acc/sqrt(nfolds))
tot.data <- c('All data',mean.acc,sd.acc,min.acc, max.acc)

frac.corr.nojc <- corr.nojc/tot.nojc
frac.corr.jc <- corr.jc/tot.jc

mean.nojc <- mean(frac.corr.nojc)
sd.nojc <- sd(frac.corr.nojc)
min.nojc <- mean.nojc - 1.96*(sd.nojc/sqrt(nfolds))
max.nojc <- mean.nojc + 1.96*(sd.nojc/sqrt(nfolds))
tot.nojc <- c('no_jc',mean.nojc,sd.nojc,min.nojc, max.nojc)

mean.jc <- mean(frac.corr.jc)
sd.jc <- sd(frac.corr.jc)
min.jc <- mean.jc - 1.96*(sd.jc/sqrt(nfolds))
max.jc <- mean.jc + 1.96*(sd.jc/sqrt(nfolds))
tot.jc <- c('jc',mean.jc,sd.jc,min.jc, max.jc)

overall <- rbind(tot.nojc,tot.jc)
overall <- data.frame(overall)
colnames(overall) <- c('data','means','sds','mins','maxs')

overall$means <- as.numeric(as.character(overall$means))
overall$sds <- as.numeric(as.character(overall$sds))
overall$mins <- as.numeric(as.character(overall$mins))
overall$maxs <- as.numeric(as.character(overall$maxs))
overall

axis_labels <- c("JC", "no-JC")

p <- ggplot(overall, aes(x = data, y = means, ymin = mins, ymax = maxs)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_x_discrete(breaks=c("jc", "no_jc"),
                  labels=axis_labels) +
  labs(x = "Class", y = "Accuracy") +
  ggtitle("")

p

###################################################################################
####################################################################################

# do analysis with XGBoost
colnames(dat.jc.nojc)

xgb_cols <- c("cl_type", "ebh_lbh", "genre", "txt_type"  ,"s_p_order" ,"CP", 
              "cl_len", "subj_len", "main_sub", 
              "Ques", "mother") 
dat.xgb <- dat.jc.nojc[,xgb_cols]

# creation of lab (label or output) and sp.m (the predictors)
lab <- ifelse(dat.xgb$cl_type == 'jc', 1, 0)
sp.m <- sparse.model.matrix(cl_type ~ .-1, data = dat.xgb)
head(sp.m)

# 5-fold CV is applied
nfolds = 5

set.seed(3)
folds <- sample(rep(1:nfolds, length.out = nrow(sp.m)), size = nrow(sp.m), replace = F)

CV.xgb <- lapply(1:nfolds, function(x){ 
  
  fold_x <- which(folds != x)
  train_nojc <- which(lab[fold_x] == 0)
  train_jc <- which(lab[fold_x] == 1)
  
  # apply oversampling
  nojc <- sp.m[fold_x,][train_nojc,]
  jc <- sp.m[fold_x,][train_jc,]
  samp_ind <- sample(1:nrow(jc), nrow(nojc), replace = T)
  jc2 <- jc[samp_ind,]
  train <- rbind(nojc, jc2)
  label_train <- c(rep(0, nrow(nojc)), rep(1, nrow(jc2)))
  
  # train the xgb model
  model_xg <- xgboost(data = train, eta = 0.1, label = label_train, maxdepth=4, nrounds=250, objective='binary:logistic')
  
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

corr.jc <- numeric(nfolds)
tot.jc <- numeric(nfolds)

corr.nojc <- numeric(nfolds)
tot.nojc <- numeric(nfolds)

rocs <- list()

for (i in 1:nfolds) {
  xgb.tab <- table(CV.xgb[[i]][[2]], CV.xgb[[i]][[1]])
  tot.acc[i] <- sum(diag(xgb.tab))/sum(xgb.tab)
  corr.jc[i] <- xgb.tab[1,1]
  tot.jc[i] <- sum(xgb.tab[1,])
  corr.nojc[i] <- xgb.tab[2,2]
  tot.nojc[i] <- sum(xgb.tab[2,])
  print(xgb.tab)
  print(xgb.tab[1,])
  print(sum(xgb.tab[1,]))
  true.v <- CV.xgb[[i]][[2]]
  print(true.v)
  true.v.num <- ifelse(true.v == 'jc', 1,0)
  pred.v <- CV.xgb[[i]][[1]]
  pred.v.num <- ifelse(pred.v == 'jc', 1,0)
  print(roc(true.v, CV.xgb[[i]][[4]]), auc=T)
  rocs[[i]] <- roc(true.v, CV.xgb[[i]][[4]])
}

###############################################################

# make roc plot

plot(rocs[[1]], lty=3,add=FALSE,asp=NA,main='')
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

corr.jc <- numeric(nfolds)
tot.jc <- numeric(nfolds)

corr.nojc <- numeric(nfolds)
tot.nojc <- numeric(nfolds)

for (i in 1:nfolds) {
  xgb.tab <- table(CV.xgb[[i]][[1]], CV.xgb[[i]][[2]])
  tot.acc[i] <- sum(diag(xgb.tab))/sum(xgb.tab)
  corr.jc[i] <- xgb.tab[2,2]
  tot.jc[i] <- sum(xgb.tab[,2])
  corr.nojc[i] <- xgb.tab[1,1]
  tot.nojc[i] <- sum(xgb.tab[,1])
  print(xgb.tab)
  print(xgb.tab[1,])
  print(sum(xgb.tab[1,]))
}

mean.acc <- mean(tot.acc)
sd.acc <- sd(tot.acc)
min.acc <- mean.acc - 1.96*(sd.acc/sqrt(nfolds))
max.acc <- mean.acc + 1.96*(sd.acc/sqrt(nfolds))
tot.data <- c('All data',mean.acc,sd.acc,min.acc, max.acc)

frac.corr.nojc <- corr.nojc/tot.nojc
frac.corr.jc <- corr.jc/tot.jc

frac.corr.nojc
frac.corr.jc

mean.nojc <- mean(frac.corr.nojc)
sd.nojc <- sd(frac.corr.nojc)
min.nojc <- mean.nojc - 1.96*(sd.nojc/sqrt(nfolds))
max.nojc <- mean.nojc + 1.96*(sd.nojc/sqrt(nfolds))
tot.nojc <- c('no_jc',mean.nojc,sd.nojc,min.nojc, max.nojc)

mean.jc <- mean(frac.corr.jc)
sd.jc <- sd(frac.corr.jc)
min.jc <- mean.jc - 1.96*(sd.jc/sqrt(nfolds))
max.jc <- mean.jc + 1.96*(sd.jc/sqrt(nfolds))
tot.jc <- c('jc',mean.jc,sd.jc,min.jc, max.jc)

overall <- rbind(tot.data,tot.nojc,tot.jc)
overall
overall <- data.frame(overall)
colnames(overall) <- c('data','means','sds','mins','maxs')
overall$means <- as.numeric(as.character(overall$means))
overall$sds <- as.numeric(as.character(overall$sds))
overall$mins <- as.numeric(as.character(overall$mins))
overall$maxs <- as.numeric(as.character(overall$maxs))

overall <- overall[2:3,]
overall

axis_labels <- c('????', 'no-????')

p <- ggplot(overall, aes(x = data, y = means, ymin = mins, ymax = maxs)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  scale_x_discrete(breaks=c("jc", "no_jc"),
                   labels=axis_labels) +
  labs(x = "Class", y = "Accuracy") +
  ggtitle("")

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
  ggtitle("") +
  ylab("Average importance (with confidence interval)")  +
  scale_x_discrete(labels=c("subj_len"="Subject length",
                            "txt_typeQ" = "Discourse Q",
                            "pc_len"="PreC length",
                            "cl_len"="Clause length",
                            "s_p_orderSP"="Order Subj-PreC",
                            "mothernominal"="Mother verbless",
                            "CPKJ"="Conj KJ",
                            "ebh_lbhrabbinic" = "Phase Rabbinic", 
                            "genreprose" = "Genre Prose",
                            "genreprophecy" = "Genre prophecy",
                            "CPW"="Conj W",
                            "motherperf"="Mother Perfect",
                            "main_subSubAdv"="Subordinate adverbial",
                            "motherwayq"="Mother wayyiqtol",
                            "CPno_conj"="No conj",
                            "ebh_lbhother"="Phase other",
                            "ebh_lbhebh"="Phase EBH",
                            "Ques"="Question phrase",
                            "main_subSubArg" = "Subordinate Argument",
                            "txt_typeN"="Discourse N",
                            "ebh_lbhlbh"="Phase LBH",
                            "motherimpv"="Mother imperative",
                            "main_subMod" = "Subordinate Modifier",
                            "motherno_pred" = "Mother no-pred",
                            "ebh_lbhqh"="Phase Qumran",
                            "motherptcp" ="Mother part pass",
                            "motherptca"="Mother part act",
                            "CP>M"="Conj >M",
                            "motherinfc"="Mother InfC",
                            "CPC"= "Conj C",
                            "txt_typeD"="Discourse D",
                            "CPPN"="Conj PN",
                            "CPLW"="CP LW",
                            "motherno_mother"="No mother"
  ))

p



