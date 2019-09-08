library(tidyverse)
library(lattice)
library(randomForest)
library(xgboost)
library(Matrix)
library(pROC)


setwd("C:/Users/geitb/Documents/SynVar/Proefschrift/Tripartite_TF_scripts")

# import tripartite biblical data
dat.trip.b1 <- read.csv('tripartite_bib.csv', header = T)
head(dat.trip.b1)

dat.trip.b2 <- read.csv('tripartite_eppr_bib.csv', header = T)
dat.trip.b <- rbind(dat.trip.b1, dat.trip.b2)

head(dat.trip.b)
dim(dat.trip.b)

dat.trip.b <- dat.trip.b[dat.trip.b$language == 'Hebrew',]
dat.trip.b$cl_type <- 'tripartite'

dat.trip.b2 <- data.frame(dat.trip.b$cl_type, dat.trip.b$cl_id, dat.trip.b$book, dat.trip.b$book2,
                        dat.trip.b$chapter, dat.trip.b$verse, dat.trip.b$ebh_lbh, dat.trip.b$genre, 
                        dat.trip.b$txt_type, dat.trip.b$main_sub)

colnames(dat.trip.b2) <- c("cl_type", "cl_id", "book", "book2", "chapter", "verse", "ebh_lbh", "genre", "txt_type", "main_sub")
  
head(dat.trip.b2)
dim(dat.trip.b2)

###########################################################################
#import bipartite biblical data

dat.bip <- read.csv('C:/Users/geitb/Documents/SynVar/Proefschrift/HJH_NOM_scripts/hyh_nom_bib.csv')
dat.bip <- dat.bip[dat.bip$language == 'Hebrew', ]
head(dat.bip)

#remove hyh clause
dat.bip <- dat.bip[dat.bip$cl_type == 'nom',]
head(dat.bip)
dat.bip$cl_type <- 'bipartite'

#remove resumptive
dat.bip <- dat.bip[ which(dat.bip$clause_rela != 'Resu_'),]

#remove some other stuff, among which EPPr (threemembered)
dat.bip <- dat.bip[dat.bip$EPPr != 1,]
dat.bip <- dat.bip[dat.bip$Objc != 1,]
dat.bip <- dat.bip[dat.bip$Exst != 1,]
dat.bip <- dat.bip[dat.bip$pc_type != 'VP',]
dat.bip <- droplevels(dat.bip)
head(dat.bip)

dat.bip2 <- data.frame(dat.bip$cl_type, dat.bip$cl_id, dat.bip$book, dat.bip$book2,
                       dat.bip$chapter, dat.bip$verse, dat.bip$ebh_lbh, dat.bip$genre, dat.bip$txt_type
                       ,dat.bip$main_sub)

colnames(dat.bip2) <- c("cl_type", "cl_id", "book", "book2", "chapter", "verse", "ebh_lbh", "genre", "txt_type", "main_sub")

dim(dat.bip2)
###########################################################################################

# preprocess extrabiblical data
dat.trip.xb1 <- read.csv('tripartite_xbib.csv', header = T)
dat.trip.xb2 <- read.csv('tripartite_eppr_xbib.csv', header = T)

dat.trip.xb <- rbind(dat.trip.xb1, dat.trip.xb2)
dim(dat.trip.xb)
head(dat.trip.xb)

head(dat.trip.xb)
dat.trip.xb <- dat.trip.xb[dat.trip.xb$language == 'Hebrew',]
dim(dat.trip.xb)
dat.trip.xb$cl_type <- 'tripartite'

dat.trip.xb2 <- data.frame(dat.trip.xb$cl_type, dat.trip.xb$cl_id, dat.trip.xb$book, dat.trip.xb$book2,
                          dat.trip.xb$chapter, dat.trip.xb$verse, dat.trip.xb$ebh_lbh, dat.trip.xb$genre, 
                          dat.trip.xb$txt_type, dat.trip.xb$main_sub)

colnames(dat.trip.xb2) <- c("cl_type", "cl_id", "book", "book2", "chapter", "verse", "ebh_lbh", "genre", "txt_type", "main_sub")

head(dat.trip.xb2)

###########################################

#import bipartite extrabiblical data

dat.xbip <- read.csv('C:/Users/geitb/Documents/SynVar/Proefschrift/HJH_NOM_scripts/hyh_nom_xbib.csv')
dat.xbip <- dat.xbip[dat.xbip$language == 'Hebrew', ]
head(dat.xbip)
dim(dat.xbip)

#remove hyh clause
dat.xbip <- dat.xbip[dat.xbip$cl_type == 'nom',]
head(dat.xbip)
dat.xbip$cl_type <- 'bipartite'

#remove resumptive
dat.xbip <- dat.xbip[ which(dat.xbip$clause_rela != 'Resu_'),] # | dat.xbip$subj_type != 'PPrP'),]

#remove some other stuff, among which EPPr (threemembered)
dat.xbip <- dat.xbip[dat.xbip$EPPr != 1,]
dat.xbip <- dat.xbip[dat.xbip$Objc != 1,]
dat.xbip <- dat.xbip[dat.xbip$Exst != 1,]
dat.xbip <- dat.xbip[dat.xbip$pc_type != 'VP',]
dat.xbip <- droplevels(dat.xbip)
head(dat.xbip)

dat.xbip2 <- data.frame(dat.xbip$cl_type, dat.xbip$cl_id, dat.xbip$book, dat.xbip$book2,
                       dat.xbip$chapter, dat.xbip$verse, dat.xbip$ebh_lbh, dat.xbip$genre, dat.xbip$txt_type
                       ,dat.xbip$main_sub)

colnames(dat.xbip2) <- c("cl_type", "cl_id", "book", "book2", "chapter", "verse", "ebh_lbh", "genre", "txt_type", "main_sub")


#################################################################################

# merge biblical and extrabiblical datasets

dat.b <- rbind(dat.trip.b2, dat.bip2)
dat.b$subcorp <- 'biblical'

dat.xb <- rbind(dat.trip.xb2, dat.xbip2)
dat.xb$subcorp <- 'extrabiblical'

dat.t <- rbind(dat.b, dat.xb)

colnames(dat.t)
table(dat.t$cl_type, dat.t$subcorp)

# fix order of levels of column book
dat.t$book2 <- factor(dat.t$book2, levels = unique(dat.t$book2))

dat.t$book.ch <- paste(dat.t$book, dat.t$chapter, sep = '_')

# relabel genre of some verses
dat.t$genre <- as.character(dat.t$genre)
dat.t$genre[dat.t$book.ch == 'Genesis_49' & as.numeric(dat.t$verse) > 1 & as.numeric(dat.t$verse) < 28] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Exodus_15' & as.numeric(dat.t$verse) > 1 & as.numeric(dat.t$verse) < 19] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Numbers_6' & as.numeric(dat.t$verse) > 23 & as.numeric(dat.t$verse) < 27] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Numbers_21' & as.numeric(dat.t$verse) > 27 & as.numeric(dat.t$verse) < 31] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Numbers_23' & as.numeric(dat.t$verse) > 7 & as.numeric(dat.t$verse) < 11] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Numbers_23' & as.numeric(dat.t$verse) > 18 & as.numeric(dat.t$verse) < 25] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Numbers_24' & as.numeric(dat.t$verse) > 3 & as.numeric(dat.t$verse) < 10] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Numbers_24' & as.numeric(dat.t$verse) > 15 & as.numeric(dat.t$verse) < 25] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Deuteronomy_32' & as.numeric(dat.t$verse) < 44] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Deuteronomy_33' & as.numeric(dat.t$verse) > 2] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Judges_5' & as.numeric(dat.t$verse) > 1 & as.numeric(dat.t$verse) < 31] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Judges_9' & as.numeric(dat.t$verse) > 7 & as.numeric(dat.t$verse) < 16] <- 'poetry'
dat.t$genre[dat.t$book.ch == '1_Samuel_2' & as.numeric(dat.t$verse) < 11] <- 'poetry'
dat.t$genre[dat.t$book.ch == '2_Samuel_22' & as.numeric(dat.t$verse) > 2] <- 'poetry'
dat.t$genre[dat.t$book.ch == '2_Samuel_23' & as.numeric(dat.t$verse) > 2 & as.numeric(dat.t$verse) < 8] <- 'poetry'
dat.t$genre[dat.t$book.ch == '2_Samuel_1' & as.numeric(dat.t$verse) > 18 & as.numeric(dat.t$verse) < 28] <- 'poetry'

dat.t$genre[dat.t$book.ch == 'Jonah_2' & as.numeric(dat.t$verse) > 3 & as.numeric(dat.t$verse) < 11] <- 'poetry'

dat.t$genre[dat.t$book.ch == 'Daniel_2' & as.numeric(dat.t$verse) > 19 & as.numeric(dat.t$verse) < 24] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Daniel_8' & as.numeric(dat.t$verse) > 22 & as.numeric(dat.t$verse) < 27] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Daniel_12' & as.numeric(dat.t$verse) < 4] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Nehemiah_9' & as.numeric(dat.t$verse) > 5 & as.numeric(dat.t$verse) < 38] <- 'poetry'
dat.t$genre[dat.t$book.ch == '1_Chronicles_16' & as.numeric(dat.t$verse) > 7 & as.numeric(dat.t$verse) < 37] <- 'poetry'
dat.t$genre[dat.t$book.ch == 'Job_1' & as.numeric(dat.t$verse) < 21] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Job_2' & as.numeric(dat.t$verse) < 14] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Job_42' & as.numeric(dat.t$verse) > 6 & as.numeric(dat.t$verse) < 18] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Isaiah_36' & as.numeric(dat.t$verse) < 23] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Isaiah_37' & as.numeric(dat.t$verse) < 22] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Isaiah_38' & as.numeric(dat.t$verse) < 10] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Isaiah_38' & as.numeric(dat.t$verse) > 20] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Isaiah_39'] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Jeremiah_52'] <- 'prose'
dat.t$genre[dat.t$book.ch == 'Jeremiah_39' | dat.t$book.ch == 'Jeremiah_40'] <- 'prose'

dat.t$genre[dat.t$book == 'Daniel' &  as.numeric(dat.t$chapter) > 6  & as.numeric(dat.t$chapter) < 13 ] <- 'prophecy' 

dat.t$genre <- as.factor(dat.t$genre)

dat.t$verse <- as.numeric(as.character(dat.t$verse))

dat.t$genre <- as.factor(dat.t$genre)
dat.t$main_sub <- as.factor(dat.t$main_sub)

dim(dat.t)

############################################################################

dat.t <- droplevels(dat.t)
table(dat.t$cl_type, dat.t$subcorp)

#######################################################################

# make explorative mosaic plots

ebh_lbh <- table(dat.t$ebh_lbh, dat.t$cl_type)
mosaicplot(ebh_lbh, main = 'Figure 4.38. Language phase and clause type', cex.axis = 1.2, color = c('#E6B0AA', '#5DADE2'), las = 2)

#############################################################################################

main_sub <- table(dat.t$main_sub, dat.t$cl_type)
mosaicplot(main_sub, main = 'Figure 4.39. Main and subordinate clauses and clause type', cex.axis = 1.2, color = c('#E6B0AA', '#5DADE2'), las = 2)

###############################################

txt_type <- table(dat.t$txt_type, dat.t$cl_type)
mosaicplot(txt_type, main = 'Figure 4.40. Discourse type and clause type', cex.axis = 1.2, color = c('#E6B0AA', '#5DADE2'), las = 2)

######################################################################

genre <- table(dat.t$genre, dat.t$cl_type)
mosaicplot(genre, main = 'Figure 4.41. Genre and clause type', cex.axis = 1.2, color = c('#E6B0AA', '#5DADE2'), las = 2)

#########################################################################


# Random Forest with naive oversampling
dat.bi.tri <- dat.t

nfolds = 5

set.seed(3)
folds <- sample(rep(1:nfolds, length.out = nrow(dat.bi.tri)), size = nrow(dat.bi.tri), replace = F)
head(dat.bi.tri)
table(dat.bi.tri$cl_type)

# use ntree = 1000 and mtry = 4
CV.rf <- lapply(1:nfolds, function(x){ 
  
  data.bi.tri <- dat.bi.tri[folds != x,]

  # make oversampled training set
  data.bip <- data.bi.tri[data.bi.tri$cl_type=='bipartite',]
  data.trip <- data.bi.tri[data.bi.tri$cl_type=='tripartite',]
  
  data.trip.oversamp <- data.trip[sample(1:nrow(data.trip),nrow(data.bip),replace = T),]
  
  data.oversamp <- rbind(data.trip.oversamp,data.bip)
  
  # train RF model
  rf.model <- randomForest(cl_type ~ ebh_lbh+main_sub+genre+txt_type, ntree=1000, mtry=4, data = data.oversamp)
  # predict on test set
  preds <- predict(rf.model,  dat.bi.tri[folds == x,], type="prob")
  
  # choose cut off at 0.95 
  preds10 <- ifelse(preds[,1] > 0.2, 'tripartite', 'bipartite')
  return(list(preds10, true.vals = dat.bi.tri$cl_type[folds == x], importance(rf.model), preds[,2]))
  
})

for (i in 1:nfolds){ 
  print(table(CV.rf[[i]][[1]],CV.rf[[i]][[2]]))
}


############################################################################

tot.acc <- numeric(nfolds)

corr.trip <- numeric(nfolds)
tot.trip <- numeric(nfolds)

corr.bip <- numeric(nfolds)
tot.bip <- numeric(nfolds)

for (i in 1:nfolds) {
  rf.tab <- table(CV.rf[[i]][[1]], CV.rf[[i]][[2]])
  tot.acc[i] <- sum(diag(rf.tab))/sum(rf.tab)
  corr.trip[i] <- rf.tab[2,1]
  tot.trip[i] <- sum(rf.tab[,1])
  corr.bip[i] <- rf.tab[1,2]
  tot.bip[i] <- sum(rf.tab[,2])
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
tot.bipart <- c('bipartite',mean.bip,sd.bip,min.bip, max.bip)

mean.trip <- mean(frac.corr.trip)
sd.trip <- sd(frac.corr.trip)
min.trip <- mean.trip - 1.96*(sd.trip/sqrt(nfolds))
max.trip <- mean.trip + 1.96*(sd.trip/sqrt(nfolds))
tot.tripart <- c('tripartite',mean.trip,sd.trip,min.trip, max.trip)

overall <- rbind(tot.bipart,tot.tripart)
overall <- data.frame(overall)
colnames(overall) <- c('data','means','sds','mins','maxs')
overall$means <- as.numeric(as.character(overall$means))
overall$sds <- as.numeric(as.character(overall$sds))
overall$mins <- as.numeric(as.character(overall$mins))
overall$maxs <- as.numeric(as.character(overall$maxs))

p <- ggplot(overall, aes(x = data, y = means, ymin = mins, ymax = maxs)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  labs(x = "Class", y = "Mean accuracy (with confidence interval)") +
  ggtitle("")

p

######################################################

# calculate ROC's

tot.acc <- numeric(nfolds)

corr.trip <- numeric(nfolds)
tot.trip <- numeric(nfolds)

corr.bip <- numeric(nfolds)
tot.bip <- numeric(nfolds)

tot.bip
tot.trip

rocs <- list()

for (i in 1:nfolds) {
  rf.tab <- table(CV.rf[[i]][[2]], CV.rf[[i]][[1]])
  tot.acc[i] <- sum(diag(rf.tab))/sum(rf.tab)
  corr.trip[i] <- rf.tab[1,1]
  tot.trip[i] <- sum(rf.tab[1,])
  corr.bip[i] <- rf.tab[2,2]
  tot.bip[i] <- sum(rf.tab[2,])
  
  print(rf.tab)
  print(rf.tab[1,])
  print(sum(rf.tab[1,]))
  true.v <- CV.rf[[i]][[2]]
  
  true.v.num <- ifelse(true.v == 'tripartite', 1,0)
  pred.v <- CV.rf[[i]][[1]]
  pred.v.num <- ifelse(pred.v == 'tripartite', 1,0)  
  
  rocs[[i]] <- roc(true.v.num, CV.rf[[i]][[4]])
}
rocs


###############################################################

# make roc plot

plot(rocs[[1]], lty=3,add=FALSE,asp=NA,main='')
for (i in 2:nfolds) {
  lines(rocs[[i]], lty=3)
}

################################################################

# calculate variable importance and make plot

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
  ggtitle("") +
  ylab("Average importance (with confidence interval)") +
  scale_x_discrete(labels=c("ebh_lbh" = "Language phase", "main_sub" = "Main/subordinate",
                            "txt_type" = "Discourse", "genre" = "Genre"))

  
p

###################################################################################
###################################################################################

# do XGBoost

head(dat.bi.tri)
dat.bi.tri <- dat.bi.tri[, c("cl_type", "ebh_lbh", "genre", "txt_type", "main_sub")]

dat.bi.tri <- dat.bi.tri[sample(1:nrow(dat.bi.tri)),]
dat11 <- dat.bi.tri

table(dat.bi.tri$cl_type)

lab <- ifelse(dat11$cl_type == 'tripartite', 1, 0)

sp.m <- sparse.model.matrix(cl_type ~ .-1, data = dat11)

nfolds = 5

set.seed(3)
folds <- sample(rep(1:nfolds, length.out = nrow(sp.m)), size = nrow(sp.m), replace = F)

CV.xgb <- lapply(1:nfolds, function(x){
  
  fold_x <- which(folds != x)
  train_bip <- which(lab[fold_x] == 0)
  train_trip <- which(lab[fold_x] == 1)
  
  bip <- sp.m[fold_x,][train_bip,]
  trip <- sp.m[fold_x,][train_trip,]
  
  samp_ind <- sample(1:nrow(trip), nrow(bip), replace = T)
  trip2 <- trip[samp_ind,]
  train <- rbind(bip, trip2)
  label_train <- c(rep(0, nrow(bip)), rep(1, nrow(trip2)))
  
  model_xg <- xgboost(data = train, label = label_train, eta = 0.1, maxdepth=3, nrounds=50 , objective='binary:logistic')
  preds <- predict(model_xg,  sp.m[folds == x,], type='response')
  preds.resp <- ifelse(preds > 0.49, 1, 0)
  
  importance <- xgb.importance(colnames(train), model_xg)
  return(list(preds=preds.resp, true.vals = lab[folds == x], importance, preds, model_xg))
})

for (i in 1:nfolds){ 
  print(table(CV.xgb[[i]][[1]],CV.xgb[[i]][[2]]))
}

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
tot.bipart <- c('Bipartite clauses',mean.bip,sd.bip,min.bip, max.bip)

mean.trip <- mean(frac.corr.trip)
sd.trip <- sd(frac.corr.trip)
min.trip <- mean.trip - 1.96*(sd.trip/sqrt(nfolds))
max.trip <- mean.trip + 1.96*(sd.trip/sqrt(nfolds))
tot.tripart <- c('Tripartite clauses',mean.trip,sd.trip,min.trip, max.trip)

overall <- rbind(tot.data,tot.bipart,tot.tripart)
overall <- data.frame(overall)
colnames(overall) <- c('data','means','sds','mins','maxs')
overall$means <- as.numeric(as.character(overall$means))
overall$sds <- as.numeric(as.character(overall$sds))
overall$mins <- as.numeric(as.character(overall$mins))
overall$maxs <- as.numeric(as.character(overall$maxs))

overall <- overall[2:3,]
overall
p <- ggplot(overall, aes(x = data, y = means, ymin = mins, ymax = maxs)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar() + 
  labs(x = "") +
  ggtitle("") +
  ylab("Average accuracy (with confidence interval)")

p

##############################################################################################################

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
  labs(y = "Average importance (with confidence interval") +
  ggtitle("") +
  scale_x_discrete(labels=c("txt_typeQ" = "Discourse_Q", 
                              "main_sub_SubArg" = "Subordinate_Arg",
                              "ebh_lbhlbh" = "Phase_LBH",
                              "main_subSubMod" = "Sub_Mod",
                              "genreprophecy" = "Genre_Prophecy",
                              "main_subSubAdv" = "Sub_Av",
                              "ebh_lbhrabbinic" = "Phase_Rabbinic",
                              "genreprose" = "Genre_Prose",
                              "ebh_lbhebh" = "Phase_EBH",
                              "ebh_lbhother" = "Phase_Other",
                              "ebh_lbhqumranic" = "Phase_Qumranic",
                              "txt_typeD" = "Discourse_D",
                              "txt_typeN" = "Discourse_N",
                              "ebh_lbhepigraphic" = "Phase_Epigraphic"
                              ))


p

#########################################################################################################

# ROC of XGB

tot.acc <- numeric(nfolds)

corr.trip <- numeric(nfolds)
tot.trip <- numeric(nfolds)

corr.bip <- numeric(nfolds)
tot.bip <- numeric(nfolds)

tot.bip
tot.trip

rocs <- list()

for (i in 1:nfolds) {
  xgb.tab <- table(CV.xgb[[i]][[2]], CV.xgb[[i]][[1]])
  tot.acc[i] <- sum(diag(xgb.tab))/sum(xgb.tab)
  corr.trip[i] <- xgb.tab[1,1]
  tot.trip[i] <- sum(xgb.tab[1,])
  corr.bip[i] <- xgb.tab[2,2]
  tot.bip[i] <- sum(xgb.tab[2,])
  print(xgb.tab)
  print(xgb.tab[1,])
  print(sum(xgb.tab[1,]))
  
  true.v <- CV.xgb[[i]][[2]]
  print(true.v)
  
  rocs[[i]] <- roc(true.v, CV.xgb[[i]][[4]])
}
rocs

CV.xgb[[1]][[4]]
###############################################################

# make roc plot

plot(rocs[[1]], lty=3,add=FALSE,asp=NA,main='')
for (i in 2:nfolds) {
  lines(rocs[[i]], lty=3)
}

########################################################################

# stripchart with probabilities
df <- data.frame(CV.xgb[[3]][[2]],CV.xgb[[3]][[4]])
colnames(df) <- c('true_val', 'prob')
df$true_val <- as.factor(df$true_val)
ggp <- ggplot(df, aes(true_val,prob))+ geom_jitter(aes(colour = true_val))
ggp






