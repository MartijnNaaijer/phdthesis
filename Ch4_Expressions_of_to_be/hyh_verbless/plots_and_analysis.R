# 1. Import the relevant packages, the datasets and do some preprocessing

library(mgcv)
library(itsadug)
library(lattice)

setwd("C:/Users/geitb/Documents/SynVar/Proefschrift/HJH_NOM_scripts")

################################################################

#import extrabiblical

dat.ex.bib <- read.csv('hyh_nom_xbib.csv', header = TRUE)
head(dat.ex.bib)
dat.ex.bib <- dat.ex.bib[dat.ex.bib$language == 'Hebrew',]
dat.ex.bib$subcorp <- ifelse(dat.ex.bib$book2 %in% c('1QM', '1QH','1QS'), 'Qumran Hebrew', 'Epigraphic Hebrew')
dat.ex.bib$subcorp[dat.ex.bib$book2 %in% c('Pirqe', 'Shirata')] <- 'Rabbinic Hebrew'
table(dat.ex.bib$cl_type, dat.ex.bib$ebh_lbh)

###########################################################################

#import biblical texts

dat <- read.csv('hyh_nom_bib.csv', header = TRUE)
dat <- dat[dat$language == 'Hebrew', ]
dat$subcorp <- "Biblical Hebrew"

dat.t <- rbind(dat, dat.ex.bib)

dat.t$book.ch <- paste(dat.t$book, dat.t$chapter, sep = '_')

dat.t$genre <- as.character(dat.t$genre)

# import adapt_genre(), file can be found in folder "various"
source("adapt_genre.R")

# adapt genre for specific verses
dat.t <- adapt_genre(dat.t)

dat.t$genre <- as.factor(dat.t$genre)

dat.t <- dat.t[dat.t$language %in% c('hbo', 'Hebrew'), ]
dat.t$verse <- as.numeric(as.character(dat.t$verse))
dat.t <- dat.t[dat.t$pc_type != 'VP',]

# relevel variables
dat.t$genre <- relevel(dat.t$genre, ref = 'prose')
dat.t$main_sub <- relevel(dat.t$main_sub, ref = 'Main')
dat.t$txt_type <- relevel(dat.t$txt_type, ref = 'N')
dat.t$mother <- relevel(dat.t$mother, ref = 'nominal')

dat.t$genre <- as.factor(dat.t$genre)
dat.t$main_sub <- as.factor(dat.t$main_sub)
dat.t$subj_type_det <- paste(dat.t$subj_type, dat.t$subj_det, sep = '.')
dat.t$pc_type_det <- paste(dat.t$pc_type, dat.t$pc_det, sep = '.')
dat.t$subj_type_det <- as.factor(dat.t$subj_type_det)
dat.t$pc_type_det <- as.factor(dat.t$pc_type_det)

dat.t$pc_type <- relevel(dat.t$pc_type, ref = 'PP')
dat.t$pc_type <- as.factor(dat.t$pc_type)

dat.t$subj_type <- relevel(dat.t$subj_type, ref = 'NP')

dat.t$subj_type_det <- relevel(dat.t$subj_type_det, ref = 'NP.und')
dat.t$pc_type_det <- relevel(dat.t$pc_type_det, ref = 'PP.det')
dat.t$ebh_lbh <- relevel(dat.t$ebh_lbh, ref = 'ebh')
dat.t$genre <- relevel(dat.t$genre, ref = 'prose')
dat.t$nega <- relevel(dat.t$nega, ref = 'non_neg')
dat.t$s_p_order <- relevel(dat.t$s_p_order, ref = 'SP')

dat.t <- dat.t[order(dat.t$cl_id),]

# merge language phase and genre
dat.t$phase_genre <- paste(dat.t$ebh_lbh, dat.t$genre, sep = '.')
dat.t$phase_genre <- as.factor(dat.t$phase_genre)
dat.t$phase_genre <- relevel(dat.t$phase_genre, ref = 'ebh.prose')

#remove resumptive and subj is PPrP (three membered)
dat.t <- dat.t[ which(dat.t$clause_rela != 'Resu_' | dat.t$subj_type != 'PPrP'),]

#remove EPPr (threemembered)
dat.t <- dat.t[dat.t$EPPr != 1,]
dat.t <- dat.t[dat.t$Objc != 1,]
dat.t <- dat.t[dat.t$Exst != 1,]
dat.t <- droplevels(dat.t)

############################################################
# id preparation

minims <- tapply(dat.t$cl_id, dat.t$book2, function(x) min(x) -1)
minims <- as.data.frame(minims)

id.vec <- numeric(0)
n <- 1

for (bo in dat.t$book2){
  id.vec[n] <- minims[bo,]
  n <- n + 1
}

dat.t$id.vec <- id.vec
dat.t$cl_id2 <- dat.t$cl_id - dat.t$id.vec

#############################################################################

str(dat.t)

dat.t <- droplevels(dat.t)

##################################################################################

dat.t$Modi <- as.factor(dat.t$Modi)
dat.t$Time <- as.factor(dat.t$Time)
dat.t$Intj <- as.factor(dat.t$Intj)
dat.t$Adju <- as.factor(dat.t$Adju)
dat.t$Cmpl <- as.factor(dat.t$Cmpl)
dat.t$Loca <- as.factor(dat.t$Loca)
dat.t$Ques <- as.factor(dat.t$Ques)
dat.t$Rela <- as.factor(dat.t$Rela)
dat.t$Conj <- as.factor(dat.t$Conj)


############################################################################

# remove cases with mother ""
dat.t <- dat.t[dat.t$mother != "",]

# remove PP subject
dat.t <- dat.t[dat.t$subj_type != "PP",]
table(dat.t$subj_type)
levels(dat.t$pc_type)
table(dat.t$pc_type)

############################################################################


# remove clauses containing negation phrases
# verbless clauses might be negated in a different way (eg with >JN),
# , which creates disbalance in the dataset

head(dat.t)
dat.t <- dat.t[dat.t$Nega == 0 & dat.t$NCop == 0,]
dim(dat.t)
table(dat.t$cl_type)

###########################################################################

# subtract mean of numeric variables

dat.t$cl_len2 <- dat.t$cl_len - mean(dat.t$cl_len)
dat.t$subj_len <- dat.t$subj_len - mean(dat.t$subj_len)
dat.t$pc_len <- dat.t$pc_len - mean(dat.t$pc_len)

mean(dat.t$cl_len2)
hist(dat.t$cl_len2)
###########################################################################

# 2. do exploration
#explorative plots 
par(mfrow=c(1, 1))
table(dat.t$nega)

# language phase 
table(dat$ebh_lbh)
phase <- table(dat.t$ebh_lbh, dat.t$cl_type)
colnames(phase) <- c('HYH clauses', 'verbless clauses')
mosaicplot(phase, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)
table(dat.t$cl_type, dat.t$subcorp)
table(dat.t$cl_type)
nrow(dat.t)

ebh_lbh_books <- dat.t[dat.t$ebh_lbh %in% c("ebh", "lbh"),]
ebh_lbh_books <- droplevels(ebh_lbh_books)
ebh_lbh_books$book2 <- factor(ebh_lbh_books$book2, levels = unique(ebh_lbh_books$book2))
ebh_lbh_table <- table(ebh_lbh_books$book2, ebh_lbh_books$cl_type)
colnames(ebh_lbh_table) <- c('HYH clauses', 'verbless clauses')
mosaicplot(ebh_lbh_table, main = '', cex.axis = 1.2, color = c('#E6B0AA', '#5DADE2'), las = 2)

table(dat.t$cl_type, dat.t$book2)
head(dat.t)
dat.chron.hyh <- dat.t[dat.t$book2 == "Chronicles" & dat.t$cl_type == "hyh", ]
stripchart(dat.chron.hyh$cl_id, method = "jitter")

dat.chron.nom <- dat.t[dat.t$book2 == "Chronicles" & dat.t$cl_type == "nom", ]
stripchart(dat.chron.nom$cl_id, method = "jitter")

dat.chron <- dat.t[dat.t$book2 == "Chronicles" ,]
chron.data <- list(dat.chron.hyh, dat.chron.nom)

dat.chron$cl_type <- as.character(dat.chron$cl_type)
dat.chron$cl_type[dat.chron$cl_type == 'nom'] <- 'verbless'
dat.chron$cl_type[dat.chron$cl_type == 'hyh'] <- 'HYH'
head(dat.chron)
stripchart(dat.chron$cl_id~dat.chron$cl_type,
           main = "",
           xlab = "",
           xaxt='n',
           method = 'jitter')


# Main_sub 
dat.t.ms <- dat.t[dat.t$main_sub != 'Undc',]
dat.t.ms <- droplevels(dat.t.ms)
m.s <- table(dat.t.ms$main_sub, dat.t.ms$cl_type)
colnames(m.s) <- c('HYH clauses', 'verbless clauses')
mosaicplot(m.s, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# discourse 
discourse <- table(dat.t$txt_type, dat.t$cl_type)
colnames(discourse) <- c('HYH clauses', 'verbless clauses')
mosaicplot(discourse, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)


# genre 
genre <- table(dat.t$genre, dat.t$cl_type)
colnames(genre) <- c('HYH clauses', 'verbless clauses')
mosaicplot(genre, main = '', cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

# make spineplot of clause length versus clause type
len_table <- table(dat.t$cl_len, dat.t$cl_type)
colnames(len_table) <- c('HYH clauses', 'verbless clauses')
spineplot(len_table, xlab = "Clause length in phrases", 
          main = "")


#length vs other vars

# ebh
ebh_dat <- dat.t[dat.t$ebh_lbh == 'ebh',]
phase_len <- table(ebh_dat$cl_len, ebh_dat$cl_type)
str(phase_len)
spineplot(phase_len)

# lbh
lbh_dat <- dat.t[dat.t$ebh_lbh == 'lbh',]
phase_len_l <- table(lbh_dat$cl_len, lbh_dat$cl_type)
str(phase_len_l)
spineplot(phase_len_l)

# pc_type
# PP
pc_pp <- dat.t[dat.t$pc_type == "PP",]
pc_pp_table <- table(pc_pp$cl_len, pc_pp$cl_type)
spineplot(pc_pp_table)

# NP
pc_np <- dat.t[dat.t$pc_type == "NP",]
pc_np_table <- table(pc_np$cl_len, pc_np$cl_type)
spineplot(pc_np_table)

# phase and pc_type
table(dat.t$ebh_lbh, dat.t$cl_type, dat.t$pc_type)

pc_pp <- dat.t[dat.t$pc_type == "PP",]
table(pc_pp$cl_type, pc_pp$cl_len, pc_pp$ebh_lbh)

# pc_len vs discourse (not so shocking)
table(dat.t$cl_type, dat.t$cl_len, dat.t$txt_type)

# pc_len vs main_sub (not so shocking)
table(dat.t$cl_type, dat.t$cl_len, dat.t$main_sub)

# pc_len vs discourse (not so shocking)

table(dat.t$cl_type, dat.t$cl_len, dat.t$genre)

# select only ebh and lbh and NP and PP
small_dat <- dat.t[dat.t$ebh_lbh %in% c('ebh', 'lbh') & dat.t$pc_type %in% c('NP', 'PP'),]
table(small_dat$cl_type, small_dat$ebh_lbh, small_dat$pc_type)


# genre and discourse together
library(vcd)
mosaic(structable(dat.t$genre, dat.t$txt_type, dat.t$cl_type), rot_labels = c(right = -45))

#######################################################################################
# exploration of other phrase types

time <- table(dat.t$Time, dat.t$cl_type)
colnames(time) <- c('HYH clauses', 'verbless clauses')
mosaicplot(time, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

time_txt_type <- table(dat.t$Time, dat.t$txt_type)
mosaicplot(time_txt_type, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

ques <- table(dat.t$Ques, dat.t$cl_type)
colnames(ques) <- c('HYH clauses', 'verbless clauses')
mosaicplot(ques, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

ques_txt_type <- table(dat.t$Ques, dat.t$txt_type)
mosaicplot(ques_txt_type, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

conj <- table(dat.t$Conj, dat.t$cl_type)
colnames(conj) <- c('HYH clauses', 'verbless clauses')
mosaicplot(conj, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

modi <- table(dat.t$Modi, dat.t$cl_type)
colnames(modi) <- c('HYH clauses', 'verbless clauses')
mosaicplot(modi, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

intj <- table(dat.t$Intj, dat.t$cl_type)
colnames(intj) <- c('HYH clauses', 'verbless clauses')
mosaicplot(intj, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

intj_txt_type <- table(dat.t$Intj, dat.t$txt_type)
mosaicplot(intj_txt_type, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)


rela <- table(dat.t$Rela, dat.t$cl_type)
colnames(rela) <- c('HYH clauses', 'verbless clauses')
mosaicplot(rela, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)

nega <- table(dat.t$Nega, dat.t$cl_type)
colnames(nega) <- c('HYH clauses', 'verbless clauses')
mosaicplot(nega, cex.axis = 1.2,color = c('#E6B0AA', '#5DADE2'), las = 2)
#########################################################################################


# how do different levels of subject collocate with different levels of predicate complement
mosaicplot(table(dat.t$subj_type_det, dat.t$pc_type_det), las = 1, xlab = 'subject', ylab = 'predicate complement')

#subject type vs s-p-order
table(dat.t$subj_type, dat.t$s_p_order)

######################################################
table(dat.t$Nega, dat.t$cl_type)
dat.t$Nega <- as.factor(dat.t$Nega)
str(dat.t)

with.nega <- dat.t[dat.t$Nega == 1,]
table(with.nega$cl_type)

######################################################

# make model

dat.t <- droplevels(dat.t)
table(dat.t$subj_type)
levels(dat.t$pc_type)
table(dat.t$pc_type)
table(dat.t$Nega)
table(dat.t$NCoS)

levels(dat.t$pc_type) <- c("PP","AdjP", "AdvP", "DPrP", "InrP", "IPrP", "NP","PPrP", "NP")

bam.fit.t <- bam(cl_type ~ main_sub + genre + ebh_lbh + Conj + Intj + Time + Ques + pc_type + cl_len2 + mother + s(cl_id2, book2, bs = 'fs', m=1), data = dat.t, family = 'binomial')
summary(bam.fit.t)

# interesting alternative, control k in the smooth, does not make much difference here, but could be explored further

# bam.fit.t <- bam(cl_type ~  main_sub + genre + ebh_lbh + pc_type + cl_len2 +
#                   mother + Conj + Intj + Time + Ques +
#                   s(cl_id2, book2, bs = 'fs', m=1, k=15), data = dat.t, family = 'binomial')

summary(bam.fit.t)
gam.check(bam.fit.t)
plot(bam.fit.t)

summary(bam.fit.t)
pp <- get_modelterm(bam.fit.t, select = 1, as.data.frame=TRUE)

lattice::xyplot(fit~cl_id2|book2, data = pp, type = 'l')

# no ques and intj, Q differs significantly from N
bam.fit.withtxttype <- bam(cl_type ~  main_sub + genre + txt_type + ebh_lbh + 
                             pc_type + cl_len2 + mother + Conj + Time  + 
                             s(cl_id2, book2, bs = 'fs', m=1), data = dat.t, family = 'binomial')
summary(bam.fit.withtxttype)

bam.fit.noquesintj <- bam(cl_type ~  main_sub + genre + txt_type + ebh_lbh + pc_type + cl_len2 +
                            mother + s_p_order  + Conj + Time  +
                            s(cl_id2, book2, bs = 'fs', m=1), data = dat.t, family = 'binomial')

summary(bam.fit.noquesintj)


################################################################################

# plot the model

plot(bam.fit.t, 
     pages = 1, 
     trans = plogis, 
     shift = coef(bam.fit.t)[1], 
     seWithMean = TRUE)

# split in distinct plots, one for each book
pp <- get_modelterm(bam.fit, select=2, as.data.frame=TRUE)
lattice::xyplot(fit~cl_id2|book2, data = pp, type = 'l')


par(mfrow=c(1, 2))

#make plots of Intj and Ques
plot_parametric(bam.fit.t, pred=list(Ques = levels(dat.t$Ques)), col = 'red', main = "Question phrases", xlab = 'log odds')
plot_parametric(bam.fit.t, pred=list(Intj = levels(dat.t$Intj)), col = 'red', main = "Interjection phrases", xlab = 'log odds')

#############################################################################
par(mfrow=c(1, 2))

plot_parametric(bam.fit.t, pred=list(Conj = levels(dat.t$Conj)), col = 'red', 
                main = "Conjunction phrases", xlab = 'log odds')
plot_parametric(bam.fit.t, pred=list(Time = levels(dat.t$Time)), col = 'red', 
                main = "Time phrases", xlab = 'log odds')

###############################################################

par(mfrow=c(1, 1))

# ebh_lbh
plot_parametric(bam.fit.t, 
                pred=list(ebh_lbh = c("ebh", "lbh")), 
                col = 'red', rm.ranef=F, 
                main = '',
                xlab = 'log odds')

# plot main_sub
plot_parametric(bam.fit.t, pred=list(main_sub = levels(dat.t$main_sub)), 
                col = 'red', rm.ranef=F, 
                main = '',
                xlab = 'log odds')

# genre
plot_parametric(bam.fit.t, pred=list(genre = c("prose", "poetry")), 
                col = 'red',rm.ranef=F, 
                main = '',
                xlab = 'log odds')

# mother
plot_parametric(bam.fit.t, pred=list(mother = c("impf", "nominal", "impv", "infc", "no_mother", 
                                                "perf", "no_pred", "ptca", "wayq")), 
                col = 'red',rm.ranef=F, 
                main = '',
                xlab = 'log odds')

# pc_type
summary(bam.fit.t)
levels(dat.t$pc_type)
plot_parametric(bam.fit.t, pred=list(pc_type = c("AdjP","IPrP","NP", "PP")), 
                col = 'red',rm.ranef=T, 
                main = '',
                xlab = 'log odds')

#########################################################################################
#visualizations of the model with visreg
library(visreg)
visreg(bam.fit.t)
