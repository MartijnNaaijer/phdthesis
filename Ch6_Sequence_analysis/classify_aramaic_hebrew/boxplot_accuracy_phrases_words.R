# Analyses of section 6.4.3

# import the accuracies of phrase and word level analyses
preds_phr <- read.csv('aram_heb_phrases.csv', header = TRUE)
head(preds_phr)

preds_words <- read.csv('aram_heb_words.csv', header = TRUE)
head(preds_words)

# check stabilization of the cumulative mean for phrases
y_phr <- cumsum(preds_phr$accuracy) / seq_along(preds_phr$accuracy)
plot(y_phr, type = 'b',
     xlab = '',
     main = 'Cumulative mean of 200 accuracies (phrase level)')

# check stabilization of the cumulative mean for words
y_word <- cumsum(preds_words$accuracy) / seq_along(preds_words$accuracy)
plot(y_word, type = 'b',
     xlab = '',
     main = 'Cumulative mean of 200 accuracies (word level)')


# make boxplot of accuracies, Figure 6.13
boxplot(all_preds, preds_w_level,
        main = 'Figure 6.13. Boxplots of predicted accuracies of classifying Aramaic and Hebrew clauses',
        names = c("Phrase level", "Word level"))
