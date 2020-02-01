
adapt_genre <- function(dat.t) {
  # relabel genre of some verses
  
  dat.t$book.ch <- paste(dat.t$book, dat.t$chapter, sep = '_')
  
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
  
  return(dat.t)
  
  }