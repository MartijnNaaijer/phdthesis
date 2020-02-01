# phdthesis
The repository contains scripts and data related to my thesis "Clause Structure Variation in Biblical Hebrew, a Quantitative Approach"

The data on which the analyses are based are extracted from the ETCBC database of the Hebrew Bible and related texts, 
using the open source software Text-Fabric. See: https://github.com/Dans-labs/text-fabric and also: https://etcbc.github.io/bhsa/
The latter link gives a description of the electronic edition of the Hebrew Bible, developed at the ETCBC, which is based on the text
of the fifthe edition of the Biblia Hebraica Stuttgartensia (BHS).

The repository contains Python (.ipynb) and R files. Data are extracted with Text-Fabric using Python, most postprocessing is doen with R.
Dependencies of the Pyhton scripts are Text-Fabric, Tensorflow 2.0 for gpu, sklearn numpy, and pandas.

In Ch2_History_of_scholarship, you find a word count of the prose tale of Job.
In Ch4_Expressions_of_to_be, you find three subfolders. These correspond with the sections 4.2, 4.3, and 4.4 in the thesis.
In Ch5_Verbal_valence, you find datasets and analysis for chapter 5.
Ch6_Sequence_analysis contains scripts for chapter 6. You need to run the Python scripts on a GPU and tensorflow-gpu.

The map "Various" contains some useful scripts. adapt_genre.R is used in various other scripts for releveling the genre of clauses in some biblical verses. main_subordinate_clauses.ipynb contains some functions, in which it is decided whether a clause is a main clause or a subordinate clause. It is based on the research of Marianne Kaajan. subgenres_synvar.xls is made by Dirk Bakker. In this file you can find the way we have divided the Hebrew Bible in different genres. appendixB.R contains the simulations used in Appendix B of the thesis.

The following R packages were used:

factoextra:  
Alboukadel Kassambara and Fabian Mundt (2017). factoextra: Extract and Visualize the Results of Multivariate Data Analyses. R package version 1.0.5. https://CRAN.R-project.org/package=factoextra  
itsadug:  
van Rij J, Wieling M, Baayen R, van Rijn H (2017). “itsadug: Interpreting Time Series an Autocorrelated Data Using GAMMs.” R package version 2.3.  
Matrix:  
Douglas Bates and Martin Maechler (2018). Matrix: Sparse and Dense Matrix Classes and Methods. R package version 1.2-15. https://CRAN.R-project.org/package=Matrix  
mgcv:  
Wood, S.N. (2017) Generalized Additive Models: An Introduction with R (2nd edition). Chapman and Hall/CRC.  
randomForest:  
A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.  
tidyverse:  
Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1. https://CRAN.R-project.org/package=tidyverse  
visreg:  
Breheny P and Burchett W (2017). Visualization of Regression Models Using visreg. The R Journal, 9: 56-71.  
xgboost:  
Tianqi Chen, Tong He, Michael Benesty, Vadim Khotilovich, Yuan Tang, Hyunsu Cho, Kailong Chen, Rory Mitchell, Ignacio Cano, Tianyi Zhou, Mu Li, Junyuan Xie, Min Lin, Yifeng Geng and Yutian Li (2018). xgboost: Extreme Gradient Boosting. R package version 0.71.2.
https://CRAN.R-project.org/package=xgboost  

In the Python scipts I used next to Text-Fabric:  
Keras  
https://keras.io, recently it was included in the TensorFlow library    
pandas  
https://pandas.pydata.org  



