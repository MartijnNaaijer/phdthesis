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

The map "Various" contains some useful scripts. adapt_genre.R is used in various other scripts for releveling the genre of clauses in some biblical verses. main_subordinate_clauses.ipynb contains some functions, in which it is decided whether a clause is a main clause or a subordinate clause. It is based on the research of Marianne Kaajan. subgenres_synvar.xls is made by Dirk Bakker. In this file you can find the way we have divided the Hebrew Bible in different genres.
