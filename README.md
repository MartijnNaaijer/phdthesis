# phdthesis
The repository contains scripts and data related to my thesis "Clause Structure Variation in Biblical Hebrew, a Quantitative Approach"

The data on which the analyses are based are extracted from the ETCBC database of the Hebrew Bible and related texts, 
using the open source software Text-Fabric. See: https://github.com/Dans-labs/text-fabric and also: https://etcbc.github.io/bhsa/
The latter link gives a description of the electronic edition of the Hebrew Bible, developed at the ETCBC, which is based on the text
of the fifthe edition of the Biblia Hebraica Stuttgartensia (BHS).

The repository contains Python (.ipynb) and R files. Data are extracted with Text-Fabric using Python, most postprocessing is doen with R.
Dependencies of the Pyhton scripts are Text-Fabric, Tensorflow 2.0 for gpu, sklearn numpy, and pandas.

The map "Various" contains some useful scripts. adapt_genre.R is used in various other scripts for releveling the genre of clauses in some biblical verses. main_subordinate_clauses.ipynb contains some functions, in which it is decided whether a clause is a main clause or a subordinate clause. It is based on the research of Marianne Kaajan. subgenres_synvar.xls is made by Dirk Bakker. In this file you can find the way we have divided the Hebrew Bible in different genres.
