In this folder you find four files with tripartite clauses.
These are:
tripartite_bib.csv
tripartite_eppr_bib.csv 
tripartite_xbib.csv
tripartite_eppr_xbib.csv

The xbib-files contain data about clauses from extrabiblical texts. 
The eppr-files contain data about clauses with a so-called Enclitic Personal Pronoun. In these tripartite clauses there is no 
agreement between the subject and the copular pronoun, for instance Psalms 44:5 אַתָּה־ה֣וּא מַלְכִּ֣י אֱלֹהִ֑ים.

These are encoded in a slightly different way in the etcbc database, and for clarity I have saved them in separate files.
The files tripartite_bib.csv and tripartite_xbib.csv contain other candidates of tripartite clauses. They are encoded with the value
"ReSu" in the feature rela, and they have a personal pronoun as subject.

The analysis can be found in the file mosaic_plots_RF_XGB_bip_trip.R.
