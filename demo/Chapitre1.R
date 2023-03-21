#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 1"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 1
#page 9
#q()
?read.table

#page 10
help(read.table)
#help(package="package")
example(plot)
help("read.table",help_type="html")

#page 11
help("read.table",help_type="text")
help.start()
options(help_type="html")
options(help_type="text")
2+8

#page 12
2+8
120:155
sqrt(4)

#page 13
#source(file="C://chemin//vers//nomdefichier//fichier.R",echo=TRUE)
#source(file=".../repertoire/fichier.R",echo=TRUE)
#source(file="fichier.R",echo=TRUE)
## Si "fichier.R" est dans le r\'epertoire de travail

# Exercice 1.1
#page 18
#install.packages("BioStatR")
help(package="BioStatR")
#install.packages("devtools")
#library(devtools)
#install_github("fbertran/BioStatR")

# Exercice 1.2
# 1)
10:25

#page 19
seq(from=10,to=25,by=1)
seq(10,25,1)
# 2)
seq(from=20,to=40,by=5)
seq(20,40,5)
# 3)
rep(x=28,times=10)

#page 20
rep(28,10)


