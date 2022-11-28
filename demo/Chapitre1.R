#' ---
#' title: "Initiation \`a la statistique avec R, code et compl\'ements chapitre 1"
#' author: "Fr\'ed\'eric Bertrand et Myriam Maumy-Bertrand"
#' date: "04 octobre 2022"
#' ---

#Chapitre 1
#page 9
q()
#page 10
?read.table
help(read.table)
help(package="package")
example(plot)

#page 11
help("read.table",help_type="html")
help("read.table",help_type="text")
help.start()

options(help_type="html")
options(help_type="text")

#Chapitre 1
#page 9
#q()

#page 10
?read.table
help(read.table)

#aide pour le package dont le nom est "stats"
help(package="stats")
example(plot)

#page 11
help("read.table",help_type="html")
help("read.table",help_type="text")
help.start()
options(help_type="html")
options(help_type="text")

#page 12
2+8
120:155
sqrt(4)

#page 13
#source(file="C://chemin//vers//nomdefichier//fichier.R",echo=TRUE)
#source(file=".../repertoire/fichier.R",echo=TRUE)

#page 14
#source(file="fichier.R",echo=TRUE)
## Si "fichier.R" est dans le r\'epertoire de travail

# Exercice 1.1
#page 18
#install.packages("BioStatR")
#install.packages("devtools")
#library(devtools)
#install_github("fbertran/BioStatR")

#page 19
help(package="BioStatR")

# Exercice 1.2
# 1)
10:25
seq(from=10,to=25,by=1)
seq(10,25,1)
# 2)
seq(from=20,to=40,by=5)

#page 20
seq(20,40,5)
# 3)
rep(x=28,times=10)
rep(28,10)


