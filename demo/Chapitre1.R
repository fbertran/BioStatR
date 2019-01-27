#' ---
#' title: "Initiation \`a la statistique avec R, code et compl\'ements chapitre 1"
#' author: "Fr\'ed\'eric Bertrand et Myriam Maumy-Bertrand"
#' date: "11 d\'ecembre 2018"
#' ---

#Chapitre 1
#page 10
#q()
?read.table
help(read.table)

#aide pour le package dont le nom est "stats"
help(package="stats")
example(plot)

#page 11
example(plot)
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
#source(file="C://chemin//vers//nomdefichier//fichier.R",echo=T)
#source(file=".../repertoire/fichier.R",echo=T)

#page 14
#source(file="fichier.R",echo=T)
## Si "fichier.R" est dans le r\'epertoire de travail

# Exercice 1.1
#page 18
#install.packages("BioStatR")
#install.packages("BioStatR",repos="http://irma.math.unistra.fr/~fbertran/BioStatR")

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

