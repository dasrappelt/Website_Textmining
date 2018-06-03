# Installieren
install.packages("tm")  # Für Text-Mining
install.packages("SnowballC") # für Text-Stemming
install.packages("wordcloud") # Word-Cloud-Generator 
install.packages("RColorBrewer") # Farbpaletten
install.packages("RCurl")

# Laden
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

############
#alle Daten von Website laden
require("RCurl")
text <- "https://www.zeit.de/wissen/gesundheit/2018-05/global-drug-survey-drogenumfrage-deutschland-drogenkonsum/komplettansicht"
text.request <- getURL (text, ssl.verifypeer = FALSE)

#Klasse des Textes bilden
class(text.request)

#Text zu Vektor transformieren
is.vector(text.request)

#Drucken des Vektors
print(text.request)

#Parsing des HTML-Codes, damit Baumstruktur entsteht
require ("XML")
text.tree <- htmlTreeParse (text.request, useInternal=TRUE)
print(text.tree)

#Extraktion des p-Paragraphen
text.tree.parse <- unlist (xpathApply(text.tree, path = "//p", fun = xmlValue))
print(text.tree.parse)

text.txt <- NULL
for (i in 2: (length(text.tree.parse)-1)) {
  text.txt <- paste(text.txt, as.character(text.tree.parse[i]), sep = " ")
}

is.vector(text.txt)

length(text.txt)

print(text.txt)

#Entfernt \r und \n für new line
require(tm)
text.txt <- gsub("\r?\n|\r", "", text.txt)
my.corpus <- Corpus(VectorSource(text.txt))

strwrap(my.corpus[[1]])

#Text säubern
my.corpus <- tm_map(my.corpus, tolower) #Kleinschreibung
my.corpus <- tm_map(my.corpus, removePunctuation) #Punkte entfernen
my.corpus <- tm_map(my.corpus, stripWhitespace) #Leerzeichen entfernen
my.corpus <- tm_map(my.corpus, removeWords, stopwords("german")) #Stopp-Wörter entfernen
my.corpus <- tm_map(my.corpus, removeWords, c("dass","global","mehr","drogen", "menschen", "prozent", "drug", "survey")) #individuelle Wörter entfernen
my.corpus <- tm_map(my.corpus, PlainTextDocument)
strwrap(my.corpus[[1]])
print(my.corpus)

#Transformation zu TDM
tdm <- TermDocumentMatrix(my.corpus) #Erstellen der TDM
inspect(tdm[1:10,]) #Darstellung der ersten 10 Begriffe


#Word-Cloud bauen
require(wordcloud)
my.word.matrix <- as.matrix(tdm) #Konvertieren der TDM zu einer Matrix
colnames(my.word.matrix) <- "Text Minutes"
v <- sort(rowSums(my.word.matrix), decreasing = TRUE) #Sortieren der Daten
d <- data.frame(word=names (v), freq=v) #Bauen eines Datenframes für Wordcloud-Funktion
par(bg = "grey") #Hintergrundfarbe einstellen
wordcloud (d$word, d$freq, random.order = FALSE, max.word = 300, color = "white") #Wordcloud

print(my.word.matrix)