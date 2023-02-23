setwd("/Users/taylorsexton/Desktop/Evolution/Tasks/Project")
#PDF
install.packages("pdftools")
library(pdftools)
files <- list.files(pattern="pdf$")
opinions <- lapply(files, pdf_text)
install.packages("tm")
library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))
opinions.tdm <- TermDocumentMatrix(corp,
                                   control =
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(1, Inf))))
inspect(opinions.tdm[1:10,])
corp <- tm_map(corp, removePunctuation, ucp=TRUE)
opinions.tdm <- TermDocumentMatrix(corp,
                                 control =
                                   list(stopwords = TRUE,
                                        tolower = TRUE,
                                        stemming = TRUE,
                                        removeNumbers = TRUE,
                                        bounds = list(global = c(1, Inf))))
inspect(opinions.tdm[1:10,])

#TXT
my_data <- read.table("~/Desktop/Evolution/Tasks/Project/Saimaa_ringed_seal_microsat.gen", header=TRUE, sep=" ")


?read.genepop
#Hypothesis
#Do the different alleles on the loci affect whether the the seal lived past a year?


