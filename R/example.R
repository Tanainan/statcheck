library(stringr)
library(tidyverse)
library(rio)
setwd("~/Downloads/Pdfs/AMOS/Done")

x <- "aaaaaaaaaaaa 2 (54) = 60.1, p < 0.27, goodness-of-fit index (GFI) = .94, comparative fit index (CFI) = .96, and root mean square error of approximation (RMSEA) = .03. aaaaaaaaaaaaaa"
y <- "v2 = (6 df, p \ .01) 62.4, GFI = .99, AGFI = .99, PGFI = .13, RMSEA = .03, AIC = 140.4"
z <- readLines("Gutieerrez (2006).txt")
a <- readLines("Bassi (2012).txt")
b <- "and the root mean square error of approximation (RMSEA) was equal to .08, which indicated"
c <- "and the root mean square error of approximation was equal to .08, which indicated"
d <- "bb was equal to .08, which indicated"
e <- "a Chi-square of 596.8 with 335 degrees of freedom, and other goodness-of-fit statistics (CFI = .95; IFI = .95; TLI = .94; RMSEA = .06) were " #Atwater 2009
f <- " v2 = 11.7, P-value = 0.16; (b) v" #Bartomeus 2010


#chi sqr
str_view(x, regex("((chi-square|v2|2)\\s?(((\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?[<>=]\\s?\\d*\\.?\\d*)|(((\\w+\\s){0,10})\\d*\\.?\\d*)))", ignore_case = T))


#RMSEA
#str_view(x, regex("(RMSEA\\)?\\s[<>=]?\\s?\\d*\\.\\d*)", ignore_case = T))
#str_view(b, regex("(RMSEA\\)?\\s[<>=]?\\s?\\w+\\s?\\w+\\s?\\w+\\s?\\d*\\.?\\d*)", ignore_case = T))
#str_view(c, regex("(root mean square error of approximation\\)?\\s[<>=]?\\s?\\w+\\s?\\w+\\s?\\w+\\s?\\d*\\.?\\d*)", ignore_case = T))
#str_view(c, regex("((error of approximation|RMSEA\\)?)\\s?\\s?[<>=]?\\s?\\w+\\s?\\w+\\s?\\w+\\s?\\d*\\.?\\d*)", ignore_case = T))
str_view(c, regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA\\)?)\\s?\\s?[<>=]?\\s?((\\w+\\s){0,10})?\\d*\\.?\\d*)", ignore_case = T))


#N
str_view() # search for numbers and get the location in the article