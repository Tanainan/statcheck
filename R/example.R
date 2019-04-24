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
g <- " (v2(47) 1/4 251.4, p < .01), due to the large sample size, but the RMSEA suggested good fit (RMSEA 1/4 .056). T" #Belsky 2007
h <- "N = 641 2  = 194.08 d = 71 p = .000 NFI = .989 TLI = .990 RMSEA = .052" #Bradford 2008
i <- readLines("Bradford (2008).txt")
j <- readLines("Carmeli (2006).txt")

#chi sqr
str_view(i, regex("((chi-square |v2 | 2 )(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4))\\s?)|(((\\s\\w+){0,10})))\\s\\d*(\\.|\\:)?\\d*)", ignore_case = T))


#RMSEA
str_view(j, regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))


#N
str_view() # search for numbers and get the location in the article