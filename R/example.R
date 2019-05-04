library(stringr)
library(tidyverse)
library(rio)
setwd("~/Downloads/Pdfs/AMOS/Done")

x <- " fit indices: 2 (54) = 60.1, p < 0.27, goodness-of-fit index (GFI) = .94, comparative fit index (CFI) = .96, and root mean square error of approximation (RMSEA) = .03. aaaaaaaaaaaaaa"
y <- "v2 = (6 df, p \ .01) 62.4, GFI = .99, AGFI = .99, PGFI = .13, RMSEA = .03, AIC = 140.4" #Ahmetoglu 2010
z <- readLines("Gutieerrez (2006).txt")
a <- readLines("Bassi (2012).txt")
b <- "and the root mean square error of approximation (RMSEA) was equal to .08, which indicated"
c <- "and the root mean square error of approximation was equal to .08, which indicated"
d <- readLines("Nijs (2007).txt")
e <- "a Chi-square of 596.8 with 335 degrees of freedom, and other goodness-of-fit statistics (CFI = .95; IFI = .95; TLI = .94; RMSEA = .06) were " #Atwater 2009
f <- " v2 = 11.7, P-value = 0.16; (b) v" #Bartomeus 2010
g <- " (v2(47) 1/4 251.4, p < .01), due to the large sample size, but the RMSEA suggested good fit (RMSEA 1/4 .056). T" #Belsky 2007
h <- "N = 641 2  = 194.08 d = 71 p = .000 NFI = .989 TLI = .990 RMSEA = .052" #Bradford 2008
i <- readLines("Bradford (2008).txt")
j <- readLines("Carmeli (2006).txt")
k <- "tributed 8 percent (R 2 1/4 0:08, F 1/4 2:46, p 1/4 0:026), a" #case that should be detected #Carmeli
l <- "cance, 2(24) = 40.00, p = .02, whereas the value of 2/df was below the range of 3:1 (Kline, 1998). Other goodness-of-fit indexes examined fulfilled all the requirements recommended in the literature for each one (GFI and CFI > .90; RMSEA < .08). Only"

#chi sqr
str_view(e, regex("((chi-square |chi-square of|v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,20}))(\\s\\d*(\\.|\\:)?\\d*))", ignore_case = T))
str_view(e, regex("((chi-square |chi-square of|v2(| )|w2(| )|w2/df(| )|(\\:|\\() 2 )(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,10}))(\\s\\d*(\\.|\\:)?\\d*))", ignore_case = T))
# str_view(y, regex("((chi-square |chi-square of|v2(| )|(\\:|\\() 2 )
#                   ((((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|
#                   ((\\s\\w+){0,10})(\\s\\d*(\\.|\\:)?\\d*))|
#                   (([<>=]|(1/4))\\s(\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))(\\s\\d*(\\.|\\:)?\\d*))))", ignore_case = T))


#RMSEA
str_view(z, regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
rrr <- str_extract(z, regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
rrr <- rrr[!is.na(rrr)]
RMSEA <- str_extract(rrr, regex("\\.\\d+"))
RMSEA <- RMSEA[!is.na(RMSEA)]
RMSEA

# chi sqr and RMSEA
str_view(l, regex("((chi-square |chi-square of|v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,20}))(\\s\\d*(\\.|\\:)?\\d*)(\\,|\\s|\\w+|[<>=]|\\.|\\,|\\)|\\(|\\:|\\;|\\/|\\-){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))


#N
aaa <- str_extract(z, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?\\d\\d+\\s)|(\\w+\\s(?!0)\\d\\d+\\s\\w+)", ignore_case = T)) # search for numbers and get the location in the article
aaa <- aaa[!is.na(aaa)]
N <- str_extract(aaa, regex("\\d+"))
N <- N[!is.na(N)]
N

# df will be from the text of chi sqr
ddd <- str_extract(z, regex("((((degrees of freedom of)\\s\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?\\d+\\s?)|(\\s\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\(\\d+\\)))", ignore_case = T))
ddd <- ddd[!is.na(ddd)]
df <- str_extract(ddd, regex("(?!2)\\d+"))
df <- df[!is.na(df)]
df

# n = 
# integer numbers in the article
# text of numbers
# t-test df +1 or +2 [dependent (n-1)]
# F test btw subject numerator (1,df) +2 for example

# search for smallest discrepency RMSEA reported, check roundings


str_view(a, regex("\\s\\(?\\s?\\=?\\s?\\d+\\)?\\s", ignore_case = T))



#extracting the whole page
txt[nchar(txt)==0]="\n"
txt = strsplit(paste(txt,collapse=""),"\n")[[1]]
txt[grepl("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))",txt,ignore.case = T)]


a[nchar(a)==0]="\n"
a = strsplit(paste(a,collapse=""),"\n")[[1]]
a[grepl("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))",a,ignore.case = T)]
