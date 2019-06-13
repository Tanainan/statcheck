library(stringr)
library(tidyverse)
library(rio)
setwd("~/Downloads/Pdfs/AMOS/Done")

a <- readLines("Bassi (2012).txt") # 9 ppl were removed from 184
b <- readLines("Mandara (2009).txt")
c <- readLines("Rapee (2005).txt")
d <- readLines("Roest (2010).txt")
e <- readLines("Riggs (2009a).txt")
i <- readLines("Bradford (2008).txt")
j <- readLines("Carmeli (2006).txt")
m <- readLines("Sani (2008).txt")

o <- readLines("van der Heijden (2009).txt")
p <- readLines("Lee (2007).txt")
q <- readLines("Loyens (2007).txt") # RMSEA chi2
r <- readLines("Zhao (2013).txt")
s <- readLines("Kark (2009).txt")

oo <- "five hundred and six part"
ooo <- "often ten intend"
oi <- "oooo sixty-three pfgslj"

#chi sqr
str_view(e, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s\\d*(\\.|\\:)\\d+))", ignore_case = T))


#RMSEA
str_view(z, regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
rrr <- str_extract_all(chi2Raw, regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
rrr <- rrr[!is.na(rrr)]
RMSEA <- str_extract_all(unlist(rrr), regex("\\.\\d+"))
RMSEA <- RMSEA[!is.na(RMSEA)]
RMSEA

# chi sqr and RMSEA
# str_view(n, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
# chi2Raw <- str_extract(a, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,20}))(\\s\\d*(\\.|\\:)?\\d*)(\\,|\\s|\\w+|[<>=]|\\.|\\,|\\)|\\(|\\:|\\;|\\/|\\-){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
# chi2Raw <- chi2Raw[!is.na(chi2Raw)]
# Chi2 <- str_extract(chi2Raw, regex("\\d+\\.\\d*"))
# Chi2 <- Chi2[!is.na(Chi2)]
# Chi2


ccc <- str_subset(j, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
chi2Loc <- str_extract(ccc, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,20}))(\\s\\d*(\\.|\\:)?\\d*))",
                             ignore_case = TRUE))
chi2Loc <- str_extract_all(ccc, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,20}))(\\s\\d*(\\.|\\:)?\\d*))",
                                  ignore_case = TRUE))
Chi2 <- str_extract_all(unlist(chi2Loc), regex("\\d*\\.\\d+"))
Chi2






#N
aaa <- str_extract_all(i, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?\\d\\d+\\s)|(\\w+\\s(?!0)\\d\\d+\\s\\w+)", ignore_case = T)) # search for numbers and get the location in the article
aaa <- aaa[!is.na(aaa)]
N <- str_extract_all(unlist(aaa), regex("\\d+"))
N <- N[!is.na(N)]
unlist(N)

# df will be from the text of chi sqr
ddd <- str_extract_all(z, regex("((((degrees of freedom of)\\s(?!0)\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?(?!0)\\d+\\s?)|(\\s(?!0)\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\((?!0)\\d+\\)))", ignore_case = T))
ddd <- ddd[!is.na(ddd)]
df <- str_extract_all(unlist(ddd), regex("(?!2\\()\\d+"))
df <- df[!is.na(df)]
df


ddd <- str_extract_all(unlist(chi2RMSEA), regex("((df|d)\\s?\\=|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?|\\s(?!0)\\d+\\s(df|degrees of freedom)", ignore_case = T))
ddd

# n = 
# integer numbers in the article
# text of numbers
# t-test df +1 or +2 [dependent (n-1)]
# F test btw subject numerator (1,df) +2 for example

# search for smallest discrepency RMSEA reported, check roundings


str_view(a, regex("\\s\\(?\\s?\\=?\\s?\\d+\\)?\\s", ignore_case = T))


str_extract_all(p, regex("\\s(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)))\\s", ignore_case = T))

chi2RMSEALoc <- str_subset(p, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))

chi2RMSEA <- str_extract_all(unlist(chi2RMSEALoc), regex("(.){0,300}(root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,150}", ignore_case = TRUE))
# Get location of chi values in text:
chi2Loc <-
  str_extract_all(unlist(chi2RMSEA), regex("((chi-square |chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s?\\d*(\\.|\\:)\\d+))",
                                           ignore_case = TRUE))
# Get chi value
Chi2 <- str_extract(unlist(chi2Loc), regex("\\d*\\.\\d+"))
Chi2

dt <- NA

for(i in 1:length(unlist(chi2RMSEA))){
  if (str_detect(unlist(chi2RMSEA)[i], "[Cc]hi-square(.){0,20}(\\s?\\d*(\\.|\\:)\\d+)") == TRUE) 
  {dt[i] <- str_extract(unlist(chi2RMSEA)[i], regex("\\d*(\\.|\\:)\\d+", ignore_case = TRUE))}
  # if (str_detect(unlist(chi2RMSEA)[i], "(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s?\\d*(\\.|\\:)\\d+))") == TRUE) 
  # {dt$Chi2[i] <- str_extract(unlist(chi2RMSEA)[i], regex("\\d*\\.\\d+", ignore_case = TRUE))}
  # if (str_detect(unlist(chi2RMSEA)[i], "chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s?\\d*(\\.|\\:)\\d+))") == TRUE) 
  # {dt$Chi2[i] <- str_extract(unlist(chi2RMSEA)[i], regex("\\d*\\.\\d+", ignore_case = TRUE))}
  # if (str_detect(unlist(chi2RMSEA)[i], "chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s?\\d*(\\.|\\:)\\d+))") == TRUE) 
  # {dt$Chi2[i] <- str_extract(unlist(chi2RMSEA)[i], regex("\\d*\\.\\d+", ignore_case = TRUE))}
  # if (str_detect(unlist(chi2RMSEA)[i], "chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s?\\d*(\\.|\\:)\\d+))") == TRUE) 
  # {dt$Chi2[i] <- str_extract(unlist(chi2RMSEA)[i], regex("\\d*\\.\\d+", ignore_case = TRUE))}
  # 
}








