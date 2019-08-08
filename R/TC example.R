library(stringr)
library(tidyverse)
library(rio)
setwd("~/Downloads/Pdfs/AMOS/Done")

# AMOS

txt <- readLines("Ahmad (2004).txt") # no result found
txt <- readLines("Ahmetoglu (2010).txt") # ok
txt <- readLines("Alonso-Tapia (2010).txt") # no result found
txt <- readLines("Anderson (2008).txt") # no RMSEA
txt <- readLines("Arendasy (2007).txt") # no RMSEA
txt <- readLines("Arendasy (2012).txt") # no RMSEA
txt <- readLines("Atwater (2009).txt") # duplicate 596.8 
txt <- readLines("Bakker (2007).txt") # data, x 2 13 1/4 21:19, p 1/4 0:07, GFI 1/4 0:97, TLI 1/4 0:93, CFI 1/4 0:96, RMSEA 1/4 0:06;
txt <- readLines("Bakker (2008a).txt") # ok but cannot get N (table)
txt <- readLines("Bakker (2008b).txt") # ok
txt <- readLines("Bartomeus (2010).txt") # no result found
txt <- readLines("Bassi (2012).txt") # 9 ppl were removed from 184
txt <- readLines("Beckjord (2009).txt") # no RMSEA
txt <- readLines("Belsky (2007).txt") # ok
txt <- readLines("Bowers (2007).txt") # no result found
txt <- readLines("Bradford (2008).txt")  # 2 = chi2 ok
txt <- readLines("Branje (2007).txt") # no result found
txt <- readLines("Burt (2006).txt") # chi2 = 2, RMSEA too far apart
txt <- readLines("Burt (2009).txt") # no result
txt <- readLines("Carmeli (2006).txt") # ok
txt <- readLines("Carter (2006).txt") # read = as "s"
txt <- readLines("Castro-Costa (2008).txt") # get more results than supposed to
txt <- readLines("Cousins (2007).txt") # cannot read "=" ok
txt <- readLines("Curby (2008).txt") # no result
txt <- readLines("Dabos (2013).txt") # no result
txt <- readLines("de Marco (2006).txt") # chi2 = "v"
txt <- readLines("de Marco (2009).txt") # no result
txt <- readLines("Dunn (2006).txt") # no "=", chi2 = 2
txt <- readLines("Dunn (2012).txt") # ok
txt <- readLines("Furnham (2009).txt") # no chi2 symbol
txt <- readLines("Furuya (2009).txt") # no result found ----
txt <- readLines("Gano-Overway (2009).txt") # "=" is "s" ?????
txt <- readLines("Gaylord-Harden (2007).txt") # no chi2 symbol
txt <- readLines("Gheldof (2006).txt") # no result
txt <- readLines("Gheldof (2010).txt") # ok
txt <- readLines("Gill (2013).txt") # result too far apart
txt <- readLines("Gluszek (2011).txt") # ok! 
txt <- readLines("Goncalves-Souza (2011).txt") # no result found -----
txt <- readLines("Gutieerrez (2006).txt") # ok
txt <- readLines("Halperin (2011).txt") # ok
txt <- readLines("Hoglund (2007).txt") # no result found ---
txt <- readLines("Huta (2007).txt") # no result found ----
txt <- readLines("Jasuja (2008).txt") # doesn't read a line about df
txt <- readLines("Johnson (2008).txt") # no result found
txt <- readLines("Kark (2009).txt") # no result found
txt <- readLines("Kim (2009).txt") # ok!
txt <- readLines("Kim (2010).txt") # ok 
txt <- readLines("Klag (2006).txt") # mistakenly extract chi2 diff
txt <- readLines("Klages (2006).txt") # no result found ---
txt <- readLines("Knoll (2007).txt") # no result found ---
txt <- readLines("Kogan (2008).txt") # no result found ---
txt <- readLines("Kong (2012).txt") # no result found ---
txt <- readLines("Lachman (2006).txt") # ok
txt <- readLines("Langerhans (2007).txt") # no result found --
txt <- readLines("Lee (2007).txt") # ok!
txt <- readLines("Lee (2013).txt") # no result found
txt <- readLines("Loyens (2007).txt") # RMSEA too far from Chi2 + table result
txt <- readLines("Mandara (2009).txt") # ok!
txt <- readLines("McGrew (2007).txt") # no result found ---
txt <- readLines("Molero (2011).txt") # result table
txt <- readLines("Monsen (2009).txt") # result table
txt <- readLines("Moretti (2006).txt") # no result found ---
txt <- readLines("Moya-Larano (2006).txt") # no result found
txt <- readLines("Ngo-Metzger (2008).txt") # ok
txt <- readLines("Nijhof (2009).txt") # ok
txt <- readLines("Nijs (2007).txt") # no result found ---
txt <- readLines("Nikolopoulos (2006).txt") # no result found ---
txt <- readLines("Olano (2012).txt") # no result found ---
txt <- readLines("Owen (2009).txt") # no result found ---
txt <- readLines("Paillé (2013).txt") # no result found ---
txt <- readLines("Parchman (2010).txt") # no result found ---
txt <- readLines("Peled (2007).txt") # no result found ---
txt <- readLines("Puente (2008).txt") # no result found ---
txt <- readLines("Quilty (2004).txt") # no result found ---
txt <- readLines("Rapee (2005).txt") # ok
txt <- readLines("Reynolds (2008).txt") # ok
txt <- readLines("Riggs (2009a).txt") # no result found ---
txt <- readLines("Riggs (2009b).txt") # no result found ---
txt <- readLines("Riggs (2009c).txt") # no result found ---
txt <- readLines("Roest (2010).txt") # no result found ---
txt <- readLines("Ryan (2008).txt") # no result found ---
txt <- readLines("Sani (2007).txt") # ok
txt <- readLines("Sani (2008).txt") # ok
txt <- readLines("Schepers (2008).txt") # ok
txt <- readLines("Schippers (2010).txt") # ok
txt <- readLines("Siegrist (2007).txt") # no result found ---
txt <- readLines("Smith (2013).txt") # no result found ---
txt <- readLines("Snoek (2007).txt") # no result found ---
txt <- readLines("Sousa (2007).txt") # no result found ---
txt <- readLines("Subramony (2008).txt") # ok
txt <- readLines("Tapia (2013).txt") # no result found --- 
txt <- readLines("Teng (2011).txt") # no result found ---
txt <- readLines("van Braak (2004).txt") # no result found ---
txt <- readLines("van der Heijden (2009).txt") # ok
txt <- readLines("van der Vorst (2005).txt") # no result found ---
txt <- readLines("Vervoort (2010).txt") # no result found --- 
txt <- readLines("Wallgren (2007).txt") # no result found ---
txt <- readLines("Wu (2009).txt") # no result found ---
txt <- readLines("Zettle (2013).txt") # no result found
txt <- readLines("Zhao (2013).txt") # ok

xx <- c(readLines("Ahmad (2004).txt")
,readLines("Ahmetoglu (2010).txt")
,readLines("Alonso-Tapia (2010).txt")
,readLines("Anderson (2008).txt")
,readLines("Arendasy (2007).txt")
,readLines("Arendasy (2012).txt")
,readLines("Atwater (2009).txt")
,readLines("Bakker (2007).txt")
,readLines("Bakker (2008a).txt")
,readLines("Bakker (2008b).txt")
,readLines("Bartomeus (2010).txt")
,readLines("Bassi (2012).txt")
,readLines("Beckjord (2009).txt")
,readLines("Belsky (2007).txt")
,readLines("Bowers (2007).txt")
,readLines("Bradford (2008).txt")
,readLines("Branje (2007).txt")
,readLines("Burt (2006).txt")
,readLines("Burt (2009).txt")
,readLines("Carmeli (2006).txt")
,readLines("Carter (2006).txt")
,readLines("Castro-Costa (2008).txt")
,readLines("Cousins (2007).txt")
,readLines("Curby (2008).txt")
,readLines("Dabos (2013).txt")
,readLines("de Marco (2006).txt")
,readLines("de Marco (2009).txt")
,readLines("Dunn (2006).txt")
,readLines("Dunn (2012).txt")
,readLines("Furnham (2009).txt")
,readLines("Furuya (2009).txt")
,readLines("Gano-Overway (2009).txt")
,readLines("Gaylord-Harden (2007).txt")
,readLines("Gheldof (2006).txt")
,readLines("Gheldof (2010).txt")
,readLines("Gill (2013).txt")
,readLines("Gluszek (2011).txt")
,readLines("Goncalves-Souza (2011).txt")
#,readLines("Gutieerrez (2006).txt")
,readLines("Halperin (2011).txt")
,readLines("Hoglund (2007).txt")
,readLines("Huta (2007).txt")
,readLines("Jasuja (2008).txt")
,readLines("Johnson (2008).txt")
,readLines("Kark (2009).txt")
,readLines("Kim (2009).txt")
,readLines("Kim (2010).txt")
,readLines("Klag (2006).txt")
,readLines("Klages (2006).txt")
,readLines("Knoll (2007).txt")
,readLines("Kogan (2008).txt")
,readLines("Kong (2012).txt")
,readLines("Lachman (2006).txt")
,readLines("Langerhans (2007).txt")
,readLines("Lee (2007).txt")
,readLines("Lee (2013).txt")
,readLines("Loyens (2007).txt")
,readLines("Mandara (2009).txt")
,readLines("McGrew (2007).txt")
,readLines("Molero (2011).txt")
,readLines("Monsen (2009).txt")
,readLines("Moretti (2006).txt")
,readLines("Moya-Larano (2006).txt")
,readLines("Ngo-Metzger (2008).txt")
,readLines("Nijhof (2009).txt")
,readLines("Nijs (2007).txt")
,readLines("Nikolopoulos (2006).txt")
,readLines("Olano (2012).txt")
,readLines("Owen (2009).txt")
,readLines("Paillé (2013).txt")
,readLines("Parchman (2010).txt")
,readLines("Peled (2007).txt")
,readLines("Puente (2008).txt")
,readLines("Quilty (2004).txt")
,readLines("Rapee (2005).txt")
,readLines("Reynolds (2008).txt")
,readLines("Riggs (2009a).txt")
,readLines("Riggs (2009b).txt")
,readLines("Riggs (2009c).txt")
,readLines("Roest (2010).txt")
,readLines("Ryan (2008).txt")
,readLines("Sani (2007).txt")
,readLines("Sani (2008).txt")
,readLines("Schepers (2008).txt")
,readLines("Schippers (2010).txt")
,readLines("Siegrist (2007).txt")
,readLines("Smith (2013).txt")
,readLines("Snoek (2007).txt")
,readLines("Sousa (2007).txt")
,readLines("Subramony (2008).txt")
,readLines("Tapia (2013).txt")
,readLines("Teng (2011).txt")
,readLines("van Braak (2004).txt")
,readLines("van der Heijden (2009).txt")
,readLines("van der Vorst (2005).txt")
,readLines("Vervoort (2010).txt")
,readLines("Wallgren (2007).txt")
,readLines("Wu (2009).txt")
,readLines("Zettle (2013).txt")
,readLines("Zhao (2013).txt"))





# Mplus

setwd("~/Downloads/Pdfs/Mplus/Done")


txt <- readLines("Ammerman (2013).txt") # no
txt <- readLines("Ansell (2010).txt") # no
txt <- readLines("Armey (2011).txt") # no
txt <- readLines("Backman (2011).txt") # no
txt <- readLines("Beirens (2010).txt") # no
txt <- readLines("Beyers (2010).txt") # ok
txt <- readLines("Bindman (2013).txt") # ok
txt <- readLines("Brandlistuen (2010).txt") # no
txt <- readLines("Brook (2011).txt") # no
txt <- readLines("Bucx (2010).txt") # ok
txt <- readLines("Burns (2009).txt") # no
txt <- readLines("Byrne (2013).txt") # no
txt <- readLines("Chen (2013).txt") # no
txt <- readLines("Cheng (2009).txt") # no
txt <- readLines("Cheung (2009).txt") # no
txt <- readLines("Chmielewski (2011).txt") # no
txt <- readLines("Chun (2013).txt") # no
txt <- readLines("Cooney (2008).txt") # no
txt <- readLines("Daschmann (2011).txt") # no
txt <- readLines("Davis (2012).txt") # no
txt <- readLines("de Menezes (2011).txt") # no
txt <- readLines("de Stavola (2011).txt") # no
txt <- readLines("Dooley (2010).txt") # ok
txt <- readLines("Duncan (2011).txt") # no
txt <- readLines("Eaton (2013).pdf.txt") # no
txt <- readLines("Evans (2013).txt") # no
txt <- readLines("Ferrando (2009).txt") # no
txt <- readLines("Gardner (2009).txt") # ok
txt <- readLines("Gibbons (2010).txt") # ok
txt <- readLines("Goodwin (2012).txt") # ok
txt <- readLines("Green (2010).txt") # no
txt <- readLines("Grilli (2011).txt") # no
txt <- readLines("Gschwendner (2008).txt") # no
txt <- readLines("Gustavsson-Lilius (2012).txt") # no
txt <- readLines("Hall (2010).txt") # no
txt <- readLines("Hall (2012).txt") # no
txt <- readLines("Harring (2012).txt") # no
txt <- readLines("Hassel (2011).txt") # no
txt <- readLines("Herrenkohl (2012).txt") # no
txt <- readLines("Herzog (2009).txt") # no
txt <- readLines("Hicks (2011).txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines(".txt")
txt <- readLines("~/Downloads/Pdfs/Mplus/Done/Lehavot (2011).txt")



# EQS
setwd("~/Downloads/Pdfs/EQS/Done")
txt <- readLines("Fincham (2002).txt")









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








