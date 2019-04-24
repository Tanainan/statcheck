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

str_view(a,
         "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))"
)

gregexpr(
  "(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
  sub("^.*?\\(", "", x),
  ignore.case = TRUE
)

gregexpr(
  "p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*",
  x,
  ignore.case = TRUE
)

str_view(y,
         "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))")


str_view(x,
         regex("((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
               ignore.case = TRUE)
)

#RMSEA
#str_view(x, regex("(RMSEA\\)?\\s[<>=]?\\s?\\d*\\.\\d*)", ignore_case = T))
#str_view(b, regex("(RMSEA\\)?\\s[<>=]?\\s?\\w+\\s?\\w+\\s?\\w+\\s?\\d*\\.?\\d*)", ignore_case = T))
#str_view(c, regex("(root mean square error of approximation\\)?\\s[<>=]?\\s?\\w+\\s?\\w+\\s?\\w+\\s?\\d*\\.?\\d*)", ignore_case = T))
str_view(c, regex("((error of approximation|RMSEA\\)?)\\s?\\s?[<>=]?\\s?\\w+\\s?\\w+\\s?\\w+\\s?\\d*\\.?\\d*)", ignore_case = T))
str_view(c, regex("((error of approximation|RMSEA\\)?)\\s?\\s?[<>=]?\\s?((\\w+\\W+){,10})?\\d*\\.?\\d*)", ignore_case = T))


#N
