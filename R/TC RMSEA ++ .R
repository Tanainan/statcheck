library(plyr)
library(dplyr)
library(stringr)

source("~/Downloads/StatCheck/StatCheck/R/TC getPDF.R")
source("~/Downloads/StatCheck/StatCheck/R/TC numbers.R")
setwd("~/Downloads/Pdfs/AMOS/Done")

checkRMSEA <-
  function(x) {{
    
    Res <- data.frame(NULL)
    
    class(Res) <- c("checkRMSEA", "data.frame")
    
    if (length(x) == 0)
      return(Res)
    
    if (is.null(names(x)))
      names(x) <-  1:length(x)
    
    message("Extracting statistics...")
    pb <- txtProgressBar(max = length(x), style = 3)
    for (i in 1:length(x)) {
      txt <- x[i]
      
      #---------------------------
      
      if (length(txt) != 0){
        
      txt <- unlist(str_replace_all(txt, c("1/4" = "=", "  " = " = ", "\\[" = "(", "\\]" = ")", "(\\:)(\\d)" = ".\\2"))) # replace "1/4" to "=", double spacing to single spacing, [] to ()
      
      
      # extract paragraphs containing Chis2-values by locating RMSEA first:
      Loc <- str_subset(txt, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)(.){0,20}(\\.\\d+)))", ignore_case = T))
      Loc <- unlist(str_replace_all(Loc, c("([vcwx]2|(\\:|\\(|\\d|\\,)\\s2|[a-zA-Z]|f\\.|\\))(5)((\\d*\\.\\d+)|\\d+)" = "\\1 = \\4"))) # when "=" was read as "5" -> change back to "=" 
      Loc <- unlist(str_replace_all(Loc, c("([vcwx]2|(\\:|\\(|\\d|\\,)\\s2|[a-zA-Z]|f\\.|\\))( 5 )((\\d*\\.\\d+)|\\d+)" = "\\1 = \\4"))) # when "=" was read as "5" -> change back to "=" 

      # If Chi2 is reported before RMSEA ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++====
      CR <- str_extract_all(unlist(Loc), regex("((chi-square (?!difference)|chisquare|chi-square of|A?D?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s?2\\s?(\\(|\\=))(.){0,40}((\\d*\\,?\\d*\\.\\d+)|\\d*\\,?\\d+)(.){0,300}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,40}\\s(0?\\.\\d+))", ignore_case = TRUE))
      CR1 <- str_extract_all(unlist(CR), regex("((chi-square (?!difference)|chisquare|chi-square of|A?D?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s?2\\s?(\\(|\\=))(.){0,40}((\\d*\\,?\\d*\\.\\d+)|\\d*\\,?\\d+)(.){0,20})", ignore_case = TRUE))

      # Get Chi2 
      CR2 <- str_replace_all(unlist(CR1), c("\\((.){1,20}\\)" = "", "\\s\\s" = " ", "(\\d)(\\,)(\\d)" = "\\1\\3"))
      CR3 <- str_extract_all(unlist(CR2), regex("((chi-square (?!difference)|chisquare|chi-square of|A?D?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s?2\\s?(\\(|\\=))(.){0,20}((\\d*\\,?\\d*\\.\\d+)|\\d*\\,?\\d+))", ignore_case = TRUE))
      CR4 <- str_replace_all(unlist(CR3), c("[vcwx]2|(\\:|\\(|\\d|\\,)\\s2\\s" = ""))
      CR5 <- str_replace_all(unlist(CR4), "^[AD].*", "") # remove Chi2 difference
      CChi2 <- unlist(str_extract(unlist(CR5), regex("(?!(2\\s))((\\d*\\.\\d+)|\\d+)")))
      
      if (length(CChi2) == length(unlist(CR1))){
        C <- data.frame(Chi2.Raw = unlist(CR1), Chi2 = unlist(CChi2)) # create a data frame
        C <- na.omit(C)
      } else {C <- data.frame(NULL)}
        
      # Extract df:
      Cddd <- str_extract_all(unlist(CR), regex("((\\(|\\s)\\(?(df|d|d.f.)\\s?[<>=]?|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?\\,?|\\s?(?!0)\\d+\\s(df|degrees of freedom)|\\(\\d+\\)", ignore_case = T))
      Cddd <- Cddd[!is.na(Cddd)]
      Cdf <- str_extract_all(unlist(Cddd), regex("(?!2\\s?\\()\\d+"))
      Cdf <- unlist(Cdf[!is.na(Cdf)])
      
      
     
      # Get RMSEA
      Crrr <- str_extract_all(unlist(CR), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,10}\\s(0?\\.\\d+))", ignore_case = T))
      Crrr <- unlist(Crrr[!is.na(Crrr)])
      Crrr <- unlist(str_replace_all(unlist(Crrr), c(" , " = " < ")))
      CRMSEA <- unlist(str_extract_all(unlist(Crrr), regex("\\.\\d+")))
      CRMSEA <- unlist(CRMSEA[!is.na(CRMSEA)])
      
      if (length(CRMSEA) != length(CChi2)){
        Crrr <- str_extract_all(unlist(CR), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,20}\\s(0?\\.\\d+))", ignore_case = T))
        Crrr <- Crrr[!is.na(Crrr)]
        Crrr <- unlist(str_replace_all(unlist(Crrr), c(" , " = " < ")))
        CRMSEA <- unlist(str_extract(unlist(Crrr), regex("\\.\\d+")))
        CRMSEA <- unlist(CRMSEA[!is.na(CRMSEA)]) 
      }
      
      # See if reported RMSEA has =,< or > sign
      Csign <- str_extract(unlist(Crrr), regex("[=<>]|less than|greater than|equals to|equal to|\\s\\s"))
      Csign <- unlist(Csign[!is.na(Csign)])
      Csign <- Csign %>% str_replace_all(c("less than" = "<", "greater than" = ">", "(equals|equal) to" = "=", "  " = "=")) %>% unlist()
      if (length(Csign) == 0 & length(CRMSEA) != 0){
        Csign <- rep("=", length(CRMSEA))
      }    
      
      
      # Get N from when it is reported in the result
      Cnnn <- str_extract_all(unlist(CR), regex("(\\Wn\\s?(\\=|equals to|equal to|equal|equals)?\\s?\\d+)", ignore_case = T))
      Cnnn <- Cnnn[!is.na(Cnnn)]
      CN <- str_extract_all(unlist(Cnnn), regex("\\d+"))
      CN <- unlist(CN[!is.na(CN)])
      
      
      Chi2 <- list(as.vector(C$Chi2)) %>% unlist 
      df <- list(Cdf) %>% unlist 
      sign <- list(Csign) %>% unlist 
      Chi2.Raw <- list(as.vector(C$Chi2.Raw)) %>% unlist 
      RMSEA <- list(CRMSEA) %>% unlist 
      N <- list(CN) %>% unlist 
      nnn <- list(Cnnn) %>% unlist
      
      # If length(Chi2) != length(RMSEA) 
      # If RMSEA is reported before Chi2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++====
     
      if (length(Chi2) != length(RMSEA)){
        
      RC <- unlist(str_extract_all(unlist(Loc), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,20}(0?\\.\\d+)(.){0,80}(chi-square (?!difference)|chiquare|chi-square of|A?D?[vcw]2\\s?\\=?\\s?\\(|(\\:|\\(|\\d|\\,)\\s?2\\s?(\\(|\\=))(.){0,40}((\\d*\\,?\\d*\\.\\d+)|\\d*\\,?\\d+))(.){0,30}", ignore_case = T)))
      RC1 <- str_extract_all(unlist(CR), regex("((chi-square (?!difference)|chisquare|chi-square of|A?D?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s?2\\s?(\\(|\\=))(.){0,40}((\\d*\\,?\\d*\\.\\d+)|\\d*\\,?\\d+)(.){0,30})", ignore_case = TRUE))
      
      # Get Chi2 
      RC2 <- str_replace_all(unlist(RC1), c("\\((.){1,20}\\)" = "", "\\s\\s" = " ", "(\\d)(\\,)(\\d)" = "\\1\\3"))
      RC3 <- str_extract_all(unlist(RC2), regex("((chi-square (?!difference)|chisquare|chi-square of|A?D?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s?2\\s?(\\(|\\=))(.){0,20}((\\d*\\,?\\d*\\.\\d+)|\\d*\\,?\\d+))", ignore_case = TRUE))
      RC4 <- str_replace_all(unlist(RC3), c("[vcwx]2|(\\:|\\(|\\d|\\,)\\s2\\s" = ""))
      RC5 <- str_replace_all(unlist(RC4), "^[AD].*", "") # remove Chi2 difference
      RChi2 <- unlist(str_extract(unlist(RC5), regex("(?!(2\\s))((\\d*\\.\\d+)|\\d+)")))
     
      if (length(RChi2) == length(unlist(RC1))){
      R <- data.frame(Chi2.Raw = unlist(RC1), Chi2 = unlist(RChi2)) # create a data frame
      R <- na.omit(R)} else {R <- data.frame(NULL)}
      
      # Get df
      Rddd <- str_extract_all(unlist(RC), regex("((\\(|\\s)\\(?(df|d|d.f.)\\s?[<>=]?|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?\\,?|\\s?(?!0)\\d+\\s(df|degrees of freedom)|\\(\\d+\\)", ignore_case = T))
      Rddd <- Rddd[!is.na(Rddd)]
      Rdf <- str_extract_all(unlist(Rddd), regex("(?!2\\s?\\()\\d+"))
      Rdf <- unlist(Rdf[!is.na(Rdf)])
      
    
      # Get RMSEA
      Rrrr <- str_extract_all(unlist(RC1), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,10}\\s(0?\\.\\d+))", ignore_case = T))
      Rrrr <- Rrrr[!is.na(Rrrr)]
      Rrrr <- unlist(str_replace_all(unlist(Rrrr), c(" , " = " < ")))
      RRMSEA <- unlist(str_extract_all(unlist(Rrrr), regex("\\.\\d+")))
      RRMSEA <- unlist(RRMSEA[!is.na(RRMSEA)])
      
      if (length(RRMSEA) != length(RChi2)){
        Rrrr <- str_extract_all(unlist(RC), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,20}\\s(0?\\.\\d+))", ignore_case = T))
        Rrrr <- Rrrr[!is.na(Rrrr)]
        Rrrr <- unlist(str_replace_all(unlist(Rrrr), c(" , " = " < ")))
        RRMSEA <- unlist(str_extract(unlist(Rrrr), regex("\\.\\d+")))
        RRMSEA <- unlist(RRMSEA[!is.na(RRMSEA)]) 
      }
      
      # See if reported RMSEA has =,< or > sign
      Rsign <- str_extract(unlist(Rrrr), regex("[=<>]|less than|greater than|equals to|equal to|\\s\\s"))
      Rsign <- unlist(Rsign[!is.na(Rsign)])
      Rsign <- Rsign %>% str_replace_all(c("less than" = "<", "greater than" = ">", "(equals|equal) to" = "=", "  " = "=")) %>% unlist()
      if (length(Rsign) == 0 & length(RRMSEA) != 0){
        Rsign <- rep("=", length(RRMSEA))
      }    
      
      # Get N from when it is reported in the result
      Rnnn <- str_extract_all(unlist(RC), regex("(\\Wn\\s?(\\=|equals to|equal to|equal|equals)?\\s?\\d+)", ignore_case = T))
      Rnnn <- unlist(Rnnn[!is.na(Rnnn)])
      RN <- unlist(str_extract_all(unlist(Rnnn), regex("\\d+")))
      RN <- unlist(RN[!is.na(RN)])
      
      
      # Combine everything ++++++++++++++++++++++++++++++++++++++++++++++++
      Chi2 <- list(as.vector(R$Chi2),as.vector(C$Chi2)) %>% unlist 
      df <- list(Rdf,Cdf) %>% unlist 
      sign <- list(Rsign,Csign) %>% unlist 
      Chi2.Raw <- list(as.vector(R$Chi2.Raw),as.vector(C$Chi2.Raw)) %>% unlist 
      RMSEA <- list(RRMSEA,CRMSEA) %>% unlist 
      N <- list(RN,CN) %>% unlist 
      nnn <- list(Rnnn,Cnnn) %>% unlist
      
      }
      
      # check if there're multi-group models and extract number of groups from adjacent words 
      multi <- unlist(str_detect(txt, regex("multigroup|multi-group|multiple (groups|group)|multi-sample|multiple (samples|sample)|multisample", ignore_case = T)))
      if (length(which(multi == TRUE)) >= 1){
      ngroup <- unlist(str_extract_all(txt, regex("(.){1,15}(groups)(?!\\.)(.){1,15}", ignore_case = T)))
      ngroup <- unlist(str_extract_all(ngroup, regex("\\d+|one|two|three|four|five|six|seven|eight|nine|\\sten\\s")))
      ngroup <- ngroup %>% str_replace_all(c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "\\sten\\s" = "10")) %>% unlist()
      ngroup <- ngroup[!duplicated(ngroup)]
      } else {ngroup = NULL}
      ngroup <- ngroup[!(as.numeric(ngroup) > 10)]

      
      # If the results could be read -> compute RMSEA ++++====
      chi2RMSEA <- NULL
      if (length(Chi2) != 0){
        if (length(df) == length(Chi2)){
          if (length(RMSEA) == length(Chi2)){
            if (length(sign) == length(RMSEA)){
      # ++++++++++++++++++++++++++++++++++++
      
      if (length(N) == length(Chi2)){ # if Ns were given in the result ++++====
      
      # create a new data frame that can contain all variables
      if (length(ngroup) != 0){
        chi2RMSEA <- data.frame(Chi2 = rep(Chi2, length(ngroup)), df = rep(df, length(ngroup)), N = rep(N, length(ngroup)), Reported.RMSEA = rep(RMSEA, length(ngroup)), Chi2.Raw = rep(unlist(Chi2.Raw), length(ngroup)), Multi.group = rep(ngroup, each = length(Chi2)), N.Raw = rep(unlist(nnn), length(ngroup)))
      }
      if (length(ngroup) == 0){
        chi2RMSEA <- data.frame(Chi2 = Chi2, df = df, N = N, Reported.RMSEA = RMSEA, Chi2.Raw = unlist(Chi2.Raw), Multi.group = rep("-", length(Chi2)), N.Raw = unlist(nnn))
      }
      
      chi2RMSEA$Chi2 <- chi2RMSEA$Chi2 %>% as.character() %>% as.numeric()
      chi2RMSEA$df <- chi2RMSEA$df %>% as.character() %>% as.numeric()
      chi2RMSEA$N <- chi2RMSEA$N %>% as.character() %>% as.numeric()
      chi2RMSEA$Reported.RMSEA <- chi2RMSEA$Reported.RMSEA %>% as.character() %>% as.numeric()
      
      if (length(ngroup) != 0){
      chi2RMSEA$Multi.group <- chi2RMSEA$Multi.group %>% as.character() %>% as.numeric()
      } else {chi2RMSEA$Multi.group <- "-"}
      

      chi2RMSEA$rmsea <- NA
      chi2RMSEA$RMSEA <- NA
      chi2RMSEA$MG.rmsea <- NA
      chi2RMSEA$MG.RMSEA <- NA

      
      #Number of Ns found in the article
      chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)
      
      #Number of models found in the article
      chi2RMSEA$Total.Models <- length(unlist(Chi2.Raw))

      chi2RMSEA$Sign = sign
      
      # compute RMSEA
      for (j in 1:nrow(chi2RMSEA)){
        deci <- decimalplaces(chi2RMSEA$Reported.RMSEA[j])
        chi2RMSEA$rmsea[j] <- sqrt(max(c((chi2RMSEA$Chi2[j]-chi2RMSEA$df[j])/(chi2RMSEA$df[j]*(chi2RMSEA$N[j]-1)),0)))
        chi2RMSEA$RMSEA[j] <- round((chi2RMSEA$rmsea[j]),deci)
        if (length(ngroup) != 0){
          chi2RMSEA$MG.rmsea[j] <- sqrt(max(c((chi2RMSEA$Chi2[j]-chi2RMSEA$df[j])/(chi2RMSEA$df[j]*(chi2RMSEA$N[j]-chi2RMSEA$Multi.group[j])),0)))*sqrt(chi2RMSEA$Multi.group[j])
          chi2RMSEA$MG.RMSEA[j] <- round((chi2RMSEA$MG.rmsea[j]),deci)
        }
        if (length(ngroup) == 0){
          chi2RMSEA$MG.rmsea[j] <- "-"
          chi2RMSEA$MG.RMSEA[j] <- "-"
        }
      }
      

      chi2RMSEA$ConsistencyRMSEA <- NA # test if the reported and computed RMSEAs are the same (same amount of digits as reported)
      chi2RMSEA$ConsistencyMG.RMSEA <- NA
      for (k in 1:nrow(chi2RMSEA)){
        if ((chi2RMSEA$RMSEA[k] == chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == "=") {chi2RMSEA$ConsistencyRMSEA[k] <- "Consistent"} else
        {if ((chi2RMSEA$RMSEA[k] > chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == ">") {chi2RMSEA$ConsistencyRMSEA[k] <- "Consistent"} else
        {if ((chi2RMSEA$RMSEA[k] < chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == "<") {chi2RMSEA$ConsistencyRMSEA[k] <- "Consistent"} else {chi2RMSEA$ConsistencyRMSEA[k] <- "x"}}}
        
        if (length(ngroup) != 0){
        if ((chi2RMSEA$MG.RMSEA[k] == chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == "=") {chi2RMSEA$ConsistencyMG.RMSEA[k] <- "Consistent"} else
        {if ((chi2RMSEA$MG.RMSEA[k] > chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == ">") {chi2RMSEA$ConsistencyMG.RMSEA[k] <- "Consistent"} else
        {if ((chi2RMSEA$MG.RMSEA[k] < chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == "<") {chi2RMSEA$ConsistencyMG.RMSEA[k] <- "Consistent"} else {chi2RMSEA$ConsistencyMG.RMSEA[k] <- "x"}}}
      } else {chi2RMSEA$ConsistencyMG.RMSEA[k] <- "-"}
      }
      
      }
      
      # -------------------------------------------------------------------------------------
      
      if (length(N) != length(Chi2)){ # if Ns were not given in the result ++++====
      
      N <- NA
      
      # Get Ns from written text numbers
      word <- (unlist(str_extract_all(txt, regex("(((the|a|one|two|three|four|five|six|seven|eight|nine)?\\s)?hundred\\s(and\\s(ten\\W)?)?)?((thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|eleven|twelve)|(((twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(\\-?))?(one|two|three|four|five|six|seven|eight|nine)?))?|\\Wten\\W", ignore_case = T))))
      word <- unlist(lapply(word, function(x) x[nchar(x) >= 1]))
      word <- word %>% str_replace_all(c("(a|the) hundred" = "hundred", " and " = " ", "-" = " ", "ten\\s" = "ten", "(ty)(\\w)" = "\\1 \\2")) %>% unlist()

      
      num <- data.frame(N = rep(NA, length(word)), N.Raw = rep(NA, length(word)))
      for (n in 1:length(word)){
        num$N[n] <- unlist(word2num(word[n])[2]) %>% as.character() %>% as.numeric()
        num$N.Raw[n] <- unlist(word2num(word[n])[1])
      }
      num <- num[!(as.numeric(num$N) < 10),]
      num <- num[!duplicated(num$N),]
      
      
      # Get location of sample size (from all the integers in the article)
      N.Raw <- str_extract_all(txt, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?(\\d+\\,)?\\d{2,3}\\)?\\,?\\s)|((?!\\d+)\\w+\\,?\\s(?!0)(\\d+\\,)?\\d{2,3}\\s(?!\\d+)(?!(degrees|a\\s))\\w+)", ignore_case = T)) # search for numbers and get the location in the article
      N.Raw <- unlist(N.Raw[!is.na(N.Raw)])
      N.Raw <- unlist(N.Raw[!duplicated(N.Raw)])

      # Get Ns 
      N <- unlist(str_extract_all(unlist(N.Raw), regex("\\W(\\d+\\,)?\\d+\\W")))
      N <- unlist(str_extract_all(unlist(N), regex("(\\d+\\,)?\\d+")))
      N <- unlist(N[!is.na(N)])
      
      # Get N.Raws from written text and numbers
      N.Raw <- list(N.Raw,num$N.Raw) %>% unlist 
      
      # Combine text and numbers
      N <- list(N,num$N) %>% unlist
      N <- unlist(str_replace_all(unlist(N), "\\,", ""))

      # create a new data frame that can contain all variables
      if (length(ngroup) != 0){
      chi2RMSEA <- data.frame(Chi2 = rep(Chi2, length(N.Raw)*length(ngroup)), df = rep(df, length(N.Raw)*length(ngroup)), Reported.RMSEA = rep(RMSEA, length(N.Raw)*length(ngroup)), Chi2.Raw = rep(unlist(Chi2.Raw), length(N.Raw)*length(ngroup)), Multi.group = rep(ngroup, each = length(Chi2), times = length(N.Raw)))
      
      # add a column for N.Raw (locations)
      chi2RMSEA$N.Raw = rep(N.Raw, each = length(Chi2)*length(ngroup))
      
      # add a column for N
      chi2RMSEA$N = rep(N, each = length(Chi2)*length(ngroup))
      }
      
      if (length(ngroup) == 0){
      chi2RMSEA <- data.frame(Chi2 = rep(Chi2, length(N.Raw)), df = rep(df, length(N.Raw)), Reported.RMSEA = rep(RMSEA, length(N.Raw)), Chi2.Raw = rep(unlist(Chi2.Raw), length(N.Raw)), Multi.group = rep("-", each = length(N.Raw)*length(Chi2)))
     
       # add a column for N.Raw (locations)
      chi2RMSEA$N.Raw = rep(N.Raw, each = length(Chi2))
      
      # add a column for N
      chi2RMSEA$N = rep(N, each = length(Chi2))
      }

      
      #Number of Ns found in the article
      if (length(ngroup) != 0){
      chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)/(length(Chi2)*length(ngroup))
      } else {chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)/(length(Chi2))}
      
      #Number of models found in the article
      chi2RMSEA$Total.Models <- length(unlist(Chi2.Raw))
      
      

      chi2RMSEA$Chi2 <- chi2RMSEA$Chi2 %>% as.character() %>% as.numeric()
      chi2RMSEA$df <- chi2RMSEA$df %>% as.character() %>% as.numeric()
      chi2RMSEA$N <- chi2RMSEA$N %>% as.character() %>% as.numeric()
      chi2RMSEA$Reported.RMSEA <- chi2RMSEA$Reported.RMSEA %>% as.character() %>% as.numeric()
      
      if (length(ngroup) != 0){
      chi2RMSEA$Multi.group <- chi2RMSEA$Multi.group %>% as.character() %>% as.numeric()
      } else {chi2RMSEA$Multi.group <- "-"}
      
      chi2RMSEA$Sign = sign
      
      chi2RMSEA$rmsea <- NA
      chi2RMSEA$RMSEA <- NA
      chi2RMSEA$MG.rmsea <- NA
      chi2RMSEA$MG.RMSEA <- NA


      # compute RMSEA
      for (l in 1:nrow(chi2RMSEA)){
        deci <- decimalplaces(chi2RMSEA$Reported.RMSEA[l])
        chi2RMSEA$rmsea[l] <- sqrt(max(c((chi2RMSEA$Chi2[l]-chi2RMSEA$df[l])/(chi2RMSEA$df[l]*(chi2RMSEA$N[l]-1)),0)))
        chi2RMSEA$RMSEA[l] <- round((chi2RMSEA$rmsea[l]),deci)
        if (length(ngroup) != 0){
          chi2RMSEA$MG.rmsea[l] <- sqrt(max(c((chi2RMSEA$Chi2[l]-chi2RMSEA$df[l])/(chi2RMSEA$df[l]*(chi2RMSEA$N[l]-chi2RMSEA$Multi.group[l])),0)))*sqrt(chi2RMSEA$Multi.group[l])
          chi2RMSEA$MG.RMSEA[l] <- round((chi2RMSEA$MG.rmsea[l]),deci)
        }
        if (length(ngroup) == 0){
          chi2RMSEA$MG.rmsea[l] <- "-"
          chi2RMSEA$MG.RMSEA[l] <- "-"
        }
      }
    
          
      chi2RMSEA$ConsistencyRMSEA <- NA # test if the reported and computed RMSEAs are the same (same amount of digits as reported)
      chi2RMSEA$ConsistencyMG.RMSEA <- NA
      for (m in 1:nrow(chi2RMSEA)){
        if ((chi2RMSEA$RMSEA[m] == chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == "=") {chi2RMSEA$ConsistencyRMSEA[m] <- "Consistent"} else
        {if ((chi2RMSEA$RMSEA[m] > chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == ">") {chi2RMSEA$ConsistencyRMSEA[m] <- "Consistent"} else
        {if ((chi2RMSEA$RMSEA[m] < chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == "<") {chi2RMSEA$ConsistencyRMSEA[m] <- "Consistent"} else {chi2RMSEA$ConsistencyRMSEA[m] <- "x"}}}
        
        if (length(ngroup) != 0){
        if ((chi2RMSEA$MG.RMSEA[m] == chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == "=") {chi2RMSEA$ConsistencyMG.RMSEA[m] <- "Consistent"} else
        {if ((chi2RMSEA$MG.RMSEA[m] > chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == ">") {chi2RMSEA$ConsistencyMG.RMSEA[m] <- "Consistent"} else
        {if ((chi2RMSEA$MG.RMSEA[m] < chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == "<") {chi2RMSEA$ConsistencyMG.RMSEA[m] <- "Consistent"} else {chi2RMSEA$ConsistencyMG.RMSEA[m] <- "x"}}}
        } else {chi2RMSEA$ConsistencyMG.RMSEA[m] <- "-"}
         }

      } # for length(Chi2) != length(N)
              
         chi2RMSEA$Source = names(x)[i]
              
            }}}  else {if (length(Chi2) != length(df)| length(RMSEA) != length(Chi2)) 
              {chi2RMSEA <- data.frame(Source = names(x)[i], Chi2 = NA, df = NA, N = NA, Multi.group = NA, rmsea = NA,
                                       RMSEA = NA, MG.rmsea = NA, MG.RMSEA = NA, Sign = NA, Reported.RMSEA = NA, ConsistencyRMSEA = NA,
                                       ConsistencyMG.RMSEA = NA, Chi2.Raw = NA, N.Raw = NA, Total.Ns = NA, Total.Models = NA)}}
        }  
      }# for if loop

          # Append, clean and close:
          Res <- rbind(Res, chi2RMSEA)
          rm(chi2RMSEA)
          
    }
      

      #----------------------
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    Source <- NULL
    
  
  
    ###---------------------------------------------------------------------
    
    ### NOTE: adapt to match the empty data frame at the top of the code, and the variables you extracted in the step chi2RMSEA 
    
    # final data frame
    Res <- data.frame(
      Source = Res$Source,
      Chi2 = Res$Chi2,
      df = Res$df,
      N = Res$N,
      Multi.group = Res$Multi.group,
      RMSEA = Res$RMSEA,
      MG.RMSEA = Res$MG.RMSEA,
      Sign = Res$Sign,
      Reported.RMSEA = Res$Reported.RMSEA,
      ConsistencyRMSEA = Res$ConsistencyRMSEA,
      ConsistencyMG.RMSEA = Res$ConsistencyMG.RMSEA,
      Chi2.Raw = Res$Chi2.Raw,
      N.Raw = Res$N.Raw,
      Total.Ns = Res$Total.Ns,
      Total.Models = Res$Total.Models
    )
    
    class(Res) <- c("checkRMSEA", "data.frame")

  
    ###---------------------------------------------------------------------
    
    # Return message when there are no results
    if (nrow(Res) > 0) {
      write.csv(Res, file = "checkRMSEA results.csv", na = "NA")
    } 
    }
    
    
  
