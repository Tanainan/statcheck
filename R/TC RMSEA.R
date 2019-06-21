library(plyr)
library(dplyr)
library(stringr)

source("~/Downloads/StatCheck/StatCheck/R/getPDF.R")
source("~/Downloads/StatCheck/StatCheck/R/numbers.R")

checkRMSEA <-
  function(x) {{
    
    # Create empty data frame for main result:
    Res <-
      data.frame(NULL)
    
    class(Res) <- c("statcheck", "data.frame")
    
    if (length(x) == 0)
      return(Res)
    
    if (is.null(names(x)))
      names(x) <-  1:length(x)
    
    message("Extracting statistics...")
    pb <- txtProgressBar(max = length(x), style = 3)
    for (i in 1:length(x)) {
      txt <- x[i]
      
      #---------------------------
      
      #txt <- b
      
      txt <- unlist(str_replace_all(txt, "1/4", "=")) # replace "1/4" to "="
      
      
      # extract Chis2-values by locating RMSEA first:
      chi2RMSEALoc <- str_subset(txt, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      chi2RMSEALoc <- str_replace(unlist(chi2RMSEALoc), "  ", " ") #change from doulble-spacing to single-spacing between the text

      
      # Get location of chi values in text:
      chi2Loc1 <-
        str_extract_all(unlist(chi2RMSEALoc), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)(?!D)[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,230})",
                                                    ignore_case = TRUE))
      # Raw text of Chi2
      chi2Loc10 <-
        str_extract_all(unlist(chi2Loc1), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)(?!D)[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,50})",
                                                    ignore_case = TRUE))
      
      # Get chi value
      chi2Loc2 <-
        str_extract_all(unlist(chi2Loc1), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)(?!D)[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+))",
                                                ignore_case = TRUE))
      Chi2 <- str_extract(unlist(chi2Loc2), regex("\\d*\\.\\d+"))
        
      # Extract df:
      ddd <- str_extract_all(unlist(chi2Loc10), regex("((df|d|d.f.)\\s?([<>=]|(1/4))|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?\\,?|\\s(?!0)\\d+\\s(df|degrees of freedom)|\\(\\d+\\)", ignore_case = T))
      if (length(unlist(ddd)) != length(unlist(Chi2))){
        ddd <- str_extract_all(unlist(chi2Loc2), regex("((df|d|d.f.)\\s?([<>=]|(1/4))|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?\\,?|\\s(?!0)\\d+\\s(df|degrees of freedom)|\\(\\d+\\)", ignore_case = T))
      }
      ddd <- ddd[!is.na(ddd)]
      #ddd <- str_replace(unlist(ddd), "1/4", "")
      df <- str_extract_all(unlist(ddd), regex("(?!2\\s?\\()\\d+"))
      df <- unlist(df[!is.na(df)])

      
    
      # Get RMSEA
      sss <- str_extract_all(unlist(chi2Loc1), regex("((chi-square |chi-square of|\\s?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,300}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,40}\\s(\\d*(\\.|\\:)?\\d*))|((root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,300}(chi-square |chi-square of|\\s?[vcw]2\\s?\\(|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,20}(\\d*(\\.|\\:)\\d+))", ignore_case = T))
      sss1 <- str_extract_all(unlist(sss), regex("((chi-square |chi-square of|\\s?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,80}\\d+(.){0,80}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,40}\\s(\\d*(\\.|\\:)?\\d*))|((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,80}\\d+(.){0,80}(chi-square |chi-square of|\\s?[vcw]2\\s?\\(|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,20}(\\d*(\\.|\\:)\\d+))", ignore_case = T))
      rrr <- str_extract_all(unlist(sss1), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,10}\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
      rrr <- rrr[!is.na(rrr)]
      rrr <- str_replace(unlist(rrr), ":", ".")
      #rrr <- str_replace(unlist(rrr), "1/4", "=")
      RMSEA <- str_extract_all(unlist(rrr), regex("\\.\\d+"))
      RMSEA <- unlist(RMSEA[!is.na(RMSEA)])
      
      # See if reported RMSEA has =,< or > sign
      #sign <- str_extract_all(unlist(rrr), regex("[=<>]|less than|greater than|equals to|equal to|1/4|\\s\\s"))
      sign <- str_extract_all(unlist(rrr), regex("[=<>]|less than|greater than|equals to|equal to|\\s\\s"))
      sign <- sign[!is.na(sign)]
      sign <- sign %>% str_replace_all(c("less than" = "<", "greater than" = ">", "(equals|equal) to" = "=", "  " = "=")) %>% unlist()
          
      
      
      # Get N from when it is reported in the result
      #nnn <- str_extract_all(unlist(chi2Loc1), regex("(\\Wn\\s?(\\=|equals to|equal to|equal|equals|(1/4))?\\s?\\d+)", ignore_case = T))
      nnn <- str_extract_all(unlist(chi2Loc1), regex("(\\Wn\\s?(\\=|equals to|equal to|equal|equals)?\\s?\\d+)", ignore_case = T))
      nnn <- nnn[!is.na(nnn)]
      #nnn <- str_replace(unlist(nnn), "1/4", "")
      N <- str_extract_all(unlist(nnn), regex("\\d+"))
      N <- unlist(N[!is.na(N)])
      
      
      # add a function to count decimal places for RMSEA
      decimalplaces <- function(x) {
        if (abs(x - round(x)) > .Machine$double.eps^0.5) {
          nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
        } else {
          return(0)
        }
      }
      
      # check if there're multi-group models and extract number of groups from adjacent words 
      multi <- unlist(str_detect(txt, regex("multigroup|multi-group|multiple (groups|group)|multi-sample|multiple (samples|sample)|multisample", ignore_case = T)))
      if (length(which(multi == TRUE)) >= 1){
      ngroup <- unlist(str_extract_all(txt, regex("(.){1,15}(groups)(?!\\.)(.){1,15}", ignore_case = T)))
      ngroup <- unlist(str_extract_all(ngroup, regex("\\d+|one|two|three|four|five|six|seven|eight|nine|\\sten\\s")))
      ngroup <- ngroup %>% str_replace_all(c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "\\sten\\s" = "10")) %>% unlist()
      ngroup <- ngroup[!duplicated(ngroup)]
      } else {ngroup = NULL}

      
      # ++++++++++++++++++++++++++++++++++++
      
      
      if (length(N) == length(Chi2)){ # if Ns are given in the result
      
      # create a new data frame that can contain all variables
      if (length(ngroup) != 0){
        chi2RMSEA <- data.frame(Chi2 = rep(Chi2, length(ngroup)), df = rep(df, length(ngroup)), N = rep(N, length(ngroup), Reported.RMSEA = rep(RMSEA, length(ngroup)), Chi2.Raw = rep(unlist(chi2Loc10), length(ngroup)), Multi.group = rep(ngroup, each = length(Chi2)), N.Raw = rep(unlist(nnn), length(ngroup))))
      }
      if (length(ngroup) == 0){
        chi2RMSEA <- data.frame(Chi2 = Chi2, df = df, N = N, Reported.RMSEA = RMSEA, Chi2.Raw = unlist(chi2Loc10), Multi.group = rep("-", length(Chi2)), N.Raw = unlist(nnn))
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
      chi2RMSEA$Total.Models <- length(unlist(chi2Loc10))

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
      
      # some of the computed RMSEAs are NaN >>> change them to 0
      #chi2RMSEA$Computed[is.nan(chi2RMSEA$Computed)] <- 0
      

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
      
      #chi2RMSEA$Source = names(x)[i]
      
      
      chi2RMSEA <- chi2RMSEA[,c(#"Source",
                                "Chi2","df","N","Multi.group","rmsea","RMSEA","MG.rmsea","MG.RMSEA","Sign","Reported.RMSEA","ConsistencyRMSEA","ConsistencyMG.RMSEA","Chi2.Raw","N.Raw","Total.Ns","Total.Models")] # change columns order
      }
      
      # -------------------------------------------------------------------------------------
      
      if (length(N) != length(Chi2)){ # if Ns are not given in the result

      N <- NA
      
      # Get Ns from written text numbers
      word <- (unlist(str_extract_all(txt, regex("(((the|a|one|two|three|four|five|six|seven|eight|nine)?\\s)?hundred\\s(and\\s)?)?((thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|ten|eleven|twelve)|(((twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(\\-?))?(one|two|three|four|five|six|seven|eight|nine)?))?", ignore_case = T))))
      word <- unlist(lapply(word, function(x) x[nchar(x) >= 1]))
      word <- word %>% str_replace_all(c("(?!\\w+)\\s?ten\\s?" = "ten", "(a|the) hundred" = "hundred", " and " = " ", "-" = " "))

      
      num <- data.frame(N = rep(NA, length(word)), N.Raw = rep(NA, length(word)))
      for (i in 1:length(word)){
        num$N[i] <- unlist(word2num(word[i])[2]) %>% as.character() %>% as.numeric()
        num$N.Raw[i] <- unlist(word2num(word[i])[1])
      }
      num <- num[!(as.numeric(num$N) < 10 | as.numeric(num$N) >= 1500),]
      num <- num[!duplicated(num$N),]
      
      
      # Get location of sample size (from all the integers in the article)
      N.Raw <- str_extract_all(txt, regex("(n\\s?(\\=|equals to|equal to|equal|equals|(1/4))\\s?\\d\\d+\\,?\\s)|((?!\\d+)\\w+\\,?\\s(?!0)\\d\\d+\\s(?!\\d+)(?!(degrees|Jan|Feb|March|April|May|June|July|Augu|Sep|Oct|Nov|Dec))\\w+)", ignore_case = T)) # search for numbers and get the location in the article
      N.Raw <- unlist(N.Raw[!is.na(N.Raw)])
      #N.Raw <- str_replace(unlist(N.Raw), "1/4", "=")
      
      # Get N.Raws from written text numbers
      N.Raw <- list(N.Raw,num$N.Raw) %>% unlist 
      
      
      # Get Ns 
      N <- str_extract_all(unlist(N.Raw), regex("\\d+"))
      N <- unlist(N[!is.na(N)])
      
      # Combine text and numbers
      N <- list(N,num$N) %>% unlist 
      
      # create a new data frame that can contain all variables
      if (length(ngroup) != 0){
      chi2RMSEA <- data.frame(Chi2 = rep(Chi2, length(N.Raw)*length(ngroup)), df = rep(df, length(N.Raw)*length(ngroup)), Reported.RMSEA = rep(RMSEA, length(N.Raw)*length(ngroup)), Chi2.Raw = rep(unlist(chi2Loc10), length(N.Raw)*length(ngroup)), Multi.group = rep(ngroup, each = length(Chi2), times = length(N.Raw)))
      
      # add a column for N.Raw (locations)
      chi2RMSEA$N.Raw = rep(N.Raw, each = length(Chi2)*length(ngroup))
      
      # add a column for N
      chi2RMSEA$N = rep(N, each = length(Chi2)*length(ngroup))
      }
      
      if (length(ngroup) == 0){
      chi2RMSEA <- data.frame(Chi2 = rep(Chi2, length(N.Raw)), df = rep(df, length(N.Raw)), Reported.RMSEA = rep(RMSEA, length(N.Raw)), Chi2.Raw = rep(unlist(chi2Loc10), length(N.Raw)), Multi.group = rep("-", each = length(N.Raw)*length(Chi2)))
     
       # add a column for N.Raw (locations)
      chi2RMSEA$N.Raw = rep(N.Raw, each = length(Chi2))
      
      # add a column for N
      chi2RMSEA$N = rep(N, each = length(Chi2))
      }
      

      
      chi2RMSEA <- chi2RMSEA[!(as.numeric(chi2RMSEA$N) < 10 | as.numeric(chi2RMSEA$N) >= 1500),] # only select Ns that are greater than or equal to 10 and less than or equal to 1500
      
      
      #Number of Ns found in the article
      if (length(ngroup) != 0){
      chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)/(length(Chi2)*length(ngroup))
      } else {chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)/(length(Chi2))}
      
      #Number of models found in the article
      chi2RMSEA$Total.Models <- length(unlist(chi2Loc10))
      
      

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
      
      # some of the computed RMSEAs are NaN >>> change them to 0
      #chi2RMSEA$Computed[is.nan(chi2RMSEA$Computed)] <- 0

          
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
      
      #chi2RMSEA$Source = names(x)[i]
    
      chi2RMSEA <- chi2RMSEA[,c(#"Source",
                                "Chi2","df","N","Multi.group","rmsea","RMSEA","MG.rmsea","MG.RMSEA","Sign","Reported.RMSEA","ConsistencyRMSEA","ConsistencyMG.RMSEA","Chi2.Raw","N.Raw","Total.Ns","Total.Models")]
      }    
        
          # Append, clean and close:
          Res <- rbind(Res, chi2RMSEA)
          rm(chi2RMSEA)
          
    }
      
      #----------------------
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    Source <- NULL
    Res <- ddply(Res, .(Source), function(x)
      x[order(x$Chi2.Raw), ])
  
  
    ###---------------------------------------------------------------------
    
    ### NOTE: adapt to match the empty data frame at the top of the code, and the variables you extracted in the step chi2RMSEA 
    
    # final data frame
    Res <- data.frame(
      Source = Res$Source,
      Chi2 = Res$Chi2,
      df = Res$df,
      N = Res$N,
      Reported.RMSEA = Res$Reported.RMSEA,
      Computed.RMSEA = Res$Computed.RMSEA,
      Consistency = Res$Consistency
    )
    
    class(Res) <- c("statcheck", "data.frame")
    
    
    ###---------------------------------------------------------------------
    
    # Return message when there are no results
    if (nrow(Res) > 0) {
      return(Res)
    } else {
      Res <- cat("statcheck did not find any results\n")
    }}
    
    
  
