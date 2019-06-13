library(plyr)
library(words2number)
library(dplyr)
statcheck <-
  function(x) {{
    
    # Create empty data frame for main result:
    Res <-
      data.frame(
        Source = NULL,
        Chi2 = NULL,
        df = NULL,
        N = NULL,
        Reported.RMSEA = NULL,
        Computed.RMSEA = NULL,
        Sign = NULL,
        Consistency = NULL
        #Location = NULL
      )
    
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
      
      txt <- aa
      
      
      # extract Chis2-values by locating RMSEA first:
      chi2RMSEALoc <- str_subset(txt, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      chi2RMSEALoc <- str_replace(unlist(chi2RMSEALoc), "  ", " ") #change from doulble-spacing to single-spacing between the text

      
      # Get location of chi values in text:
      chi2Loc1 <-
        str_extract_all(unlist(chi2RMSEALoc), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,230})",
                                                    ignore_case = TRUE))
      # Raw text of Chi2
      chi2Loc10 <-
        str_extract_all(unlist(chi2Loc1), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,100})",
                                                    ignore_case = TRUE))
      
      # Get chi value
      chi2Loc2 <-
        str_extract_all(unlist(chi2Loc1), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+))",
                                                ignore_case = TRUE))
      Chi2 <- str_extract(unlist(chi2Loc2), regex("\\d*\\.\\d+"))
        
      # Extract df:
      ddd <- str_extract_all(unlist(chi2Loc1), regex("((df|d|d.f.)\\s?([<>=]|(1/4))|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?\\,?|\\s(?!0)\\d+\\s(df|degrees of freedom)|\\(\\d+\\)", ignore_case = T))
      ddd <- ddd[!is.na(ddd)]
      ddd <- str_replace(unlist(ddd), "1/4", "")
      df <- str_extract_all(unlist(ddd), regex("(?!2\\s?\\()\\d+"))
      df <- unlist(df[!is.na(df)])

      
    
      # Get RMSEA
      sss <- str_extract_all(unlist(chi2Loc1), regex("((chi-square |chi-square of|\\s?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,300}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,40}\\s(\\d*(\\.|\\:)?\\d*))|((root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,300}(chi-square |chi-square of|\\s?[vcw]2\\s?\\(|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,20}(\\d*(\\.|\\:)\\d+))", ignore_case = T))
      sss1 <- str_extract_all(unlist(sss), regex("((chi-square |chi-square of|\\s?[vcwx]2\\s?(=|\\(|of)|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,80}\\d+(.){0,80}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,40}\\s(\\d*(\\.|\\:)?\\d*))|((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,80}\\d+(.){0,80}(chi-square |chi-square of|\\s?[vcw]2\\s?\\(|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,20}(\\d*(\\.|\\:)\\d+))", ignore_case = T))
      rrr <- str_extract_all(unlist(sss1), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,10}\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
      rrr <- rrr[!is.na(rrr)]
      rrr <- str_replace(unlist(rrr), ":", ".")
      rrr <- str_replace(unlist(rrr), "1/4", "=")
      RMSEA <- str_extract_all(unlist(rrr), regex("\\.\\d+"))
      RMSEA <- unlist(RMSEA[!is.na(RMSEA)])
      
      # See if reported RMSEA has =,< or > sign
      sign <- str_extract_all(unlist(rrr), regex("[=<>]|less than|greater than|equals to|equal to|1/4|\\s\\s"))
      sign <- sign[!is.na(sign)]
      sign <- str_replace(unlist(sign), "less than", "<")
      sign <- str_replace(unlist(sign), "greater than", ">")
      sign <- str_replace(unlist(sign), "equals to", "=")
      sign <- str_replace(unlist(sign), "equal to", "=")
      sign <- str_replace(unlist(sign), "  ", "=")
      sign <- unlist(sign)
          
      
      
      # Get N from when it is reported in the result
      nnn <- str_extract_all(unlist(chi2Loc1), regex("(\\Wn\\s?(\\=|equals to|equal to|equal|equals|(1/4))?\\s?\\d+)", ignore_case = T))
      nnn <- nnn[!is.na(nnn)]
      nnn <- str_replace(unlist(nnn), "1/4", "")
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
      
      # check if there're multi-group models
      multi <- unlist(str_detect(chi2RMSEALoc, regex("multigroup|multi-group|multiple (sample|samples)|multi-sample|multisample|multiple (group|groups)", ignore_case = T)))
      multi <- unlist(str_replace_all(unlist(multi), "TRUE", "T"))
      multi <- unlist(str_replace_all(unlist(multi), "FALSE", "-"))


      if (length(N) == length(Chi2)){
      # Create data frame:
      chi2RMSEA <- data.frame(
          #Source = names(x)[i],
          Chi2 = Chi2,
          df = df,
          N = N,
          Reported.RMSEA = RMSEA,
          Chi2.Raw = unlist(chi2Loc10),
          N.Raw = nnn
        )
      
      chi2RMSEA$Multi.group <- NA
      if (length(which(multi == "T")) >= 1){chi2RMSEA$Multi.group <- "T"} else {chi2RMSEA$Multi.group <- "-"}
      
      
      chi2RMSEA$Chi2 <- chi2RMSEA$Chi2 %>% as.character() %>% as.numeric()
      chi2RMSEA$df <- chi2RMSEA$df %>% as.character() %>% as.numeric()
      chi2RMSEA$N <- chi2RMSEA$N %>% as.character() %>% as.numeric()
      chi2RMSEA$Reported.RMSEA <- chi2RMSEA$Reported.RMSEA %>% as.character() %>% as.numeric()
      

      chi2RMSEA$Computed <- NA
      chi2RMSEA$Computed.RMSEA <- NA
      chi2RMSEA$lavaan.RMSEA <- NA

      
      #Number of Ns found in the article
      chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)
      
      #Number of models found in the article
      chi2RMSEA$Total.Models <- length(unlist(chi2Loc10))

      chi2RMSEA$Sign = sign
      
      for (j in 1:nrow(chi2RMSEA)){
        deci <- decimalplaces(chi2RMSEA$Reported.RMSEA[j])
        chi2RMSEA$Computed[j] <- ((sqrt(as.numeric(chi2RMSEA$Chi2[j])-as.numeric(chi2RMSEA$df[j])))/(sqrt(as.numeric(chi2RMSEA$df[j])*(as.numeric(chi2RMSEA$N[j])-1))))
        chi2RMSEA$Computed.RMSEA[j] <- round((chi2RMSEA$Computed[j]),deci)
      }
      
      # some of the computed RMSEAs are NaN >>> change them to 0
      chi2RMSEA$Computed[is.nan(chi2RMSEA$Computed)] <- 0
      
      # lavaan RMSEA
      for (m in 1:nrow(chi2RMSEA)){
        chi2RMSEA$lavaan.RMSEA[m] <- sqrt(max(c((as.numeric(chi2RMSEA$Chi2[m])/as.numeric(chi2RMSEA$N[m]))/as.numeric(chi2RMSEA$df[m]) - 1/as.numeric(chi2RMSEA$N[m]),0)))
      }
      
      # discrepency regular and lavaan RMSEA
      chi2RMSEA$discrepancy <- round((chi2RMSEA$Computed - chi2RMSEA$lavaan.RMSEA), 6)

      # some of the computed RMSEAs are NaN >>> change them to 0
      chi2RMSEA$Computed.RMSEA[is.nan(chi2RMSEA$Computed.RMSEA)] <- 0
      

      chi2RMSEA$Consistency <- NA # test if the reported and computed RMSEAs are the same (2 digits)
      for (k in 1:nrow(chi2RMSEA)){
        if ((chi2RMSEA$Computed.RMSEA[k] == chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == "=") {chi2RMSEA$Consistency[k] <- "Consistent"} else
        {if ((chi2RMSEA$Computed.RMSEA[k] > chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == ">") {chi2RMSEA$Consistency[k] <- "Consistent"} else
        {if ((chi2RMSEA$Computed.RMSEA[k] < chi2RMSEA$Reported.RMSEA[k]) & chi2RMSEA$Sign[k] == "<") {chi2RMSEA$Consistency[k] <- "Consistent"} else {chi2RMSEA$Consistency[k] <- "x"}}}
      }
      
      
      
      chi2RMSEA <- chi2RMSEA[,c(#"Source",
                                "Chi2","df","N","Multi.group","lavaan.RMSEA","Computed","discrepancy","Computed.RMSEA","Sign","Reported.RMSEA","Consistency","Chi2.Raw","N.Raw","Total.Ns","Total.Models")] # change columns order
      }
      
      # -------------------------------------------------------------------------------------
      
      if (length(N) != length(Chi2)){
        # Create data frame:
        chi2RMSEA <- data.frame(
          #Source = names(x)[i],
          Chi2 = Chi2,
          df = df,
          Reported.RMSEA = RMSEA,
          Chi2.Raw = unlist(chi2Loc10)
        )
        
        chi2RMSEA$Multi.group <- NA
        if (length(which(multi == "T")) >= 1){chi2RMSEA$Multi.group <- "T"} else {chi2RMSEA$Multi.group <- "-"}
        
   
      N <- NA
      
      # Get Ns from written text numbers
      word <- (unlist(str_extract_all(txt, regex("(((the|a|one|two|three|four|five|six|seven|eight|nine)?\\s)?hundred\\s(and\\s)?)?((thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|ten|eleven|twelve)|(((twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(\\-?))?(one|two|three|four|five|six|seven|eight|nine)?))?", ignore_case = T))))
      word <- unlist(lapply(word, function(x) x[nchar(x) >= 1]))
      word <- str_replace_all(word, "(?!\\w+)\\s?ten\\s?", "ten")
      word <- str_replace_all(word, "(a|the) hundred", "hundred")
      word <- str_replace_all(word, " and ", " ")
      word <- str_replace_all(word, "-", " ")
      
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
      N.Raw <- str_replace(unlist(N.Raw), "1/4", "=")
      
      # Get N.Raws from written text numbers
      N.Raw <- list(N.Raw,num$N.Raw) %>% unlist 
      
      # create a new data frame that can contain all variables
      chi2RMSEA <- data.frame(Chi2 = rep(chi2RMSEA$Chi2, length(N.Raw)), df = rep(chi2RMSEA$df,length(N.Raw)), Reported.RMSEA = rep(chi2RMSEA$Reported.RMSEA, length(N.Raw)), Chi2.Raw = rep(chi2RMSEA$Chi2.Raw, length(N.Raw)), Multi.group = rep(chi2RMSEA$Multi.group, length(N.Raw)))
      
      
      # Get Ns 
      N <- str_extract_all(unlist(N.Raw), regex("\\d+"))
      N <- unlist(N[!is.na(N)])
      
      # Combine text and numbers
      N <- list(N,num$N) %>% unlist 
      

      # add a column for N locations
      chi2RMSEA$N.Raw = rep(N.Raw, each = length(Chi2))
      
      # add a column for N
      chi2RMSEA$N = rep(N, each = length(Chi2))
      
      chi2RMSEA <- chi2RMSEA[!(as.numeric(chi2RMSEA$N) < 10 | as.numeric(chi2RMSEA$N) >= 1500),] # only select Ns that are greater than or equal to 10 and less than or equal to 1500
      
      
      #Number of Ns found in the article
      chi2RMSEA$Total.Ns <- length(chi2RMSEA$N)/length(Chi2)
      
      #Number of models found in the article
      chi2RMSEA$Total.Models <- length(unlist(chi2Loc10))

      chi2RMSEA$Chi2 <- chi2RMSEA$Chi2 %>% as.character() %>% as.numeric()
      chi2RMSEA$df <- chi2RMSEA$df %>% as.character() %>% as.numeric()
      chi2RMSEA$N <- chi2RMSEA$N %>% as.character() %>% as.numeric()
      chi2RMSEA$Reported.RMSEA <- chi2RMSEA$Reported.RMSEA %>% as.character() %>% as.numeric()
      
      chi2RMSEA$Sign = sign
      
      chi2RMSEA$Computed <- NA
      chi2RMSEA$Computed.RMSEA <- NA
      chi2RMSEA$lavaan.RMSEA <- NA


      # "regular" RMSEA
      for (l in 1:nrow(chi2RMSEA)){
        deci <- decimalplaces(chi2RMSEA$Reported.RMSEA[l])
        chi2RMSEA$Computed[l] <- ((sqrt(as.numeric(chi2RMSEA$Chi2[l])-as.numeric(chi2RMSEA$df[l])))/(sqrt(as.numeric(chi2RMSEA$df[l])*(as.numeric(chi2RMSEA$N[l])-1))))
        chi2RMSEA$Computed.RMSEA[l] <- round((chi2RMSEA$Computed[l]),deci)
      }
      
      # some of the computed RMSEAs are NaN >>> change them to 0
      chi2RMSEA$Computed[is.nan(chi2RMSEA$Computed)] <- 0
      
      # lavaan RMSEA
      for (m in 1:nrow(chi2RMSEA)){
        chi2RMSEA$lavaan.RMSEA[m] <- sqrt(max(c((as.numeric(chi2RMSEA$Chi2[m])/as.numeric(chi2RMSEA$N[m]))/as.numeric(chi2RMSEA$df[m]) - 1/as.numeric(chi2RMSEA$N[m]),0)))
      }
      
      # discrepency regular and lavaan RMSEA
      chi2RMSEA$discrepancy <- round((chi2RMSEA$Computed - chi2RMSEA$lavaan.RMSEA), 6)
      
      # some of the computed RMSEAs are NaN >>> change them to 0
      chi2RMSEA$Computed.RMSEA[is.nan(chi2RMSEA$Computed.RMSEA)] <- 0

          
      chi2RMSEA$Consistency <- NA # test if the reported and computed RMSEAs are the same (same amount of digits as reported)
      for (m in 1:nrow(chi2RMSEA)){
        if ((chi2RMSEA$Computed.RMSEA[m] == chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == "=") {chi2RMSEA$Consistency[m] <- "Consistent"} else
        {if ((chi2RMSEA$Computed.RMSEA[m] > chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == ">") {chi2RMSEA$Consistency[m] <- "Consistent"} else
        {if ((chi2RMSEA$Computed.RMSEA[m] < chi2RMSEA$Reported.RMSEA[m]) & chi2RMSEA$Sign[m] == "<") {chi2RMSEA$Consistency[m] <- "Consistent"} else {chi2RMSEA$Consistency[m] <- "x"}}}
      }
      
    
      chi2RMSEA <- chi2RMSEA[,c(#"Source",
                                "Chi2","df","N","Multi.group","lavaan.RMSEA","Computed","discrepancy","Computed","Computed.RMSEA","Sign","Reported.RMSEA","Consistency","Chi2.Raw","N.Raw","Total.Ns","Total.Models")]
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
    
    
  
