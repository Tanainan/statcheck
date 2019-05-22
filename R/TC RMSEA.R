library(plyr)
library(words2number)
library(dplyr)
statcheck <-
  function(x) {{
    
    # Create empty data frame for main result:
    Res <-
      data.frame(
        Source = NULL
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
      
      
      # extract Chis2-values by locating RMSEA first:
      chi2RMSEA <- str_subset(p, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      
      
      # Get location of chi values in text:
      chi2Loc <-
        str_extract_all(chi2RMSEA, regex("((chi-square |chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s\\d*(\\.|\\:)\\d+))",
          ignore_case = TRUE))
      

      # Get chi value
      Chi2 <- str_extract(unlist(chi2Loc), regex("\\d*\\.\\d+"))
        
        # Extract df:
      dfdf <- str_extract_all(chi2RMSEA, regex("(chi-square |chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,30}((((degrees of freedom of)\\s(?!0)\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?(?!0)\\d+\\s?)|(\\s(?!0)\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\((?!0)\\d+\\)))", ignore_case = T))
      ddd <- str_extract_all(unlist(dfdf), regex("((((degrees of freedom of)\\s(?!0)\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?(?!0)\\d+\\s?)|(\\s(?!0)\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\((?!0)\\d+\\)))", ignore_case = T))
      ddd <- ddd[!is.na(ddd)]
      df <- str_extract_all(unlist(ddd), regex("(?!2\\()\\d+"))
      df <- unlist(df[!is.na(df)])
    
      # Get RMSEA
      mmm <- str_extract_all(chi2RMSEA, regex("((chi-square |chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      rrr <- str_extract_all(unlist(mmm), regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
      rrr <- rrr[!is.na(rrr)]
      RMSEA <- str_extract_all(unlist(rrr), regex("\\.\\d+"))
      RMSEA <- unlist(RMSEA[!is.na(RMSEA)])
          
      # Create data frame:
      chi2Res <- data.frame(
          #Source = names(x)[i],
          Chi2 = Chi2,
          df = df,
          Reported.RMSEA = RMSEA 
        )
          
      N <- NA
          
      # Get location of sample size (from all the integers in the article)
      aaa <- str_extract_all(txt, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?\\d\\d+\\s)|(\\w+\\s(?!0)\\d\\d+\\s\\w+)", ignore_case = T)) # search for numbers and get the location in the article
      aaa <- aaa[!is.na(aaa)]
      N <- str_extract_all(unlist(aaa), regex("\\d+"))
      N <- unlist(N[!is.na(N)])
      N <- N[!duplicated(N)] # remove duplicate Ns
      N <- N[which(as.numeric(N) >= 10)] # only select Ns that are greater than or equal to 10
          
      # create a new data frame that can contain all variables
      chi2RMSEA <- data.frame(Chi2 = rep(chi2Res$Chi2, length(N)), df = rep(chi2Res$df,length(N)), Reported.RMSEA = rep(chi2Res$Reported.RMSEA, length(N)))
          
      # add a column for N
      chi2RMSEA$N = rep(N, each = length(Chi2))
          
      chi2RMSEA$Computed.RMSEA <- NA
          
      chi2RMSEA <- data.frame(sapply(chi2RMSEA, function(x) as.numeric(as.character(x))))
 
      for (i in 1:nrow(chi2RMSEA)){
       chi2RMSEA$Computed.RMSEA[i] <- round(((sqrt(as.numeric(chi2RMSEA$Chi2[i])-as.numeric(chi2RMSEA$df[i])))/(sqrt(as.numeric(chi2RMSEA$df[i])*(as.numeric(chi2RMSEA$N[i])-1)))),2)
      }
          
      chi2RMSEA$Consistency <- NA # test if the reported and computed RMSEAs are the same (2 digits)
      for (i in 1:nrow(chi2RMSEA)){
        if (chi2RMSEA$Reported.RMSEA[i] == chi2RMSEA$Computed.RMSEA[i]) {chi2RMSEA$Consistency[i] <- "Consistent"} else {chi2RMSEA$Consistency[i] <- "Inconsistent"}
      }
          
      }
          # Append, clean and close:
          # Res <- rbind(Res, chi2Res)
          # rm(chi2Res)
          
    }
      
      #----------------------
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    Source <- NULL
    Res <- ddply(Res, .(Source), function(x)
      x[order(x$Location), ])
  
    
    ###---------------------------------------------------------------------
    
    ### NOTE: adapt to match the empty data frame at the top of the code, and the variables you extracted in the step chi2Res  
    
    # final data frame
    Res <- data.frame(
      #Source = Res$Source,
      Chi2 = chi2RMSEA$Chi2,
      df = chi2RMSEA$df,
      N = chi2RMSEA$N,
      Reported.RMSEA = chi2RMSEA$Reported.RMSEA,
      Computed.RMSEA = chi2RMSEA$Computed.RMSEA,
      Consistency = chi2RMSEA$Consistency
    )
    
    class(Res) <- c("statcheck", "data.frame")
    
    
    ###---------------------------------------------------------------------
    
    # Return message when there are no results
    if (nrow(Res) > 0) {
      return(Res)
    } else {
      Res <- cat("statcheck did not find any results\n")
    }
    
    
  
