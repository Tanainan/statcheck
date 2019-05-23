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
      
      txt <- b
      
      # extract Chis2-values by locating RMSEA first:
      chi2RMSEALoc <- str_subset(txt, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      # chi2RMSEALoc <- unlist(chi2RMSEALoc)
      # chi2RMSEALoc <- chi2RMSEALoc[!duplicated(chi2RMSEALoc)]
      
      # Get location of chi values in text:
      chi2Loc <-
        str_extract_all(unlist(chi2RMSEALoc), regex("((chi-square (?!difference)|chi-square of|(\\(|\\s)[vcw]2\\s?[=(]|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+))",
          ignore_case = TRUE))

      # Get chi value
      Chi2 <- str_extract(unlist(chi2Loc), regex("\\d*\\.\\d+"))
        
      # Extract df:
      ddd <- str_extract_all(unlist(chi2Loc), regex("((df|d)\\s?([<>=]|(1/4))|(2|2\\s)\\(|degrees of freedom of)\\s?(?!0)\\d+\\s?\\,?|\\s(?!0)\\d+\\s(df|degrees of freedom)", ignore_case = T))
      ddd <- ddd[!is.na(ddd)]
      ddd <- str_replace(unlist(ddd), "1/4", "")
      df <- str_extract_all(unlist(ddd), regex("(?!2\\s?\\()\\d+"))
      df <- unlist(df[!is.na(df)])
      
    
      # Get RMSEA
      sss <- str_extract_all(unlist(chi2RMSEALoc), regex("((chi-square |chi-square of|\\s?[vcw]2\\s?[=(]|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,300}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,40}\\s(\\d*(\\.|\\:)?\\d*))|((root mean square error of approximation|root-mean-square error of approximation|RMSEA)\\s(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,300}(chi-square |chi-square of|\\s?[vcw]2\\s?\\(|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,20}(\\d*(\\.|\\:)\\d+))", ignore_case = T))
      #mmm <- str_extract_all(chi2RMSEA, regex("((chi-square |chi-square of|(\\(| )v2(| )| [cw]2(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      sss1 <- str_extract_all(unlist(sss), regex("((chi-square |chi-square of|\\s?[vcw]2\\s?[=(]|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,40}(\\d*(\\.|\\:)\\d+)(.){0,80}\\d+(.){0,80}(root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,40}\\s(\\d*(\\.|\\:)?\\d*))|((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,20}\\s(\\d*(\\.|\\:)?\\d*)(.){0,80}\\d+(.){0,80}(chi-square |chi-square of|\\s?[vcw]2\\s?\\(|(\\:|\\(|\\d|\\,)\\s*2\\s?\\()(.){0,20}(\\d*(\\.|\\:)\\d+))", ignore_case = T))
      rrr <- str_extract_all(unlist(sss1), regex("((root mean square error of approximation|root-mean-square error of approximation|RMSEA)(.){0,10}\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
      rrr <- rrr[!is.na(rrr)]
      RMSEA <- str_extract_all(unlist(rrr), regex("\\.\\d+"))
      RMSEA <- unlist(RMSEA[!is.na(RMSEA)])
          
      
      # Get N from when it is reported in the result
      nnn <- str_extract_all(unlist(chi2Loc), regex("(n\\s?(\\=|equals to|equal to|equal|equals|(1/4))?\\s?\\d+)", ignore_case = T))
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
      
      if (length(N) == length(Chi2)){
      # Create data frame:
      chi2RMSEA <- data.frame(
          #Source = names(x)[i],
          Chi2 = Chi2,
          df = df,
          Reported.RMSEA = RMSEA,
          N = N
        )
      

      chi2RMSEA$Computed <- NA
      chi2RMSEA$Computed.RMSEA <- NA
      

      chi2RMSEA <- data.frame(sapply(chi2RMSEA, function(x) as.numeric(as.character(x))))
      

      for (i in 1:nrow(chi2RMSEA)){
        deci <- decimalplaces(chi2RMSEA$Reported.RMSEA[i])
        chi2RMSEA$Computed[i] <- ((sqrt(as.numeric(chi2RMSEA$Chi2[i])-as.numeric(chi2RMSEA$df[i])))/(sqrt(as.numeric(chi2RMSEA$df[i])*(as.numeric(chi2RMSEA$N[i])-1))))
        chi2RMSEA$Computed.RMSEA[i] <- round((chi2RMSEA$Computed[i]),deci)
      }
      

      # some of the computed RMSEAs are NaN >>> change them to NAs
      chi2RMSEA$Computed.RMSEA[is.nan(chi2RMSEA$Computed.RMSEA)] <- "NA"
      

      chi2RMSEA$Consistency <- NA # test if the reported and computed RMSEAs are the same (2 digits)
      for (i in 1:nrow(chi2RMSEA)){
        if (chi2RMSEA$Reported.RMSEA[i] == chi2RMSEA$Computed.RMSEA[i]) {chi2RMSEA$Consistency[i] <- "Consistent"} else {chi2RMSEA$Consistency[i] <- "x"}
      }}
      
      #---------------------------------------
      
      if (length(N) != length(Chi2)){
        # Create data frame:
        chi2Res <- data.frame(
          #Source = names(x)[i],
          Chi2 = Chi2,
          df = df,
          Reported.RMSEA = RMSEA
        )
   
      Ns <- NA
      
      # Get location of sample size (from all the integers in the article)
      aaa <- str_extract_all(txt, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?\\d\\d+\\s)|((?!\\d+)\\w+\\s(?!0)\\d\\d+\\s(?!\\d+)\\w+)", ignore_case = T)) # search for numbers and get the location in the article
      aaa <- aaa[!is.na(aaa)]
      Ns <- str_extract_all(unlist(aaa), regex("\\d+"))
      Ns <- unlist(Ns[!is.na(Ns)])
      Ns <- Ns[!duplicated(Ns)] # remove duplicate Ns
      Ns <- Ns[which(as.numeric(Ns) >= 10 & as.numeric(Ns) <= 1500)] # only select Ns that are greater than or equal to 10 and less than or equal to 1500
          
      # create a new data frame that can contain all variables
      chi2RMSEA <- data.frame(Chi2 = rep(chi2Res$Chi2, length(Ns)), df = rep(chi2Res$df,length(Ns)), Reported.RMSEA = rep(chi2Res$Reported.RMSEA, length(Ns)))
          
      # add a column for N
      chi2RMSEA$Ns = rep(Ns, each = length(Chi2))
          
      chi2RMSEA$Computed <- NA
      chi2RMSEA$Computed.RMSEA <- NA
      
      
      chi2RMSEA <- data.frame(sapply(chi2RMSEA, function(x) as.numeric(as.character(x))))
      
      
      for (i in 1:nrow(chi2RMSEA)){
        deci <- decimalplaces(chi2RMSEA$Reported.RMSEA[i])
        chi2RMSEA$Computed[i] <- ((sqrt(as.numeric(chi2RMSEA$Chi2[i])-as.numeric(chi2RMSEA$df[i])))/(sqrt(as.numeric(chi2RMSEA$df[i])*(as.numeric(chi2RMSEA$N[i])-1))))
        chi2RMSEA$Computed.RMSEA[i] <- round((chi2RMSEA$Computed[i]),deci)
      }
      
      # some of the computed RMSEAs are NaN >>> change them to NAs
      chi2RMSEA$Computed.RMSEA[is.nan(chi2RMSEA$Computed.RMSEA)] <- "NA"

          
      chi2RMSEA$Consistency <- NA # test if the reported and computed RMSEAs are the same (2 digits)
      for (i in 1:nrow(chi2RMSEA)){
        if (chi2RMSEA$Reported.RMSEA[i] == chi2RMSEA$Computed.RMSEA[i]) {chi2RMSEA$Consistency[i] <- "Consistent"} else {chi2RMSEA$Consistency[i] <- "x"}
      }}
          
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
    
    
  
