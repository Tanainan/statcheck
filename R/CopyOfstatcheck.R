library(plyr)
statcheck <-
  function(x) {{
    
    # Create empty data frame for main result:
    Res <-
      data.frame(
        Source = NULL,
        Statistic = NULL,
        df = NULL,
        Test.Comparison = NULL,
        Value = NULL,
        Reported.Comparison = NULL,
        RMSEA = NULL, 
        N = NULL,
        Computed = NULL
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
      
      # Get location of sample size (from all the integers in the article)
      aaa <- str_extract(txt, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?\\d\\d+\\s)|(\\w+\\s(?!0)\\d\\d+\\s\\w+)", ignore_case = T)) # search for numbers and get the location in the article
      aaa <- aaa[!is.na(aaa)]
      N <- str_extract(aaa, regex("\\d+"))
      N <- N[!is.na(N)]
      
      for (j in 1:length(N)){
      # extract Chis2-values:
      
      # Get location of chi values in text:
      chi2Loc <-
        gregexpr(
          "((chi-square |chi-square of|v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(((\\(\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\))?\\s?([<>=]|(1/4)))|((\\s\\w+){0,20}))(\\s\\d*(\\.|\\:)?\\d*)(\\,|\\s|\\w+|[<>=]|\\.|\\,|\\)|\\(|\\:|\\;|\\/|\\-){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))",
          txt,
          ignore.case = TRUE
        )[[1]]
      
      if (chi2Loc[1] != -1) {
        # Get raw text of chi2-values:
        chi2Raw <-
          substring(txt, chi2Loc, chi2Loc + attr(chi2Loc, "match.length") - 1)
        substr(chi2Raw, 1, 1)[grepl("\\d", substr(chi2Raw, 1, 1))] <-
          " "
        
        # remove sample size if reported for calculations
        # save full result for "Raw" in final data frame
        chi2Raw_inclN <- chi2Raw
        chi2Raw <-
          gsub("N\\s?=\\s?\\d*\\,?\\d*\\,?\\d*",
               "",
               chi2Raw,
               ignore.case = TRUE)
        
        # remove commas (thousands separators)
        chi2Raw <-
          gsub("(?<=\\d),(?=\\d+\\.)", "", chi2Raw, perl = TRUE)
        
        # bug fix: remove extra opening brackets
        # if a chi2 result is reported between brackets, and the chi is not read by statcheck
        # the opening bracket is translated as the chi symbol, and extracting the numerics goes wrong
        chi2Raw <-
          gsub("\\((?=2\\s?\\()", "", chi2Raw, perl = TRUE)
        
        # Extract location of numbers:
        nums <-
          gregexpr(
            "(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
            sub("^.*?\\(", "", chi2Raw),
            ignore.case = TRUE
          )
        
        # Extract df:
        ddd <- str_extract(chi2Raw, regex("((((degrees of freedom of)\\s\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?\\d+\\s?)|(\\s\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\(\\d+\\)))", ignore_case = T))
        ddd <- ddd[!is.na(ddd)]
        df <- str_extract(ddd, regex("(?!2)\\d+"))
        df <- df[!is.na(df)]
      
        # df <-
        #   as.numeric(substring(
        #     sub("^.*?\\(", "", chi2Raw),
        #     sapply(nums, '[', 1),
        #     sapply(nums, function(x)
        #       x[1] + attr(x, "match.length")[1] - 1)
        #   ))
        
        # Extract chi2-values
        suppressWarnings(chi2ValsChar <-
                           substring(
                             sub("^.*?\\(", "", chi2Raw),
                             sapply(nums, '[', 2),
                             sapply(nums, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(chi2Vals <- as.numeric(chi2ValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", chi2ValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]", chi2Raw)
        testEq <- substring(
          chi2Raw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Get RMSEA
        rrr <- str_extract(chi2Raw, regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
        rrr <- rrr[!is.na(rrr)]
        RMSEA <- str_extract(rrr, regex("\\.\\d+"))
        RMSEA <- RMSEA[!is.na(RMSEA)]
        
        # Extract chi2-values
        # suppressWarnings(chi2ValsChar <-
        #                    substring(
        #                      sub("^.*?\\(", "", chi2Raw),
        #                      sapply(nums, '[', 2),
        #                      sapply(nums, function(x)
        #                        x[2] + attr(x, "match.length")[2] - 1)
        #                    ))
        
        # Extract p-values
        # suppressWarnings(pValsChar <-
        #                    substring(
        #                      sub("^.*?\\(", "", chi2Raw),
        #                      sapply(nums, '[', 3),
        #                      sapply(nums, function(x)
        #                        x[3] + attr(x, "match.length")[3] - 1)
        #                    ))
        # 
        # suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", chi2Raw, ignore.case = TRUE)
        pEq <- substring(
          chi2Raw,
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        pEq[grepl("ns", chi2Raw, ignore.case = TRUE)] <- "ns"
        
        # determine number of decimals of p value
        # dec <-
        #   attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        # dec[dec < 0] <- 0
        
        Computed <- round(((sqrt(as.numeric(chi2Vals)-as.numeric(df)))/(sqrt(as.numeric(df)*(as.numeric(N)-1)))),3)
        
        # Create data frame:
        chi2Res <- data.frame(
          Source = names(x)[i],
          Statistic = "Chi2",
          df = df,
          #df2 = NA,
          Test.Comparison = testEq,
          Value = chi2Vals,
          Reported.Comparison = pEq,
          Computed = Computed,
          #Reported.P.Value = pVals,
          RMSEA = RMSEA, # find a way to extract RMSEA
          N = N,     # find a way to extract N
          Location = chi2Loc,
          Raw = chi2Raw_inclN
          #stringsAsFactors = FALSE,
          #dec = dec,
          #testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, chi2Res)
        rm(chi2Res)
        
      }}
      
      #----------------------
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    Source <- NULL
    Res <- ddply(Res, .(Source), function(x)
      x[order(x$Location), ])
    
    # if (nrow(Res) > 0) {
    #   # remove p values greater than one
    #   Res <- Res[Res$Reported.P.Value <= 1 |
    #                is.na(Res$Reported.P.Value), ]
    }
    
    ###---------------------------------------------------------------------
    
    ### NOTE: adapt to match the empty data frame at the top of the code, and the variables you extracted in the step chi2Res  
    
    # final data frame
    Res <- data.frame(
      Source = Res$Source,
      Statistic = Res$Statistic,
      df = Res$df,
      #df2 = Res$df2,
      N = Res$N,
      Test.Comparison = Res$Test.Comparison,
      Value = Res$Value,
      Reported.Comparison = Res$Reported.Comparison,
      #Reported.P.Value = Res$Reported.P.Value,
      RMSEA = Res$RMSEA,
      Computed.RMSEA = Res$Computed,
      Raw = Res$Raw
      
    )
    
    class(Res) <- c("statcheck", "data.frame")
    
    
    ###---------------------------------------------------------------------
    
    # Return message when there are no results
    if (nrow(Res) > 0) {
      return(Res)
    } else {
      Res <- cat("statcheck did not find any results\n")
    }
    
    
  }
