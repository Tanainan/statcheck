library(plyr)
library(words2number)
statcheck <-
  function(x) {{
    
    # Create empty data frame for main result:
    Res <-
      data.frame(
        Source = NULL,
        df = NULL,
        Chi2 = NULL,
        Reported.Comparison = NULL,
        RMSEA = NULL, 
        #N = NULL,
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
      
      
      # extract Chis2-values by locating RMSEA first:
      chi2RMSEA <- str_subset(txt, regex("(((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
      
      
      # Get location of chi values in text:
      chi2Loc <-
        str_extract_all(chi2RMSEA, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s\\d*(\\.|\\:)\\d+))",
          ignore_case = TRUE))

      # Get chi value
      Chi2 <- str_extract(unlist(chi2Loc), regex("\\d*\\.\\d+"))
      
      # Get location of chi values in text:
      # chi2Loc <-
      #   gregexpr(
      #     "((chi-square |chi-square of|v2(| )|w2(| )|w2/df(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,20}(\\s\\d*(\\.|\\:)\\d+))",
      #     chi2RMSEA,
      #     ignore.case = TRUE
      #   )[[1]]

      # if (chi2Loc[1] != -1) {
      #   # Get raw text of chi2-values:
      #   chi2Raw <-
      #     substring(chi2RMSEA, chi2Loc, chi2Loc + attr(chi2Loc, "match.length") - 1)
      #   substr(chi2Raw, 1, 1)[grepl("\\d", substr(chi2Raw, 1, 1))] <-
      #     " "

        # remove sample size if reported for calculations
        # save full result for "Raw" in final data frame
        # chi2Raw_inclN <- chi2Raw
        # chi2Raw <-
        #   gsub("N\\s?=\\s?\\d*\\,?\\d*\\,?\\d*",
        #        "",
        #        chi2Raw,
        #        ignore.case = TRUE)

        # remove commas (thousands separators)
        # chi2Raw <-
        #   gsub("(?<=\\d),(?=\\d+\\.)", "", chi2Raw, perl = TRUE)

        # bug fix: remove extra opening brackets
        # if a chi2 result is reported between brackets, and the chi is not read by statcheck
        # the opening bracket is translated as the chi symbol, and extracting the numerics goes wrong
        # chi2Raw <-
        #   gsub("\\((?=2\\s?\\()", "", chi2Raw, perl = TRUE)

        # Extract location of numbers:
        # nums <-
        #   gregexpr(
        #     "(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
        #     sub("^.*?\\(", "", chi2Raw),
        #     ignore.case = TRUE
        #   )
        
        # Extract df:
      dfdf <- str_extract_all(chi2RMSEA, regex("(chi-square |chi-square of|(\\(| )v2(| )|w2(| )|(\\:|\\(|\\d|\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,30}((((degrees of freedom of)\\s(?!0)\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?(?!0)\\d+\\s?)|(\\s(?!0)\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\((?!0)\\d+\\)))", ignore_case = T))
      ddd <- str_extract_all(unlist(dfdf), regex("((((degrees of freedom of)\\s(?!0)\\d+)|(df|( |\\()d|\\()\\s?\\=\\s?(?!0)\\d+\\s?)|(\\s(?!0)\\d+\\s?(df|degrees of freedom|\\)))|((2|2 )\\((?!0)\\d+\\)))", ignore_case = T))
      ddd <- ddd[!is.na(ddd)]
      df <- str_extract_all(unlist(ddd), regex("(?!2\\()\\d+"))
      df <- df[!is.na(df)]
    
        
        # Extract chi2-values
        # suppressWarnings(chi2ValsChar <-
        #                    substring(
        #                      sub("^.*?\\(", "", chi2Raw),
        #                      sapply(nums, '[', 2),
        #                      sapply(nums, function(x)
        #                        x[2] + attr(x, "match.length")[2] - 1)
        #                    ))
        # 
        # suppressWarnings(chi2Vals <- as.numeric(chi2ValsChar))
        
        # # Extract number of decimals test statistic
        # testdec <-
        #   attr(regexpr("\\.\\d+", chi2ValsChar), "match.length") - 1
        # testdec[testdec < 0] <- 0
        # 
        # # Extract (in)equality test statistic
        # testEqLoc <- gregexpr("\\)\\s?[<>=]", chi2Raw)
        # testEq <- substring(
        #   chi2Raw,
        #   sapply(testEqLoc, function(x)
        #     x[1] + attr(x, "match.length")[1] - 1),
        #   sapply(testEqLoc, function(x)
        #     x[1] + attr(x, "match.length")[1] - 1)
        # )
        # 
        # # Extract (in)equality
        # eqLoc <-
        #   gregexpr("p\\s?[<>=]", chi2Raw, ignore.case = TRUE)
        # pEq <- substring(
        #   chi2Raw,
        #   sapply(eqLoc, function(x)
        #     x[1] + attr(x, "match.length")[1] - 1),
        #   sapply(eqLoc, function(x)
        #     x[1] + attr(x, "match.length")[1] - 1)
        # )
        # pEq[grepl("ns", chi2Raw, ignore.case = TRUE)] <- "ns"
        
      
        # Get RMSEA
        mmm <- str_extract_all(chi2RMSEA, regex("((chi-square |chi-square of|(\\(| )v2(| )|w2(| )|(\\:|\\(|\\d\\,) 2 |(\\:|\\(|\\d|\\,) 2)(.){0,100}((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*)))", ignore_case = T))
        rrr <- str_extract_all(unlist(mmm), regex("((root mean square error of approximation|root-mean-square error of approximation|\\(?RMSEA\\)?)\\s(([<>=]|(1/4))|((\\w+\\s){0,10}))\\s(\\d*(\\.|\\:)?\\d*))", ignore_case = T))
        rrr <- rrr[!is.na(rrr)]
        RMSEA <- str_extract_all(unlist(rrr), regex("\\.\\d+"))
        RMSEA <- unlist(RMSEA[!is.na(RMSEA)])
        
        
        # for (j in 1:length(N)){
        #   for (k in 1:length(Chi2)){
        #     for (l in 1:length(df)){
        #       for (m in 1:length(RMSEA)){

         # Computed[[j,k,l,m]] <- round(((sqrt(as.numeric(Chi2[k])-as.numeric(df[l])))/(sqrt(as.numeric(df[l])*(as.numeric(N[j])-1)))),3)


        #       }
        #     }
        #   }
        # }
          
          # Create data frame:
          chi2Res <- data.frame(
            Source = names(x)[i],
            df = df,
            #df2 = NA,
            Value = Chi2,
            #Computed = Computed,
            #Reported.P.Value = pVals,
            RMSEA = RMSEA, # find a way to extract RMSEA
            #N = N,     # find a way to extract N
            Location = chi2Loc,
            Raw = chi2Raw_inclN
            #stringsAsFactors = FALSE,
            #dec = dec,
            #testdec = testdec
          )
          
          N <- NA
          
          # Get location of sample size (from all the integers in the article)
          aaa <- str_extract_all(txt, regex("(n\\s?(\\=|equals to|equal to|equal|equals)\\s?\\d\\d+\\s)|(\\w+\\s(?!0)\\d\\d+\\s\\w+)", ignore_case = T)) # search for numbers and get the location in the article
          aaa <- aaa[!is.na(aaa)]
          N <- str_extract_all(unlist(aaa), regex("\\d+"))
          N <- unlist(N[!is.na(N)])
          
          chi2Res$Computed <- NA
          
          for (m in 1:length(N)) {
           for (n in 1:length(chi2Res)){
            
             Result <- data.frame(
               Chi2 = NULL,
               df = NULL,
               N = NULL,
               RMSEA = NULL
             )

             
          Result <- round(((sqrt(as.numeric(chi2Res$Chi2[n])-as.numeric(chi2Res$df[n])))/(sqrt(as.numeric(chi2Res$df[n])*(as.numeric(N[m])-1)))),3)
   
           Result <- data.frame(
            Chi2 = Chi2[n],
            df = df[n],
            N = N[m],
            RMSEA = RMSEA[n]
          ) 
          
              }
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
    
    # if (nrow(Res) > 0) {
    #   # remove p values greater than one
    #   Res <- Res[Res$Reported.P.Value <= 1 |
    #                is.na(Res$Reported.P.Value), ]
  
    
    ###---------------------------------------------------------------------
    
    ### NOTE: adapt to match the empty data frame at the top of the code, and the variables you extracted in the step chi2Res  
    
    # final data frame
    Res <- data.frame(
      Source = Res$Source,
      Chi2 = Res$Value,
      df = Res$df,
      N = Res$N,
      Reported.RMSEA = Res$RMSEA,
      Computed.RMSEA = Res$Computed
      #Raw.Text = Res$Raw
      
    )
    
    class(Res) <- c("statcheck", "data.frame")
    
    
    ###---------------------------------------------------------------------
    
    # Return message when there are no results
    if (nrow(Res) > 0) {
      return(Res)
    } else {
      Res <- cat("statcheck did not find any results\n")
    }
    
    
  
