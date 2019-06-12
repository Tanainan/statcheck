# extract written number in the text
#word <- (unlist(str_extract_all(txt, regex("(\\w+\\shundred\\s(and\\s)?\\w+(\\-\\w+)?)|(thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|\\sten\\s|eleven|twelve)", ignore_case = T))))
word <- (unlist(str_extract_all(s, regex("(((the|a|one|two|three|four|five|six|seven|eight|nine)?\\s)?hundred\\s(and\\s)?)?((thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|ten|eleven|twelve)|(((twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(\\-?))?(one|two|three|four|five|six|seven|eight|nine)?))?", ignore_case = T))))
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


word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
}
