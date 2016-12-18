library(dplyr)
library(magrittr)

baseR = c('base','compiler','datasets','graphics','grDevices','grid','methods','parallel','splines','stats','stats4','tcltk','utils')

explicit = function(fileIn,fileOut,
                    ignore = 'base'){

    file = readLines(fileIn)
    
    parseData = file %>% paste(collapse='\n') %>% parse(text = .) %>% getParseData %>% 
        filter(token %in% c('SYMBOL_PACKAGE','NS_GET','SYMBOL_FUNCTION_CALL'))
    
    # filter the functions that are already explicit
    alreadyExplicitPackage = parseData$token %in% 'SYMBOL_PACKAGE' %>% which 
    alreadyExplicitNS = parseData$token %in% 'NS_GET' %>% which
    assertthat::assert_that(all(alreadyExplicitPackage+1 == alreadyExplicitNS))
    alreadExplicitFunction = alreadyExplicitNS + 1
    alreadyExplicit = c(alreadyExplicitPackage,alreadyExplicitNS,alreadExplicitFunction)
    
    parseData = parseData[!1:nrow(parseData) %in% alreadyExplicit,]
    
    parseData$package = parseData$text %>%
        lapply(function(x){
            if(exists(x)){
                gsub("^.+namespace.(.+)>$", "\\1", capture.output(environment(get(x))))
            } else {
                # functions that are taken as arguments are parsed as SYMBOL_FUNCTION_CALLs but they don not exits
                # in the current environment
                'nonExist'
            }
        }) %>% unlist
    parseData %<>%
        filter((!package %in% c(ignore, 'nonExist', 'NULL'))&(!grepl('environment:',package)))
    
    out = file
    for (i in 1:nrow(parseData)){
        line = file[parseData[i,]$line1]
        out[parseData[i,]$line1] = paste0(substr(line,1,parseData[i,]$col1-1),
                        parseData[i,]$package,'::',
                        substr(line,parseData[i,]$col1,parseData[i,]$col2),
                        substr(line,parseData[i,]$col2+1,nchar(line)))
    }
    
    fileOut = file(fileOut,open = 'w')
    writeLines(out, fileOut)
    close(fileOut)
}
