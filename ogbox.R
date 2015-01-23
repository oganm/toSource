# allows directly assigning list outputs to variables
# http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
    args <- as.list(match.call())
    args <- args[-c(1:2,length(args))]
    length(value) <- length(args)
    for(i in seq(along=args)) {
        a <- args[[i]]
        if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
    }
    x
}


# just a lazy way to get the last value
ans = function(){
    .Last.value
}


# clears all variables and functions
purge =   function() {
    rm(list = ls(.GlobalEnv, all.names = T), envir = .GlobalEnv)
}


# reads specific lines from a file. for some reason it is kinda slow. works much
# faster from command line. it then places the lines to a function and returns
# the output
# modified from
# http://stackoverflow.com/questions/18235269/efficiently-reading-specific-lines-from-large-files-into-r
checkLines = function(daFile,lines,fun = readLines, ...){
    outAll = vector(mode= 'list',length = length(lines))
    for (i in 1:length(lines)){
        con = pipe(paste0("sed -n -e'",lines[i],"p' ",daFile))
        out = fun(con, ...)
        outAll[[i]] = out
    }
    return(outAll)
}


# remember to match the cases. 
# defaults to .R if extension not given
# no need to use quotes
sourceGithub = function(user, repo, script){
    user = substitute(user)
    user = as.character(user)
    repo = substitute(repo)
    repo = as.character(repo)
    script = substitute(script)
    script = as.character(script)
    
    require(RCurl)
    if (!grepl('[.](r|R)',script)){
        script = paste0(script,'.R')
    }
    text = getURL(paste0(
        "https://raw.githubusercontent.com/",user,'/',repo,'/master/',script),
        ssl.verifypeer=FALSE) 
    source(textConnection(text))
}

gsubMult = function(patterns, replacements, x,
                    ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    for (i in 1:length(patterns)){
        x = gsub(patterns[i],replacements[i],x,
                 ignore.case, perl, fixed, useBytes)
    }
    return(x)
}

getParent = function(step = 1){
    wd = getwd()
    for (i in 1:step){
        setwd('..')
    }
    parent = getwd()
    setwd(wd)
    return(paste(parent,'/',sep=''))
}

#merges lists by their common names. adds non common ones.
mergeList = function(aList,bList,forceUnique=T){
    allNames = unique(c(names(aList),names(bList)))
    outList = vector(mode= "list",length = length(allNames))
    names(outList) = allNames
    outList = sapply(allNames,function(x){
        out=(c(aList[[x]],bList[[x]]))
        if (forceUnique){
            out = unique(out)
        }
        return(out)
    })
    return(outList)
}

findInList = function(object, aList){
    indexes = vector()
    for (i in 1:length(aList)){
        if (object %in% aList[[i]]){
            indexes = c(indexes, i)
        }
    }
    return(indexes)
}

listCount = function(aList){
    length(unlist(aList))
}

trimNAs = function(aVector) {
    return(aVector[!is.na(aVector)])
}

trimElement = function (aVector,e){
    return(aVector[!(aVector %in% e)])
}

listDepth = function(deList){
    step = 1
    while (T){
        if (typeof(eval( parse(text = paste(c("deList",rep('[[1]]',step)),sep='',collapse = '')))) != "list"){
            return(step)
        }
        step = step +1
    }
}

#source
#http://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/
repRow<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
}

repCol<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

repIndiv = function (aVector, n){
    output = vector(length = length(aVector) * n)
    step = 1
    for (i in aVector){
        output[(step * n - n + 1):(n * step)] = rep(i, n)
        step = step + 1
    }
    return(output)
}

# http://stackoverflow.com/questions/6513378/create-a-variable-capturing-the-most-frequent-occurence-by-group
mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#load that bloody function no matter what
insist = function(name){
    name = substitute(name)
    name = as.character(name)
    if (!require(name, character.only = T)) {
        install.packages(name)
        Sys.sleep(5)
        library(name, character.only = T, logical.return = F)
    }
}



#direct text eval
teval = function(daString){
    eval(parse(text=daString))
}


# for navigating through list of lists with teval
listParse = function (daList,daArray){
    out = ''
    for (i in daArray){
        out = paste0(out , '[[' ,  daArray[i] , ']]')
    }
    eval(parse(text=paste0('daList' , out)))
}

#returns the final step as a list
listParseW = function (daList,daArray){
    out = ''
    if (length(daArray) > 1){
        for (i in daArray[1 : (length(daArray) - 1)]){
            out = paste0(out , '[[' ,  i , ']]')
        }
    }
    out = paste0(out ,'[', daArray[length(daArray)], ']')
    eval(parse(text=paste0('daList' , out)))
}

# sets the list element
listSet = function(daList,daArray ,something){
    name = substitute(daList)
    name = as.character(name)
    out = ''
    for (i in daArray){
        out = paste0(out , '[[' ,  i , ']]')
    }

    eval(parse(text = paste0(name, out, '<<-something')))
}

listStr = function(daArray){
    out = ''
    for (i in daArray[1 : length(daArray)]){

        out = paste0(out, '[[',  i, ']]')
    }
    return(out)
}

listStrW = function(daArray){
    out = ''
    if (length(daArray) > 1){
        for (i in daArray[1 : (length(daArray) - 1)]){
            out = paste0('out','[[',i, ']]')
        }
    }
    out = paste0(out,'[', daArray[length(daArray)],']')
    return(out)
}



#concatanate to preallocated. only works for non zero values and with numeric or boolean stuff
"%c%" = function (x, y){
    start = which(x == 0)[1]
    x[start:(start+length(y) - 1)]= y
    return(x)
}



# turn every member of daList to a color from the palette
toColor = function(daList, palette = NA){

    daList = as.factor(daList)
    uniq = unique(daList)
    if (is.na(palette[1])){
        palette = rainbow(length(uniq))
        names(palette) = uniq
    }
    cols = vector (length = length(daList))

    #to match palette names to uniq names so that custom naming is possible
    if (!is.null(names(palette))){
        palette = trimNAs(palette[match(uniq,names(palette))])
    }


    for (i in 1:length(uniq)){
        cols[daList == uniq[i]]= palette[i]
    }

    out = list()
    out$cols = cols
    out$palette = palette

    return(out)
}

#to use with ggplot violins. adapted from http://stackoverflow.com/questions/17319487/median-and-quartile-on-violin-plots-in-ggplot2
median.quartile <- function(x){
    out <- quantile(x, probs = c(0.25,0.5,0.75))
    ICR = out[3] - out[1]
    out = c(out[1] - 1.5 * ICR ,out, out[3] + 1.5 * ICR)
    if (out[1] < min(x)){
        out[1] = min(x)
    }
    if (out[5] > max(x)){
        out[5] = max(x)
    }
    names(out) <- c("whisDown","ymin","y","ymax","whisUp")
    return(out)
}

threeQuartile <- function(x){
    out <- quantile(x, probs = c(0.25,0.5,0.75))
    names(out) <- c("ymin","y","ymax")
    return(out)
}

# function acronyms ----
len = length
as.char = as.character
as.df = as.data.frame
as.num = as.numeric

coVar = function(x) ( 100*sd(x)/mean(x) )
