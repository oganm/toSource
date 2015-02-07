# replacemt to gsmDown. Didn't remove it in case I'm using it somewhere. This
# will include all GEO related functions
library(RCurl)
require(stringr)
eval( expr = parse( text = getURL(
    "https://raw.githubusercontent.com/oganm/toSource/master/ogbox.R",
    ssl.verifypeer=FALSE) ))


gsmFind = function(GSE, regex=''){
    # finds GSMs that match to a regular expression from a GSE (description not GSM ID)
    library(RCurl)
    page = getURL(paste0('www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', GSE))
    gsms = regmatches(page,gregexpr(paste0('GSM[0-9]*?(?=<.*\n.*?',regex,'.*?</td)'),page,perl=T))[[1]]
    return(gsms)
}


gsmSize = function(gsm, warnings = T){
    library(RCurl)
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))
    fileURL = gsubMult(c('%5F','%2E','%2D','%2B'),
                       c('_'  , '.',  '-',  '+'),
                       regmatches(page,
                                  gregexpr('ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?gz',
                                           page,
                                           perl = T))[[1]])
    if (len(fileURL) == 0){
        if (warnings){
            warning(paste(gsm,"doesn't have a file attached"))
        }
        return(invisible(F))
    }
    sizeString = getURL(fileURL)
    size = as.numeric(
        str_extract(sizeString, perl('(?<=(Content-Length: )).*?(?=\r)')))
    return(size)
}

gsmDown = function(gsm,outfile, overwrite = F, warnings = T){
    # downloads a given GSM
    dir.create(dirname(outfile), showWarnings=F,recursive=T)
    if (file.exists(outfile) & !overwrite){
        print('you already have it bro. i aint gonna get it again')
        print(basename(outfile))
        return(invisible(F))
    }
    library(RCurl)
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))
    
    fileURL = gsubMult(c('%5F','%2E','%2D','%2B'),
                       c('_'  , '.',  '-',  '+'),
                       regmatches(page,
                                  gregexpr('ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?gz',
                                           page,
                                           perl = T))[[1]])
    if (len(fileURL) == 0){
        if (warnings){
            warning(paste(gsm,"doesn't have a file attached"))
        }
        return(invisible(F))
    }
    download.file(fileURL,paste0(outfile,'.gz'))
    system(paste0('gunzip -f "',outfile,'.gz"'))
    invisible(T)
}


gseDown = function(GSE,regex ='',outDir, extension = '.cel',overwrite=F){
    # downloads GSMs matching a regular expression from a GSE (description not GSM ID)
    library(RCurl)
    gsms = gsmFind(GSE, regex)
    for (i in 1:length(gsms)){
        gsmDown(gsms[i],paste0(outDir,'/', gsms[i],extension),overwrite)
    }
}
