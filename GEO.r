# replacemt to gsmDown. Didn't remove it in case I'm using it somewhere. This
# will include all GEO related functions
library(RCurl)

eval( expr = parse( text = getURL(
    "https://raw.githubusercontent.com/oganm/toSource/master/ogbox.r",
    ssl.verifypeer=FALSE) ))


gsmFind = function(GSE, regex=''){
    # finds GSMs that match to a regular expression from a GSE (description not GSM ID)
    library(RCurl)
    page = getURL(paste0('www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', GSE))
    gsms = regmatches(page,gregexpr(paste0('GSM[0-9]*?(?=<.*\n.*?',regex,'.*?</td)'),page,perl=T))[[1]]
    return(gsms)
}

gsmDown = function(gsm,outfile){
    # downloads a given GSM
    library(RCurl)
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))
    download.file(
        gsubMult(c('%5F','%2E','%2D'),
                 c('_'  , '.', '-'),
                 regmatches(page,gregexpr('ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?gz',page,perl = T))[[1]]),
        paste0(outfile,'.gz'))
    system(paste0('gunzip ',outfile,'.gz'))
}


gseDown = function(GSE,regex ='',outDir, extension = '.cel'){
    # downloads GSMs matching a regular expression from a GSE (description not GSM ID)
    library(RCurl)
    gsms = gsmFind(GSE, regex)
    for (i in 1:length(gsms)){
        if (progBar ==T){setTxtProgressBar(pb,i)}
        gsmDown(gsms[i],paste0(outDir,'/', gsms[i],extension))
    }
}
