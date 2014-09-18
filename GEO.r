# replacemt to gsmDown. Didn't remove it in case I'm using it somewhere. This
# will include all GEO related functions

GSMfind = function(GSE, regex=''){
    require(RCurl)
    page = getURL(paste0('www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', GSE))
    gsms = regmatches(page,gregexpr(paste0('GSM[0-9]*?(?=<.*\n.*?',regex,'</td)'),page,perl=T))
    return(gsms)
}

gsmDown = function(gsm,outfile){
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))

    download.file(
        gsubMult(c('%5F','%2E','%2D'),
                 c('_'  , '.', '-'),
                 regmatches(page,gregexpr('ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?gz',page,perl = T))[[1]]),
        outfile)
}