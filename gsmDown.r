gsmDown = function(gsm,outfile){
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))
    
    download.file(
        gsubMult(c('%5F','%2E','%2D'),
                 c('_'  , '.', '-'),
                 regmatches(page,gregexpr('ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?gz',page,perl = T))[[1]]),
        outfile)
}