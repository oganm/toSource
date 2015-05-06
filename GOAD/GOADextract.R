require(stringr)
require(RCurl)
require(knitr)
site = getURL('http://bioinf.nl:8080/GOAD/databaseSelectServlet?comparisonID=on&fold_change=ALL&p_value=0&comparison_ids=999999999')
size = as.numeric(str_extract(site,perl('(?<=Size: ).*?(?=<)')))

datasets = vector(mode = 'list', length = size)

for (id in 1:size){
    url = paste0('http://bioinf.nl:8080/GOAD/databaseSelectServlet?comparisonID=on&fold_change=ALL&p_value=0&comparison_ids=', id)
    site = getURL(url)
    if (grepl('HTTP Status 500',site)){
        break
    }
    relevant = str_extract(site,'geneTable.*?\n.*?\n.*?\n.*?\n.*')
    relevant = unlist(strsplit(relevant,split='\n'))
    Comparison = str_extract(relevant[2], perl('(?<=>).*?(?=<)'))
    Author = str_extract(relevant[4], perl('(?<=h4>).*?(?= \\()'))
    Paper = str_extract(relevant[3], perl('(?<=h4>).*?(?= <)'))
    Year = str_extract(relevant[4], perl('(?<=\\().*?(?=\\))'))
    
    datasets[[id]] = c(Comparison = Comparison,ID = id, Paper = Paper, Author = Author,Year = Year)

}

datasets = t(as.data.frame(datasets))
rownames(datasets) = NULL
write.table(datasets,file = 'datasets.tsv',sep='\t', row.names = F)
# dataFile = file('GOAD/datasets.md')
# writeLines( kable(datasets), dataFile)
# close(dataFile)
