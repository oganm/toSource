# gets sets of differentially expressed genes from glia open access database
# currently hosted at http://bioinf.nl:8080/GOAD
# can work with up to 4 ids
library(RCurl)
library(stringr)
goadDifGenes = function(ids, p,foldChange=c('ALL','UP','DOWN')){
    url = paste0("http://bioinf.nl:8080/GOAD/databaseSelectServlet?comparisonID=on&fold_change=", 
                 foldChange, 
                 '&p_value=',p,
                 '&comparison_ids=',paste(ids,collapse='%2C'))
    
    site = getURL(url)
    
    list = str_extract(site, perl('(?<=(var vennData = )).*') )
    # separate components
    list = unlist(strsplit(unlist(list), '([}]|[\"]),([\"]|[{])'))
    # find group names and locations
    groupHeads = grep('name',list)
    groupNames = str_extract(list[groupHeads],perl('(?<=:[\"]).*'))
    # some cleaning
    list[grep('data',list)] = str_extract(list[grep('data',list)], perl('(?<=:\\[\\").*'))
    list[grep('}|[]]',list)] =  str_extract(list[grep('}|[]]',list)], perl('^.*?(?=[\"])'))
    
    
    geneList = vector(mode = 'list', length = length(groupNames))
    names(geneList) = groupNames
    for (i in 1:length(groupHeads)){
        if (i != length(groupHeads)){
            geneList[[i]] = list[(groupHeads[i]+1):(groupHeads[i+1]-1)]
        } else{
            geneList[[i]] =list[(groupHeads[i]+1):length(list)]
        }
    }   
    return(geneList)
}
