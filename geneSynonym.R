synoTarget = 'Data/GeneNames'


prepGenes = function(tax=c(9606,10090),removeFull=T){
    
    synoTarget = 'Data/GeneNames'
    require(bit64)
    require(data.table)
    download.file('ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene_info.gz',paste0(synoTarget,'/gene_info.gz'))
    system(paste0('gunzip ',synoTarget,'/gene_info.gz'))
    
    #sep2 is not yet implemented
    geneInfo = fread(paste0(synoTarget,'/gene_info'),sep='\t',skip=1, header = F)
    
    
    names(geneInfo) = c('tax_id', 'GeneID', 'Symbol', 'LocusTag', 'Synonyms', 
                        'dbXrefs', 'chromosome', 'map_location', 'description',
                        'type_of_gene', 'Symbol_from_nomenclature_authority', 
                        'Full_name_from_nomenclature_authority', 'Nomenclature_status',
                        'Other_designations', 'Modification_date')
    
    geneInfo = geneInfo[geneInfo$tax_id %in% tax,c('Symbol','Synonyms'),with=F]
    
    deNames = apply(geneInfo,1,function(x){
        if (x[2]!='-'){
            paste0(x[1],'|',x[2])
        } else{
            x[1]
        }
    })
    write.table(deNames, paste0(synoTarget,'/geneSynonyms'),row.names=F,col.names=F,quote=F)
    if (removeFull){
        file.remove(paste0(synoTarget,'/gene_info'))
    }
}

geneSynonym = function(genes){
    leData = readLines(paste0(synoTarget,'/geneSynonyms'))
    synos = sapply(genes,function(x){
        strsplit(grep(paste0('(^|[|])',x,'($|[|])'),leData,value=T),split='[|]')
    })
    return(synos)
}

