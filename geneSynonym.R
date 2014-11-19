synoTarget = 'Data/GeneNames'


prepGenes = function(tax=c(9606,10090),removeFull=T){
    
    synoTarget = 'Data/GeneNames'
    require(bit64)
    require(data.table)
    download.file('ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene_info.gz',paste0(synoTarget,'/gene_info.gz'))
    system(paste0('gunzip ',synoTarget,'/gene_info.gz'))
    
    #sep2 is not yet implemented for fread
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

geneSynonym = function(genes,cores = 1){
    # I kept the single core sapply version in case installing parallel is a
    # problem somewhere.
    # if a gene name given and it is not on the list, it spews out a warning 
    # DOES NOT PRINT WARNINGS WHEN USING MULTIPLE CORES
    leData = readLines(paste0(synoTarget,'/geneSynonyms'))
    
    geneSearcher = function(x){
        synonyms = strsplit(grep(paste0('(^|[|])',x,'($|[|])'),leData,value=T),split='[|]')
        if (length(synonyms)==0){
            synonyms = x
            warning(paste0('Gene ',x,' could not be found in the list. Returning own name'))
        }
        return(synonyms)
    }
    
    if (cores == 1){
        synos = sapply(genes,geneSearcher)
        return(synos)
    } else {
        require(parallel)
        # so that I wont fry my laptop
        if (detectCores()<cores){ 
            cores = detectCores()
            print('max cores exceeded')
            print(paste('set core no to',cores))
        }
        options(warn=1)
        synos = simplify2array(
            mclapply(genes, geneSearcher, mc.cores = cores)
        )
        names(synos) = genes
        return(synos)
    }
}

