


# this is to set the directory for file storage.
synoTarget = 'Data/GeneNames'


prepGenes = function(tax=c(9606,10090),removeFull=T,forceUpdate=F, synoTarget = 'Data/GeneNames'){
    synoTarget <<- synoTarget
    # to download the data and prepare it for other functions to use. stores the
    # data on disk for later use. removeFull chooses if the whole data file should be kept
    # if forceUpdate = F it does not overwrite the files if they aldready exists
    library(data.table)
    if (forceUpdate){
        file.remove(paste0(synoTarget,'/gene_info'))
        file.remove(paste0(synoTarget,'/gene_info.gz'))
    }
    if (!(file.exists(paste0(synoTarget, '/gene_info'))
        | file.exists(paste0(synoTarget, '/gene_info.gz')))){
        download.file('ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/gene_info.gz',paste0(synoTarget,'/gene_info.gz'))
    }
    if(!(file.exists(paste0(synoTarget, '/gene_info')))){
        system(paste0('gunzip ',synoTarget,'/gene_info.gz'))
    }
    
    #sep2 is not yet implemented for fread
    geneInfo = fread(paste0(synoTarget,'/gene_info'),sep='\t',skip=1, header = F)
    
    
    setnames(geneInfo,old = names(geneInfo),new= 
                 c('tax_id', 'GeneID', 'Symbol', 'LocusTag', 'Synonyms',                         
                   'dbXrefs', 'chromosome', 'map_location', 'description',
                   'type_of_gene', 'Symbol_from_nomenclature_authority', 
                   'Full_name_from_nomenclature_authority', 'Nomenclature_status',
                   'Other_designations', 'Modification_date'))
    
    # this is a data.table hence with=F is required to call with strings
    # with=F is also much faster than calling them directly with names for some
    # reason
    geneInfo = geneInfo[geneInfo$tax_id %in% tax,c('Symbol','Synonyms','tax_id'),with=F]
    
    deNames = apply(geneInfo,1,function(x){
        if (x[2]!='-'){
            paste0(x[1],'|',x[2])
        } else{
            x[1]
        }
    })
    
    for (i in tax){
        write.table(deNames[geneInfo[,tax_id]==i], paste0(synoTarget,'/',i),
                    row.names=F, col.names=F, quote=F)
    }
    
    if (removeFull){
        file.remove(paste0(synoTarget,'/gene_info'))
    }
}

loadSyno = function(synoTarget = 'Data/GeneNames'){
    synoTarget <<- synoTarget
}

mouseSyno = function(genes,cores=1){
    geneSynonym(genes,10090,cores)
}

humanSyno = function(genes,cores=1){
    geneSynonym(genes,9606,cores)
}

geneSynonym = function(genes,tax,cores = 1){
    # I kept the single core sapply version in case installing parallel is a
    # problem somewhere.
    # if a gene name given and it is not on the list, it spews out a warning 
    # DOES NOT PRINT WARNINGS WHEN USING MULTIPLE CORES
    leData = readLines(paste0(synoTarget,'/',tax))
    
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

