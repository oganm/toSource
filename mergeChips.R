mergeChips = function(affy1,affy2){
    # for merging affymetrix chips of different generations.
    require(affy)
    PNList1 = probeNames(affy1)
    PNList2 = probeNames(affy2)
    
    if (length(PNList2) >length(PNList1)){
        affyTemp = affy2
        affy2 = affy1
        affy1 = affyTemp
        PNList1 = probeNames(affy1)
        PNList2 = probeNames(affy2)
    }
    
    
        subsetList = PNList1[PNList1 %in% PNList2]

    
        
    subsetPm = pm(affy1, unique(subsetList))
    subsetPmOldOrdered = pm(affy2, unique(subsetList))
    subsetPmOld = pm(affy2)
    
    allPm = cbind(subsetPm, subsetPmOldOrdered)
    
    rownames(allPm) = rownames(subsetPmOld)
    
    subset = NULL
    verbose = TRUE
    destructive = TRUE
    normalize = TRUE
    background = TRUE
    bgversion = 2
    ngenes = length(geneNames(affy2))
    allpNList = split(0:(length(PNList2) - 1), PNList2)
    
    
    #rownames(allPm) = 1:nrow(allPm)
    exprs <- .Call("rma_c_complete", allPm, 
                   allpNList, ngenes, normalize, background, bgversion, 
                   verbose, PACKAGE = "affy")
    
    phenoD = combine(phenoData(affy1), phenoData(affy2))
    annot =  annotation(affy2)
    protocolD = combine(protocolData(affy1), protocolData(affy2))
    experimentD = experimentData(affy2)
    
    
    newNormalized = new("ExpressionSet", phenoData = phenoD, annotation = annot, 
                        protocolData = protocolD, experimentData = experimentD, 
                        exprs = exprs)
    return(newNormalized)
    
}