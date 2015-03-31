library(limma)
library(e1071)
# finds differentially expressed genes between groups that has high fold change
# and confidence to identify potentially wrongly labeled samples. can mistake
# contamination with mislabeling but you wouldn't want either I suppose.
# If samples are inherently very close to each other... well you're screwed...
# if most of your samples are mislabeld... again you're screwed.
# if they are mislabeled and outliers... i just don't know...
# might be unwise to use it before batch correction

# assumes collumns are samples and rows are genes
# rownames should be gene identifiers

matchMaker = function(expr, groups, pThresh=1e-5, foldTresh = 5) {
    uniGroups = unique(groups)
    pairwise = combn(uniGroups,2)
    # create a list for all samples to store mislabeling information, will be
    # trimmed later on.
    mislabel = vector(mode= 'list', length = ncol(expr))
    mislabelProbs = vector(mode= 'list', length = ncol(expr))
    names(mislabel) = colnames(expr)
    names(mislabelProbs) = colnames(expr)
    for (i in 1:ncol(pairwise)){
        subsetExpr = expr[,groups %in% pairwise[,i]]
        subsetGroups  = groups[groups %in% pairwise[,i]]
        mm = model.matrix(~ subsetGroups,data.frame(subsetGroups))
        fit <- lmFit(subsetExpr, mm)
        fit <- eBayes(fit)
        dif = topTable(fit, coef=colnames(fit$design)[2],
                       lfc = log(foldTresh,base=2),
                       number = Inf, 
                       p.value = pThresh)
        if (nrow(dif)==0){
            print(paste('no genes found for',paste(pairwise[,i],collapse='/') ,'pair'))
            next()
        }
        
        bay = naiveBayes(t(subsetExpr[rownames(subsetExpr) %in% dif$ID,]), subsetGroups) 
        prediction = predict(bay, t(subsetExpr[rownames(subsetExpr) %in% dif$ID,]), 'raw', treshold = 0.001)
        
        uniSubGroups = unique(subsetGroups)
        # you shouldn't really need the 0.5 treshold it's often just a 1 or 0
        mislabelSamp = colnames(subsetExpr)[!((prediction[,uniSubGroups[1]]>0.5) == (subsetGroups == uniSubGroups[1]))]
        mislabelLabel = apply(prediction[!((prediction[,uniSubGroups[1]]>0.5) == (subsetGroups == uniSubGroups[1])),,drop=F],
                              1,function(x){
                                  colnames(prediction)[(x[1]>0.5)*1+(x[2]>0.5)*2]
                              })
        j = 0
        mislabelprop = apply(prediction[!((prediction[,uniSubGroups[1]]>0.5) == (subsetGroups == uniSubGroups[1])),,drop=F],
                             1,function(x){
                                 j <<- j +1
                                 x[(x[1]>0.5)*1+(x[2]>0.5)*2]
                             })
        
        if (len(mislabelSamp)!=0){
            sapply(1:len(mislabelSamp), function(i){
                mislabel[[mislabelSamp[i]]] <<- c(mislabel[[mislabelSamp[i]]], mislabelLabel[i])
            })
        }
        
        if (len(mislabelprop)!=0){
            sapply(1:len(mislabelprop), function(i){
                mislabelProbs[[mislabelSamp[i]]] <<- c(mislabelProbs[[mislabelSamp[i]]], mislabelprop[i])
            })
        }
    }
    
    mislabel = mislabel[!sapply(mislabel, is.null)]
    mislabelProbs = mislabelProbs[!sapply(mislabelProbs, is.null)]
    
    finalDecision = vector(mode = 'list',length = len(mislabel))
    names(finalDecision) = names(mislabel)
    for (i in 1:len(mislabel)){
        if (len(mislabel[[i]])==1){
            finalDecision[[i]] = mislabel[[i]]
        } else {
            pairs = combn(mislabel[[i]],2)
            roundRobin = vector(length= ncol(pairs))
            for (j in 1:ncol(pairs)){
                subsetExpr = expr[,groups %in% pairs[,j]]
                subsetGroups  = groups[groups %in% pairs[,j]]
                mm = model.matrix(~ subsetGroups,data.frame(subsetGroups))
                fit <- lmFit(subsetExpr, mm)
                fit <- eBayes(fit)
                dif = topTable(fit, coef=colnames(fit$design)[2],
                               lfc = log(foldTresh,base=2),
                               number = Inf, 
                               p.value = pThresh)
                if (nrow(dif)==0){
                    print(paste('no genes found for',paste(pairs[,j],collapse='/') ,'pair'))
                    roundRobin[j] = 'undecided'
                    next()
                }
                
                bay = naiveBayes(t(subsetExpr[rownames(subsetExpr) %in% dif$ID,]), subsetGroups) 
                prediction = predict(bay, t(expr[rownames(expr) %in% dif$ID,
                                                       colnames(expr) %in% names(mislabel[i])]), 
                                     'raw', treshold = 0.001)
                roundRobin[j] = colnames(prediction)[prediction>0.5]
            }
            
            if (max(table(roundRobin)) == len(mislabel[[i]])-1){
                finalDecision[[i]] = names(table(roundRobin))[which.max(table(roundRobin))]
            } else {
                finalDecision[[i]] = rbind(pairs, roundRobin)
            }
            
        }
    }
    
}
