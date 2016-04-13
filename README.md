toSource
========

**GEO:** Has functions to deal with GEO database.
* **gsmDown:** Downloads GSM files from GEO repository. Normally you should be able to do it through webservice but just having GSM IDs is not enough since some files are also saved with their descriptions. This goes to the main website and gets the FTP link from there. It doesn't respect the crawl delay from robots.txt. You might want to add a sleep command somewhere in between.
* **gsmFind:** Given a GSE ID and a regex, it looks for GSM with matching descriptions to the regex.
* **gseDown:** Given a GSE ID and a regex, it downloads GSMs with matching descriptions to the regex.

**mergeChips:** Merges two chips of different generations that share probes. Probe names must be preserved. Applies RMA using Affy package

**homologene:** Uses homologene to get mouse-human homology information. Changing the species and adding new ones is quite easy.

**geneSynonym:** To find gene synonyms
* **prepGenes:** Fetchs gene_info.gz from NCBI, takes a subset based on species of interest and creates a smaller file only including synonyms. Defaults to human and mouse as species. Also by default it deletes the original downloaded file
* **geneSynonym:** given a list of genes, it looks for synonyms in the created file.

**gemmaAnnotate:** Uses annotations from [Gemma](http://www.chibi.ubc.ca/Gemma/home.html) to assign your probesets to gene symbols. This is supposed to be better since Gemma re-does the sequence alignments for probesets. There are a few probesets that map to multiple genes. These gene names are separated by a "|"
* **getGemmaAnnotGoogle:** Downloads annotation file for the chip for a given GEO id. Uses google's search API to reach the chip though. Normally works but don't use a chip that doesn't exist in gemma. If you chip is good it's probably in gemma.
* **getGemmaAnnot:** Given a gemma id, downloads the annotation file. I'll try to add a way to get gemma id-geo id matchings.
* **gemmaAnnot:** Annotates an ExoressionSet object. Commonly the output of pre-processing functions like `rma`. Outputs to a data frame and/or to a file. Output is invisible when writing to file.

**matchFiles:** Compares the files in two folders to see if they have the same content using md5sums. Useful when meddling with CEL files of questionable sources...


**matchMaker:** Disclaimer: This didn't work as well as I expected in my data, since some samples were quite close to each other and batch effects were strong. I don't suggest it's usage. Identifies wrongly labelled samples in a dataset using questionable bayesian clustering methods based on differentially expressed genes between each pair of groups. Individual members of the groups are reclustered and if they cluster wrongly, this is reported. If this happens in multiple pairwise comparisons, a bayesian classifier will be trained for every pair in the list of potential matches, and the sample is placed in a group based on each classifier. If one group wins in all classification "competitions" that match is reported. If there are disputes, result of every pairwise comparison for that sample is reported. Assumes groups are sufficiently distant from each other. Adjusting fold change and p value tresholds might be required for individual datasets

**ogbox:** stuff and things
