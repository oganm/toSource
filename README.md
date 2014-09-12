toSource
========

**gsmDown:** Downloads GSM files from GEO repository. Normally you should be able to do it through webservice but just having GSM IDs is not enough since some files are also saved with their descriptions. This goes to the main website and gets the FTP link from there. It doesn't respect the crawl delay from robots.txt. You might want to add a sleep command somewhere in between.

**mergeChips:** Merges two chips of different generations that share probes. Probe names must be preserved. Applies RMA using Affy package

**ogbox:** stuff and things
