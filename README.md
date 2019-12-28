# PDF-to-Data.frame

我的碩士論文中，需要整理大量的分類群發表資料作為描述性統計的資料來源。
這些資料目前已有學者統整於成冊的電子書中，但「.pdf」的檔案格式無法直接用作分析。
這個專案使用R package "pdftools"將檔案讀入R，並搭配正則表達式將該文本整理成data.frame的格式。
本次抽取的電子書為：Gagné RJ, Jaschhof M. 2017. A catalog of Cecidomyiidae (Diptera) of the world, 4th Edition. Digital. 762 pp.  
這是一份關於至2017年為止，世界上癭蚋科昆蟲之發表資訊的重要文獻。

P.S.   
分類群發表資料本身有一定程度的結構化，並非如一般文章，故本次資料清理相對輕鬆。

