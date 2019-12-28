library(dplyr)
library(ggplot2)
library(pdftools)
library(tidyverse)



###------依序做完下列步驟即可完成文本資料抽取。--------
##讀入檔案
datdir = "D:/目標資料夾/"
book = paste(datdir,"Gagne, RJ & Jaschhof, M (2017) A catalog of Cecidomyiidae (Diptera) of the world, 4th Edition. Digital, 762pp.pdf", sep = '')

pages = pdf_text(book) %>% strsplit(split = "\r\n"); length(pages)
ExtractTable = data.frame("page" = c(),"row" = c(), "text" = c())


#Find all species name rows, ver.2. >> OK
for (i in 7:569){
  
    rows = grep("^\\S",pages[[i]])
    
    if (length(rows) != 0){
    for (k in 1:length(rows)) {
      if (k < length(rows)) {
        taxadata =  ""
      for (j in rows[k]:(rows[k+1]-1)) {
        taxadata = paste(taxadata,pages[[i]][j], sep = "")
      }
      document = data.frame("page" = i, "row" = rows[k], "text" = taxadata, stringsAsFactors = FALSE)
      colnames(document) <- c("page", "row", "text")
      ExtractTable <- rbind(ExtractTable,document) 
      }else{
        
      taxadata = ""
      for (j in rows[k]:length(pages[[i]])) {
        taxadata = paste(taxadata,pages[[i]][j], sep = "")
      }
      rowsnext = grep("^\\S",pages[[i+1]])
      for (j in 2:(rowsnext[1]-1)) {
        taxadata = paste(taxadata,pages[[i+1]][j], sep = "")
      }
      
      document = data.frame("page" = i, "row" = rows[k], "text" = taxadata, stringsAsFactors = FALSE)
      colnames(document) <- c("page", "row", "text")
      ExtractTable <- rbind(ExtractTable,document)   
      }
      }
    }else{}
    cat('now is handling',i,'-th page, (still',(length(pages)-i),'to go!!)', '\n')
}


#Find all Genus name rows. >> OK
GenusTable = data.frame("page" = c(),"row" = c(), "genus" = c())
for (i in 7:569){
  
  rows = grep("^\\s{25,}[A-Z]+",pages[[i]])
  #rows = rows[-1]
  if (length(rows) != 0){
    for (k in 1:length(rows)) {
        document = data.frame("page" = i, "row" = rows[k], "genus" = pages[[i]][rows[k]], stringsAsFactors = FALSE)
        colnames(document) <- c("page", "row", "genus")
        GenusTable <- rbind(GenusTable,document)   
    }
  }else{}
  cat('now is handling',i,'-th page, (still',(length(pages)-i),'to go!!)', '\n')
}
GenusTable$genus = str_squish(GenusTable$genus)

#Create Genus column, and fill it with right genus names.
ExtractTable$Genus = ""

for (l in 1:length(ExtractTable$page)) {
  genuscut = max(which(ExtractTable[l,]$page >= GenusTable$page))
  newcut = min(which(GenusTable$page == GenusTable[genuscut,]$page))
  if (ExtractTable[l,]$page == GenusTable[newcut,]$page && ExtractTable[l,]$row < GenusTable[newcut,]$row){ 
    ExtractTable[l,]$Genus = GenusTable[newcut-1,]$genus
  }else{
    ExtractTable[l,]$Genus = GenusTable[newcut,]$genus
  }
  cat('now is handling',l,'-th page, (still',(length(ExtractTable$page)-l),'to go!!)', '\n')
}


#Create Genus column, and fill it with right genus names. Ver. 1120 >> OK
ExtractTable$Genus = ""

for (l in 1:length(ExtractTable$page)) {
  
  genuscut = which(ExtractTable[l,]$page == GenusTable$page)
  
  if (length(genuscut) != 0 ) {
  row_order =  GenusTable[genuscut,]$row
  row_order = c(ExtractTable[l,]$row, row_order)
  row_order = sort(row_order)  #將目標列與Genus的列數排序
  row_pos = which(row_order == ExtractTable[l,]$row)
  finalrow = which(GenusTable[genuscut,]$row == row_order[row_pos-1])
  
  if (length(finalrow) != 0) {
  ExtractTable[l,]$Genus = GenusTable[genuscut,][finalrow,]$genus #填入正確genus於目標欄位
  }else{
    if (length(GenusTable[min(genuscut)-1,]$genus) != 0) {
      ExtractTable[l,]$Genus = GenusTable[min(genuscut)-1,]$genus
    }else{
      ExtractTable[l,]$Genus =  "" 
    }  
  }
  }else{
      tl = l
      while (length(which(ExtractTable[tl,]$page-1 == GenusTable$page)) == 0) {
        tl = tl-1
      }
      ExtractTable[l,]$Genus = GenusTable[max(which(ExtractTable[tl,]$page-1 == GenusTable$page)),]$genus 
      #此情況表示該列的屬在前一頁or前好幾頁(即同一頁無屬名的情況)，找出前一個屬並填入
  
      }
  cat('now is handling',l,'-th page, (still',(length(ExtractTable$page)-l),'to go!!)', '\n')
}


#Change the Genus name to lower case >> OK
ExtractTable$Genus_normal = ""
for (i in 1:length(ExtractTable$page)) {
ExtractTable[i,]$Genus_normal =paste(substring(ExtractTable[i,]$Genus, 1, 1), tolower(substring(ExtractTable[i,]$Genus, 2)), sep = "")
}


## Split each text into several column, within each is a useful info.
#Create those new column, and fill it with right info. >> OK
ExtractTable$Species = ""
ExtractTable$Host = ""

for (i in 1:length(ExtractTable$page)) {
  
content = ExtractTable[i,]$text %>% strsplit(split = ". ", fixed = TRUE)
Familyfirst = 3
Familylast = max(grep("\\([A-Z][a-z]+aceae)$|idae)$",content[[1]]))
if (Familylast == -Inf ) {
  ExtractTable[i,]$Host = "Unknown"
}else{
    host = ""
  for (j in Familyfirst:Familylast) {
    host = paste(host, content[[1]][j], sep = ". ")
  }
    host = substring(host, regexec("^.", host)[[1]][1]+2, regexec(")$", host)[[1]][1])
    host = str_squish(host)
    ExtractTable[i,]$Host = host
}
ExtractTable[i,]$Species = content[[1]][1]
cat('now is handling',i,'-th page, (still',(length(ExtractTable$page)-i),'to go!!)', '\n')
}

##Create host genus column, and fill it with the first host genus of the species.
#If the species's host isn't plant, paste all host.
#If the species has more than one host, paste all host name.  >> OK
ExtractTable$Host_genus=""
for (i in 1:length(ExtractTable$page)) {
  if (grepl("\\([A-Z][a-z]+aceae)", ExtractTable[i,]$Host) == TRUE) {
    match = gregexpr("\\([A-Z][a-z]+aceae)", ExtractTable[i,]$Host)[[1]]
    if (length(match) > 1) {
      ExtractTable[i,]$Host_genus = gsub(" \\([A-Z][a-z]+aceae)", "", ExtractTable[i,]$Host)
      ExtractTable[i,]$Host_genus = gsub(";", ",", ExtractTable[i,]$Host_genus)
      ExtractTable[i,]$Host_genus = gsub("and ", "", ExtractTable[i,]$Host_genus)
    }else{
      ExtractTable[i,]$Host_genus = substring(ExtractTable[i,]$Host, 1, (regexec(" ", ExtractTable[i,]$Host)[[1]]-1))  
    }  
    
  }else{
    ExtractTable[i,]$Host_genus = ExtractTable[i,]$Host
  }
  cat('now is handling',i,'-th page, (still',(length(ExtractTable$page)-i),'to go!!)', '\n')
  }
  

##Create Distribution column, and fill it with the distribution of the species. >> OK
ExtractTable$Distribution = ""
for (i in 1:length(ExtractTable$page)) {
content = ExtractTable[i,]$text %>% strsplit(split = ". ", fixed = TRUE)
ExtractTable[i,]$Distribution = str_squish(content[[1]][2])
cat('now is handling',i,'-th page, (still',(length(ExtractTable$page)-i),'to go!!)', '\n')
}


##Create reference column, and fill it with the description reference of the species. >> ok.
ExtractTable$Des_Reference = ""
for (i in 1:length(ExtractTable$page)) {
  content = ExtractTable[i,]$text %>% strsplit(split = ". ", fixed = TRUE)
  
  if (length(content[[1]]) > 1) {
  ##需要另存成變數，理想狀況是只填上文獻
    if (class(try(grep(content[[1]][1], content[[1]]), silent = TRUE)) != "try-error") {
      REF = (str_squish(content[[1]][grep(content[[1]][1], content[[1]])[2]]) %>% strsplit(split = " ", fixed = TRUE))[[1]]
      #N_REF = REF[3]
      
      #從第二個大寫的字開始。
      N_REF = REF[grep("^[A-Z][a-z]+",REF)[2]]
      
      if (is.na(N_REF) == TRUE) {
        N_REF = "-"
        ExtractTable[i,]$Des_Reference = N_REF
      }else{
        for (l in 4:length(REF)) {
          N_REF = paste(N_REF, REF[l], sep = " ")
        }
        ExtractTable[i,]$Des_Reference = N_REF
      }
      
    }else{
      ExtractTable[i,]$Des_Reference = "-"  
    }
  
  }else{
  ExtractTable[i,]$Des_Reference = "-" 
  }
  
  cat('now is handling',i,'-th page, (still',(length(ExtractTable$page)-i),'to go!!)', '\n')
}

##Create Type info. column, and fill it with the info. >> OK
ExtractTable$Type_information = ""
for (i in 1:length(ExtractTable$page)) {
  content = ExtractTable[i,]$text %>% strsplit(split = ". ", fixed = TRUE)
  ExtractTable[i,]$Type_information = content[[1]][length(content[[1]])]
  
  cat('now is handling',i,'-th page, (still',(length(ExtractTable$page)-i),'to go!!)', '\n')
}


##儲存結果
outfile = "D:/Research project/草本植物癭蚋 (20181109-)/階段性任務/1_世界菊科蟲癭分布pattern/MidgeExtractTable.txt"
write.table(ExtractTable, outfile, sep="\t", quote=F, row.names=F, col.names=T)
