
library(readxl)
library(dplyr)

path_master='/Users/ali/Desktop/Aug23/human_rna/rna_human/AD_DECODE_data022222.csv'
data=read.csv(path_master )
datatemp=data%>%dplyr::select(MRI_Exam,genotype, sex,  age,risk_for_ad, Subject, )#subselect
#nchar(datatemp[111,1])
datatemp=na.omit(datatemp)
# datatemp[nchar(datatemp$DWI)==1,]=matrix(NA,1,dim(datatemp)[2])
# datatemp=na.omit(datatemp)
# datatemp[substr(datatemp$DWI,1,1)!="N",]=matrix(NA,1,dim(datatemp)[2])
datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring
datatemp$MRI_Exam=as.numeric(datatemp$MRI_Exam) # make dwi numeric
# datatemp=datatemp[datatemp$Genotype!="HN",]

####
path_connec="/Users/ali/Desktop/Aug23/human_rna/rna_human/connectome/"
file_list=list.files(path_connec, pattern = "*plain.csv")
temp_conn= read.csv2( paste0(path_connec,file_list[1]) , sep = ",", header = F)
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
sum(is.na(temp_conn))
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$MRI_Exam[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
  temp_connec= read.csv2( paste0(path_connec,file_list[temp_index]) , sep = ",", header = F)
  #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
  colnames(temp_connec)=NA
  connectivity[,,i]=dense_rank(as.matrix(temp_connec))
  
  }
  else
    notfound=c(notfound, datatemp$MRI_Exam[i])
  
}

 notfound=notfound[2:length(notfound)]
 not_found_index=which( datatemp$MRI_Exam  %in%  notfound )

 datatemp=datatemp[-not_found_index,]
 connectivity=connectivity[,,-not_found_index]
sum(is.na(connectivity))

response=datatemp
#setwd(system("pwd", intern = T) )


RNA=read.delim('/Users/ali/Desktop/Aug23/human_rna/rna_human/humanwithsymbol.txt',sep="\t", header = T)
RNA=t(RNA)
RNA_rows=rownames(RNA)

# RNA_rows= as.numeric(gsub("\\D", "", RNA_rows))
#RNA_rows=RNA_rows[2:(length(RNA_rows)-1)]

RNA_data=RNA[2:(dim(RNA)[1]-1),]
colnames(RNA_data)=RNA[dim(RNA)[1],]
rownames(RNA_data)=RNA_rows[2:(length(RNA_rows)-1)]
#rownames(RNA_data)=NULL
sum(is.na(RNA_data))
#RNA_data[!is.na(RNA_data)]=NA
sum(is.na(RNA_data))


# exist_response_ID=sub("\\s*\\:.*$", "",  response$Subject)
response$Subject
# response$Subject=as.numeric(gsub("\\D", "", exist_response_ID )) 
#as.numeric(rownames(RNA_data))


found=0
notfound=0
for (jj in 1:dim(RNA_data)[1]) {
  index=which( row.names( RNA_data)   [jj]== response$Subject      )
  # index=which( as.numeric(row.names( RNA_data)   [jj])== as.numeric(response$Subject)      )
  if (length(index)>0){
        found=c(found,index)
  }
  
  else notfound=c(notfound,jj)
    
  }
found=found[2:length(found)]

response=response[found,]
dim(response)

connectivity=connectivity[,,found]
dim(connectivity)

notfound=notfound[2:length(notfound)] # all found
 RNA_data=RNA_data[-notfound,]

dim(RNA_data)

rownames(RNA_data)=NULL


save(response, file="response.rda")
save(connectivity, file="connectivity.rda")
save(RNA_data, file="RNA_data.rda")





