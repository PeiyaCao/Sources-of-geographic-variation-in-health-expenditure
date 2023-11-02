options(digits = 15)
options(scipen = 200)
library(reshape2)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(rjson)
library(foreign)

#Load hospital discharge file
load("G:/曹裴娅/毕业论文/数据整理/第四季度16至19/2017_zxy/zxy_2017.RData")
load("G:/曹裴娅/毕业论文/数据整理/2018_zxy/第四季度16至19/zxy_2018.RData")
load("G:/曹裴娅/毕业论文/数据整理/2019_zxy/第四季度16至19/zxy_2019.RData")
data <- rbind(zxy_2017,zxy_2018,zxy_2019)
rm(zxy_2017)
rm(zxy_2018)
rm(zxy_2019)
data$ID <- c(1:nrow(data))
length(unique(data$ID))
write.csv(data,"G:/曹裴娅/毕业论文/zxs/data_new.csv",row.names=F)
names(data)
data_new <- data[,c("ID","XZZ","XZZ_XZQH","YLJGID","TX_DZ","DEPT_ADRRESSCODE")]
length(unique(data_new$ID))
rm(data)

#Load town/subdistrict data
xz_data <- read.csv("G:/曹裴娅/毕业论文/原始数据/town_2019.csv")
length(unique(xz_data$town_code))
xz_data$county_code <- substr(xz_data$town_code,1,6)
names(xz_data)
qx_data <- read.csv("G:/曹裴娅/毕业论文/原始数据/qx_data.csv")
length(unique(qx_data$county_code))

########
length(which(data_new$XZZ%in% c("NA","","-","--","---","----","------")))
data_new$XZZ[which(data_new$XZZ %in%  c("NA","","-","--","---","----","------"))] <- NA
data_new$XZZ_XZQH[which(data_new$XZZ_XZQH %in%  c("NA","","-","--","---","----","------"))] <- NA

#####Remove records of addresses with missing values----------
shan1 <- which(is.na(data_new$XZZ)==T & is.na(data_new$XZZ_XZQH)==T)
shan1 <- data_new[shan1,]
data1 <- anti_join(data_new,shan1)
nrow(data1)+nrow(shan1)==nrow(data_new)

#####Remove records outside Sichuan province----
shan2<-data1[grep("新疆维吾尔自治区|乌鲁木齐市|西藏|拉萨市|陕西省|云南|贵州省|重庆|广东省|湖南省|湖北省|
                  |山东省|江西省|福建省|安徽省|浙江省|江苏省|黑龙江省|吉林省|辽宁省|内蒙古|山西省|河北省|河南省|
                  |甘肃|北京市|天津市|广西壮族自治区|海南省|青海省|西宁市|西安|香港特别行政区",data1$XZZ),]
data2 <- anti_join(data1,shan2)
nrow(data2)+nrow(shan1)+nrow(shan2)==nrow(data_new)

#####Records only with county info----
a <- qx_data$qx
b <- paste0(qx_data$X,qx_data$qx)
c <- paste0("四川省",b)
d <- paste0("四川省",a)
e <- "四川省"
f <- unique(paste0("四川省",qx_data$X))
g <- qx_data$X
shan3 <- data2[data2$XZZ %in% c(a,b,c,d,e,f,g),]
data3 <- anti_join(data2,shan3)
nrow(data3)+nrow(shan1)+nrow(shan2)+nrow(shan3)==nrow(data_new)

#Records with NA addresses
shan4 <- data3[which(is.na(data3$XZZ)==T),]
data4 <- anti_join(data3,shan4)
nrow(data4)+nrow(shan1)+nrow(shan2)+nrow(shan3)+nrow(shan4)==nrow(data_new)

#Records with only numbers in addresses
shan5 <- data4[grep(pattern = "^[0-9]*$",data4$XZZ,fixed=F),]
data5 <- anti_join(data4,shan5)
nrow(data5)+nrow(shan1)+nrow(shan2)+nrow(shan3)+nrow(shan4)+nrow(shan5)==nrow(data_new)

#####Use the "town/subdistrict" in the name of the current addresses to locate----
###Records can be located to towns/subdistricts
data5_town<-data5[grep("镇",data5$XZZ),]
xz_data$town_name <- as.character(xz_data$town_name)
town<-xz_data[grep("镇",xz_data$town_name),]
town$town_name_new <- sapply(strsplit(town$town_name,"街道"),function(x){x[1]})
town$IND<-nchar(town$town_name_new)
data5_town$XZZ<-as.character(data5_town$XZZ)
data5_town$town_IND<-nchar(sapply(strsplit(data5_town$XZZ,"镇"),function(x){x[1]}))+1
data5_town$newid<-c(1:nrow(data5_town))

data5_town_T<-data.frame()
data5_town_F<-data.frame()
for (i in unique(town$IND)) {
  town_IND<-town[town$IND==i,]
  town_IND$town_name_O<-substr(town_IND$town_name,1,i-1)
  data5_town$town_name <- substr(data5_town$XZZ,data5_town$town_IND-(i-1),data5_town$town_IND-1)
  data5town_IND<-data5_town[data5_town$town_name %in% town_IND$town_name_O,]
  if(nrow(data5town_IND)==0)
  {next}else{
       town_IND_T<-town_IND[town_IND$town_name_O %in% data5town_IND$town_name,]
    town_IND_T$Code_new<-substr(town_IND_T$town_code,1,6)
       for (m in 1:nrow(data5town_IND)){
      town_IND_T_IND<-town_IND_T[town_IND_T$town_name_O==data5town_IND$town_name[m],]
      if(data5town_IND$XZZ_XZQH[m] %in% town_IND_T_IND$Code_new){
        data5town_IND$town_code_IND[m]<-T
      }else{data5town_IND$town_code_IND[m]<-F}
    }
  }
  data5_town_T<-rbind(data5_town_T,data5town_IND[data5town_IND$town_code_IND==T,])
  data5_town_F<-rbind(data5_town_F,data5town_IND[data5town_IND$town_code_IND==F,])
}


data5_town_F<-data5_town_F[!duplicated(data5_town_F$newid),]
data5_town_F_D<-data5_town_F[data5_town_F$newid %in% data5_town_T$newid,]
data5_town_F_new<-anti_join(data5_town_F,data5_town_F_D)
data5_town$newid<-as.numeric(data5_town$newid);data5_town_F_new$newid<-as.numeric(data5_town_F_new$newid)
data5_town_O<-anti_join(data5_town,rbind(data5_town_F_new,data5_town_T),by="newid")
nrow(data5_town_F_new)+nrow(data5_town_T)+nrow(data5_town_O)==nrow(data5_town)
#Match with town/subdistricts code
town$Name_first <- substr(town$town_name_new,1,town$IND-1)
town$Code_first <- substr(town$town_code,1,6)
town$town_code <- as.character(town$town_code)
for (i in 1:nrow(data5_town_T)) {
  IND <- town[town$Code_first==data5_town_T$XZZ_XZQH[i],]
  IND <- as.data.frame(IND)
  data5_town_T$Code_stv_P[i] <- IND[IND$Name_first==data5_town_T$town_name[i],]$town_code
}

##The name of the current addresses include the name of the towns/subdistricts in Sichuan province and the district and county where it is located are consistent.
data5_v.qt <- anti_join(data5,data5_town)
data5_village <- data5_v.qt[grep("乡",data5_v.qt$XZZ),]
village<-xz_data[grep("乡",xz_data$town_name),]
village$town_name_new <- sapply(strsplit(village$town_name,"街道"),function(x){x[1]})
village$IND<-nchar(village$town_name_new)
data5_village$XZZ<-as.character(data5_village$XZZ)
data5_village$town_IND<-nchar(sapply(strsplit(data5_village$XZZ,"乡"),function(x){x[1]}))+1
data5_village$newid<-matrix(c(1:nrow(data5_village)),nrow(data5_village),1)
data5_village_T<-data.frame()
data5_village_F<-data.frame()

for (i in unique(village$IND)) {
  village_IND<-village[village$IND==i,]
  village_IND$town_name_O<-substr(village_IND$town_name,1,i-1)
   data5_village$town_name <- substr(data5_village$XZZ,data5_village$town_IND-(i-1),data5_village$town_IND-1)
   data5village_IND<-data5_village[data5_village$town_name %in% village_IND$town_name_O,]
    if(nrow(data5village_IND)==0)
  {next}else{
       village_IND_T<-village_IND[village_IND$town_name_O %in% data5village_IND$town_name,]
    village_IND_T$Code_new<-substr(village_IND_T$town_code,1,6)
        for (m in 1:nrow(data5village_IND)){
      village_IND_T_IND<-village_IND_T[village_IND_T$town_name_O==data5village_IND$town_name[m],]
      if(data5village_IND$XZZ_XZQH[m] %in% village_IND_T_IND$Code_new){
        data5village_IND$town_code_IND[m]<-T
      }else{data5village_IND$town_code_IND[m]<-F}
    }
  }
  data5_village_T<-rbind(data5_village_T,data5village_IND[data5village_IND$town_code_IND==T,])
  data5_village_F<-rbind(data5_village_F,data5village_IND[data5village_IND$town_code_IND==F,])
}

data5_village_F<-data5_village_F[!duplicated(data5_village_F$newid),]
data5_village_F_D<-data5_village_F[data5_village_F$newid %in% data5_village_T$newid,]
data5_village_F_new<-anti_join(data5_village_F,data5_village_F_D)
data5_village$newid<-as.numeric(data5_village$newid);data5_village_F_new$newid<-as.numeric(data5_village_F_new$newid)
data5_village_O<-anti_join(data5_village,rbind(data5_village_F_new,data5_village_T),by="newid")
nrow(data5_village_F_new)+nrow(data5_village_T)+nrow(data5_village_O)==nrow(data5_village)
length(unique(data5_village_T$newid))==nrow(data5_village_T)
data5_village_T_unique <- data5_village_T[!duplicated(data5_village_T$newid),]
data5_village_T_D <- anti_join(data5_village_T,data5_village_T_unique)
data5_village_T_D <- data5_village_T[data5_village_T$newid %in% data5_village_T_D$newid,]
unique(data5_village_T_D$town_name)
data5_village_T_unique_true <- anti_join(data5_village_T,data5_village_T_D)
data5_village_T_D_true <- data5_village_T_D[data5_village_T_D$town_name=="老寨子",]
data5_village_T_new <- rbind(data5_village_T_unique_true,data5_village_T_D_true)
nrow(data5_village_F_new)+nrow(data5_village_T_new)+nrow(data5_village_O)==nrow(data5_village)

#Match with town/subdistricts code
village$Name_first <- substr(village$town_name_new,1,village$IND-1)
village$Code_first <- substr(village$town_code,1,6)
village$town_code <- as.character(village$town_code)
for (i in 1:nrow(data5_village_T_new)) {
  IND <- village[village$Code_first==data5_village_T_new$XZZ_XZQH[i],]
  IND <- as.data.frame(IND)
  data5_village_T_new$Code_stv_P[i] <- IND[IND$Name_first==data5_village_T_new$town_name[i],]$town_code
}

##Processing address changes from towns to subdistricts
town$town_code <- as.character(town$town_code);village$town_code <- as.character(village$town_code);xz_data$town_code <- as.character(xz_data$town_code)
ts <- anti_join(xz_data,rbind(town,village))
ts$town_name_new <- sapply(strsplit(ts$town_name,"街道"),function(x){x[1]})
ts$IND <- nchar(ts$town_name_new)
ts$Code_first <- substr(ts$town_code,1,6)
ts$Name_ts<-ts$town_name_new
ts$IND_ts<-ts$IND

town$Name_ts<-substr(town$town_name_new,1,nchar(town$town_name_new)-1)
town$IND_ts <- nchar(town$Name_ts)
village$Name_ts <- substr(village$town_name_new,1,nchar(village$town_name_new)-1)
village$IND_ts <- nchar(village$Name_ts)

names(ts)
names(town)
names(village)
qt <- rbind(ts,town[,c(1:12,14:16)],village[,c(1:12,14:16)])

names(data5_town_O)
names(data5_town_F_new)
names(data5_village_O)
names(data5_village_F_new)
data5_tv_qt <- rbind(data5_town_O,data5_town_F_new[,c(1:9)],data5_village_O,data5_village_F_new[,c(1:9)])
nrow(data5_tv_qt)+nrow(data5_town_T)+nrow(data5_village_T_new)
nrow(data5_town)+nrow(data5_village)
data5_tv_qt$XZZ<-as.character(data5_tv_qt$XZZ)
data5_tv_qt$newid<-matrix(c(1:nrow(data5_tv_qt)),nrow(data5_tv_qt),1)
data5_tv_qt_T<-data.frame()
data5_tv_qt_F<-data.frame()


for (i in unique(qt$IND_ts)) {
  qt_IND<-qt[qt$IND_ts==i,]
  qt_IND$town_name_O<-qt_IND$Name_ts
  data5_tv_qt$town_name <- substr(data5_tv_qt$XZZ,data5_tv_qt$town_IND-(i),data5_tv_qt$town_IND-1)
  data5_tv_qt_IND<-data5_tv_qt[data5_tv_qt$town_name %in% qt_IND$town_name_O,]
  if(nrow(data5_tv_qt_IND)==0)
  {next}else{
    qt_IND_T<-qt_IND[qt_IND$town_name_O %in% data5_tv_qt_IND$town_name,]
    qt_IND_T$Code_new<-substr(qt_IND_T$town_code,1,6)
    for (m in 1:nrow(data5_tv_qt_IND)){
      qt_IND_T_IND<-qt_IND_T[qt_IND_T$town_name_O==data5_tv_qt_IND$town_name[m],]
      if(data5_tv_qt_IND$XZZ_XZQH[m] %in% qt_IND_T_IND$Code_new){
        data5_tv_qt_IND$town_code_IND[m]<-T
      }else{data5_tv_qt_IND$town_code_IND[m]<-F}
    }
  }
  data5_tv_qt_T<-rbind(data5_tv_qt_T,data5_tv_qt_IND[data5_tv_qt_IND$town_code_IND==T,])
  data5_tv_qt_F<-rbind(data5_tv_qt_F,data5_tv_qt_IND[data5_tv_qt_IND$town_code_IND==F,])
}

data5_tv_qt_F<-data5_tv_qt_F[!duplicated(data5_tv_qt_F$newid),]
data5_tv_qt_F_D<-data5_tv_qt_F[data5_tv_qt_F$newid %in% data5_tv_qt_T$newid,]
data5_tv_qt_F_new<-anti_join(data5_tv_qt_F,data5_tv_qt_F_D)
data5_tv_qt$newid<-as.numeric(data5_tv_qt$newid);data5_tv_qt_F_new$newid<-as.numeric(data5_tv_qt_F_new$newid)
data5_tv_qt_O<-anti_join(data5_tv_qt,rbind(data5_tv_qt_F_new,data5_tv_qt_T),by="newid")
nrow(data5_tv_qt_F_new)+nrow(data5_tv_qt_T)+nrow(data5_tv_qt_O)==nrow(data5_tv_qt)

#Match with town/subdistricts code
qt$Code_first <- substr(qt$town_code,1,6)
qt$town_code <- as.character(qt$town_code)
for (i in 1:nrow(data5_tv_qt_T)) {
  IND <- qt[qt$Code_first==data5_tv_qt_T$XZZ_XZQH[i],]
  IND <- as.data.frame(IND)
  data5_tv_qt_T$Code_stv_P[i] <- IND[IND$Name_ts==data5_tv_qt_T$town_name[i],]$town_code
}

names(data5_tv_qt_F_new)
names(data5_tv_qt_O)
data5_tv_qt_ts <- rbind(data5_tv_qt_F_new[,c(1:9)],data5_tv_qt_O)
nrow(data5_town_T)+nrow(data5_village_T_new)+nrow(data5_tv_qt_T)+nrow(data5_tv_qt_ts)==nrow(data5_town)+nrow(data5_village)
qt <- as.data.frame(qt)
qt_CF <- qt[duplicated(qt$Name_ts),]
qt_new <- qt[!qt$Name_ts %in% qt_CF$Name_ts,]
qt_CF <- anti_join(qt,qt_new)
nrow(qt_new)+nrow(qt_CF)==nrow(qt)

data5_tv_qt_ts$newid <- c(1:nrow(data5_tv_qt_ts))
data5_tv_qt_ts_T<-data.frame()
data5_tv_qt_ts_F<-data.frame()

for (i in unique(qt_new$IND_ts)) {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                度
  qt_new_IND<-qt_new[qt_new$IND_ts==i,]
  qt_new_IND$town_name_O<-qt_new_IND$Name_ts
  data5_tv_qt_ts$town_name <- substr(data5_tv_qt_ts$XZZ,data5_tv_qt_ts$town_IND-(i),data5_tv_qt_ts$town_IND-1)
  data5_tv_qt_ts_IND<-data5_tv_qt_ts[data5_tv_qt_ts$town_name %in% qt_new_IND$town_name_O,]
  data5_tv_qt_ts_T<-rbind(data5_tv_qt_ts_T,data5_tv_qt_ts_IND)
}
length(unique(data5_tv_qt_ts_T$newid))==nrow(data5_tv_qt_ts_T)
data5_tv_qt_ts_T_cf <- data5_tv_qt_ts_T[duplicated(data5_tv_qt_ts_T$newid),]
unique(data5_tv_qt_ts_T_cf$town_name)
data5_tv_qt_ts_T_cf_new <- data5_tv_qt_ts_T[data5_tv_qt_ts_T$newid %in% data5_tv_qt_ts_T_cf$newid,]
data5_tv_qt_ts_T_cf_new <- data5_tv_qt_ts_T_cf_new[order(data5_tv_qt_ts_T_cf_new$newid),]
unique(data5_tv_qt_ts_T_cf_new[seq(1,nrow(data5_tv_qt_ts_T_cf_new),2),"town_name"])
unique(data5_tv_qt_ts_T_cf_new[seq(2,nrow(data5_tv_qt_ts_T_cf_new),2),"town_name"])
data5_tv_qt_ts_T_cf_new$IND <- nchar(data5_tv_qt_ts_T_cf_new$town_name)
data5_tv_qt_ts_T_cf_new$ind <- NA
for (i in seq(1,nrow(data5_tv_qt_ts_T_cf_new),2)) {
  data5_tv_qt_ts_T_cf_new$ind[i] <- ifelse(data5_tv_qt_ts_T_cf_new[i,"IND"]<data5_tv_qt_ts_T_cf_new[i+1,"IND"],F,T)
}
for (i in seq(2,nrow(data5_tv_qt_ts_T_cf_new),2)) {
  data5_tv_qt_ts_T_cf_new$ind[i] <- ifelse(data5_tv_qt_ts_T_cf_new[i,"IND"]<data5_tv_qt_ts_T_cf_new[i-1,"IND"],F,T)
}
data5_tv_qt_ts_T_cf_new <- data5_tv_qt_ts_T_cf_new[which(data5_tv_qt_ts_T_cf_new$ind==T),c(1:9)]
unique(data5_tv_qt_ts_T_cf_new$town_name)

data5_tv_qt_ts_T_unique <- data5_tv_qt_ts_T[!data5_tv_qt_ts_T$newid %in% data5_tv_qt_ts_T_cf$newid,]
length(unique(data5_tv_qt_ts_T_unique$newid))==nrow(data5_tv_qt_ts_T_unique)
data5_tv_qt_ts_T_new <- rbind(data5_tv_qt_ts_T_cf_new,data5_tv_qt_ts_T_unique)
length(unique(data5_tv_qt_ts_T_new$newid))==nrow(data5_tv_qt_ts_T_new)

data5_tv_qt_ts_T_new <- merge(data5_tv_qt_ts_T_new,qt[,c(4,14)],by.x = "town_name",by.y = "Name_ts")

length(unique(data5_tv_qt_ts_F$newid))==nrow(data5_tv_qt_ts_F)

data5_tv_qt_ts_true_data_new <- data5_tv_qt_ts_T_new
names(data5_tv_qt_ts_true_data_new)[10] <- c("Code_stv_P")
names(data5_town_T)
names(data5_village_T_new)
names(data5_tv_qt_T)
names(data5_tv_qt_ts_true_data_new)
data5_final_T <- rbind(data5_town_T[,c(1:6,11)],data5_village_T_new[,c(1:6,11)],data5_tv_qt_T[,c(1:6,11)],data5_tv_qt_ts_true_data_new[,c(2:7,10)])
data5_tv_qt_ts_F <- anti_join(data5_tv_qt_ts,data5_tv_qt_ts_true_data_new,by="newid")
nrow(data5_tv_qt_ts_true_data_new)+nrow(data5_tv_qt_ts_F)==nrow(data5_tv_qt_ts)

#####The address containing the info after the county----
names(data5)
names(data5_town)
names(data5_village)
data5_qt <- anti_join(data5,rbind(data5_town[,c(1:6)],data5_village[,c(1:6)]))
names(data5_qt)
names(data5_tv_qt_ts_F)
data6 <- rbind(data5_qt,data5_tv_qt_ts_F[,c(1:6)])
nrow(data6)+nrow(data5_final_T)==nrow(data5)
length(unique(data6$ID))==nrow(data6)

h <- c(b,c,d)
H <- c()
for (i in 1:length(h)) {
  H <- paste0(H,h[i],"|")
}
H <- substr(H,1,nchar(H)-1)
H
A <- c()
for (i in 1:length(a)) {
  A <- paste0(A,a[i],"|")
}
A <- substr(A,1,nchar(A)-1)
A
data6_ll_dz <- data6[grep(A,data6$XZZ),]
data6_ll_dz_1 <- data6[grep(H,data6$XZZ),]
data6_ll_dz_2 <- anti_join(data6_ll_dz,data6_ll_dz_1)
data6_ll_dz_2$XZZ_new <- paste("四川省",data6_ll_dz_2$XZZ,sep="")
data6_ll_dz_1$XZZ_new <- data6_ll_dz_1$XZZ
data6_ll_dz_new <- rbind(data6_ll_dz_1,data6_ll_dz_2)
data6_ll_dz_new$XZZ_new <- gsub("-","",data6_ll_dz_new$XZZ_new)
data6_ll_dz_new$XZZ_new <- gsub("#","",data6_ll_dz_new$XZZ_new)

g_new <- unique(g)
L <- c(a,e,g_new)
data6_ll_dz_test <- data6_ll_dz_new
data6_ll_dz_test$IND <- F
data6_ll_dz_test$XZZ_IND <- NA
for (i in 1:length(L)) {
  data6_ll_dz_test$XZZ_IND[which(data6_ll_dz_test$IND==F)]<- NA
  data6_ll_dz_test$XZZ_IND[which(data6_ll_dz_test$IND==F)] <- unlist(lapply(strsplit(data6_ll_dz_test$XZZ_new[which(data6_ll_dz_test$IND==F)],L[i]),paste,collapse=""))
  data6_ll_dz_test$IND[which(data6_ll_dz_test$IND==F)] <- ifelse(nchar(data6_ll_dz_test$XZZ_IND[which(data6_ll_dz_test$IND==F)])<nchar(data6_ll_dz_test$XZZ_new[which(data6_ll_dz_test$IND==F)]),T,F)
  print(i)
}
sum(data6_ll_dz_test$IND)

data6_ll_dz_test$IND_1 <- F
data6_ll_dz_test$XZZ_IND_1 <- NA
for (i in 1:length(L)) {
  data6_ll_dz_test$XZZ_IND_1[which(data6_ll_dz_test$IND_1==F)]<- NA
  data6_ll_dz_test$XZZ_IND_1[which(data6_ll_dz_test$IND_1==F)] <- unlist(lapply(strsplit(data6_ll_dz_test$XZZ_IND[which(data6_ll_dz_test$IND_1==F)],L[i]),paste,collapse=""))
  data6_ll_dz_test$IND_1[which(data6_ll_dz_test$IND_1==F)] <- ifelse(nchar(data6_ll_dz_test$XZZ_IND_1[which(data6_ll_dz_test$IND_1==F)])<nchar(data6_ll_dz_test$XZZ_IND[which(data6_ll_dz_test$IND_1==F)]),T,F)
  print(i)
}
sum(data6_ll_dz_test$IND_1)

data6_ll_dz_test$IND_2 <- F
data6_ll_dz_test$XZZ_IND_2 <- NA
for (i in 1:length(L)) {
  data6_ll_dz_test$XZZ_IND_2[which(data6_ll_dz_test$IND_2==F)]<- NA
  data6_ll_dz_test$XZZ_IND_2[which(data6_ll_dz_test$IND_2==F)] <- unlist(lapply(strsplit(data6_ll_dz_test$XZZ_IND_1[which(data6_ll_dz_test$IND_2==F)],L[i]),paste,collapse=""))
  data6_ll_dz_test$IND_2[which(data6_ll_dz_test$IND_2==F)] <- ifelse(nchar(data6_ll_dz_test$XZZ_IND_2[which(data6_ll_dz_test$IND_2==F)])<nchar(data6_ll_dz_test$XZZ_IND_1[which(data6_ll_dz_test$IND_2==F)]),T,F)
  print(i)
}
sum(data6_ll_dz_test$IND_2)

data6_ll_dz_test$IND_3 <- F
data6_ll_dz_test$XZZ_IND_3 <- NA
for (i in 1:length(L)) {
  data6_ll_dz_test$XZZ_IND_3[which(data6_ll_dz_test$IND_3==F)]<- NA
  data6_ll_dz_test$XZZ_IND_3[which(data6_ll_dz_test$IND_3==F)] <- unlist(lapply(strsplit(data6_ll_dz_test$XZZ_IND_2[which(data6_ll_dz_test$IND_3==F)],L[i]),paste,collapse=""))
  data6_ll_dz_test$IND_3[which(data6_ll_dz_test$IND_3==F)] <- ifelse(nchar(data6_ll_dz_test$XZZ_IND_3[which(data6_ll_dz_test$IND_3==F)])<nchar(data6_ll_dz_test$XZZ_IND_2[which(data6_ll_dz_test$IND_3==F)]),T,F)
  print(i)
}
sum(data6_ll_dz_test$IND_3)
which(data6_ll_dz_test$XZZ_IND_3=="")
shan6 <- data6_ll_dz_test[which(data6_ll_dz_test$XZZ_IND_3==""),]#
data6_ll_dz_final <- anti_join(data6_ll_dz_new,shan6,by="ID")#####Records for locating addresses by Baidu map API
data6_fenpei <- anti_join(data6,data6_ll_dz_new)
nrow(shan1)+nrow(shan2)+nrow(shan3)+nrow(shan4)+nrow(shan5)+nrow(data5_final_T)+nrow(shan6)+nrow(data6_ll_dz_final)+nrow(data6_fenpei)==nrow(data_new)

#####Using Baidu map API for locating addresses into town/subdistrict----

#####Classify records that can only be located in  counties into two parts: (1)records that need to be removed;(2) records that are used for randomly assigned.----
names(shan3)
names(shan4)
names(shan5)
names(shan6)
fenpei_1 <- rbind(shan3,shan6[,c(1:6)])
fenpei_2 <- rbind(shan4,shan5)

fenpei_shan_2 <- fenpei_2[!fenpei_2$XZZ_XZQH %in% unique(xz_data$county_code),]
QH_old <- as.data.frame(table(fenpei_shan_1[which(nchar(fenpei_shan_1$XZZ_XZQH)==6 & substr(fenpei_shan_1$XZZ_XZQH,1,1)==5),]$XZZ_XZQH))
qx_data_old <- data.frame(c("隆昌市","射洪市","简阳市","郫都区","叙州区","双流区","安州区","马尔康市","彭山区","罗江区"),
                          c(511028,510922,512081,510124,511521,510122,510724,513229,511422,510626))
names(qx_data_old) <- names(qx_data)[c(1:2)]
qx_data_new <- rbind(qx_data_old,qx_data[,c(1:2)])

fenpei_shan_2_new <- fenpei_2[!fenpei_2$XZZ_XZQH %in% unique(qx_data_new$county_code),]
fenpei_2_new <- fenpei_2[!fenpei_2$ID %in% fenpei_shan_2_new$ID,]
unique(fenpei_2_new$XZZ_XZQH %in% qx_data_new$county_code)
length(unique(qx_data_new$county_code))==nrow(qx_data_new)
fenpei_2_new <- merge(fenpei_2_new,qx_data_new,by.x = "XZZ_XZQH",by.y = "county_code")
nrow(fenpei_shan_2_new)+nrow(fenpei_2_new)==nrow(fenpei_2)

fenpei_shan_1 <- anti_join(fenpei_1,fenpei_1[grep(A,fenpei_1$XZZ),])
fenpei_shan_1_new <- fenpei_shan_1[!fenpei_shan_1$XZZ_XZQH %in% unique(qx_data_new$county_code),]
fenpei_1_new <- anti_join(fenpei_1,fenpei_shan_1_new)
nrow(fenpei_1_new)+nrow(fenpei_shan_1_new)==nrow(fenpei_1)
fenpei_1_new_1 <- fenpei_1_new[fenpei_1_new$XZZ_XZQH %in% unique(qx_data_new$county_code),]
fenpei_1_new_1 <- merge(fenpei_1_new_1,qx_data_new,by.x = "XZZ_XZQH",by.y = "county_code")
fenpei_1_new_2 <- fenpei_1_new[!fenpei_1_new$ID %in% fenpei_1_new_1$ID,]
nrow(fenpei_1_new_2)+nrow(fenpei_1_new_1)==nrow(fenpei_1_new)
fenpei_1_new_2$qx <- NA
A <- a
A <- as.character(A)
for (i in 1:length(A)) {
  fenpei_1_new_2$qx[grep(A[i],fenpei_1_new_2$XZZ)] <- A[i]
}


nrow(fenpei_shan_1_new)+nrow(fenpei_1_new_1)+nrow(fenpei_1_new_2)==nrow(fenpei_1)
data6_fenpei_shan <- data6_fenpei[!data6_fenpei$XZZ_XZQH %in% unique(qx_data_new$county_code),]
data6_fenpei_new <- anti_join(data6_fenpei,data6_fenpei_shan)
data6_fenpei_new <- merge(data6_fenpei_new,qx_data_new,by.x = "XZZ_XZQH",by.y = "county_code")

nrow(fenpei_shan_2_new)+nrow(fenpei_shan_1_new)+nrow(fenpei_1_new_1)+nrow(fenpei_1_new_2)+nrow(fenpei_2_new)==nrow(shan3)+nrow(shan4)+nrow(shan5)+nrow(shan6)

#####Summary----
###Records need to be removed
shan1$Label <- "无任何地址信息"
shan2$Label <- "外省地址"
fenpei_shan_2_new$Label <- "现住址无区县信息且行政区划不在四川省内"
fenpei_shan_1_new$Label <- "现住址无区县信息且行政区划不在四川省内"
data6_fenpei_shan$Label <- "无规律地址且行政区划不在四川省内"
shan <- rbind(shan1,shan2,fenpei_shan_2_new,fenpei_shan_1_new,data6_fenpei_shan)
write.csv(shan,"G:/曹裴娅/毕业论文/cpy_第四季度/第四季度数据整理/shan.csv",row.names=F)

###Records used for randomly assigned
fenpei_1_new_1$Label <- "现住址只能定位到区县"
fenpei_1_new_2$Label <- "现住址只能定位到区县"
fenpei_2_new$Label <- "现住址为NA或数字但可通过行政区划定位到区县"
data6_fenpei_new$Label <- "现住址无任何规律但可通过行政区划定位到区县"
names(fenpei_1_new_1)
names(fenpei_1_new_2)
names(fenpei_2_new)
names(data6_fenpei_new)
fenpei <- rbind(fenpei_1_new_1[,c(2,3,1,4:8)],fenpei_1_new_2,fenpei_2_new[,c(2,3,1,4:8)],data6_fenpei_new[,c(2,3,1,4:8)])
write.csv(fenpei,"G:/曹裴娅/毕业论文/cpy_第四季度/第四季度数据整理/fenpei.csv",row.names=F)

###Located to towns/subdistricts 
data5_final_T$Lable <- "通过现住址直接定位到乡镇"
zip <- data5_final_T
write.csv(zip,"G:/曹裴娅/毕业论文/cpy_第四季度/第四季度数据整理/zip.csv",row.names = F)

###Records for locating towns/subdistricts by Baidu map API 
ll <- data6_ll_dz_final
ll$Lable <- "跑经纬度后定位到乡镇"
write.csv(ll,"G:/曹裴娅/毕业论文/cpy_第四季度/ll.csv",row.names=F)

###Check data
nrow(shan)+nrow(fenpei)+nrow(zip)+nrow(ll)==nrow(data_new)

#####Randomly assigned records based on county info and weights derived from population proportions----
fenpei <- merge(fenpei,qx_data,by="qx")
unique(fenpei$county_code)
fenpei_new <- data.frame()
set.seed(1)
for (i in 1:nrow(qx_data)) {
  qx_code_temp <- qx_data$county_code[i]
  fenpei_temp <- fenpei[fenpei$county_code==qx_code_temp,]
  xz_temp <- xz_data[xz_data$county_code==qx_code_temp,]
  fenpei_temp$Code_stv_P<- sample(xz_temp$town_code,nrow(fenpei_temp),xz_temp$pop,replace = T)
  fenpei_new <- rbind(fenpei_new,fenpei_temp)
}
write.csv(fenpei_new,"G:/曹裴娅/毕业论文/cpy_第四季度/第四季度数据整理/fenpei_new.csv",row.names=F)

#Prepare data for Baidu map API process
library(data.table)
options(digits = 15)
options(scipen = 200)
ll_spatial_1 <- read.csv("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial1.csv")
ll_spatial_2 <- read.csv("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial2.csv")
ll_spatial_3 <- fread("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial_join_3.txt",encoding="UTF-8")
ll_spatial_4 <- fread("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial_join_4.txt",encoding="UTF-8")

ll_old <- read.csv("G:\\曹裴娅\\毕业论文\\cpy_第四季度\\ll.csv")
ll_1 <- ll[ll$ID %in% ll_old$ID,]
ll_old$newid <- c(1:nrow(ll_old))
ll_spatial_1 <- read.csv("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial1.csv")
ll_spatial_2 <- read.csv("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial2.csv")
ll_spatial_3 <- fread("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial_join_3.txt",encoding="UTF-8")
ll_spatial_4 <- fread("G:/曹裴娅/毕业论文/cpy_第四季度/经纬度数据清理/spatial_join_4.txt",encoding="UTF-8")
names(ll_spatial_1)
names(ll_spatial_2)
names(ll_spatial_3)
names(ll_spatial_4)
ll_spatial <- rbind(ll_spatial_1,ll_spatial_2,ll_spatial_3,ll_spatial_4)
rm(ll_spatial_1)
rm(ll_spatial_2)
rm(ll_spatial_3)
rm(ll_spatial_4)
gc()
names(ll_spatial)
names(ll_1)
names(ll_old_new)
ll_old_new <- merge(ll_old,ll_spatial[,c("newid","longitude_","latitude_w")],by="newid")
ll_1_new <- merge(ll_1,ll_old_new[,c("ID","longitude_","latitude_w")],by="ID")
write.csv(ll_1_new,"G:/曹裴娅/毕业论文/cpy_第四季度/第四季度数据整理/ll_new.csv",row.names = F)
names(ll_1_new)

write.table(ll_1_new[1:1000000,c("ID","longitude_","latitude_w")],"G:/曹裴娅/毕业论文/cpy_第四季度/ll_1_new_1.txt",sep=",",row.names = F,quote = F)
write.table(ll_1_new[c(1000001:2000000),c("ID","longitude_","latitude_w")],"G:/曹裴娅/毕业论文/cpy_第四季度/ll_1_new_2.txt",sep=",",row.names = F,quote = F)
write.table(ll_1_new[c(2000001:3000000),c("ID","longitude_","latitude_w")],"G:/曹裴娅/毕业论文/cpy_第四季度/ll_1_new_3.txt",sep=",",row.names = F,quote = F)
write.table(ll_1_new[c(3000001:nrow(ll_1_new)),c("ID","longitude_","latitude_w")],"G:/曹裴娅/毕业论文/cpy_第四季度/ll_1_new_4.txt",sep=",",row.names = F,quote = F)
town_2019 <- read.csv("G:\\曹裴娅\\毕业论文\\四川省_2019\\xz_rs_1.gis.csv")
replicate <- town_2019[duplicated(town_2019$town_code),]

#----------------------------------------------------------------------------------------------------#
#Main analysis
#Prepare data for hospital service areas delineation
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
load("G:\\曹裴娅\\毕业论文\\cpy_第四季度\\最终数据\\data.Rdata")
data <- data[,c("YLJGID","JJLX","DEPT_CLASS","JGFLGL","DEPT_ADRRESSCODE","TX_DZ","YYDJ_J","YLFKFS","XB","NL","ZY","XZZ_XZQH","RYTJ","RYSJ","CYSJ","MZZD","JBBM","RYZD","JBBM1","BWHBZ","SSJCZBM1",
                "SSJCZBM1","SSJCZRQ1","LYFS","ZFY","ZYZD","JBDM","JBDM1","JBDM2","JBDM3","JBDM4","JBDM5","JBDM6","JBDM7","JBDM8","JBDM9","JBDM10","JBDM11","JBDM12","JBDM13","JBDM14","JBDM15","Year","ID")]
load("G:/曹裴娅/毕业论文/HSA费用地区差异/数据/data_final.Rdata")
##Remove records of randomly assigned
fenpei <- read.csv("G:/曹裴娅/毕业论文/cpy_第四季度/第四季度数据整理/fenpei_new.csv")
data_final <- data_final[!data_final$ID  %in% fenpei$ID,]

#1 Calculate health-seeking behavior flows  based on hospital discharge data 
library(dplyr)
library(tidyverse)
options(scipen = 200)
result <- data_final %>% group_by(stv_code_p,YLJGID) %>% 
  summarise(count=n()) %>% 
  mutate(all=sum(count),
         percent=count/all,
         rank=rank(-percent,ties.method = "average")) %>% 
  arrange(desc(percent),by_group=T)

length(unique(result$stv_code_p))
length(unique(result$YLJGID))

#Match patient id and hospital id with administrative data
street <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\street.txt",encoding="UTF-8")
street <- subset(street,select = c(town_code,Name,pop))
street$pop <- street$pop *10000
street$OriginID <- c(1:nrow(street))
names(street) <- c("Patient_code","Name","POPU","Patient_ZoneID")
jg <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\jg_dart_new.csv")
jg$DestinationID <- c(1:nrow(jg))
jg <- subset(jg,select = c(YLJGID,stv_code_jg,DestinationID))
names(jg) <- c("YLJGID","Hosp_code","Hosp_ZoneID")
pat <- result[!duplicated(result$stv_code_p),c("stv_code_p")]
pat <- merge(pat,street[,c("Patient_code","Patient_ZoneID","POPU")],by.x = "stv_code_p",by.y = "Patient_code" )
names(pat) <- c("Patient_code","Patient_ZoneID","POPU")
hos <- result[!duplicated(result$YLJGID),c("YLJGID")]
hos <- merge(hos,jg,by="YLJGID") 
shan <- anti_join(street,pat)#15
write.csv(shan,"G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\shanpat.csv",row.names = F)
shan <- anti_join(jg,hos)
shan <- shan[!duplicated(shan$Hosp_code),]#102
write.csv(shan,"G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\shanhos.csv",row.names = F)
names(shan)
names(result)
shanhos <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\shanhos.csv")
shanhos <- shanhos[c(1:102),]

#Match existing flows with travel time 
od <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\ODFlowData\\OD_TravelTime.txt",encoding="UTF-8")
od <- merge(od,jg[,c("YLJGID","Hosp_ZoneID")],by.x="DestinationID",by.y="Hosp_ZoneID")
od$index <- paste(od$OriginID,od$YLJGID,sep="")
shanhos$index <- paste(shanhos$Patient_ZoneID,shanhos$YLJGID,sep="")
shanhos <- merge(shanhos,od[,c("Total_minute","index")],by="index")

#travel time
result <- subset(result,select = c(stv_code_p,YLJGID,count))
names(result) <- c("Patient_code","YLJGID","AllFlows")
result <- merge(result,pat,by="Patient_code")
result <- merge(result,hos,by="YLJGID")
result$index <- paste(result$Patient_ZoneID,result$YLJGID,sep="")
result <- merge(result,od[,c("Total_minute","index")],by="index")
result_new <- rbind(result,shanhos)
length(unique(result_new$Patient_code))#3440
length(unique(result_new$Hosp_code))#743
fwrite(result_new,"G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\hsa_od.csv",row.names = F)

od <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\hsa_od.csv")#有0的flows
od <- od[,c("YLJGID","Patient_code","AllFlows","POPU","Hosp_code","Total_minute")]

#Match hospital id with Hosp_ZoneID
zoneID <- od_1[!duplicated(Hosp_code),]
od <- merge(od,zoneID[,c("Hosp_code","Hosp_ZoneID")],by="Hosp_code")
zoneID <- od_1[!duplicated(Patient_code),]
od <- merge(od,zoneID[,c("Patient_code","Patient_ZoneID")],by="Patient_code")

#Calculate total flows grouped by origin and destination
result <- aggregate(AllFlows ~ Hosp_ZoneID+Patient_ZoneID,data=od, sum)

#Calculate average travel time grouped by origin and destination
a <- aggregate(Total_minute ~ Hosp_ZoneID+Patient_ZoneID,data=od, mean)
a <- a[order(a$Hosp_ZoneID), ]
result <- result[order(result$Hosp_ZoneID), ]
a$ID <- c(1:nrow(a))
result$ID <- c(1:nrow(result))
c <- merge(result,a[,c("ID","Total_minute")],by="ID")
od_all <- c[,c("Hosp_ZoneID","Patient_ZoneID","AllFlows","Total_minute")] 
write.csv(od_all,"G:\\曹裴娅\\毕业论文\\huff\\od_1020.csv",row.names = F)

#HSA delineation by ArcGIS Pro HSA Delineation Pro.tbx

#Attributes of hospital service areas (HSAs) 
#Load attributes of counties
HSA_sc <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\dhsa_st.txt",encoding="UTF-8")
qx_data <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\qx_data.csv")
street <- fread("G:\\曹裴娅\\毕业论文\\huff\\street.txt",encoding="UTF-8")
names(HSA_sc)
HSA_sc <- HSA_sc[,c("Zipcode","POPU","HSAID")]
HSA_sc <- merge(HSA_sc,street[,c("town_code","county_cod","qx")],by.x = "Zipcode",by.y = "town_code")
sc_17 <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2017_四川省区县人口GDP.csv")
names(sc_17) <- c("qx","qx_17pop","qx_17GDP","qx_17perGDP")
HSA_sc <- merge(HSA_sc,sc_17,by="qx")
sc_18 <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2018_四川省区县人口GDP.csv")
names(sc_18) <- c("qx","qx_18pop","qx_18GDP","qx_18perGDP")
HSA_sc <- merge(HSA_sc,sc_18,by="qx")
sc_19 <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2019_四川省区县人口GDP.csv")
names(sc_19) <- c("qx","qx_19pop","qx_19GDP","qx_19perGDP")
HSA_sc <- merge(HSA_sc,sc_19,by="qx")

#Calculate population within HSAs (Proportions of population in each town/subdistrict in the county * county population)
HSA_sc <- merge(HSA_sc,aggregate(POPU~county_cod,data = HSA_sc, sum),by= "county_cod")
names(HSA_sc)[15] <- "county_pop"
HSA_sc$weight <- HSA_sc$POPU.x/HSA_sc$county_pop
HSA_sc$qx_17pop_adjust <- HSA_sc$weight*HSA_sc$qx_17pop
HSA_sc$qx_18pop_adjust <- HSA_sc$weight*HSA_sc$qx_18pop
HSA_sc$qx_19pop_adjust <- HSA_sc$weight*HSA_sc$qx_19pop
length(unique(HSA_sc$Zipcode))
length(unique(HSA_sc$HSAID))

#Calculate per capita GDP within HSAs ((Per capita GDP of the county*town/subdistrict population)/ population of HSA)
#2017 
HSA_sc$pop_17_adjustnew <- HSA_sc$qx_17pop_adjust*10000
HSA_sc$totalGDP_17 <- HSA_sc$pop_17_adjustnew*HSA_sc$qx_17perGDP
aggregate(totalGDP_17 ~ HSAID,data = HSA_sc,sum)
HSA_sc <- merge(HSA_sc,aggregate(totalGDP_17 ~ HSAID,data = HSA_sc,sum),by="HSAID")
names(HSA_sc)[22] <- "hsa_GDP_17"
aggregate(pop_17_adjustnew ~ HSAID,data = HSA_sc,sum)
HSA_sc <- merge(HSA_sc,aggregate(pop_17_adjustnew ~ HSAID,data = HSA_sc,sum),by="HSAID")
names(HSA_sc)[23] <- "hsa_pop_17"
HSA_sc$hsa17_perGDP <-HSA_sc$hsa_GDP_17/HSA_sc$hsa_pop_17
#2018
HSA_sc$pop_18_adjustnew <- HSA_sc$qx_18pop_adjust*10000
HSA_sc$totalGDP_18 <- HSA_sc$pop_18_adjustnew*HSA_sc$qx_18perGDP
aggregate(totalGDP_18 ~ HSAID,data = HSA_sc,sum)
HSA_sc <- merge(HSA_sc,aggregate(totalGDP_18 ~ HSAID,data = HSA_sc,sum),by="HSAID")
names(HSA_sc)[27] <- "hsa_GDP_18"
aggregate(pop_18_adjustnew ~ HSAID,data = HSA_sc,sum)
HSA_sc <- merge(HSA_sc,aggregate(pop_18_adjustnew ~ HSAID,data = HSA_sc,sum),by="HSAID")
names(HSA_sc)[28] <- "hsa_pop_18"
HSA_sc$hsa18_perGDP <-HSA_sc$hsa_GDP_18/HSA_sc$hsa_pop_18
#2019
HSA_sc$pop_19_adjustnew <- HSA_sc$qx_19pop_adjust*10000
HSA_sc$totalGDP_19 <- HSA_sc$pop_19_adjustnew*HSA_sc$qx_19perGDP
aggregate(totalGDP_19 ~ HSAID,data = HSA_sc,sum)
HSA_sc <- merge(HSA_sc,aggregate(totalGDP_19 ~ HSAID,data = HSA_sc,sum),by="HSAID")
names(HSA_sc)[32] <- "hsa_GDP_19"
aggregate(pop_19_adjustnew ~ HSAID,data = HSA_sc,sum)
HSA_sc <- merge(HSA_sc,aggregate(pop_19_adjustnew ~ HSAID,data = HSA_sc,sum),by="HSAID")
names(HSA_sc)[33] <- "hsa_pop_19"
HSA_sc$hsa19_perGDP <-HSA_sc$hsa_GDP_19/HSA_sc$hsa_pop_19

#149 HSAs in total
HSA_group <- HSA_sc[!duplicated(HSA_sc$HSAID),]

#Calculate hospital beds per 1,000 population
jg_dart <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\jg_street.txt")
hos <- merge(hos,jg_dart[,c("YLJGID","YYDJ_J","wgs_x","wgs_y","X2017_bed_1","X2018_bed_1","X2019_bed_1")],by="YLJGID") 
jg_dart <- hos 
jg_dart <- merge(jg_dart,HSA_sc[,c("HSAID","Zipcode")],by.x = "town_code",by.y = "Zipcode")
a <- jg_dart[!jg_dart$HSAID %in% HSA_group$HSAID,]
shan <- anti_join(jg_dart,hos) 
hos_hsa <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\hos_hsa.txt")
hos <- merge(hos,hos_hsa[,c("YLJGID","HSAID")],by="YLJGID")
jg_dart <- hos 
jg_hsa<- merge(jg_dart,HSA_group[,c("hsa_pop_17","hsa_pop_18","hsa_pop_19","HSAID")],by="HSAID")
#2017
names(jg_hsa) 
jg_hsa <- merge(jg_hsa,aggregate(X2017_bed_1~HSAID,data = jg_hsa,sum),by="HSAID")
names(jg_hsa)[14] <- "allbed_17"
#2018
jg_hsa <- merge(jg_hsa,aggregate(X2018_bed_1~HSAID,data = jg_hsa,sum),by="HSAID")
names(jg_hsa)[15] <- "allbed_18"
#2019
jg_hsa <- merge(jg_hsa,aggregate(X2019_bed_1~HSAID,data = jg_hsa,sum),by="HSAID")
names(jg_hsa)[16] <- "allbed_19"

jg_hsa$perbed_17 <- jg_hsa$allbed_17/jg_hsa$hsa_pop_17*1000
jg_hsa$perbed_18 <- jg_hsa$allbed_18/jg_hsa$hsa_pop_18*1000
jg_hsa$perbed_19 <- jg_hsa$allbed_19/jg_hsa$hsa_pop_19*1000
hsa <- jg_hsa[!duplicated(jg_hsa$HSAID),]
names(jg_hsa)

fwrite(jg_hsa,"G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\hos_hsa_dart.csv",row.names=F)#149

#Other atributes of HSAs
DHSA <- subset(HSA_group,select = c("HSAID","hsa17_perGDP","hsa18_perGDP","hsa19_perGDP","hsa_pop_17",
                                    "hsa_pop_18","hsa_pop_19"))
DHSA <- merge(DHSA,hsa[,c("HSAID","perbed_17","perbed_18","perbed_19")],by="HSAID")
HSA_LI <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\dhsa_st.txt",encoding="UTF-8")
HSA_LI <- HSA_LI[!duplicated(HSA_LI$HSAID),]
HSA_LI <- fwrite(HSA_LI,"G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\HSA_LI.csv",row.names=F)
DHSA <- merge(DHSA,HSA_LI[,c("HSAID","LI")],by = "HSAID")
DHSA <- merge(DHSA,HSA_LI[,c("COUNT_ZoneID","HSAID")],by = "HSAID")

#Calculate number of hospitals within HSAs
jg_dart$count <- 1
jg_dart <- merge(jg_dart,aggregate(count~HSAID,data = jg_dart,sum),by="HSAID")
hsa_jg <- jg_dart[!duplicated(jg_dart$HSAID),]
hsa_jg$count.y <- as.numeric(hsa_jg$count.y)
DHSA <- merge(DHSA,hsa_jg[,c("HSAID","count.y")],by="HSAID")
names(DHSA)[13] <- "SumHos"

sum(DHSA$SumHos)#2367 hospitals
fwrite(DHSA,"G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_149.csv",row.names=F)

#Descriptive analysis of HSAs
#Population 
sum(DHSA$hsa_pop_17)
sum(DHSA$hsa_pop_18)
sum(DHSA$hsa_pop_19)
summary(DHSA$hsa_pop_17)
summary(DHSA$hsa_pop_18)
summary(DHSA$hsa_pop_19)
#Number of towns/subdistricts
summary(DHSA$COUNT_ZoneID)
#Number of hospitals 
summary(DHSA$SumHos)
#Location index
summary(DHSA$LI)
#Per capita GDP
summary(DHSA$hsa17_perGDP)
summary(DHSA$hsa18_perGDP)
summary(DHSA$hsa19_perGDP)
#Hospital beds per 1,000 population
summary(DHSA$perbed_17)
summary(DHSA$perbed_18)
summary(DHSA$perbed_19)

#LI map
hsa_LI <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\HSA_LI.csv")
hsa_LI <- hsa_LI[,c("HSAID","POPU","LI")]
# #Cumulative proportions
hsa_LI <- hsa_LI[order(hsa_LI$LI,decreasing = F),]
hsa_LI$cum_percent <- cumsum(hsa_LI$POPU/sum(hsa_LI$POPU))
hsa_LI$cum_percent <- hsa_LI$cum_percent *100
hsa_LI$LI <- hsa_LI$LI *100
library(ggplot2)
p <- ggplot(data = hsa_LI,aes(x=LI,y=cum_percent,group=1))+
  geom_line(colour="darkgreen",size=1.5)+
  labs(x="Localization Index for Hospital Service Areas",y="Cumulative Percentage of Population in Sichuan")+
  theme_bw()+ #去掉背景灰色
  theme_classic()+ #去掉背景框线
  theme(title = element_text(size = 15,face = "bold"),
        axis.text.y.left = element_text(size = 15),
        axis.text.x.bottom = element_text(size = 15),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(50,100),breaks = seq(50,100,10),labels = paste(as.character(seq(50,100,10)),rep("%",6)))+
  guides(fill=guide_legend(reverse = T))
print(p)
ggsave("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\HSA_Dartmouth_new.jpg",p,width = 8,height = 8,dpi = 800)

#Data cleaning for regional variation of health expenditure analysis
DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_149.csv",encoding = "UTF-8")
HSA_community <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\dhsa_st.txt",encoding = "UTF-8")
DHSA <- merge(DHSA,HSA_community[,c("HSAID","Zipcode")],by="HSAID")
names(data_final)
data_final <- data_final[,c(1:46)]
data_final$ID <- as.character(data_final$ID)
names(data_final)
class(data$ID)
data$ID <- as.character(data$ID)
data_final <- merge(data_final[,c("ID","stv_code_p","stv_code_jg")],data,by="ID")
length(unique(data_final$stv_code_p))#3425
names(data_final)
names(DHSA)
data_17 <- data_final[data_final$Year==2017,]
data_17 <- merge(data_17,DHSA[,c("HSAID","Zipcode","hsa_pop_17")],by.x = "stv_code_p",by.y = "Zipcode")
names(data_17)[48] <- "hsa_pop"
data_18 <- data_final[data_final$Year==2018,]
data_18 <- merge(data_18,DHSA[,c("HSAID","Zipcode","hsa_pop_18")],by.x = "stv_code_p",by.y = "Zipcode")
names(data_18)[48] <- "hsa_pop"
data_19 <- data_final[data_final$Year==2019,]
data_19 <- merge(data_19,DHSA[,c("HSAID","Zipcode","hsa_pop_19")],by.x = "stv_code_p",by.y = "Zipcode")
names(data_19)[48] <- "hsa_pop"
nrow(data_17)+nrow(data_18)+nrow(data_19)==nrow(data_final)
data_final <- rbind(data_17,data_18,data_19)
save.image("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_hsacompletecases.Rdata")

#Load data with complete cases
load("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_hsacompletecases.Rdata")
names(data_final)[47] <- "HSAID_P"
table(data_final$Year,useNA = "ifany")
length(unique(data_final$HSAID_P))
#Screening total expenditures
#Retain 95% of records after removing extreme values from total expenditures
class(data_final$ZFY)
data_final$ZFY <- as.numeric(data_final$ZFY)
summary(data_final$ZFY)
#Remove records with NA total expenditures 
shan <- data_final[is.na(data_final$ZFY),]
table(shan$Year,useNA = "ifany")
data_final <- data_final[complete.cases(data_final$ZFY),]
data_final <- data_final[order(data_final$ZFY,decreasing=T),]
#Remove records with NA total expenditures 
shan <- filter(data_final,data_final$ZFY==0)
table(shan$Year,useNA = "ifany")
data_final <- data_final[data_final$ZFY!=0,]
data_final <- data_final[order(data_final$ZFY,decreasing=T),]
#Remove records with total expenditures lower than 100 RMB
shan <- filter(data_final,data_final$ZFY<100) 
table(shan$Year,useNA = "ifany")
data_final <- data_final[data_final$ZFY>100,]
data_final <- data_final[order(data_final$ZFY,decreasing=T),]
summary(data_final$ZFY)
data_final <- data_final[c(2:7441132),]
ck975<-function(x){
  quantile(data_final$ZFY,0.975)}
ck975(data_final$ZFY) #97.5% 37091.93
ck25<-function(x){
  quantile(data_final$ZFY,0.025)}
ck25(data_final$ZFY)#2.5% 862.5
data_final<- data_final[data_final$ZFY>=862.5 & data_final$ZFY<=37091.93,] 
table(data_final$Year,useNA = "ifany")

#Data cleaning for other variables in hospital discharge file
table(data_final$XB,useNA = "ifany")
#Keep records with exsiting gender other than NA
shan <- data_final[is.na(data_final$XB),]
data_final <- data_final[!is.na(data_final$XB),]
table(data_final$XB,useNA = "ifany")
gc()
#Keep records with exsiting gender other than missing values
shan <- filter(data_final,data_final$XB==0|data_final$XB==9)
data_final <- data_final[!data_final$XB==0,]
data_final <- data_final[!data_final$XB==9,]
table(data_final$XB,useNA = "ifany")
data_final <- data_final[order(data_final$XB,decreasing=T),]
data_final[data_final$ID=="4260082",]$XB <- 0
data_final <- data_final[!data_final$XB==0,]
#Age classification
data_final$age[data_final$NL<=14] <- "Young"
data_final$age[data_final$NL>=15 & data_final$NL<=64] <- "Middle"
data_final$age[data_final$NL>=65] <- "Elder"
table(data_final$age,useNA = "ifany")


#Calculate per capita total expenditure within HSAs
#2017
names(data)
data_17 <- data_final[data_final$Year==2017,]
gc()
#Adjust by age and gender
data_17$agew[data_17$age=="Young"]<-0.167
data_17$agew[data_17$age=="Middle"]<-0.694
data_17$agew[data_17$age=="Elder"]<-0.139
data_17$genderw[data_17$XB==1]<-0.513
data_17$genderw[data_17$XB==2]<-0.487
#Unadjusted per capita expenditures
names(data_17)
summary(data_17$ZFY)
bbb <- aggregate(ZFY~HSAID_P,data=data_17,sum)
data_17 <- merge(data_17,bbb,by="HSAID_P")
names(data_17)[52] <- "hsa_zfy"
data_17$zfy_perpat<- data_17$hsa_zfy/data_17$hsa_pop
summary(data_17$zfy_perpat)
#Adjusted per capita expenditures by age and gender
names(data_17)[29] <- "ZFY"
data_17$ad_zfy <-data_17$ZFY*data_17$agew*data_17$genderw
bbb <- aggregate(ad_zfy~HSAID_P,data=data_17,sum)
data_17 <- merge(data_17,bbb,by="HSAID_P")
names(data_17)[55] <- "hsa_zfy_ad"
names(data_17)[54] <- "ad_zfy"
data_17$zfy_perpat_ad<- data_17$hsa_zfy_ad/data_17$hsa_pop
names(data_17)
summary(data_17$zfy_perpat_ad)

#2018
data_18 <- data_final[data_final$Year==2018,]
gc()
#Adjust by age and gender
data_18$agew[data_18$age=="Young"]<-0.148
data_18$agew[data_18$age=="Middle"]<-0.715
data_18$agew[data_18$age=="Elder"]<-0.137
data_18$genderw[data_18$XB==1]<-0.513
data_18$genderw[data_18$XB==2]<-0.487
#Unadjusted per capita expenditures
names(data_18)
summary(data_18$ZFY)
bbb <- aggregate(ZFY~HSAID_P,data=data_18,sum)
data_18 <- merge(data_18,bbb,by="HSAID_P")
names(data_18)[52] <- "hsa_zfy"
data_18$zfy_perpat<- data_18$hsa_zfy/data_18$hsa_pop
summary(data_18$zfy_perpat)
#Adjusted per capita expenditures by age and gender
names(data_18)[29] <- "ZFY"
data_18$ad_zfy <-data_18$ZFY*data_18$agew*data_18$genderw
bbb <- aggregate(ad_zfy~HSAID_P,data=data_18,sum)
data_18 <- merge(data_18,bbb,by="HSAID_P")
names(data_18)[55] <- "hsa_zfy_ad"
names(data_18)[54] <- "ad_zfy"
data_18$zfy_perpat_ad<- data_18$hsa_zfy_ad/data_18$hsa_pop
names(data_18)
summary(data_18$zfy_perpat_ad)
#2019
data_19 <- data_final[data_final$Year==2019,]
gc()
data_19$agew[data_19$age=="Young"]<-0.147
data_19$agew[data_19$age=="Middle"]<-0.709
data_19$agew[data_19$age=="Elder"]<-0.144
data_19$genderw[data_19$XB==1]<-0.513
data_19$genderw[data_19$XB==2]<-0.487
#Unadjusted per capita expenditures
names(data_19)
summary(data_19$ZFY)
bbb <- aggregate(ZFY~HSAID_P,data=data_19,sum)
data_19 <- merge(data_19,bbb,by="HSAID_P")
names(data_19)[52] <- "hsa_zfy"
data_19$zfy_perpat<- data_19$hsa_zfy/data_19$hsa_pop
summary(data_19$zfy_perpat)
#Adjusted per capita expenditures by age and gender
names(data_19)[29] <- "ZFY"
data_19$ad_zfy <-data_19$ZFY*data_19$agew*data_19$genderw
bbb <- aggregate(ad_zfy~HSAID_P,data=data_19,sum)
data_19 <- merge(data_19,bbb,by="HSAID_P")
names(data_19)[55] <- "hsa_zfy_ad"
names(data_19)[54] <- "ad_zfy"
data_19$zfy_perpat_ad<- data_19$hsa_zfy_ad/data_19$hsa_pop
names(data_19)
summary(data_19$zfy_perpat_ad)

fwrite(data_17,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_17_149.csv",row.names=F)
fwrite(data_18,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_18_149.csv",row.names=F)
fwrite(data_19,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_19_149.csv",row.names=F)

data_17 <- fread("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_17_149.csv")
data_18 <- fread("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_18_149.csv")
data_19 <- fread("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_19_149.csv")
nrow(data_17)+nrow(data_18)+nrow(data_19)==nrow(data_final)
#CV calculation
myfunc_cv <- function(x)
{cv <- sd(x)/mean(x)
return(cv)}
#
hsa_17 <- data_17[!duplicated(data_17$HSAID_P),c("HSAID_P","Year","zfy_perpat","zfy_perpat_ad")]
hsa_18 <- data_18[!duplicated(data_18$HSAID_P),c("HSAID_P","Year","zfy_perpat","zfy_perpat_ad")]
hsa_19 <- data_19[!duplicated(data_19$HSAID_P),c("HSAID_P","Year","zfy_perpat","zfy_perpat_ad")]
hsa_zfy <- rbind(hsa_17,hsa_18,hsa_19)
#cv of adjusted  per capita expenditures
myfunc_cv(hsa_17$zfy_perpat_ad)#0.287
myfunc_cv(hsa_18$zfy_perpat_ad)#0.272
myfunc_cv(hsa_19$zfy_perpat_ad)#0.275
#cv of unadjusted  per capita expenditures
myfunc_cv(hsa_17$zfy_yperpat)#0.298
myfunc_cv(hsa_18$zfy_yperpat)#0.282
myfunc_cv(hsa_19$zfy_yperpat)#0.282

#Per capita total expenditures by year
hsa_17$zfy_yperpat <- hsa_17$zfy_perpat*4
hsa_17$yzfy_perpat_ad <- hsa_17$zfy_perpat_ad*4
hsa_18$zfy_yperpat <- hsa_18$zfy_perpat*4
hsa_18$yzfy_perpat_ad <- hsa_18$zfy_perpat_ad*4
hsa_19$zfy_yperpat <- hsa_19$zfy_perpat*4
hsa_19$yzfy_perpat_ad <- hsa_19$zfy_perpat_ad*4

summary(hsa_17$zfy_yperpat)
sd(hsa_17$zfy_yperpat)
summary(hsa_18$zfy_yperpat)
sd(hsa_18$zfy_yperpat)
summary(hsa_19$zfy_yperpat)
sd(hsa_19$zfy_yperpat)
summary(hsa_17$yzfy_perpat_ad)
sd(hsa_17$yzfy_perpat_ad)
summary(hsa_18$yzfy_perpat_ad)
sd(hsa_18$yzfy_perpat_ad)
summary(hsa_19$yzfy_perpat_ad)
sd(hsa_19$yzfy_perpat_ad)

fwrite(hsa_17,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_17_149.csv",row.names=F)
fwrite(hsa_18,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_18_149.csv",row.names=F)
fwrite(hsa_19,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_19_149.csv",row.names=F)

hsa_17 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_17_149.csv")
hsa_18 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_18_149.csv")
hsa_19 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_19_149.csv")

hsa_zfy <- rbind(hsa_17,hsa_18,hsa_19)

#Reional variation of per capita total expenditures by dots 
hsa_zfy$Year <- as.factor(hsa_zfy$Year)
ggplot(hsa_zfy,aes(x=Year,y=log(zfy_perpat_ad)))+
  geom_dotplot(binaxis = "y",binwidth = 0.01,stackdir = "center")+
  ylab(label = "Age-gender adjusted per capita inpatient expenditures (log-transformed)") +
  xlab(label = NULL) +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15))
ggsave("G:/曹裴娅/毕业论文/HSA费用地区差异/结果/图/医院服务区地区差异图(调整后)-EN_new.jpg",width = 10,height = 10,dpi = 600)

#IQR calculation(75% - 25%)
IQR(hsa_17$yzfy_perpat_ad)#54
IQR(hsa_18$yzfy_perpat_ad)#59
IQR(hsa_19$yzfy_perpat_ad)#75

#Preparation of demand and Supply variables within HSAs in each year (based on hospital discharge file)
#2017
#Age
table(data_17$XB,useNA = "ifany")
#insurance
data_17 <- data_17[!is.na(data_17$YLFKFS),]
table(data_17$YLFKFS,useNA = "ifany")
oldvals <- c("01","02","03","04","05","06","07","08","99")
newvals <- factor(c("1","2","3","4","5","6","7","8","9"))#Urban employment basic medical insurance (UEBMI)=1， Urban resident basic medical insurance (URBMI)=2，New cooperative medical scheme (NCMS)=3，poverty relief=4 business insurance=5,  Full public -expenses=6 ,Full self-expenses=7 , other insurances=8, other=9
data_17$health_insur <- newvals[match(data_17$YLFKFS,oldvals)]
table(data_17$health_insur,useNA = "ifany")

#Admission sources 1Transferred from the emergency department within the hospital 2Transferred from the outpatient department within the hospital 3Transferred from other hospital 9others
data_17 <- data_17[!is.na(data_17$RYTJ),]
table(data_17$RYTJ,useNA = "ifany")
oldvals <- c("1","2","3","9")
newvals <- factor(c("1","2","3","3"))
data_17$RYTJ_new <- newvals[match(data_17$RYTJ,oldvals)]
table(data_17$RYTJ_new,useNA = "ifany")

#Charlson Comorbidity index
data_17$cormorbidity_all <- paste(data_17$JBDM1,data_17$JBDM2,data_17$JBDM3,data_17$JBDM4,data_17$JBDM5,data_17$JBDM6,data_17$JBDM7,data_17$JBDM8,data_17$JBDM9,data_17$JBDM10,data_17$JBDM11,data_17$JBDM12,data_17$JBDM13,data_17$JBDM14,data_17$JBDM15,sep = ",")
#cci1(charlson index)
#Score 1
data_17$cci1=ifelse(grepl(pattern="I21|I22|I25.2",data_17$cormorbidity_all),1,0)#Myocardial infarction*/
data_17$cci2=ifelse(grepl(pattern="I09.9|I11.0|I13.0|I13.2|I25.5|I42.0|I42.5|I42.6|I42.7|I42.8|I42.9|I43|I50|P29.0",data_17$cormorbidity_all),1,0)##Congestive heart failure*/
data_17$cci3=ifelse(grepl(pattern='I70|I71|I73.1|I73.8|I73.9|I77.1|I79.0|I79.2|K55.1|K55.8|K55.9|Z95.8|Z95.9',data_17$cormorbidity_all),1,0)#Peripheral vascular disease*
data_17$cci4=ifelse(grepl(pattern='G45|G46|H34.0|I60|I61|I62|I63|I64|I65|I66|I67|I68|I69',data_17$cormorbidity_all),1,0)#Cerebrovascular disease*
data_17$cci5=ifelse(grepl(pattern='F00|F01|F02|F03|F05.1|G30|G31.1',data_17$cormorbidity_all),1,0)#Dementia*
data_17$cci6=ifelse(grepl(pattern='I27.8|I27.9|J40|J41|J42|J43|J44|J45|J46|J47|J60|J61|J62|J63|J64|J65|J66|J67|J68.4|J70.1|J70.3',data_17$cormorbidity_all),1,0)#Chronic pulmonary disease*/
data_17$cci7=ifelse(grepl(pattern='M05|M06|M31.5|M32|M33|M34|M35.1|M35.3|M36.0',data_17$cormorbidity_all),1,0)#Rheumatic disease*
data_17$cci8=ifelse(grepl(pattern="K25|K26|K27|K28",data_17$cormorbidity_all),1,0)#*Peptic ulcer disease*/
data_17$cci9=ifelse(grepl(pattern="B18|K70.0|K70.1|K70.2|K70.3|K70.9|K71.3|K71.4|K71.5|K71.7|K73|K74|K76.0|K76.2|K76.3|K76.4|K76.8|K76.9|Z94.4",data_17$cormorbidity_all),1,0)#* Mild liver disease*/
data_17$cci10=ifelse(grepl(pattern="E10.0|E10.1|E10.6|E10.8|E10.9|E11.0|E11.1|E11.6|E11.8|E11.9|E12.0|E12.1|E12.6|E12.8|E12.9|E13.0|E13.1|E13.6|E13.8|E13.9|E14.0|E14.1|E14.6|E14.8|E14.9",data_17$cormorbidity_all),1,0)#Diabetes without chronic complication*/
#Score 2 
data_17$cci11=ifelse(grepl(pattern="E10.2|E10.3|E10.4|E10.5|E10.7|E11.2|E11.3|E11.4|E11.5|E11.7|E12.2|E12.3|E12.4|E12.5|E12.7|E13.2|E13.3|E13.4|E13.5|E13.7|E14.2|E14.3|E14.4|E14.5|E14.7",data_17$cormorbidity_all),2,0)#Diabetes with chronic complication*/
data_17$cci12=ifelse(grepl(pattern="G04.1|G11.4|G80.1|G80.2|G81|G82|G83.0|G83.1|G83.2|G83.3|G83.4|G83.9",data_17$cormorbidity_all),2,0)#*Hemiplegia or paraplegia*/
data_17$cci13=ifelse(grepl(pattern="I12.0|I13.1|N03.2|N03.3|N03.4|N03.5|N03.6|N03.7|N05.2|N05.3|N05.4|N05.5|N05.6|N05.7|N18|N19|N25.0|Z49.0|Z49.1|Z49.2|Z94.0|Z99.2",data_17$cormorbidity_all),2,0)#*Renal disease*/
data_17$cci14=ifelse(grepl(pattern="C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97",data_17$cormorbidity_all),2,0)#*Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin*/
#Score 3
data_17$cci15=ifelse(grepl(pattern="I85.0|I85.9|I86.4|I98.2|K70.4|K71.1|K72.1|K72.9|K76.5|K76.6|K76.7",data_17$cormorbidity_all),3,0)#*Moderate or severe liver disease*/
data_17$cci16=ifelse(grepl(pattern="C77|C78|C79|C80",data_17$cormorbidity_all),3,0)#*Metastatic solid tumor*/
#Score 6
data_17$cci17=ifelse(grepl(pattern="B20|B21|B22|B24",data_17$cormorbidity_all),6,0)#*AIDS/HIV*/
####Calculate harlson index
data_17$charlson_score <- data_17$cci1 + data_17$cci2 + data_17$cci3 + data_17$cci4 + data_17$cci5+data_17$cci6+data_17$cci7+data_17$cci8+data_17$cci9+data_17$cci10+
  data_17$cci11 + data_17$cci12 +data_17$cci13 +data_17$cci14 + data_17$cci15 + data_17$cci16 + data_17$cci17
names(data_17)

##Occupational Category
# 11	Civil servant 1
# 13	Professional skill worker 2
# 17	Clerk 3
# 21	Business manager  4
# 24	Worker 5
# 27  Farmer 6
# 31	Student 7
# 37	Military 8
# 51	Freelancer 9
# 54	Self-employed worker 10
# 70	unemployment 11
# 80	Retire 12
# 90	other 13
data_17 <- data_17[!is.na(data_17$ZY),]
table(data_17$ZY,useNA = "ifany")
oldvals <- c("11","13","17","21","24","27","31","37","51","54","70","80","90")
newvals <- factor(c("1","1","1","1","2","3","7","7","4","7","5","6","7"))
#1:"1-4" 
#2 "5"
#3 "6"
#4 "9"
#5 "11"
#6 "12"
#7 " 7  8  10  13" 
data_17$job <- newvals[match(data_17$ZY,oldvals)]
table(data_17$job,useNA = "ifany")

#（1 Private hospitals，0 Public hospitals）
##Public hospitals are registered as state-owned and collective hospitals (code 11, 12 on hospital discharge file)
data_17 <- data_17[!is.na(data_17$JJLX),]#
data_17$gsl <- ifelse(data_17$JJLX==11|data_17$JJLX==12,0,1)
table(data_17$gsl,useNA = "ifany")

#Hospital levels
table(data_17$YYDJ_J,useNA = "ifany")

#Characteristics within HSAs
names(hsa_17)
DHSA <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_149.csv")
DHSA <- merge(DHSA,hsa_17[,c("zfy_perpat_ad","zfy_perpat","zfy_yperpat","yzfy_perpat_ad","HSAID_P")],by.x="HSAID",by.y ="HSAID_P")
names(DHSA)
DHSA_17 <- DHSA[,c("HSAID","hsa17_perGDP","hsa_pop_17","perbed_17","yzfy_perpat_ad")]
names(DHSA_17) <- c("HSAID","PerGDP","POPU","Perbed","yzfy_perpat_ad")
data_17 <- merge(data_17,DHSA_17[,c("HSAID","PerGDP")],by.x ="HSAID_P",by.y = "HSAID")
data_17 <- merge(data_17,DHSA_17[,c("HSAID","Perbed")],by.x ="HSAID_P",by.y = "HSAID")
length(unique(data_17$HSAID_P))

# Urbanization rate (administrative data)
town <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\town_2019.csv")
data_17 <- merge(data_17,town[,c("town_code","X")],by.x = "stv_code_p",by.y = "town_code" )
urban <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\urbanr.csv")
urban <- urban[urban$year==2017,]

HSA_sc <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\dhsa_st.txt",encoding="UTF-8")
qx_data <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\qx_data.csv")
street <- fread("G:\\曹裴娅\\毕业论文\\huff\\street.txt",encoding="UTF-8")
HSA_sc <- merge(HSA_sc,street[,c("town_code","county_cod","qx")],by.x = "Zipcode",by.y = "town_code")
HSA_sc <- merge(HSA_sc,qx_data[,c("county_code","X")],by.x = "county_cod",by.y = "county_code")
HSA_sc <- merge(HSA_sc,urban,by.x = "X",by.y = "city")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
write.csv(hsa_shizhou,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city_new.csv",row.names = F)

#The urbanization rate of an HSA is calculated based on the urbanization rate of each county in Sichuan Province. 
#The counties covered within the HSA are identified to calculate the mean urbanization rate of the HSA.
library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1_new.xlsx",1,header = F)
graph_1 <- merge(graph_1,urban,by.x = "X2",by.y = "city")
graph_1 <- graph_1[,c(9,2)]
names(graph_1) <- c("urbanr","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2_new.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,urban,by.x = "value",by.y = "city")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","urbanr","year")
graph_2 <- merge(graph_2,aggregate(urbanr~HSAID,data =graph_2,sum),by="HSAID")
graph_2$urbanr <- graph_2$urbanr.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("urbanr","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3_new.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,urban,by.x = "value",by.y = "city")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","urbanr","year")
graph_3 <- merge(graph_3,aggregate(urbanr~HSAID,data =graph_3,sum),by="HSAID")
graph_3$urbanr <- graph_3$urbanr.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("urbanr","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)
summary(hsa_r$urbanr)
names(DHSA_17)
class(DHSA_17$HSAID)
DHSA_17$HSAID <- as.character(DHSA_17$HSAID)
class(hsa_r$HSAID)

DHSA_17 <- merge(DHSA_17,hsa_r,by="HSAID")

#Share emergency admissions
names(data_17)
table(data_17$RYTJ_new)
data_17_j <- data_17[data_17$RYTJ_new==1,]
data_17_j$j <- 1
data_17_other <- anti_join(data_17,data_17_j,by="ID")
names(data_17_other)
data_17_other$j <- 0
data_j <- rbind(data_17_j[,c("ID","j")],data_17_other[,c("ID","j")])
data_17 <- merge(data_17,data_j,by="ID")
table(data_17$j,useNA = "ifany")
data_17 <- merge(data_17,aggregate(j~HSAID_P,data =data_17,sum),by="HSAID_P")
names(data_17)[85] <- "hsa_j"
data_17$count <- 1
class(data_17$count)
data_17 <- merge(data_17,aggregate(count~HSAID_P,data =data_17,sum),by="HSAID_P")
names(data_17)[87] <- "hsa_count"
data_17$jrate <-data_17$hsa_j/data_17$hsa_count*100
summary(data_17$jrate)
hsa_jrate <- data_17[!duplicated(data_17$HSAID_P),]
DHSA_17 <- merge(DHSA,hsa_jrate[,c("HSAID_P","jrate")],by.x="HSAID",by.y = "HSAID_P")

#Unemployment rate
names(data_17)
table(data_17$job,useNA = "ifany")
require(dplyr)
a <- data_17%>%
  group_by(job,HSAID_P)%>%
  summarise(Sum_job=sum(count.x))
b <- dcast(data = a,HSAID_P ~ job)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7")
b[is.na(b)] <- 0
b <- transform(b,pro=X5/(X1+X2+X3+X4+X5+X6+X7)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[19] <- "unemploy_Pro"

#Average Charlson comorbidity index within HSAs
names(data_17)
summary(data_17$charlson_score)
data_17 <- merge(data_17,aggregate(charlson_score~HSAID_P,data =data_17,sum),by="HSAID_P")
names(data_17)[89] <- "hsa_CCI"
data_17$CCI_average <-data_17$hsa_CCI/data_17$hsa_count
summary(data_17$CCI_average)
hsa_CCI <- data_17[!duplicated(data_17$HSAID_P),]
DHSA_17 <- merge(DHSA_17,hsa_CCI[,c("HSAID_P","CCI_average")],by.x="HSAID",by.y = "HSAID_P")

#Average age within HSAs
data_1 <- merge(data_17,aggregate(NL~HSAID_P,data = data_17[,c("NL","HSAID_P")],mean),by="HSAID_P")
data_1 <- data_1[!duplicated(data_1$HSAID_P),]
names(data_1)[91] <- "AverAge"
DHSA_17 <- merge(DHSA_17,data_1[,c("AverAge","HSAID_P")],by.x = "HSAID",by.y = "HSAID_P")

#Share aged 65 and over
data_17$age <- as.factor(data_17$age)
a <- aggregate(count.x ~HSAID_P+age,data = data_17,sum)
b <- dcast(data = a,HSAID_P ~ age)
names(b) <- c("HSAID","X1","X2","X3")
b <- transform(b,pro=X1/(X1+X2+X3)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[22] <- "Elder_Pro"

#Share of females
names(data_17)
data_17$XB <- as.factor(data_17$XB)
require(dplyr)
b <- dcast(data = a,HSAID_P ~ XB)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X2/(X1+X2)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[23] <- "Female_Pro"

#Share payment by insurance
names(data_17)
table(data_17$health_insur,useNA = "ifany")
require(dplyr)
a <- aggregate(count.x~health_insur+HSAID_P,data = data_17,sum)
b <- dcast(data = a,HSAID_P ~ health_insur)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7","X8","X9")
b[is.na(b)] <- 0
b <- transform(b,pro=(X1+X2+X3+X5+X8)/(X1+X2+X3+X4+X5+X6+X7+X8+X9)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[25] <- "insurancepay_Pro"

#Share of critical conditions
names(data_17)
table(data_17$BWHBZ,useNA = "ifany")
require(dplyr)
a <- aggregate(count.x~BWHBZ+HSAID_P,data = data_17,sum)
b <- dcast(data = a,HSAID_P ~ BWHBZ)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X1/(X1+X2)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[26] <- "bw_Pro"

#Share private hospitals (number of private hospitals/number of hospitals in total within HSAs)
jg <-data_17[!duplicated(data_17$YLJGID),]
jg_dart <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\hos_hsa.txt",encoding="UTF-8")
jg <- merge(jg,jg_dart[,c("YLJGID","HSAID")],by="YLJGID")
names(jg)[94] <- "HSAID_jg"
length(unique(jg$HSAID_jg))#149个hsa
names(jg)
jg$count <- 1
jg_hsa <- merge(jg,aggregate(count~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg_hsa)[96] <- "Sumhos"
names(jg_hsa)
jg_hsa <- merge(jg_hsa,aggregate(gsl~HSAID_jg,data = jg_hsa,sum),by="HSAID_jg")
names(jg_hsa)[97] <- "hsa_gsl"
jg_hsa$gsl_pro <- jg_hsa$hsa_gsl/jg_hsa$Sumhos*100
DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_149.csv",encoding = "UTF-8")
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
names(DHSA)
DHSA_17 <- merge(DHSA_17,jg[,c("HSAID_jg","gsl_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Share tertiary hospitals
jg_hsa$sanji <- ifelse(jg_hsa$YYDJ_J==3,1,0)
# jg_hsa <- left_join(jg_hsa,jg_hsa_dart,by="YLJGID")
names(jg)
jg_hsa <- merge(jg_hsa,aggregate(sanji~HSAID_jg,data = jg_hsa[,c("sanji","HSAID_jg")],sum),by="HSAID_jg")
names(jg_hsa)[100] <- "hsa_sanji"
jg_hsa$sanji_pro <- jg_hsa$hsa_sanji/jg_hsa$Sumhos*100
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
DHSA_17 <- merge(DHSA_17,jg[,c("HSAID_jg","sanji_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Herfindahl-Hirschman index within HSAs
library(data.table)
jg_hsa_17 <- jg_hsa[,c("HSAID_jg","YLJGID")]
names(jg_dart)
jg_hsa_17 <- merge(jg_hsa_17,jg_dart[,c("YLJGID","X2017_bed_")])
jg_hsa_17 <-merge(jg_hsa_17,aggregate(X2017_bed_~HSAID_jg,data = jg_hsa_17,sum),by="HSAID_jg")
names(jg_hsa_17)[4] <- "hsa_bed"
jg_hsa_17$share <- jg_hsa_17$X2017_bed_/jg_hsa_17$hsa_bed #Number of beds in each hospital/Number of beds within HSAs
jg_hsa_17$shares2 <- (jg_hsa_17$share)^2
jg_hsa_17 <- merge(jg_hsa_17,aggregate(shares2~HSAID_jg,data = jg_hsa_17,sum),by="HSAID_jg") 
names(jg_hsa_17)[7] <- "HHI"
jg_hsa_17$HHI_new <- jg_hsa_17$HHI*10000
hsa_HHI <- jg_hsa_17[!duplicated(jg_hsa_17$HSAID_jg),]
fwrite(jg_hsa_17,"jg_hsa_17.csv",row.names = F)
DHSA_17 <- merge(DHSA_17,hsa_HHI[,c("HSAID_jg","HHI_new")],by.x = "HSAID",by.y = "HSAID_jg")

#Death and birth rate within HSAs 
HSA_sc <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\dhsa_st.txt",encoding="UTF-8")
HSA_sc <- merge(HSA_sc,street[,c("town_code","county_cod","qx")],by.x = "Zipcode",by.y = "town_code")
HSA_sc <- merge(HSA_sc,qx_data[,c("county_code","X")],by.x = "county_cod",by.y = "county_code")
r <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2017-19出生率与死亡率.csv")
HSA_sc <- merge(HSA_sc,r,by.x = "X",by.y = "市")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou

class(hsa_shizhou$V3)
fwrite(hsa_shizhou,"hsa_shizhou_149.csv",row.names = F)

library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_birth.xlsx",1,header = F)
graph_1 <- merge(graph_1,r,by.x = "X2",by.y = "市")
graph_1 <- graph_1[,c(9:14,2)]
names(graph_1) <- c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_birth_1.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,r,by.x = "value",by.y = "市")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_2 <- merge(graph_2,aggregate(bir17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir19~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea19~HSAID,data =graph_2,sum),by="HSAID")
graph_2$bir17 <- graph_2$bir17.y/2
graph_2$bir18 <- graph_2$bir18.y/2
graph_2$bir19 <- graph_2$bir19.y/2
graph_2$dea17 <- graph_2$dea17.y/2
graph_2$dea18 <- graph_2$dea18.y/2
graph_2$dea19 <- graph_2$dea19.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_birth_2.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,r,by.x = "value",by.y = "市")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_3 <- merge(graph_3,aggregate(bir17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir19~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea19~HSAID,data =graph_3,sum),by="HSAID")
graph_3$bir17 <- graph_3$bir17.y/3
graph_3$bir18 <- graph_3$bir18.y/3
graph_3$bir19 <- graph_3$bir19.y/3
graph_3$dea17 <- graph_3$dea17.y/3
graph_3$dea18 <- graph_3$dea18.y/3
graph_3$dea19 <- graph_3$dea19.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)

#HSA data in 2017
DHSA_17 <- merge(DHSA_17,hsa_r[,c("bir17","dea17","HSAID")],by="HSAID")
names(DHSA_17)[30] <- "birthr"
names(DHSA_17)[31] <- "deathr"
DHSA_17_1 <- DHSA_17[,c(1,2,5,8,15,17,32,18:20,22:31)]
names(DHSA_17_1)[6] <- "adjust_perpatzfy"
write.csv(DHSA_17_1,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_17_149.csv",row.names=F)
names(DHSA_17_1)

#2018
#Gender
data_18 <- data_18[!is.na(data_18$XB),]
table(data_18$XB,useNA = "ifany")
data_18 <- data_18[!data_18$XB==0,]
data_18 <- data_18[!data_18$XB==9,]
#insurance
data_18 <- data_18[!is.na(data_18$YLFKFS),]
names(data_18)
table(data_18$YLFKFS,useNA = "ifany")
oldvals <- c("01","02","03","04","05","06","07","08","99")
newvals <- factor(c("1","2","3","4","5","6","7","8","9"))
data_18$health_insur <- newvals[match(data_18$YLFKFS,oldvals)]
table(data_18$health_insur,useNA = "ifany")

#Admission sources
data_18 <- data_18[!is.na(data_18$RYTJ),]#
table(data_18$RYTJ,useNA = "ifany")
oldvals <- c("1","2","3","9")
newvals <- factor(c("1","2","3","3"))
data_18$RYTJ_new <- newvals[match(data_18$RYTJ,oldvals)]
table(data_18$RYTJ_new,useNA = "ifany")

#Charlson Comorbidity index
data_18$cormorbidity_all <- paste(data_18$JBDM1,data_18$JBDM2,data_18$JBDM3,data_18$JBDM4,data_18$JBDM5,data_18$JBDM6,data_18$JBDM7,data_18$JBDM8,data_18$JBDM9,data_18$JBDM10,data_18$JBDM11,data_18$JBDM12,data_18$JBDM13,data_18$JBDM14,data_18$JBDM15,sep = ",")
#cci1(charlson index)
#Score 1 
data_18$cci1=ifelse(grepl(pattern="I21|I22|I25.2",data_18$cormorbidity_all),1,0)#Myocardial infarction*/
data_18$cci2=ifelse(grepl(pattern="I09.9|I11.0|I13.0|I13.2|I25.5|I42.0|I42.5|I42.6|I42.7|I42.8|I42.9|I43|I50|P29.0",data_18$cormorbidity_all),1,0)##Congestive heart failure*/
data_18$cci3=ifelse(grepl(pattern='I70|I71|I73.1|I73.8|I73.9|I77.1|I79.0|I79.2|K55.1|K55.8|K55.9|Z95.8|Z95.9',data_18$cormorbidity_all),1,0)#Peripheral vascular disease*
data_18$cci4=ifelse(grepl(pattern='G45|G46|H34.0|I60|I61|I62|I63|I64|I65|I66|I67|I68|I69',data_18$cormorbidity_all),1,0)#Cerebrovascular disease*
data_18$cci5=ifelse(grepl(pattern='F00|F01|F02|F03|F05.1|G30|G31.1',data_18$cormorbidity_all),1,0)#Dementia*
data_18$cci6=ifelse(grepl(pattern='I27.8|I27.9|J40|J41|J42|J43|J44|J45|J46|J47|J60|J61|J62|J63|J64|J65|J66|J67|J68.4|J70.1|J70.3',data_18$cormorbidity_all),1,0)#Chronic pulmonary disease*/
data_18$cci7=ifelse(grepl(pattern='M05|M06|M31.5|M32|M33|M34|M35.1|M35.3|M36.0',data_18$cormorbidity_all),1,0)#Rheumatic disease*
data_18$cci8=ifelse(grepl(pattern="K25|K26|K27|K28",data_18$cormorbidity_all),1,0)#*Peptic ulcer disease*/
data_18$cci9=ifelse(grepl(pattern="B18|K70.0|K70.1|K70.2|K70.3|K70.9|K71.3|K71.4|K71.5|K71.7|K73|K74|K76.0|K76.2|K76.3|K76.4|K76.8|K76.9|Z94.4",data_18$cormorbidity_all),1,0)#* Mild liver disease*/
data_18$cci10=ifelse(grepl(pattern="E10.0|E10.1|E10.6|E10.8|E10.9|E11.0|E11.1|E11.6|E11.8|E11.9|E12.0|E12.1|E12.6|E12.8|E12.9|E13.0|E13.1|E13.6|E13.8|E13.9|E14.0|E14.1|E14.6|E14.8|E14.9",data_18$cormorbidity_all),1,0)#Diabetes without chronic complication*/
#Score 2
data_18$cci11=ifelse(grepl(pattern="E10.2|E10.3|E10.4|E10.5|E10.7|E11.2|E11.3|E11.4|E11.5|E11.7|E12.2|E12.3|E12.4|E12.5|E12.7|E13.2|E13.3|E13.4|E13.5|E13.7|E14.2|E14.3|E14.4|E14.5|E14.7",data_18$cormorbidity_all),2,0)#Diabetes with chronic complication*/
data_18$cci12=ifelse(grepl(pattern="G04.1|G11.4|G80.1|G80.2|G81|G82|G83.0|G83.1|G83.2|G83.3|G83.4|G83.9",data_18$cormorbidity_all),2,0)#*Hemiplegia or paraplegia*/
data_18$cci13=ifelse(grepl(pattern="I12.0|I13.1|N03.2|N03.3|N03.4|N03.5|N03.6|N03.7|N05.2|N05.3|N05.4|N05.5|N05.6|N05.7|N18|N19|N25.0|Z49.0|Z49.1|Z49.2|Z94.0|Z99.2",data_18$cormorbidity_all),2,0)#*Renal disease*/
data_18$cci14=ifelse(grepl(pattern="C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97",data_18$cormorbidity_all),2,0)#*Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin*/
#Score 3
data_18$cci15=ifelse(grepl(pattern="I85.0|I85.9|I86.4|I98.2|K70.4|K71.1|K72.1|K72.9|K76.5|K76.6|K76.7",data_18$cormorbidity_all),3,0)#*Moderate or severe liver disease*/
data_18$cci16=ifelse(grepl(pattern="C77|C78|C79|C80",data_18$cormorbidity_all),3,0)#*Metastatic solid tumor*/
#Score 6
data_18$cci17=ifelse(grepl(pattern="B20|B21|B22|B24",data_18$cormorbidity_all),6,0)#*AIDS/HIV*/
####Calculate charlson index
data_18$charlson_score <- data_18$cci1 + data_18$cci2 + data_18$cci3 + data_18$cci4 + data_18$cci5+data_18$cci6+data_18$cci7+data_18$cci8+data_18$cci9+data_18$cci10+
  data_18$cci11 + data_18$cci12 +data_18$cci13 +data_18$cci14 + data_18$cci15 + data_18$cci16 + data_18$cci17
names(data_18)

#Charlson Comorbidity index
data_18 <- data_18[!is.na(data_18$ZY),]
table(data_18$ZY,useNA = "ifany")
oldvals <- c("11","13","17","21","24","27","31","37","51","54","70","80","90")
newvals <- factor(c("1","1","1","1","2","3","7","7","4","7","5","6","7"))
data_18$job <- newvals[match(data_18$ZY,oldvals)]
table(data_18$job,useNA = "ifany")

#（1 Private hospitals，0 Public hospitals）
data_18 <- data_18[!is.na(data_18$JJLX),]
data_18$gsl <- ifelse(data_18$JJLX==11|data_18$JJLX==12,0,1)
table(data_18$gsl,useNA = "ifany")

DHSA <- merge(DHSA,hsa_18[,c("zfy_perpat_ad","zfy_perpat","HSAID_P")],by.x = "HSAID",by.y ="HSAID_P")
names(DHSA)
DHSA_18 <- DHSA[,c("HSAID","hsa18_perGDP","hsa_pop_18","perbed_18","zfy_perpat","zfy_perpat_ad")]
names(DHSA_18) <- c("HSAID","PerGDP","POPU","Perbed","zfy_perpat","adjust_perpatzfy")

# Urbanization rate (administrative data)
names(data_18)
town <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\town_2019.csv")
data_18 <- merge(data_18,town[,c("town_code","X")],by.x = "stv_code_p",by.y = "town_code" )
urban <- read.csv("urbanr.csv")
urban <- urban[urban$year==2018,]
names(urban)

HSA_sc <- merge(HSA_sc,urban,by.x = "X",by.y = "city")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
hsa_shizhou <- as.data.frame(hsa_shizhou)

library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1_new.xlsx",1,header = F)
graph_1 <- merge(graph_1,urban,by.x = "X2",by.y = "city")
graph_1 <- graph_1[,c(9,2)]
names(graph_1) <- c("urbanr","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2_new.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,urban,by.x = "value",by.y = "city")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","urbanr","year")
graph_2 <- merge(graph_2,aggregate(urbanr~HSAID,data =graph_2,sum),by="HSAID")
graph_2$urbanr <- graph_2$urbanr.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("urbanr","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3_new.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,urban,by.x = "value",by.y = "city")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","urbanr","year")
graph_3 <- merge(graph_3,aggregate(urbanr~HSAID,data =graph_3,sum),by="HSAID")
graph_3$urbanr <- graph_3$urbanr.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("urbanr","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)
summary(hsa_r$urbanr)
names(DHSA_18)
class(DHSA_18$HSAID)
DHSA_18$HSAID <- as.character(DHSA_18$HSAID)
class(hsa_r$HSAID)
hsa_r$HSAID <- as.character(hsa_r$HSAID)
#合并大表
DHSA_18 <- merge(DHSA_18,hsa_r,by="HSAID")

#Share emergency admissions
names(data_18)
table(data_18$RYTJ_new)
data_18_j <- data_18[data_18$RYTJ_new==1,]
data_18_j$j <- 1
data_18_other <- anti_join(data_18,data_18_j,by="ID")
names(data_18_other)
data_18_other$j <- 0
data_j <- rbind(data_18_j[,c("ID","j")],data_18_other[,c("ID","j")])
data_18 <- merge(data_18,data_j,by="ID")
table(data_18$j)
data_18 <- merge(data_18,aggregate(j~HSAID_P,data =data_18,sum),by="HSAID_P")
names(data_18)[83] <- "hsa_j"
data_18$count <- 1
data_18 <- merge(data_18,aggregate(count~HSAID_P,data =data_18,sum),by="HSAID_P")
names(data_18)[85] <- "hsa_count"
data_18$jrate <-data_18$hsa_j/data_18$hsa_count*100
table(data_18$jrate)
hsa_jrate <- data_18[!duplicated(data_18$HSAID),]
hsa_jrate$HSAID_P <- as.character(hsa_jrate$HSAID_P)
DHSA_18 <- merge(DHSA_18,hsa_jrate[,c("HSAID_P","jrate")],by.x = "HSAID",by.y="HSAID_P")

#Unemployment rate
table(data_18$job,useNA = "ifany")
names(data_18)
require(dplyr)
a <- data_18%>%
  group_by(job,HSAID_P)%>%
  summarise(Sum_job=sum(count.x))
b <- dcast(data = a,HSAID_P ~ job)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7")
b[is.na(b)] <- 0
b <- transform(b,pro=X5/(X1+X2+X3+X4+X5+X6+X7)*100)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[9] <- "unemploy_Pro"

#Average Charlson comorbidity index within HSAs
names(data_18)
summary(data_18$charlson_score)
data_18 <- merge(data_18,aggregate(charlson_score~HSAID_P,data =data_18,sum),by="HSAID_P")
names(data_18)[87] <- "hsa_CCI"
data_18$CCI_average <-data_18$hsa_CCI/data_18$hsa_count
summary(data_18$CCI_average)
hsa_CCI <- data_18[!duplicated(data_18$HSAID_P),]
hsa_CCI$HSAID_P <- as.character(hsa_CCI$HSAID_P)
DHSA_18 <- merge(DHSA_18,hsa_CCI[,c("HSAID_P","CCI_average")],by.x = "HSAID",by.y="HSAID_P")

#Share aged 65 and over
data_18$age <- as.factor(data_18$age)
a <- aggregate(count.x ~HSAID_P+age,data = data_18,sum)
b <- dcast(data = a,HSAID_P ~ age)
names(b) <- c("HSAID","X1","X2","X3")
b <- transform(b,pro=X1/(X1+X2+X3)*100)
class(b$HSAID)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[11] <- "Elder_Pro"

#Share of females
names(data_18)
require(dplyr)
a <- data_18%>%
  group_by(XB,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ XB)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X2/(X1+X2)*100)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[12] <- "Female_Pro"

#Share payment by insurance
names(data_18)
table(data_18$health_insur,useNA = "ifany")
require(dplyr)
a <- data_18%>%
  group_by(health_insur,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ health_insur)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7","X8","X9")
b <- transform(b,pro=(X1+X2+X3+X5+X8)/(X1+X2+X3+X4+X5+X6+X7+X8+X9)*100)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[14] <- "insurancepay_Pro"

#Share of critical conditions
names(data_18)
table(data_18$BWHBZ,useNA = "ifany")
require(dplyr)
a <- data_18%>%
  group_by(BWHBZ,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ BWHBZ)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X1/(X1+X2)*100)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[15] <- "bw_Pro"

#Share private hospitals 
jg <-data_18[!duplicated(data_18$YLJGID),]
jg_dart <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\hos_hsa.txt",encoding="UTF-8")
jg <- merge(jg,jg_dart[,c("YLJGID","HSAID")],by="YLJGID")
names(jg)[92] <- "HSAID_jg"
length(unique(jg_hsa$HSAID_jg))#149个hsa
names(jg)
jg$count <- 1
jg_hsa <- merge(jg,aggregate(count.x~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg_hsa)[93] <- "Sumhos"
names(jg_hsa)
jg_hsa <- jg_hsa[,c(1:92,94)]
jg_hsa <- merge(jg_hsa,aggregate(gsl~HSAID_jg,data = jg_hsa,sum),by="HSAID_jg")
names(jg_hsa)[94] <- "hsa_gsl"
jg_hsa$gsl_pro <- jg_hsa$hsa_gsl/jg_hsa$Sumhos*100
jg_hsa <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
jg_hsa$HSAID_jg <- as.character(jg_hsa$HSAID_jg)
DHSA_18 <- merge(DHSA_18,jg_hsa[,c("HSAID_jg","gsl_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Share tertiary hospitals
jg_hsa$sanji <- ifelse(jg_hsa$YYDJ_J==3,1,0)
names(jg_hsa)
jg_hsa <- merge(jg_hsa,aggregate(sanji~HSAID_jg,data = jg_hsa[,c("sanji","HSAID_jg")],sum),by="HSAID_jg")
names(jg_hsa)[95] <- "hsa_sanji"
jg_hsa$sanji_pro <- jg_hsa$hsa_sanji/jg_hsa$Sumhos*100
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
names(jg)
jg$HSAID_jg <- as.character(jg$HSAID_jg)
DHSA_18 <- DHSA171819[DHSA171819$Year==2018,]
DHSA_18 <- DHSA_18[,c(1:16,18:21)]
DHSA_18 <- merge(DHSA_18,jg[,c("HSAID_jg","sanji_pro")],by.x ="HSAID",by.y = "HSAID_jg")

#Herfindahl-Hirschman index within HSAs
library(data.table)
jg_hsa_18 <- jg_hsa[,c("HSAID_jg","YLJGID")]
names(jg_dart)
jg_hsa_18 <- merge(jg_hsa_18,jg_dart[,c("YLJGID","X2018_bed_")],by="YLJGID")
jg_hsa_18 <-merge(jg_hsa_18,aggregate(X2018_bed_ ~ HSAID_jg,data = jg_hsa_18,sum),by="HSAID_jg")
names(jg_hsa_18)[4] <- "hsa_bed"
jg_hsa_18$share <- jg_hsa_18$X2018_bed_/jg_hsa_18$hsa_bed #每家医院的床位数/医院服务区中所有的床位数
jg_hsa_18$shares2 <- (jg_hsa_18$share)^2 #平方
jg_hsa_18 <- merge(jg_hsa_18,aggregate(shares2~HSAID_jg,data = jg_hsa_18,sum),by="HSAID_jg") #求和
names(jg_hsa_18)[7] <- "HHI"
jg_hsa_18$HHI_new <- jg_hsa_18$HHI*10000
hsa_HHI <- jg_hsa_18[!duplicated(jg_hsa_18$HSAID_jg),]
fwrite(jg_hsa_18,"jg_hsa_18.csv",row.names = F)
hsa_HHI$HSAID_jg <- as.character(hsa_HHI$HSAID_jg)
DHSA_18 <- merge(DHSA_18,hsa_HHI[,c("HSAID_jg","HHI_new")],by.x = "HSAID",by.y = "HSAID_jg")

#Death and birth rate within HSAs 
r <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2017-19出生率与死亡率.csv")
HSA_sc <- merge(HSA_sc,r,by.x = "X",by.y = "市")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
class(hsa_shizhou$V3)

fwrite(hsa_shizhou,"hsa_shizhou.csv",row.names = F)

library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1_new.xlsx",1,header = F)
graph_1 <- merge(graph_1,r,by.x = "X2",by.y = "市")
a <- anti_join(graph_1,graph_2)
graph_1 <- graph_1[,c(9:14,2)]
names(graph_1) <- c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2_new.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,r,by.x = "value",by.y = "市")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_2 <- merge(graph_2,aggregate(bir17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir19~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea19~HSAID,data =graph_2,sum),by="HSAID")
graph_2$bir17 <- graph_2$bir17.y/2
graph_2$bir18 <- graph_2$bir18.y/2
graph_2$bir19 <- graph_2$bir19.y/2
graph_2$dea17 <- graph_2$dea17.y/2
graph_2$dea18 <- graph_2$dea18.y/2
graph_2$dea19 <- graph_2$dea19.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3_new.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,r,by.x = "value",by.y = "市")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_3 <- merge(graph_3,aggregate(bir17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir19~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea19~HSAID,data =graph_3,sum),by="HSAID")
graph_3$bir17 <- graph_3$bir17.y/3
graph_3$bir18 <- graph_3$bir18.y/3
graph_3$bir19 <- graph_3$bir19.y/3
graph_3$dea17 <- graph_3$dea17.y/3
graph_3$dea18 <- graph_3$dea18.y/3
graph_3$dea19 <- graph_3$dea19.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)

hsa_r$HSAID <- as.character(hsa_r$HSAID)
DHSA_18 <- merge(DHSA_18,hsa_r[,c("bir18","dea18","HSAID")],by="HSAID")
names(DHSA_18)[19] <- "birthr"
names(DHSA_18)[20] <- "deathr"
names(DHSA_18)
write.csv(DHSA_18,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_18_149.csv",row.names=F)

#2019
names(data_19)
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
#Gender
data_19 <- data_19[!is.na(data_19$XB),]
table(data_19$XB,useNA = "ifany")
data_19 <- data_19[!data_19$XB==0,]
data_19 <- data_19[!data_19$XB==9,]
#insurance
data_19 <- data_19[!is.na(data_19$YLFKFS),]
names(data_19)
table(data_19$YLFKFS,useNA = "ifany")
oldvals <- c("01","02","03","04","05","06","07","08","09","99")
newvals <- factor(c("1","2","3","4","5","6","7","8","9","9"))
data_19$health_insur <- newvals[match(data_19$YLFKFS,oldvals)]
table(data_19$health_insur,useNA = "ifany")

#Admission sources
data_19 <- data_19[!is.na(data_19$RYTJ),]#
table(data_19$RYTJ)
oldvals <- c("1","2","3","9")
newvals <- factor(c("1","2","3","3"))
data_19$RYTJ_new <- newvals[match(data_19$RYTJ,oldvals)]
table(data_19$RYTJ_new,useNA = "ifany")

#Charlson Comorbidity index
data_19$cormorbidity_all <- paste(data_19$JBDM1,data_19$JBDM2,data_19$JBDM3,data_19$JBDM4,data_19$JBDM5,data_19$JBDM6,data_19$JBDM7,data_19$JBDM8,data_19$JBDM9,data_19$JBDM10,data_19$JBDM11,data_19$JBDM12,data_19$JBDM13,data_19$JBDM14,data_19$JBDM15,sep = ",")
#cci1(charlson index)
#Score 1
data_19$cci1=ifelse(grepl(pattern="I21|I22|I25.2",data_19$cormorbidity_all),1,0)#Myocardial infarction*/
data_19$cci2=ifelse(grepl(pattern="I09.9|I11.0|I13.0|I13.2|I25.5|I42.0|I42.5|I42.6|I42.7|I42.8|I42.9|I43|I50|P29.0",data_19$cormorbidity_all),1,0)##Congestive heart failure*/
data_19$cci3=ifelse(grepl(pattern='I70|I71|I73.1|I73.8|I73.9|I77.1|I79.0|I79.2|K55.1|K55.8|K55.9|Z95.8|Z95.9',data_19$cormorbidity_all),1,0)#Peripheral vascular disease*
data_19$cci4=ifelse(grepl(pattern='G45|G46|H34.0|I60|I61|I62|I63|I64|I65|I66|I67|I68|I69',data_19$cormorbidity_all),1,0)#Cerebrovascular disease*
data_19$cci5=ifelse(grepl(pattern='F00|F01|F02|F03|F05.1|G30|G31.1',data_19$cormorbidity_all),1,0)#Dementia*
data_19$cci6=ifelse(grepl(pattern='I27.8|I27.9|J40|J41|J42|J43|J44|J45|J46|J47|J60|J61|J62|J63|J64|J65|J66|J67|J68.4|J70.1|J70.3',data_19$cormorbidity_all),1,0)#Chronic pulmonary disease*/
data_19$cci7=ifelse(grepl(pattern='M05|M06|M31.5|M32|M33|M34|M35.1|M35.3|M36.0',data_19$cormorbidity_all),1,0)#Rheumatic disease*
data_19$cci8=ifelse(grepl(pattern="K25|K26|K27|K28",data_19$cormorbidity_all),1,0)#*Peptic ulcer disease*/
data_19$cci9=ifelse(grepl(pattern="B18|K70.0|K70.1|K70.2|K70.3|K70.9|K71.3|K71.4|K71.5|K71.7|K73|K74|K76.0|K76.2|K76.3|K76.4|K76.8|K76.9|Z94.4",data_19$cormorbidity_all),1,0)#* Mild liver disease*/
data_19$cci10=ifelse(grepl(pattern="E10.0|E10.1|E10.6|E10.8|E10.9|E11.0|E11.1|E11.6|E11.8|E11.9|E12.0|E12.1|E12.6|E12.8|E12.9|E13.0|E13.1|E13.6|E13.8|E13.9|E14.0|E14.1|E14.6|E14.8|E14.9",data_19$cormorbidity_all),1,0)#Diabetes without chronic complication*/
#Score 2  
data_19$cci11=ifelse(grepl(pattern="E10.2|E10.3|E10.4|E10.5|E10.7|E11.2|E11.3|E11.4|E11.5|E11.7|E12.2|E12.3|E12.4|E12.5|E12.7|E13.2|E13.3|E13.4|E13.5|E13.7|E14.2|E14.3|E14.4|E14.5|E14.7",data_19$cormorbidity_all),2,0)#Diabetes with chronic complication*/
data_19$cci12=ifelse(grepl(pattern="G04.1|G11.4|G80.1|G80.2|G81|G82|G83.0|G83.1|G83.2|G83.3|G83.4|G83.9",data_19$cormorbidity_all),2,0)#*Hemiplegia or paraplegia*/
data_19$cci13=ifelse(grepl(pattern="I12.0|I13.1|N03.2|N03.3|N03.4|N03.5|N03.6|N03.7|N05.2|N05.3|N05.4|N05.5|N05.6|N05.7|N18|N19|N25.0|Z49.0|Z49.1|Z49.2|Z94.0|Z99.2",data_19$cormorbidity_all),2,0)#*Renal disease*/
data_19$cci14=ifelse(grepl(pattern="C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97",data_19$cormorbidity_all),2,0)#*Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin*/
#Score 3
data_19$cci15=ifelse(grepl(pattern="I85.0|I85.9|I86.4|I98.2|K70.4|K71.1|K72.1|K72.9|K76.5|K76.6|K76.7",data_19$cormorbidity_all),3,0)#*Moderate or severe liver disease*/
data_19$cci16=ifelse(grepl(pattern="C77|C78|C79|C80",data_19$cormorbidity_all),3,0)#*Metastatic solid tumor*/
#Score 6
data_19$cci17=ifelse(grepl(pattern="B20|B21|B22|B24",data_19$cormorbidity_all),6,0)#*AIDS/HIV*/
####Calculate charlson index
data_19$charlson_score <- data_19$cci1 + data_19$cci2 + data_19$cci3 + data_19$cci4 + data_19$cci5+data_19$cci6+data_19$cci7+data_19$cci8+data_19$cci9+data_19$cci10+
  data_19$cci11 + data_19$cci12 +data_19$cci13 +data_19$cci14 + data_19$cci15 + data_19$cci16 + data_19$cci17
names(data_19)


##Occupational Category
data_19 <- data_19[!is.na(data_19$ZY),]
table(data_19$ZY,useNA = "ifany")
oldvals <- c("-","11","13","17","21","24","27","31","37","51","54","70","80","90")
newvals <- factor(c("7","1","1","1","1","2","3","7","7","4","7","5","6","7"))
data_19$job <- newvals[match(data_19$ZY,oldvals)]
table(data_19$job,useNA = "ifany")

#（1 Private hospitals，0 Public hospitals）
data_19 <- data_19[!is.na(data_19$JJLX),]
data_19$gsl <- ifelse(data_19$JJLX==11|data_19$JJLX==12,0,1)
table(data_19$gsl,useNA = "ifany")

DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_149.csv",encoding = "UTF-8")
DHSA <- merge(DHSA,hsa_19[,c("zfy_perpat_ad","zfy_yperpat","HSAID_P")],by.x = "HSAID",by.y ="HSAID_P")
names(DHSA)
DHSA_19 <- DHSA[,c("HSAID","hsa19_perGDP","hsa_pop_19","perbed_19","zfy_yperpat","zfy_perpat_ad")]
names(DHSA_19) <- c("HSAID","PerGDP","POPU","Perbed","zfy_perpat","adjust_perpatzfy")

# Urbanization rate (administrative data)
names(data_19)
town <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\town_2019.csv")
data_19 <- merge(data_19,town[,c("town_code","X")],by.x = "stv_code_p",by.y = "town_code" )
urban <- read.csv("urbanr.csv")
urban <- urban[urban$year==2019,]
names(urban)

hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
hsa_shizhou <- as.data.frame(hsa_shizhou)

library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1_new.xlsx",1,header = F)
graph_1 <- merge(graph_1,urban,by.x = "X2",by.y = "city")
graph_1 <- graph_1[,c(9,2)]
names(graph_1) <- c("urbanr","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2_new.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,urban,by.x = "value",by.y = "city")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","urbanr","year")
graph_2 <- merge(graph_2,aggregate(urbanr~HSAID,data =graph_2,sum),by="HSAID")
graph_2$urbanr <- graph_2$urbanr.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("urbanr","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3_new.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,urban,by.x = "value",by.y = "city")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","urbanr","year")
graph_3 <- merge(graph_3,aggregate(urbanr~HSAID,data =graph_3,sum),by="HSAID")
graph_3$urbanr <- graph_3$urbanr.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("urbanr","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)
summary(hsa_r$urbanr)
DHSA_19 <- merge(DHSA_19,hsa_r,by="HSAID")

#Share emergency admissions
names(data_19)
table(data_19$RYTJ_new)
data_19_j <- data_19[data_19$RYTJ_new==1,]
data_19_j$j <- 1
data_19_other <- anti_join(data_19,data_19_j,by="ID")
names(data_19_other)
data_19_other$j <- 0
data_j <- rbind(data_19_j[,c("ID","j")],data_19_other[,c("ID","j")])
data_19 <- merge(data_19,data_j,by="ID")
table(data_19$j)
data_19 <- merge(data_19,aggregate(j~HSAID_P,data =data_19,sum),by="HSAID_P")
names(data_19)[82] <- "hsa_j"
data_19$count <- 1
data_19 <- merge(data_19,aggregate(count~HSAID_P,data =data_19,sum),by="HSAID_P")
names(data_19)[84] <- "hsa_count"
data_19$jrate <-data_19$hsa_j/data_19$hsa_count*100
table(data_19$jrate)
hsa_jrate <- data_19[!duplicated(data_19$HSAID),]
DHSA_19 <- merge(DHSA_19,hsa_jrate[,c("HSAID_P","jrate")],by.x = "HSAID",by.y="HSAID_P")

#Unemployment rate
table(data_19$job,useNA = "ifany")
require(dplyr)
a <- data_19%>%
  group_by(job,HSAID_P)%>%
  summarise(Sum_job=sum(count.x))
b <- dcast(data = a,HSAID_P ~ job)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7")
b[is.na(b)] <- 0
b <- transform(b,pro=X5/(X1+X2+X3+X4+X5+X6+X7)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[9] <- "unemploy_Pro"

#Average Charlson comorbidity index within HSAs
names(data_19)
summary(data_19$charlson_score)
data_19 <- merge(data_19,aggregate(charlson_score~HSAID_P,data =data_19,sum),by="HSAID_P")
names(data_19)[86] <- "hsa_CCI"
data_19$CCI_average <-data_19$hsa_CCI/data_19$hsa_count
summary(data_19$CCI_average)
hsa_CCI <- data_19[!duplicated(data_19$HSAID_P),]
DHSA_19 <- merge(DHSA_19,hsa_CCI[,c("HSAID_P","CCI_average")],by.x = "HSAID",by.y="HSAID_P")

#Share aged 65 and over
data_19$age <- as.factor(data_19$age)
a <- aggregate(count.x ~HSAID_P+age,data = data_19,sum)
b <- dcast(data = a,HSAID_P ~ age)
names(b) <- c("HSAID","X1","X2","X3")
b <- transform(b,pro=X1/(X1+X2+X3)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[11] <- "Elder_Pro"

#Share of females
names(data_19)
require(dplyr)
a <- data_19%>%
  group_by(XB,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ XB)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X2/(X1+X2)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[12] <- "Female_Pro"

#Share payment by insurance
names(data_19)
table(data_19$health_insur,useNA = "ifany")
require(dplyr)
a <- data_19%>%
  group_by(health_insur,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ health_insur)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7","X8","X9")
b <- transform(b,pro=(X1+X2+X3+X5+X8)/(X1+X2+X3+X4+X5+X6+X7+X8+X9)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[14] <- "insurancepay_Pro"

#Share of critical conditions
names(data_19)
table(data_19$BWHBZ,useNA = "ifany")
require(dplyr)
a <- data_19%>%
  group_by(BWHBZ,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ BWHBZ)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X1/(X1+X2)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[15] <- "bw_Pro"

#Share private hospitals (number of private hospitals/number of hospitals in total within HSAs)
jg <-data_19[!duplicated(data_19$YLJGID),]
jg_dart <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\数据\\hos_hsa.txt",encoding="UTF-8")
jg <- merge(jg,jg_dart[,c("YLJGID","HSAID")],by="YLJGID")
names(jg)[91] <- "HSAID_jg"
length(unique(jg$HSAID_jg))
names(jg)
jg <- merge(jg,aggregate(count.x~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg)[92] <- "Sumhos"
names(jg)
jg <- merge(jg,aggregate(gsl~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg)[93] <- "hsa_gsl"
jg$gsl_pro <- jg$hsa_gsl/jg$Sumhos*100
jg_hsa <- jg[!duplicated(jg$HSAID_jg),]
DHSA_19 <- merge(DHSA_19,jg_hsa[,c("HSAID_jg","gsl_pro")],by.x = "HSAID",by.y = "HSAID_jg")
names(DHSA_19)

#Share tertiary hospitals
jg$sanji <- ifelse(jg$YYDJ_J==3,1,0)
jg <- merge(jg,aggregate(sanji~HSAID_jg,data = jg[,c("sanji","HSAID_jg")],sum),by="HSAID_jg")
names(jg)[96] <- "hsa_sanji"
jg <- merge(jg,aggregate(count.x~HSAID_jg,data = jg,sum),by="HSAID_jg")
jg$sanji_pro <- jg$hsa_sanji/jg$Sumhos*100
jg <- jg[!duplicated(jg$HSAID_jg),]
names(jg)
DHSA_19 <- merge(DHSA_19,jg[,c("HSAID_jg","sanji_pro")],by.x ="HSAID",by.y = "HSAID_jg")

#Herfindahl-Hirschman index within HSAs
library(data.table)
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
jg_hsa_19 <- jg[,c("HSAID_jg","YLJGID")]
names(jg_dart)
jg_hsa_19 <- merge(jg_hsa_19,jg_dart[,c("YLJGID","X2019_bed_")],by="YLJGID")
jg_hsa_19 <-merge(jg_hsa_19,aggregate(X2019_bed_ ~ HSAID_jg,data = jg_hsa_19,sum),by="HSAID_jg")
names(jg_hsa_19)[4] <- "hsa_bed"
jg_hsa_19$share <- jg_hsa_19$X2019_bed_/jg_hsa_19$hsa_bed
jg_hsa_19$shares2 <- (jg_hsa_19$share)^2 
jg_hsa_19 <- merge(jg_hsa_19,aggregate(shares2~HSAID_jg,data = jg_hsa_19,sum),by="HSAID_jg")
names(jg_hsa_19)[7] <- "HHI"
jg_hsa_19$HHI_new <- jg_hsa_19$HHI*10000
hsa_HHI <- jg_hsa_19[!duplicated(jg_hsa_19$HSAID_jg),]
fwrite(jg_hsa_19,"jg_hsa_19.csv",row.names = F)
DHSA_19 <- merge(DHSA_19,hsa_HHI[,c("HSAID_jg","HHI_new")],by.x = "HSAID",by.y = "HSAID_jg")

#Death and birth rate within HSAs 
r <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2017-19出生率与死亡率.csv")
HSA_sc <- merge(HSA_sc,r,by.x = "X",by.y = "市")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
class(hsa_shizhou$V3)

fwrite(hsa_shizhou,"hsa_shizhou.csv",row.names = F)

library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1_new.xlsx",1,header = F)
graph_1 <- merge(graph_1,r,by.x = "X2",by.y = "市")
a <- anti_join(graph_1,graph_2)
graph_1 <- graph_1[,c(9:14,2)]
names(graph_1) <- c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2_new.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,r,by.x = "value",by.y = "市")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_2 <- merge(graph_2,aggregate(bir17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir19~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea19~HSAID,data =graph_2,sum),by="HSAID")
graph_2$bir17 <- graph_2$bir17.y/2
graph_2$bir18 <- graph_2$bir18.y/2
graph_2$bir19 <- graph_2$bir19.y/2
graph_2$dea17 <- graph_2$dea17.y/2
graph_2$dea18 <- graph_2$dea18.y/2
graph_2$dea19 <- graph_2$dea19.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3_new.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,r,by.x = "value",by.y = "市")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_3 <- merge(graph_3,aggregate(bir17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir19~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea19~HSAID,data =graph_3,sum),by="HSAID")
graph_3$bir17 <- graph_3$bir17.y/3
graph_3$bir18 <- graph_3$bir18.y/3
graph_3$bir19 <- graph_3$bir19.y/3
graph_3$dea17 <- graph_3$dea17.y/3
graph_3$dea18 <- graph_3$dea18.y/3
graph_3$dea19 <- graph_3$dea19.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)

DHSA_19 <- merge(DHSA_19,hsa_r[,c("bir19","dea19","HSAID")],by="HSAID")
names(DHSA_19)[19] <- "birthr"
names(DHSA_19)[20] <- "deathr"
write.csv(DHSA_19,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_19_149.csv",row.names=F)

#Prepare HSA-data of 3 years 
DHSA_17 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_17_149.csv")
DHSA_18 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_18_149.csv")
DHSA_19 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_19_149.csv")

names(DHSA_17_1)
names(DHSA_18)
names(DHSA_19)
DHSA_17_1$Year <- 2017
DHSA_18$Year <- 2018
DHSA_19$Year <- 2019
names(DHSA_17_1)[2] <- "PerGDP"
names(DHSA_17_1)[3] <- "POPU"
names(DHSA_17_1)[4] <- "Perbed"
names(DHSA_19)
DHSA_18 <- DHSA_18[,c(1:16,21,17:20)]
DHSA171819 <- rbind(DHSA_17_1,DHSA_18,DHSA_19)
fwrite(DHSA171819,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA171819_149.csv",row.names = F)

DHSA171819 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA171819_149.csv")

#Additional descriptive analysis of HSA characteristics
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  s <- sd(x)
  cv <- sd(x)/mean(x)
  max <- max(x)
  min <- min(x)
  return <- (c(mean=m,stdev=s,cv=cv,max=max,min=min))
}
names(DHSA171819)
myvars <- c("Elder_Pro","Female_Pro","jrate","bw_Pro","CCI_average","birthr","deathr","unemploy_Pro","PerGDP","urbanr","Perbed","gsl_pro","sanji_pro","HHI_new","insurancepay_Pro")
dstats <- function(x)sapply(x, mystats)
dstats(DHSA171819[myvars])

DHSA171819$Year <- as.factor(DHSA171819$Year)
hist(log(DHSA171819$yzfy_perpat_ad),breaks = 20)
hist(log(DHSA171819$zfy_perpat),breaks = 20)
hist(log(DHSA171819$PerGDP),breaks = 20)

install.packages('tidyverse') 
install.packages('modelsummary') 
library(dplyr)
library(tidyverse) 
library(modelsummary) 
#OLS model in HSA level
#multiciliary test
library(car)
fit1 <- lm(log(zfy_perpat)~Elder_Pro+Female_Pro+jrate+bw_Pro+CCI_average+deathr+birthr+unemploy_Pro+log(PerGDP)+urbanr+Perbed+gsl_pro+sanji_pro+log(HHI_new)+insurancepay_Pro,data = DHSA171819)
vif(fit1)

#VIF
data1<-read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\VIF-paper.csv")
data1$variables<-factor(data1$variables,levels = data1$variables[order(data1$VIF)])
library(ggplot2)
p2<-ggplot(data1,aes(VIF,variables,fill=IND))+
  geom_segment(aes(x=0,xend=VIF,y=variables,yend=variables))+
  geom_point(shape=21,size=3.5,colour="black")+
  theme_bw()+
  scale_fill_brewer(type = "qual",palette = "Set1")+
  # scale_color_manual(values = c("green","red","yellow"))+
  labs(x="VIF",y="variables")+
  theme(panel.grid.minor= element_blank())+
  theme(panel.grid.major=element_blank())+
  theme(axis.title = element_text(size=22,face = "bold"),
        axis.text = element_text(size=21))+
  theme(legend.background = element_blank(),
        legend.position = "top")+
  theme(legend.text = element_text(size = 20))
p2
ggsave("VIF.jpg",width = 30,height = 35,units = c("cm"),dpi = 600)

#ols with robust standard errors
DHSA171819$Year <- as.factor(DHSA171819$Year)
fit1 <- lm(log(zfy_perpat)~Elder_Pro+Female_Pro+jrate+bw_Pro+CCI_average+deathr+birthr+unemploy_Pro+
             log(PerGDP)+urbanr+Perbed+gsl_pro+sanji_pro+log(HHI_new)+insurancepay_Pro+Year,data = DHSA171819)
msummary(list(fit1),
         vcov=c("robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
summary(fit1)
confint(fit1)

#Shapley value decomposition method 
library(MASS)
install.packages("ShapleyValue")
library(ShapleyValue)
library(tidyverse)
y <- log(DHSA171819$zfy_perpat)
names(DHSA171819)
DHSA171819$GDP_new <- log(DHSA171819$PerGDP)
DHSA171819$HHI_new2 <- log(DHSA171819$HHI_new)
names(DHSA171819)
x <- as.data.frame(DHSA171819[,c(11,12,8,15,10,19,20,9,22,7,4,16:17,23,14,21)])
x$Year <- as.factor(x$Year)
value <- shapleyvalue(y,x)
library(data.table)
fwrite(value,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\结果\\表\\shapleyvalue_149.csv",row.names=F)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Robust analysis with incomplete cases
DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_151.csv",encoding = "UTF-8")
HSA_community <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dhsa_street.txt",encoding = "UTF-8")
DHSA <- merge(DHSA,HSA_community[,c("HSAID","Zipcode")],by="HSAID")
names(data_final)
data_final$ID <- as.character(data_final$ID)
names(data_final)
class(data$ID)
data$ID <- as.character(data$ID)
data_final <- merge(data_final[,c("ID","stv_code_p","stv_code_jg")],data,by="ID")
length(unique(data_final$stv_code_p))
names(data_final)
names(DHSA)
data_17 <- data_final[data_final$Year==2017,]
data_17 <- merge(data_17,DHSA[,c("HSAID","Zipcode","hsa_pop_17")],by.x = "stv_code_p",by.y = "Zipcode")
names(data_17)[48] <- "hsa_pop"
data_18 <- data_final[data_final$Year==2018,]
data_18 <- merge(data_18,DHSA[,c("HSAID","Zipcode","hsa_pop_18")],by.x = "stv_code_p",by.y = "Zipcode")
names(data_18)[48] <- "hsa_pop"
data_19 <- data_final[data_final$Year==2019,]
data_19 <- merge(data_19,DHSA[,c("HSAID","Zipcode","hsa_pop_19")],by.x = "stv_code_p",by.y = "Zipcode")
names(data_19)[48] <- "hsa_pop"
nrow(data_17)+nrow(data_18)+nrow(data_19)==nrow(data_final)

#HSA delineation with incompelte case by ArcGIS Pro (151 HSAs was generated)

#2017
#Gender
table(data_17$XB,useNA = "ifany")
#insurance
data_17 <- data_17[!is.na(data_17$YLFKFS),]
table(data_17$YLFKFS,useNA = "ifany")
oldvals <- c("01","02","03","04","05","06","07","08","99")
newvals <- factor(c("1","2","3","4","5","6","7","8","9"))
data_17$health_insur <- newvals[match(data_17$YLFKFS,oldvals)]
table(data_17$health_insur,useNA = "ifany")

#Admission sources
data_17 <- data_17[!is.na(data_17$RYTJ),]
table(data_17$RYTJ,useNA = "ifany")
oldvals <- c("1","2","3","9")
newvals <- factor(c("1","2","3","3"))
data_17$RYTJ_new <- newvals[match(data_17$RYTJ,oldvals)]
table(data_17$RYTJ_new,useNA = "ifany")

#Charlson Comorbidity index
data_17$cormorbidity_all <- paste(data_17$JBDM1,data_17$JBDM2,data_17$JBDM3,data_17$JBDM4,data_17$JBDM5,data_17$JBDM6,data_17$JBDM7,data_17$JBDM8,data_17$JBDM9,data_17$JBDM10,data_17$JBDM11,data_17$JBDM12,data_17$JBDM13,data_17$JBDM14,data_17$JBDM15,sep = ",")
#cci1(charlson index)
#Score 1
data_17$cci1=ifelse(grepl(pattern="I21|I22|I25.2",data_17$cormorbidity_all),1,0)#Myocardial infarction*/
data_17$cci2=ifelse(grepl(pattern="I09.9|I11.0|I13.0|I13.2|I25.5|I42.0|I42.5|I42.6|I42.7|I42.8|I42.9|I43|I50|P29.0",data_17$cormorbidity_all),1,0)##Congestive heart failure*/
data_17$cci3=ifelse(grepl(pattern='I70|I71|I73.1|I73.8|I73.9|I77.1|I79.0|I79.2|K55.1|K55.8|K55.9|Z95.8|Z95.9',data_17$cormorbidity_all),1,0)#Peripheral vascular disease*
data_17$cci4=ifelse(grepl(pattern='G45|G46|H34.0|I60|I61|I62|I63|I64|I65|I66|I67|I68|I69',data_17$cormorbidity_all),1,0)#Cerebrovascular disease*
data_17$cci5=ifelse(grepl(pattern='F00|F01|F02|F03|F05.1|G30|G31.1',data_17$cormorbidity_all),1,0)#Dementia*
data_17$cci6=ifelse(grepl(pattern='I27.8|I27.9|J40|J41|J42|J43|J44|J45|J46|J47|J60|J61|J62|J63|J64|J65|J66|J67|J68.4|J70.1|J70.3',data_17$cormorbidity_all),1,0)#Chronic pulmonary disease*/
data_17$cci7=ifelse(grepl(pattern='M05|M06|M31.5|M32|M33|M34|M35.1|M35.3|M36.0',data_17$cormorbidity_all),1,0)#Rheumatic disease*
data_17$cci8=ifelse(grepl(pattern="K25|K26|K27|K28",data_17$cormorbidity_all),1,0)#*Peptic ulcer disease*/
data_17$cci9=ifelse(grepl(pattern="B18|K70.0|K70.1|K70.2|K70.3|K70.9|K71.3|K71.4|K71.5|K71.7|K73|K74|K76.0|K76.2|K76.3|K76.4|K76.8|K76.9|Z94.4",data_17$cormorbidity_all),1,0)#* Mild liver disease*/
data_17$cci10=ifelse(grepl(pattern="E10.0|E10.1|E10.6|E10.8|E10.9|E11.0|E11.1|E11.6|E11.8|E11.9|E12.0|E12.1|E12.6|E12.8|E12.9|E13.0|E13.1|E13.6|E13.8|E13.9|E14.0|E14.1|E14.6|E14.8|E14.9",data_17$cormorbidity_all),1,0)#Diabetes without chronic complication*/
#Score 2 
data_17$cci11=ifelse(grepl(pattern="E10.2|E10.3|E10.4|E10.5|E10.7|E11.2|E11.3|E11.4|E11.5|E11.7|E12.2|E12.3|E12.4|E12.5|E12.7|E13.2|E13.3|E13.4|E13.5|E13.7|E14.2|E14.3|E14.4|E14.5|E14.7",data_17$cormorbidity_all),2,0)#Diabetes with chronic complication*/
data_17$cci12=ifelse(grepl(pattern="G04.1|G11.4|G80.1|G80.2|G81|G82|G83.0|G83.1|G83.2|G83.3|G83.4|G83.9",data_17$cormorbidity_all),2,0)#*Hemiplegia or paraplegia*/
data_17$cci13=ifelse(grepl(pattern="I12.0|I13.1|N03.2|N03.3|N03.4|N03.5|N03.6|N03.7|N05.2|N05.3|N05.4|N05.5|N05.6|N05.7|N18|N19|N25.0|Z49.0|Z49.1|Z49.2|Z94.0|Z99.2",data_17$cormorbidity_all),2,0)#*Renal disease*/
data_17$cci14=ifelse(grepl(pattern="C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97",data_17$cormorbidity_all),2,0)#*Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin*/
#Score 3
data_17$cci15=ifelse(grepl(pattern="I85.0|I85.9|I86.4|I98.2|K70.4|K71.1|K72.1|K72.9|K76.5|K76.6|K76.7",data_17$cormorbidity_all),3,0)#*Moderate or severe liver disease*/
data_17$cci16=ifelse(grepl(pattern="C77|C78|C79|C80",data_17$cormorbidity_all),3,0)#*Metastatic solid tumor*/
#Score 6
data_17$cci17=ifelse(grepl(pattern="B20|B21|B22|B24",data_17$cormorbidity_all),6,0)#*AIDS/HIV*/
###Calculte charlson index
data_17$charlson_score <- data_17$cci1 + data_17$cci2 + data_17$cci3 + data_17$cci4 + data_17$cci5+data_17$cci6+data_17$cci7+data_17$cci8+data_17$cci9+data_17$cci10+
  data_17$cci11 + data_17$cci12 +data_17$cci13 +data_17$cci14 + data_17$cci15 + data_17$cci16 + data_17$cci17
names(data_17)

##Occupational Category
data_17 <- data_17[!is.na(data_17$ZY),]
table(data_17$ZY,useNA = "ifany")
oldvals <- c("11","13","17","21","24","27","31","37","51","54","70","80","90")
newvals <- factor(c("1","1","1","1","2","3","7","7","4","7","5","6","7"))
data_17$job <- newvals[match(data_17$ZY,oldvals)]
table(data_17$job,useNA = "ifany")

#（1 Private hospitals，0 Public hospitals
data_17 <- data_17[!is.na(data_17$JJLX),]#
data_17$gsl <- ifelse(data_17$JJLX==11|data_17$JJLX==12,0,1)
table(data_17$gsl,useNA = "ifany")

#Hospital levels
table(data_17$YYDJ_J,useNA = "ifany")

#Share emergency admissions
names(data_17)
table(data_17$RYTJ_new)
data_17_j <- data_17[data_17$RYTJ_new==1,]
data_17_j$j <- 1
data_17_other <- anti_join(data_17,data_17_j,by="ID")
names(data_17_other)
data_17_other$j <- 0
data_j <- rbind(data_17_j[,c("ID","j")],data_17_other[,c("ID","j")])
data_17 <- merge(data_17,data_j,by="ID")
table(data_17$j,useNA = "ifany")
data_17 <- merge(data_17,aggregate(j~HSAID_P,data =data_17,sum),by="HSAID_P")
names(data_17)[85] <- "hsa_j"
data_17$count <- 1
class(data_17$count)
data_17 <- merge(data_17,aggregate(count~HSAID_P,data =data_17,sum),by="HSAID_P")
names(data_17)[87] <- "hsa_count"
data_17$jrate <-data_17$hsa_j/data_17$hsa_count*100
summary(data_17$jrate)
hsa_jrate <- data_17[!duplicated(data_17$HSAID_P),]
DHSA_17 <- merge(DHSA,hsa_jrate[,c("HSAID_P","jrate")],by.x="HSAID",by.y = "HSAID_P")

#Unemployment rate
names(data_17)
table(data_17$job,useNA = "ifany")
data_17$count <- 1
require(dplyr)
a <- data_17%>%
  group_by(job,HSAID_P)%>%
  summarise(Sum_job=sum(count))
b <- dcast(data = a,HSAID_P ~ job)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7")
b[is.na(b)] <- 0
b <- transform(b,pro=X5/(X1+X2+X3+X4+X5+X6+X7)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[19] <- "unemploy_Pro"

#Average Charlson comorbidity index within HSAs
names(data_17)
summary(data_17$charlson_score)
data_17 <- merge(data_17,aggregate(charlson_score~HSAID_P,data =data_17,sum),by="HSAID_P")
names(data_17)[90] <- "hsa_CCI"
data_17$CCI_average <-data_17$hsa_CCI/data_17$hsa_count
summary(data_17$CCI_average)
hsa_CCI <- data_17[!duplicated(data_17$HSAID_P),]
DHSA_17 <- merge(DHSA_17,hsa_CCI[,c("HSAID_P","CCI_average")],by.x="HSAID",by.y = "HSAID_P")

#Share aged 65 and over
data_17$age <- as.factor(data_17$age)
data_17$count <- 1
a <- aggregate(count ~HSAID_P+age,data = data_17,sum)
b <- dcast(data = a,HSAID_P ~ age)
names(b) <- c("HSAID","X1","X2","X3")
b <- transform(b,pro=X1/(X1+X2+X3)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[24] <- "Elder_Pro"

#Share of females
names(data_17)
data_17$XB <- as.factor(data_17$XB)
require(dplyr)
b <- dcast(data = a,HSAID_P ~ XB)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X2/(X1+X2)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[22] <- "Female_Pro"

fwrite(data_17,"data_17q.csv",row.names = F)
data_17 <- read.csv("data_17q.csv",na.strings = c("NA","","-","--","---","----","------"))

#Share payment by insurance
names(data_17)
table(data_17$health_insur,useNA = "ifany")
require(dplyr)
a <- aggregate(count.x~health_insur+HSAID_P,data = data_17,sum)
b <- dcast(data = a,HSAID_P ~ health_insur)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7","X8","X9")
b[is.na(b)] <- 0
b <- transform(b,pro=(X1+X2+X3+X5+X8)/(X1+X2+X3+X4+X5+X6+X7+X8+X9)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[25] <- "insurancepay_Pro"

#Share of critical conditions
names(data_17)
table(data_17$BWHBZ,useNA = "ifany")
require(dplyr)
a <- aggregate(count.x~BWHBZ+HSAID_P,data = data_17,sum)
b <- dcast(data = a,HSAID_P ~ BWHBZ)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X1/(X1+X2)*100)
DHSA_17 <- merge(DHSA_17,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_17)[26] <- "bw_Pro"

#Share of critical conditions
data_17 <- read.csv("data_17.csv",na.strings = c("NA","","-","--","---","----","------"))
jg <-data_17[!duplicated(data_17$YLJGID),]
jg_dart <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\jg_dhsa.csv")
jg <- merge(jg,jg_dart[,c("YLJGID","HSAID")],by="YLJGID")
names(jg)[101] <- "HSAID_jg"
length(unique(jg$HSAID_jg))
names(jg)
jg$count <- 1
jg_hsa <- merge(jg,aggregate(count~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg_hsa)[102] <- "Sumhos"
names(jg_hsa)
jg_hsa <- merge(jg_hsa,aggregate(gsl~HSAID_jg,data = jg_hsa,sum),by="HSAID_jg")
names(jg_hsa)[103] <- "hsa_gsl"
jg_hsa$gsl_pro <- jg_hsa$hsa_gsl/jg_hsa$Sumhos*100
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_151.csv",encoding = "UTF-8")
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
names(DHSA)
DHSA_17 <- merge(DHSA_17,jg[,c("HSAID_jg","gsl_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Share tertiary hospitals
jg_hsa$sanji <- ifelse(jg_hsa$YYDJ_J==3,1,0)
names(jg)
jg_hsa <- merge(jg_hsa,aggregate(sanji~HSAID_jg,data = jg_hsa[,c("sanji","HSAID_jg")],sum),by="HSAID_jg")
names(jg_hsa)[106] <- "hsa_sanji"
jg_hsa$sanji_pro <- jg_hsa$hsa_sanji/jg_hsa$Sumhos*100
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
DHSA_17 <- merge(DHSA_17,jg[,c("HSAID_jg","sanji_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Herfindahl-Hirschman index within HSAs
library(data.table)
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
jg_hsa_17 <- jg_hsa[,c("HSAID_jg","YLJGID")]
jg_dart <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\jg_dhsa.csv")
names(jg_dart)
jg_hsa_17 <- merge(jg_hsa_17,jg_dart[,c("YLJGID","X2017_bed_1.x")])
jg_hsa_17 <-merge(jg_hsa_17,aggregate(X2017_bed_1.x~HSAID_jg,data = jg_hsa_17,sum),by="HSAID_jg")
names(jg_hsa_17)[4] <- "hsa_bed"
jg_hsa_17$share <- jg_hsa_17$X2017_bed_1.x/jg_hsa_17$hsa_bed 
jg_hsa_17$shares2 <- (jg_hsa_17$share)^2 
jg_hsa_17 <- merge(jg_hsa_17,aggregate(shares2~HSAID_jg,data = jg_hsa_17,sum),by="HSAID_jg") 
names(jg_hsa_17)[7] <- "HHI"
jg_hsa_17$HHI_new <- jg_hsa_17$HHI*10000
hsa_HHI <- jg_hsa_17[!duplicated(jg_hsa_17$HSAID_jg),]
fwrite(jg_hsa_17,"jg_hsa_17.csv",row.names = F)
DHSA_17 <- merge(DHSA_17,hsa_HHI[,c("HSAID_jg","HHI_new")],by.x = "HSAID",by.y = "HSAID_jg")

#Death and birth rate within HSAs 
HSA_sc <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dhsa_street.txt",encoding="UTF-8")
qx_data <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\qx_data.csv")
street <- fread("G:\\曹裴娅\\毕业论文\\huff\\street.txt",encoding="UTF-8")
HSA_sc <- merge(HSA_sc,street[,c("town_code","county_cod","qx")],by.x = "Zipcode",by.y = "town_code")
HSA_sc <- merge(HSA_sc,qx_data[,c("county_code","X")],by.x = "county_cod",by.y = "county_code")
r <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\2017-19出生率与死亡率.csv")
HSA_sc <- merge(HSA_sc,r,by.x = "X",by.y = "市")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
class(hsa_shizhou$V3)
fwrite(hsa_shizhou,"hsa_shizhou.csv",row.names = F)
library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1.xlsx",1,header = F)
graph_1 <- merge(graph_1,r,by.x = "X2",by.y = "市")
a <- anti_join(graph_1,graph_2)
graph_1 <- graph_1[,c(9:14,2)]
names(graph_1) <- c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,r,by.x = "value",by.y = "市")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_2 <- merge(graph_2,aggregate(bir17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea17~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea18~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(bir19~HSAID,data =graph_2,sum),by="HSAID")
graph_2 <- merge(graph_2,aggregate(dea19~HSAID,data =graph_2,sum),by="HSAID")
graph_2$bir17 <- graph_2$bir17.y/2
graph_2$bir18 <- graph_2$bir18.y/2
graph_2$bir19 <- graph_2$bir19.y/2
graph_2$dea17 <- graph_2$dea17.y/2
graph_2$dea18 <- graph_2$dea18.y/2
graph_2$dea19 <- graph_2$dea19.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,r,by.x = "value",by.y = "市")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","bir17","dea17","bir18","dea18","bir19","dea19")
graph_3 <- merge(graph_3,aggregate(bir17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea17~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea18~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(bir19~HSAID,data =graph_3,sum),by="HSAID")
graph_3 <- merge(graph_3,aggregate(dea19~HSAID,data =graph_3,sum),by="HSAID")
graph_3$bir17 <- graph_3$bir17.y/3
graph_3$bir18 <- graph_3$bir18.y/3
graph_3$bir19 <- graph_3$bir19.y/3
graph_3$dea17 <- graph_3$dea17.y/3
graph_3$dea18 <- graph_3$dea18.y/3
graph_3$dea19 <- graph_3$dea19.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("bir17","bir18","bir19","dea17","dea18","dea19","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)
DHSA_17 <- DHSA171819[DHSA171819$Year==2017,]
DHSA_17 <- merge(DHSA_17,hsa_r[,c("bir17","dea17","HSAID")],by="HSAID")
names(DHSA_17)[20] <- "birthr"
names(DHSA_17)[21] <- "deathr"
write.csv(DHSA_17,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_17.csv",row.names=F)
names(DHSA_17)
DHSA_17 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_17.csv")

#2018
#Gender
data_18 <- data_18[!is.na(data_18$XB),]
table(data_18$XB,useNA = "ifany")
data_18 <- data_18[!data_18$XB==0,]
data_18 <- data_18[!data_18$XB==9,]
#insurance
data_18 <- data_18[!is.na(data_18$YLFKFS),]
names(data_18)
table(data_18$YLFKFS,useNA = "ifany")
oldvals <- c("1","2","3","4","5","6","7","8","99")
newvals <- factor(c("1","2","3","4","5","6","7","8","9"))
data_18$health_insur <- newvals[match(data_18$YLFKFS,oldvals)]
table(data_18$health_insur,useNA = "ifany")

#Admission sources
data_18 <- data_18[!is.na(data_18$RYTJ),]#
table(data_18$RYTJ,useNA = "ifany")
oldvals <- c("1","2","3","9")
newvals <- factor(c("1","2","3","3"))
data_18$RYTJ_new <- newvals[match(data_18$RYTJ,oldvals)]
table(data_18$RYTJ_new,useNA = "ifany")

#Charlson Comorbidity index
data_18$cormorbidity_all <- paste(data_18$JBDM1,data_18$JBDM2,data_18$JBDM3,data_18$JBDM4,data_18$JBDM5,data_18$JBDM6,data_18$JBDM7,data_18$JBDM8,data_18$JBDM9,data_18$JBDM10,data_18$JBDM11,data_18$JBDM12,data_18$JBDM13,data_18$JBDM14,data_18$JBDM15,sep = ",")
#cci1(charlson index)
#Score 1
data_18$cci1=ifelse(grepl(pattern="I21|I22|I25.2",data_18$cormorbidity_all),1,0)#Myocardial infarction*/
data_18$cci2=ifelse(grepl(pattern="I09.9|I11.0|I13.0|I13.2|I25.5|I42.0|I42.5|I42.6|I42.7|I42.8|I42.9|I43|I50|P29.0",data_18$cormorbidity_all),1,0)##Congestive heart failure*/
data_18$cci3=ifelse(grepl(pattern='I70|I71|I73.1|I73.8|I73.9|I77.1|I79.0|I79.2|K55.1|K55.8|K55.9|Z95.8|Z95.9',data_18$cormorbidity_all),1,0)#Peripheral vascular disease*
data_18$cci4=ifelse(grepl(pattern='G45|G46|H34.0|I60|I61|I62|I63|I64|I65|I66|I67|I68|I69',data_18$cormorbidity_all),1,0)#Cerebrovascular disease*
data_18$cci5=ifelse(grepl(pattern='F00|F01|F02|F03|F05.1|G30|G31.1',data_18$cormorbidity_all),1,0)#Dementia*
data_18$cci6=ifelse(grepl(pattern='I27.8|I27.9|J40|J41|J42|J43|J44|J45|J46|J47|J60|J61|J62|J63|J64|J65|J66|J67|J68.4|J70.1|J70.3',data_18$cormorbidity_all),1,0)#Chronic pulmonary disease*/
data_18$cci7=ifelse(grepl(pattern='M05|M06|M31.5|M32|M33|M34|M35.1|M35.3|M36.0',data_18$cormorbidity_all),1,0)#Rheumatic disease*
data_18$cci8=ifelse(grepl(pattern="K25|K26|K27|K28",data_18$cormorbidity_all),1,0)#*Peptic ulcer disease*/
data_18$cci9=ifelse(grepl(pattern="B18|K70.0|K70.1|K70.2|K70.3|K70.9|K71.3|K71.4|K71.5|K71.7|K73|K74|K76.0|K76.2|K76.3|K76.4|K76.8|K76.9|Z94.4",data_18$cormorbidity_all),1,0)#* Mild liver disease*/
data_18$cci10=ifelse(grepl(pattern="E10.0|E10.1|E10.6|E10.8|E10.9|E11.0|E11.1|E11.6|E11.8|E11.9|E12.0|E12.1|E12.6|E12.8|E12.9|E13.0|E13.1|E13.6|E13.8|E13.9|E14.0|E14.1|E14.6|E14.8|E14.9",data_18$cormorbidity_all),1,0)#Diabetes without chronic complication*/
#Score 2
data_18$cci11=ifelse(grepl(pattern="E10.2|E10.3|E10.4|E10.5|E10.7|E11.2|E11.3|E11.4|E11.5|E11.7|E12.2|E12.3|E12.4|E12.5|E12.7|E13.2|E13.3|E13.4|E13.5|E13.7|E14.2|E14.3|E14.4|E14.5|E14.7",data_18$cormorbidity_all),2,0)#Diabetes with chronic complication*/
data_18$cci12=ifelse(grepl(pattern="G04.1|G11.4|G80.1|G80.2|G81|G82|G83.0|G83.1|G83.2|G83.3|G83.4|G83.9",data_18$cormorbidity_all),2,0)#*Hemiplegia or paraplegia*/
data_18$cci13=ifelse(grepl(pattern="I12.0|I13.1|N03.2|N03.3|N03.4|N03.5|N03.6|N03.7|N05.2|N05.3|N05.4|N05.5|N05.6|N05.7|N18|N19|N25.0|Z49.0|Z49.1|Z49.2|Z94.0|Z99.2",data_18$cormorbidity_all),2,0)#*Renal disease*/
data_18$cci14=ifelse(grepl(pattern="C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97",data_18$cormorbidity_all),2,0)#*Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin*/
#Score 3
data_18$cci15=ifelse(grepl(pattern="I85.0|I85.9|I86.4|I98.2|K70.4|K71.1|K72.1|K72.9|K76.5|K76.6|K76.7",data_18$cormorbidity_all),3,0)#*Moderate or severe liver disease*/
data_18$cci16=ifelse(grepl(pattern="C77|C78|C79|C80",data_18$cormorbidity_all),3,0)#*Metastatic solid tumor*/
#Score 6
data_18$cci17=ifelse(grepl(pattern="B20|B21|B22|B24",data_18$cormorbidity_all),6,0)#*AIDS/HIV*/
####Calculate charlson index
data_18$charlson_score <- data_18$cci1 + data_18$cci2 + data_18$cci3 + data_18$cci4 + data_18$cci5+data_18$cci6+data_18$cci7+data_18$cci8+data_18$cci9+data_18$cci10+
  data_18$cci11 + data_18$cci12 +data_18$cci13 +data_18$cci14 + data_18$cci15 + data_18$cci16 + data_18$cci17
names(data_18)

#Occupational Category
data_18 <- data_18[!is.na(data_18$ZY),]
table(data_18$ZY,useNA = "ifany")
oldvals <- c("11","13","17","21","24","27","31","37","51","54","70","80","90")
newvals <- factor(c("1","1","1","1","2","3","7","7","4","7","5","6","7"))
data_18$job <- newvals[match(data_18$ZY,oldvals)]
table(data_18$job,useNA = "ifany")

#（1 Private hospitals，0 Public hospitals）
data_18 <- data_18[!is.na(data_18$JJLX),]
data_18$gsl <- ifelse(data_18$JJLX==11|data_18$JJLX==12,0,1)
table(data_18$gsl,useNA = "ifany")

DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_151.csv",encoding = "UTF-8")
DHSA <- merge(DHSA,hsa_18[,c("zfy_perpat_ad","zfy_perpat","HSAID_P")],by.x = "HSAID",by.y ="HSAID_P")
names(DHSA)
DHSA_18 <- DHSA[,c("HSAID","hsa18_perGDP","hsa_pop_18","perbed_18","zfy_perpat","zfy_perpat_ad")]
names(DHSA_18) <- c("HSAID","PerGDP","POPU","Perbed","zfy_perpat","adjust_perpatzfy")

# Urbanization rate (administrative data)
names(data_18)
town <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\town_2019.csv")
data_18 <- merge(data_18,town[,c("town_code","X")],by.x = "stv_code_p",by.y = "town_code" )
urban <- read.csv("urbanr.csv")
urban <- urban[urban$year==2018,]
names(urban)

HSA_sc <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dhsa_street.txt",encoding="UTF-8")
qx_data <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\qx_data.csv")
street <- fread("G:\\曹裴娅\\毕业论文\\huff\\street.txt",encoding="UTF-8")
HSA_sc <- merge(HSA_sc,street[,c("town_code","county_cod","qx")],by.x = "Zipcode",by.y = "town_code")
HSA_sc <- merge(HSA_sc,qx_data[,c("county_code","X")],by.x = "county_cod",by.y = "county_code")
HSA_sc <- merge(HSA_sc,urban,by.x = "X",by.y = "city")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
hsa_shizhou <- as.data.frame(hsa_shizhou)
# write.csv(hsa_shizhou,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city.csv", row.names=F)
library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1.xlsx",1,header = F)
graph_1 <- merge(graph_1,urban,by.x = "X2",by.y = "city")
graph_1 <- graph_1[,c(9,2)]
names(graph_1) <- c("urbanr","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,urban,by.x = "value",by.y = "city")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","urbanr","year")
graph_2 <- merge(graph_2,aggregate(urbanr~HSAID,data =graph_2,sum),by="HSAID")
graph_2$urbanr <- graph_2$urbanr.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("urbanr","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,urban,by.x = "value",by.y = "city")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","urbanr","year")
graph_3 <- merge(graph_3,aggregate(urbanr~HSAID,data =graph_3,sum),by="HSAID")
graph_3$urbanr <- graph_3$urbanr.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("urbanr","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)
summary(hsa_r$urbanr)
names(DHSA_18)
class(DHSA_18$HSAID)
DHSA_18$HSAID <- as.character(DHSA_18$HSAID)
class(hsa_r$HSAID)
DHSA_18 <- merge(DHSA_18,hsa_r,by="HSAID")

#Share emergency admissions
names(data_18)
table(data_18$RYTJ_new)
data_18_j <- data_18[data_18$RYTJ_new==1,]
data_18_j$j <- 1
data_18_other <- anti_join(data_18,data_18_j,by="ID")
names(data_18_other)
data_18_other$j <- 0
data_j <- rbind(data_18_j[,c("ID","j")],data_18_other[,c("ID","j")])
data_18 <- merge(data_18,data_j,by="ID")
table(data_18$j)
data_18 <- merge(data_18,aggregate(j~HSAID_P,data =data_18,sum),by="HSAID_P")
names(data_18)[95] <- "hsa_j"
data_18$count <- 1
data_18 <- merge(data_18,aggregate(count~HSAID_P,data =data_18,sum),by="HSAID_P")
names(data_18)[97] <- "hsa_count"
data_18$jrate <-data_18$hsa_j/data_18$hsa_count*100
table(data_18$jrate)
hsa_jrate <- data_18[!duplicated(data_18$HSAID),]
hsa_jrate$HSAID_P <- as.character(hsa_jrate$HSAID_P)
DHSA_18 <- merge(DHSA_18,hsa_jrate[,c("HSAID_P","jrate")],by.x = "HSAID",by.y="HSAID_P")

#Unemployment rate
table(data_18$job,useNA = "ifany")
names(data_18)
data_18$count <- 1
require(dplyr)
a <- data_18%>%
  group_by(job,HSAID_P)%>%
  summarise(Sum_job=sum(count))
b <- dcast(data = a,HSAID_P ~ job)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7")
b[is.na(b)] <- 0
b <- transform(b,pro=X5/(X1+X2+X3+X4+X5+X6+X7)*100)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[9] <- "unemploy_Pro"

#Average Charlson comorbidity index within HSAs
names(data_18)
summary(data_18$charlson_score)
data_18 <- merge(data_18,aggregate(charlson_score~HSAID_P,data =data_18,sum),by="HSAID_P")
names(data_18)[80] <- "hsa_CCI"
data_18$CCI_average <-data_18$hsa_CCI/data_18$hsa_count
summary(data_18$CCI_average)
hsa_CCI <- data_18[!duplicated(data_18$HSAID_P),]
hsa_CCI$HSAID_P <- as.character(hsa_CCI$HSAID_P)
DHSA_18 <- merge(DHSA_18,hsa_CCI[,c("HSAID_P","CCI_average")],by.x = "HSAID",by.y="HSAID_P")

#Share aged 65 and over
data_18$age <- as.factor(data_18$age)
data_18$count <- 1
a <- aggregate(count ~HSAID_P+age,data = data_18,sum)
b <- dcast(data = a,HSAID_P ~ age)
names(b) <- c("HSAID","X1","X2","X3")
b <- transform(b,pro=X1/(X1+X2+X3)*100)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[24] <- "Elder_Pro"

#Share of females
names(data_18)
#data_18$count <- 1
require(dplyr)
a <- data_18%>%
  group_by(XB,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ XB)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X2/(X1+X2)*100)
b$HSAID <- as.character(b$HSAID)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[12] <- "Female_Pro"

fwrite(data_18,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\data_18.csv",row.names = F)

#Share payment by insurance
names(data_18)
table(data_18$health_insur,useNA = "ifany")
require(dplyr)
a <- data_18%>%
  group_by(health_insur,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ health_insur)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7","X8","X9")
b <- transform(b,pro=(X1+X2+X3+X5+X8)/(X1+X2+X3+X4+X5+X6+X7+X8+X9)*100)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[14] <- "insurancepay_Pro"

#Share of critical conditions
names(data_18)
table(data_18$BWHBZ,useNA = "ifany")
require(dplyr)
a <- data_18%>%
  group_by(BWHBZ,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ BWHBZ)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X1/(X1+X2)*100)
DHSA_18 <- merge(DHSA_18,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_18)[15] <- "bw_Pro"

#Share private hospitals
data_18 <- read.csv("data_18.csv",na.strings = c("NA","","-","--","---","----","------"))
jg <-data_18[!duplicated(data_18$YLJGID),]
jg_dart <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\jg_dhsa.csv")
jg <- merge(jg,jg_dart[,c("YLJGID","HSAID")],by="YLJGID")
names(jg)[100] <- "HSAID_jg"
length(unique(jg$HSAID_jg))
names(jg)
jg$count <- 1
jg_hsa <- merge(jg,aggregate(count~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg_hsa)[101] <- "Sumhos"
names(jg_hsa)
jg_hsa <- merge(jg_hsa,aggregate(gsl~HSAID_jg,data = jg_hsa,sum),by="HSAID_jg")
names(jg_hsa)[95] <- "hsa_gsl"
jg_hsa$gsl_pro <- jg_hsa$hsa_gsl/jg_hsa$Sumhos*100
jg_hsa <- jg_hsa[!duplicated(jg_hsa$HSAID_P),]
DHSA_18 <- merge(DHSA_18,jg_hsa[,c("HSAID_jg","gsl_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Share tertiary hospitals
jg_hsa$sanji <- ifelse(jg_hsa$YYDJ_J==3,1,0)
names(jg)
jg_hsa <- merge(jg_hsa,aggregate(sanji~HSAID_jg,data = jg_hsa[,c("sanji","HSAID_jg")],sum),by="HSAID_jg")
names(jg_hsa)[98] <- "hsa_sanji"
jg_hsa$sanji_pro <- jg_hsa$hsa_sanji/jg_hsa$Sumhos*100
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
names(jg)
jg$HSAID_jg <- as.character(jg$HSAID_jg)
DHSA_18 <- merge(DHSA_18,jg[,c("HSAID_jg","sanji_pro")],by.x ="HSAID",by.y = "HSAID_jg")
names(DHSA_18)
write.csv(DHSA_18,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_18.csv", row.names= F)

#Herfindahl-Hirschman index within HSAs
library(data.table)
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
jg_hsa_18 <- jg_hsa[,c("HSAID_jg","YLJGID")]
jg_dart <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\jg_dhsa.csv")
names(jg_dart)
jg_hsa_18 <- merge(jg_hsa_18,jg_dart[,c("YLJGID","X2018_bed_1.x")],by="YLJGID")
jg_hsa_18 <-merge(jg_hsa_18,aggregate(X2018_bed_1.x ~ HSAID_jg,data = jg_hsa_18,sum),by="HSAID_jg")
names(jg_hsa_18)[4] <- "hsa_bed"
jg_hsa_18$share <- jg_hsa_18$X2018_bed_1.x.x/jg_hsa_18$hsa_bed 
jg_hsa_18$shares2 <- (jg_hsa_18$share)^2 
jg_hsa_18 <- merge(jg_hsa_18,aggregate(shares2~HSAID_jg,data = jg_hsa_18,sum),by="HSAID_jg") 
names(jg_hsa_18)[7] <- "HHI"
jg_hsa_18$HHI_new <- jg_hsa_18$HHI*10000
hsa_HHI <- jg_hsa_18[!duplicated(jg_hsa_18$HSAID_jg),]
fwrite(jg_hsa_18,"jg_hsa_18.csv",row.names = F)
hsa_HHI$HSAID_jg <- as.integer(hsa_HHI$HSAID_jg)
DHSA_18 <- merge(DHSA_18,hsa_HHI[,c("HSAID_jg","HHI_new")],by.x = "HSAID",by.y = "HSAID_jg")

#2019
names(data_19)
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")

#Gender
data_19 <- data_19[!is.na(data_19$XB),]
table(data_19$XB,useNA = "ifany")
data_19 <- data_19[!data_19$XB==0,]
data_19 <- data_19[!data_19$XB==9,]
#insurance
data_19 <- data_19[!is.na(data_19$YLFKFS),]
names(data_19)
table(data_19$YLFKFS,useNA = "ifany")
oldvals <- c("1","2","3","4","5","6","7","8","9","99")
newvals <- factor(c("1","2","3","4","5","6","7","8","9","9"))
data_19$health_insur <- newvals[match(data_19$YLFKFS,oldvals)]
table(data_19$health_insur,useNA = "ifany")

#Admission sources 
data_19 <- data_19[!is.na(data_19$RYTJ),]#
table(data_19$RYTJ)
oldvals <- c("1","2","3","9")
newvals <- factor(c("1","2","3","3"))
data_19$RYTJ_new <- newvals[match(data_19$RYTJ,oldvals)]
table(data_19$RYTJ_new,useNA = "ifany")

#Charlson Comorbidity index
data_19$cormorbidity_all <- paste(data_19$JBDM1,data_19$JBDM2,data_19$JBDM3,data_19$JBDM4,data_19$JBDM5,data_19$JBDM6,data_19$JBDM7,data_19$JBDM8,data_19$JBDM9,data_19$JBDM10,data_19$JBDM11,data_19$JBDM12,data_19$JBDM13,data_19$JBDM14,data_19$JBDM15,sep = ",")
#cci1(charlson index)
#Score 1
data_19$cci1=ifelse(grepl(pattern="I21|I22|I25.2",data_19$cormorbidity_all),1,0)#Myocardial infarction*/
data_19$cci2=ifelse(grepl(pattern="I09.9|I11.0|I13.0|I13.2|I25.5|I42.0|I42.5|I42.6|I42.7|I42.8|I42.9|I43|I50|P29.0",data_19$cormorbidity_all),1,0)##Congestive heart failure*/
data_19$cci3=ifelse(grepl(pattern='I70|I71|I73.1|I73.8|I73.9|I77.1|I79.0|I79.2|K55.1|K55.8|K55.9|Z95.8|Z95.9',data_19$cormorbidity_all),1,0)#Peripheral vascular disease*
data_19$cci4=ifelse(grepl(pattern='G45|G46|H34.0|I60|I61|I62|I63|I64|I65|I66|I67|I68|I69',data_19$cormorbidity_all),1,0)#Cerebrovascular disease*
data_19$cci5=ifelse(grepl(pattern='F00|F01|F02|F03|F05.1|G30|G31.1',data_19$cormorbidity_all),1,0)#Dementia*
data_19$cci6=ifelse(grepl(pattern='I27.8|I27.9|J40|J41|J42|J43|J44|J45|J46|J47|J60|J61|J62|J63|J64|J65|J66|J67|J68.4|J70.1|J70.3',data_19$cormorbidity_all),1,0)#Chronic pulmonary disease*/
data_19$cci7=ifelse(grepl(pattern='M05|M06|M31.5|M32|M33|M34|M35.1|M35.3|M36.0',data_19$cormorbidity_all),1,0)#Rheumatic disease*
data_19$cci8=ifelse(grepl(pattern="K25|K26|K27|K28",data_19$cormorbidity_all),1,0)#*Peptic ulcer disease*/
data_19$cci9=ifelse(grepl(pattern="B18|K70.0|K70.1|K70.2|K70.3|K70.9|K71.3|K71.4|K71.5|K71.7|K73|K74|K76.0|K76.2|K76.3|K76.4|K76.8|K76.9|Z94.4",data_19$cormorbidity_all),1,0)#* Mild liver disease*/
data_19$cci10=ifelse(grepl(pattern="E10.0|E10.1|E10.6|E10.8|E10.9|E11.0|E11.1|E11.6|E11.8|E11.9|E12.0|E12.1|E12.6|E12.8|E12.9|E13.0|E13.1|E13.6|E13.8|E13.9|E14.0|E14.1|E14.6|E14.8|E14.9",data_19$cormorbidity_all),1,0)#Diabetes without chronic complication*/
#Score 2
data_19$cci11=ifelse(grepl(pattern="E10.2|E10.3|E10.4|E10.5|E10.7|E11.2|E11.3|E11.4|E11.5|E11.7|E12.2|E12.3|E12.4|E12.5|E12.7|E13.2|E13.3|E13.4|E13.5|E13.7|E14.2|E14.3|E14.4|E14.5|E14.7",data_19$cormorbidity_all),2,0)#Diabetes with chronic complication*/
data_19$cci12=ifelse(grepl(pattern="G04.1|G11.4|G80.1|G80.2|G81|G82|G83.0|G83.1|G83.2|G83.3|G83.4|G83.9",data_19$cormorbidity_all),2,0)#*Hemiplegia or paraplegia*/
data_19$cci13=ifelse(grepl(pattern="I12.0|I13.1|N03.2|N03.3|N03.4|N03.5|N03.6|N03.7|N05.2|N05.3|N05.4|N05.5|N05.6|N05.7|N18|N19|N25.0|Z49.0|Z49.1|Z49.2|Z94.0|Z99.2",data_19$cormorbidity_all),2,0)#*Renal disease*/
data_19$cci14=ifelse(grepl(pattern="C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49|C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97",data_19$cormorbidity_all),2,0)#*Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin*/
#Score 3
data_19$cci15=ifelse(grepl(pattern="I85.0|I85.9|I86.4|I98.2|K70.4|K71.1|K72.1|K72.9|K76.5|K76.6|K76.7",data_19$cormorbidity_all),3,0)#*Moderate or severe liver disease*/
data_19$cci16=ifelse(grepl(pattern="C77|C78|C79|C80",data_19$cormorbidity_all),3,0)#*Metastatic solid tumor*/
#Score 6
data_19$cci17=ifelse(grepl(pattern="B20|B21|B22|B24",data_19$cormorbidity_all),6,0)#*AIDS/HIV*/
####Calculate charlson index
data_19$charlson_score <- data_19$cci1 + data_19$cci2 + data_19$cci3 + data_19$cci4 + data_19$cci5+data_19$cci6+data_19$cci7+data_19$cci8+data_19$cci9+data_19$cci10+
  data_19$cci11 + data_19$cci12 +data_19$cci13 +data_19$cci14 + data_19$cci15 + data_19$cci16 + data_19$cci17
names(data_19)


#Occupational Category
data_19 <- data_19[!is.na(data_19$ZY),]
table(data_19$ZY,useNA = "ifany")
oldvals <- c("-","11","13","17","21","24","27","31","37","51","54","70","80","90")
newvals <- factor(c("7","1","1","1","1","2","3","7","7","4","7","5","6","7"))
data_19$job <- newvals[match(data_19$ZY,oldvals)]
table(data_19$job,useNA = "ifany")

#（1 Private hospitals，0 Public hospitals）
data_19 <- data_19[!is.na(data_19$JJLX),]
data_19$gsl <- ifelse(data_19$JJLX==11|data_19$JJLX==12,0,1)
table(data_19$gsl,useNA = "ifany")
#
DHSA <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dHSA_地区基本特征_151.csv",encoding = "UTF-8")
DHSA <- merge(DHSA,hsa_19[,c("zfy_perpat_ad","zfy_yperpat","HSAID_P")],by.x = "HSAID",by.y ="HSAID_P")
names(DHSA)
DHSA_19 <- DHSA[,c("HSAID","hsa19_perGDP","hsa_pop_19","perbed_19","zfy_yperpat","zfy_perpat_ad")]
names(DHSA_19) <- c("HSAID","PerGDP","POPU","Perbed","zfy_perpat","adjust_perpatzfy")

# Urbanization rate 
names(data_19)
town <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\town_2019.csv")
data_19 <- merge(data_19,town[,c("town_code","X")],by.x = "stv_code_p",by.y = "town_code" )
urban <- read.csv("urbanr.csv")
urban <- urban[urban$year==2019,]
names(urban)

HSA_sc <- fread("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\dhsa_street.txt",encoding="UTF-8")
qx_data <- read.csv("G:\\曹裴娅\\毕业论文\\原始数据\\qx_data.csv")
street <- fread("G:\\曹裴娅\\毕业论文\\huff\\street.txt",encoding="UTF-8")
HSA_sc <- merge(HSA_sc,street[,c("town_code","county_cod","qx")],by.x = "Zipcode",by.y = "town_code")
HSA_sc <- merge(HSA_sc,qx_data[,c("county_code","X")],by.x = "county_cod",by.y = "county_code")
HSA_sc <- merge(HSA_sc,urban,by.x = "X",by.y = "city")
#
hsa_shizhou <- NULL
hsa <- unique(HSA_sc$HSAID)
for (i in 1:length(hsa)) {
  temp <- HSA_sc[HSA_sc$HSAID==hsa[i],]
  temp_name <- unique(temp$X)
  temp_num1 <- length(unique(temp$X))
  temp_num2 <- length(unique(temp$X))
  temp_num3 <- length(unique(temp$X))
  temp_num4 <- length(unique(temp$X))
  temp_num5 <- length(unique(temp$X))
  temp_num6 <- length(unique(temp$X))
  temp_hsa_numshi <- c(hsa[i],temp_name,temp_num1,temp_num2,temp_num3,temp_num4,temp_num5,temp_num6)
  hsa_shizhou <- rbind(hsa_shizhou,temp_hsa_numshi)
}
hsa_shizhou
hsa_shizhou <- as.data.frame(hsa_shizhou)

library(xlsx)
graph_1 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city1.xlsx",1,header = F)
graph_1 <- merge(graph_1,urban,by.x = "X2",by.y = "city")
graph_1 <- graph_1[,c(9,2)]
names(graph_1) <- c("urbanr","HSAID")
graph_2 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city2.xlsx",1,header = F)
graph_2 <- melt(graph_2,id=c("X1"))
graph_2 <- graph_2[,c("X1","value")]
graph_2 <- merge(graph_2,urban,by.x = "value",by.y = "city")
graph_2 <- as.data.frame(graph_2)
names(graph_2) <- c("shizhou","HSAID","urbanr","year")
graph_2 <- merge(graph_2,aggregate(urbanr~HSAID,data =graph_2,sum),by="HSAID")
graph_2$urbanr <- graph_2$urbanr.y/2
graph_2 <- graph_2[!duplicated(graph_2$HSAID),c("urbanr","HSAID")]
graph_3 <- read.xlsx("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\hsa_city3.xlsx",1,header = F)
graph_3 <- melt(graph_3,id=c("X1"))
graph_3 <- graph_3[,c("X1","value")]
graph_3 <- merge(graph_3,urban,by.x = "value",by.y = "city")
graph_3 <- as.data.frame(graph_3)
names(graph_3) <- c("shizhou","HSAID","urbanr","year")
graph_3 <- merge(graph_3,aggregate(urbanr~HSAID,data =graph_3,sum),by="HSAID")
graph_3$urbanr <- graph_3$urbanr.y/3
graph_3 <- graph_3[!duplicated(graph_3$HSAID),c("urbanr","HSAID")]
hsa_r <- rbind(graph_3,graph_2,graph_1)
summary(hsa_r$urbanr)
DHSA_19 <- merge(DHSA_19,hsa_r,by="HSAID")

#Share emergency admissions
names(data_19)
table(data_19$RYTJ_new)
data_19_j <- data_19[data_19$RYTJ_new==1,]
data_19_j$j <- 1
data_19_other <- anti_join(data_19,data_19_j,by="ID")
names(data_19_other)
data_19_other$j <- 0
data_j <- rbind(data_19_j[,c("ID","j")],data_19_other[,c("ID","j")])
data_19 <- merge(data_19,data_j,by="ID")
table(data_19$j)
data_19 <- merge(data_19,aggregate(j~HSAID_P,data =data_19,sum),by="HSAID_P")
names(data_19)[82] <- "hsa_j"
data_19$count <- 1
data_19 <- merge(data_19,aggregate(count~HSAID_P,data =data_19,sum),by="HSAID_P")
names(data_19)[84] <- "hsa_count"
data_19$jrate <-data_19$hsa_j/data_19$hsa_count*100
table(data_19$jrate)
hsa_jrate <- data_19[!duplicated(data_19$HSAID),]
DHSA_19 <- merge(DHSA_19,hsa_jrate[,c("HSAID_P","jrate")],by.x = "HSAID",by.y="HSAID_P")

#Unemployment rate
table(data_19$job,useNA = "ifany")
data_19$count <- 1
require(dplyr)
a <- data_19%>%
  group_by(job,HSAID_P)%>%
  summarise(Sum_job=sum(count))
b <- dcast(data = a,HSAID_P ~ job)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7")
b[is.na(b)] <- 0
b <- transform(b,pro=X5/(X1+X2+X3+X4+X5+X6+X7)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[9] <- "unemploy_Pro"

#Average Charlson comorbidity index within HSAs
names(data_19)
summary(data_19$charlson_score)
data_19 <- merge(data_19,aggregate(charlson_score~HSAID_P,data =data_19,sum),by="HSAID_P")
names(data_19)[87] <- "hsa_CCI"
data_19$CCI_average <-data_19$hsa_CCI/data_19$hsa_count
summary(data_19$CCI_average)
hsa_CCI <- data_19[!duplicated(data_19$HSAID_P),]
DHSA_19 <- merge(DHSA_19,hsa_CCI[,c("HSAID_P","CCI_average")],by.x = "HSAID",by.y="HSAID_P")

#Share aged 65 and over
data_19$age <- as.factor(data_19$age)
data_19$count <- 1
a <- aggregate(count ~HSAID_P+age,data = data_19,sum)
b <- dcast(data = a,HSAID_P ~ age)
names(b) <- c("HSAID","X1","X2","X3")
b <- transform(b,pro=X1/(X1+X2+X3)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[24] <- "Elder_Pro"

#Share of females
names(data_19)
#data_19$count <- 1
require(dplyr)
a <- data_19%>%
  group_by(XB,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ XB)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X2/(X1+X2)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[12] <- "Female_Pro"

#Share payment by insurance
names(data_19)
table(data_19$health_insur,useNA = "ifany")
require(dplyr)
a <- data_19%>%
  group_by(health_insur,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ health_insur)
names(b) <- c("HSAID","X1","X2","X3","X4","X5","X6","X7","X8","X9")
b <- transform(b,pro=(X1+X2+X3+X5+X8)/(X1+X2+X3+X4+X5+X6+X7+X8+X9)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[15] <- "insurancepay_Pro"

#Share of critical conditions
names(data_19)
table(data_19$BWHBZ,useNA = "ifany")
require(dplyr)
a <- data_19%>%
  group_by(BWHBZ,HSAID_P)%>%
  summarise(Sum_XB=sum(count.x))
b <- dcast(data = a,HSAID_P ~ BWHBZ)
names(b) <- c("HSAID","X1","X2")
b <- transform(b,pro=X1/(X1+X2)*100)
DHSA_19 <- merge(DHSA_19,b[,c("HSAID","pro")],by="HSAID")
names(DHSA_19)[16] <- "bw_Pro"

#Share private hospitals
jg <-data_19[!duplicated(data_19$YLJGID),]
jg_dart <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\jg_dhsa.csv")
jg <- merge(jg,jg_dart[,c("YLJGID","HSAID")],by="YLJGID")
names(jg)[98] <- "HSAID_jg"
length(unique(jg_hsa$HSAID_jg))
names(jg)
jg_hsa <- merge(jg,aggregate(count~HSAID_jg,data = jg,sum),by="HSAID_jg")
names(jg_hsa)[99] <- "Sumhos"
names(jg_hsa)
jg_hsa <- jg_hsa[,c(1:83,85:86,88:99)]
jg_hsa <- merge(jg_hsa,aggregate(gsl~HSAID_jg,data = jg_hsa,sum),by="HSAID_jg")
names(jg_hsa)[98] <- "hsa_gsl"
jg_hsa$gsl_pro <- jg_hsa$hsa_gsl/jg_hsa$Sumhos*100
jg_hsa <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
DHSA_19 <- merge(DHSA_19,jg_hsa[,c("HSAID_jg","gsl_pro")],by.x = "HSAID",by.y = "HSAID_jg")

#Share tertiary hospitals
jg_hsa$sanji <- ifelse(jg_hsa$YYDJ_J==3,1,0)
names(jg)
jg_hsa <- merge(jg_hsa,aggregate(sanji~HSAID_jg,data = jg_hsa[,c("sanji","HSAID_jg")],sum),by="HSAID_jg")
names(jg_hsa)[101] <- "hsa_sanji"
jg_hsa$sanji_pro <- jg_hsa$hsa_sanji/jg_hsa$Sumhos*100
jg <- jg_hsa[!duplicated(jg_hsa$HSAID_jg),]
names(jg)
DHSA_19 <- merge(DHSA_19,jg[,c("HSAID_jg","sanji_pro")],by.x ="HSAID",by.y = "HSAID_jg")

#Herfindahl-Hirschman index within HSAs
library(data.table)
setwd("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据")
jg_hsa_19 <- jg_hsa[,c("HSAID_jg","YLJGID")]
jg_dart <- read.csv("G:\\曹裴娅\\毕业论文\\HSA划分\\结果\\jg_dhsa.csv")
names(jg_dart)
jg_hsa_19 <- merge(jg_hsa_19,jg_dart[,c("YLJGID","X2019_bed_1.x")],by="YLJGID")
jg_hsa_19 <-merge(jg_hsa_19,aggregate(X2019_bed_1.x ~ HSAID_jg,data = jg_hsa_19,sum),by="HSAID_jg")
names(jg_hsa_19)[4] <- "hsa_bed"
jg_hsa_19$share <- jg_hsa_19$X2019_bed_1.x.x/jg_hsa_19$hsa_bed 
jg_hsa_19$shares2 <- (jg_hsa_19$share)^2 
jg_hsa_19 <- merge(jg_hsa_19,aggregate(shares2~HSAID_jg,data = jg_hsa_19,sum),by="HSAID_jg") 
names(jg_hsa_19)[7] <- "HHI"
jg_hsa_19$HHI_new <- jg_hsa_19$HHI*10000
hsa_HHI <- jg_hsa_19[!duplicated(jg_hsa_19$HSAID_jg),]
fwrite(jg_hsa_19,"jg_hsa_19.csv",row.names = F)
DHSA_19 <- merge(DHSA_19,hsa_HHI[,c("HSAID_jg","HHI_new")],by.x = "HSAID",by.y = "HSAID_jg")
write.csv(DHSA_19,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_19.csv", row.names = F)
#
DHSA_17 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_17.csv")
DHSA_18 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_18.csv")
DHSA_19 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA_19.csv")

DHSA_17 <- merge(DHSA_17,hsa_17[,c("HSAID_P","yzfy_perpat_ad")],by.x = "HSAID",by.y = "HSAID_P")
DHSA_18 <- merge(DHSA_18,hsa_18[,c("HSAID_P","yzfy_perpat_ad")],by.x = "HSAID",by.y = "HSAID_P")
DHSA_19 <- merge(DHSA_19,hsa_19[,c("HSAID_P","yzfy_perpat_ad")],by.x = "HSAID",by.y = "HSAID_P")
names(DHSA_17)
names(DHSA_18)
names(DHSA_19)

fwrite(DHSA171819,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA171819_new.csv",row.names = F)

DHSA171819 <- read.csv("G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\数据\\DHSA171819_new.csv")
DHSA171819$Year <- as.factor(DHSA171819$Year)
hist(log(DHSA171819$yzfy_perpat_ad),breaks = 20)
hist(log(DHSA171819$zfy_perpat),breaks = 20)
hist(log(DHSA171819$PerGDP),breaks = 20)
install.packages('tidyverse') 
install.packages('modelsummary') 
library(dplyr)
library(tidyverse) 
library(modelsummary) 

#robust check ols at HSA level
fit1 <- lm(log(zfy_perpat)~Elder_Pro+Female_Pro+jrate+bw_Pro+CCI_average+deathr+birthr+unemploy_Pro+log(PerGDP)+urbanr+Perbed+gsl_pro+sanji_pro+log(HHI_new)+insurancepay_Pro+Year,data = DHSA171819)
# model with robust standard errors
msummary(list(fit1),
         vcov=c("robust"),
         stars=c('*' = .1, '**' = .05, '***' = .01))
summary(fit1)
confint(fit1)

#Shapley vale decomposition 
names(DHSA171819)
fit <- as.data.frame(DHSA171819[,c(24,11,7,15,9,20:21,8,2,19,4,16:18,14,23)])
names(fit)
names(DHSA171819)
#
library(MASS)
install.packages("ShapleyValue")
library(ShapleyValue)
library(tidyverse)
y <- log(DHSA171819$zfy_perpat)
names(DHSA171819)
DHSA171819$GDP_new <- log(DHSA171819$PerGDP)
DHSA171819$HHI_new2 <- log(DHSA171819$HHI_new)
names(DHSA171819)
x <- as.data.frame(DHSA171819[,c(24,11,7,15,9,20:21,8,25,19,4,16:17,26,14,23)])
x$Year <- as.factor(x$Year)
value <- shapleyvalue(y,x)
library(data.table)
fwrite(value,"G:\\曹裴娅\\毕业论文\\HSA费用地区差异\\结果\\表\\shapleyvalue_151.csv",row.names=F)