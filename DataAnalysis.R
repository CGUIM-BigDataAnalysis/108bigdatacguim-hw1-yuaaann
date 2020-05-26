library(jsonlite)
library(dplyr)
library(knitr)

library(readr)
Salary104 <- read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/")
View(Salary104)

Salary107<- read_csv("~/Downloads/A17000000J-020066-Qod/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
View(Salary107)

Salary104$大職業別<-NULL
Salary104$大職業別<-Salary107$大職業別
Salary104107<-inner_join(Salary104,Salary107,by=c("大職業別"))
View(Salary104107)
Salary104107$`大學-薪資.x`<-as.numeric(Salary104107$`大學-薪資.x`)
Salary104107$`大學-薪資.y`<-as.numeric(Salary104107$`大學-薪資.y`)
#1-1
Salary_up<-filter(Salary104107,`大學-薪資.x`<`大學-薪資.y`)
View(Salary_up) 
#1-2
Salary_up$`107年度大學畢業薪資 / 104年度大學畢業薪資`<-Salary_up$`大學-薪資.y`/ Salary_up$`大學-薪資.x`
Salary_up_top10<-head(Salary_up[order(Salary_up$`107年度大學畢業薪資 / 104年度大學畢業薪資`,decreasing = T),],10)
View(Salary_up_top10)  

#1-3
Salary_up_5percent<-filter(Salary_up,`107年度大學畢業薪資 / 104年度大學畢業薪資`>1.05)
View(Salary_up_5percent)  
Salary_up_5percent_kind<-table(sapply(strsplit(Salary_up_5percent$大職業別,"-"),"[",1))
View(Salary_up_5percent_kind)  
#2
#男
Salary104$`大學-女/男`<-as.numeric(Salary104$`大學-女/男`)
s104_male_higher<-filter(Salary104,`大學-女/男`<100)
View(s104_male_higher) 
s104_male_higher<-s104_male_higher[order(s104_male_higher$`大學-女/男`),]
s104_male_higher_top10<-head(s104_male_higher,10)
View(s104_male_higher_top10)

Salary107$`大學-女/男`<-as.numeric(Salary107$`大學-女/男`)
s107_male_higher<-filter(Salary107,`大學-女/男`<100)
View(s107_male_higher) 
s107_male_higher<-s107_male_higher[order(s107_male_higher$`大學-女/男`),]
s107_male_higher_top10<-head(s107_male_higher,10)
View(s107_male_higher_top10)

#女
s104_female_higher<-filter(Salary104,`大學-女/男`>100)
View(s104_female_higher) 
s104_female_higher<-s104_female_higher[order(s104_female_higher$`大學-女/男`),]
s104_female_higher_top10<-head(s104_female_higher,10)
View(s104_female_higher_top10)

s107_female_higher<-filter(Salary107,`大學-女/男`>100)
View(s107_female_higher) 
s107_female_higher<-s107_female_higher[order(s107_female_higher$`大學-女/男`),]
s107_female_higher_top10<-head(s107_female_higher,10)
View(s107_female_higher_top10)

#3
Salary107$`研究所-薪資`<-as.numeric(Salary107$`研究所-薪資`)
Salary107$`大學-薪資`<-as.numeric(Salary107$`大學-薪資`)
Salary107$`研究所-薪資/大學-薪資`<-Salary107$`研究所-薪資`/Salary107$`大學-薪資`
s107_gra_higher<-filter(Salary107,`研究所-薪資/大學-薪資`>1)
View(s107_gra_higher)
s107_gra_higher_top10<-head(s107_gra_higher[order(s107_gra_higher$`研究所-薪資/大學-薪資`,decreasing = T),],10)
View(s107_gra_higher_top10)

#4
JobLike<-Salary107[grep("工業-專業人員|藝術_娛樂及休閒服務業-專業人員",Salary107$大職業別),]
JobLike<-select(JobLike,大職業別,`大學-薪資`,`研究所-薪資`)
View(JobLike)
JobLike$"研究所薪資與大學薪資差"<-JobLike$`研究所-薪資`-JobLike$`大學-薪資`
View(JobLike)
