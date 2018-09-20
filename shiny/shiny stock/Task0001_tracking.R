options(scipen=200)

setwd("D:/DataScience/Quant/Task_0001_R")
#-----------------------------------------------
#library(tidyverse)
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
#-----------------------------------------------
#The Global Variables
Rf<-0.035/252
DateStart<-as.Date('2017-07-19')
DateEnd<-as.Date('2017-10-24')
#-------------------------------------------------
datafiles<-str_c('./demoData',"/"
                 , dir("./demoData")[dir("./demoData")%>%str_detect(".csv")])
fund_codes<-datafiles%>% str_replace('\\.\\/demoData\\/',"")%>% str_replace(".csv","")
 

#incrs_method<-c("discrete", "log")
# default is the log

IncreaseRate<-function(rawdata,date_start,date_end,c_method)
  {
   # rawdata<-datafiles[1] #t
   # date_start<-DateStart #t
   # date_end<-DateEnd #t
   # c_method= c("discrete", "log")[2]    #"discrete"
    df<-read.csv(rawdata,stringsAsFactors=FALSE)
    names(df)<-c('date','open','high','low','close','_amount_','_count_')
    code<-(rawdata %>% str_replace('\\.\\/demoData\\/',"")%>% str_replace(".csv",""))
    df[["date"]]<-as.Date(df$date)
    df<-df[which(df$date>=date_start & df$date<(date_end+1)),c("date","close"),]
    df<-xts(x = df[-1], order.by = df$date)
    da<-Return.calculate(df,method =c_method )
    da[,1][1]<-0
    list(code,da)
   }


IncreaseRate_period<-function(rawdata)
{
  # rawdata<-datafiles[2] #t
  IncreaseRate(rawdata,date_start=DateStart,date_end=DateEnd,c_method= c("discrete", "log")[2] )
}

Fund_IncreaseRate<-lapply(datafiles,IncreaseRate_period) 



IncreaseRate_period_discrete<-function(rawdata)
{
  # rawdata<-datafiles[2] #t
  IncreaseRate(rawdata,date_start=DateStart,date_end=DateEnd,c_method= c("discrete", "log")[1] )
}

Fund_IncreaseRate_discrete<-lapply(datafiles,IncreaseRate_period_discrete) 

##############

fundnum<-c(1:length(Fund_IncreaseRate))

 theIndicators<-function(fund_num,thedata,rf){
   #fund_num=fundnum[1] #t
   # thedata=Fund_IncreaseRate#t
   # rf=Rf #t
   
   # for the range,increasement and decreasement
   period_begin<-thedata[[fund_num]][[2]][2]
   period_end<-thedata[[fund_num]][[2]][length( thedata[[fund_num]][[2]])]
  digits=8
  data.frame(code=thedata[[fund_num]][[1]],
             #alpha
             CAPM_alpha=CAPM.alpha(thedata[[fund_num]][[2]],thedata[[6]][[2]],Rf=rf) %>%round(digits),
             #beta 
             CAPM_beta=CAPM.beta(thedata[[fund_num]][[2]],thedata[[6]][[2]],Rf=rf)%>%round(digits),
             #sharpe ratio
             SharpeRatio=SharpeRatio(thedata[[fund_num]][[2]],Rf=rf)[1]%>%round(digits),
             # maxization of drawndown
             maxDrawdown=maxDrawdown(thedata[[fund_num]][[2]])%>%round(digits) %>% sca::percent(2,"")  ,
             variationRange=((as.numeric(period_end)-as.numeric(period_begin))/as.numeric(period_begin))%>%round(digits)%>% sca::percent(2,"") #?ǵ????ȼ???
             
             )
 
   
 }

 theIndicators_da<-function(fund_num)
 {
   # fund_num=fundnum[1] #t
     theIndicators(fund_num,thedata=Fund_IncreaseRate,rf=Rf)
 }
library(dplyr)
results<-lapply(fundnum,theIndicators_da) %>%bind_rows()   # warning


ktb<-knitr::kable(results)


#######################################################
# plot

rateTrend<-function(fund_num,thedata)
  {
   # fund_num=fundnum[1] #t
   # thedata=Fund_IncreaseRate_discrete#t
    data.frame(
    date=index(thedata[[fund_num]][[2]]),  # extract the date from xts obj
         close=thedata[[fund_num]][[2]][,1],
    code=paste0("code_",thedata[[fund_num]][[1]])
    )
   
  }

rateTrend_da<-function(fund_num)
  {
  # fund_num=fundnum[1] #t
    rateTrend(fund_num,thedata=Fund_IncreaseRate_discrete)
  }


rate_trends<-lapply(fundnum,rateTrend_da) %>% bind_rows()%>% #WARNING
  reshape2::dcast(date~code,value.var=c('close')) 
                 

library(plotly)


p <- plot_ly(rate_trends, x = ~date, y = ~code_040008, name = 'code_040008', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~code_070032, name = 'code_070032', mode = 'lines+markers') %>%
  add_trace(y = ~code_110022, name = 'code_110022', mode = 'lines+markers') %>%
  add_trace(y = ~code_260108, name = 'code_260108', mode = 'lines+markers') %>%
  add_trace(y = ~code_340007, name = 'code_340007', mode = 'lines+markers') %>%
  add_trace(y = ~code_hs300, name = 'code_hs300', mode = 'lines+markers') %>%
  layout(title = "Rate,Increasement&Decreasement",
         xaxis = list(title = 'date',
                      zeroline = TRUE                     ),
         yaxis = list(title = 'rate'                     ))

#p












