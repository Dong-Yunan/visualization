setwd("D:/DataScience/Quant/fund_1")

#-----------------------------------------------------------------------
library(readxl)
library(data.table)
library(stringr)

rawdas<-dir('./demoData')#%>%paste0(rawdas)
rawdas=paste0('./demoData/',rawdas)

read_rawdas<-function(rda)
{
  #   rda=paste0('./demoData/',rawdas[5])
  #rda=rawdas[1]
  df<-read_xlsx(rda) %>%data.table()
  # "日期"  "开盘价(???)"   "最高价(???)"   "最低价(???)"   "收盘???(???)"   "成交???(百万)" "成交???(???)" 
  names(df)<-c('date','opening_price','top_price','bottom_price','closing_price','amount_m','deals')
  
  #df[,`:=`(date=as.IDate(date)),]
  df<-df[date>='2017-10-19' & date<'2017-12-25' ,,]
  
  df[["code"]]<-rda
  df %>% data.table()
}
 
# read_rawdas(rda=rawdas[1])
rawdata<-rbindlist(lapply(rawdas,read_rawdas))
# rawdata[,.N,by=list(code)]
 da_df<-rawdata[,list(date,closing_price,code),] 
 setnames(da_df,"closing_price","price")
 da_df$code<-str_replace( da_df$code,"./demoData/","")%>%str_replace(".xlsx","")
 da_df<-da_df[!is.na(date),,]
 da_df_1<-dcast(da_df[!is.na(date),],date~code,value.var="price")
 #da_df_1<-replace_na(da_df_1,list(`040008`=0,`070032`=0,`110022`=0,`260108`=0,`340007`=0))
 
 #
 write.csv(da_df,"da_df.csv")
 write.csv(da_df_1,"da_df_1.csv")
 
#--------------------------------------------------------

 
 library(xts)  
 library(quantmod) 
 library(PerformanceAnalytics) 
 
 
 fund_codes<-c('040008','070032','110022','260108','340007','hs300') 
 cmp_ret<-function(idx)
 { #idx<-fund_codes[1]
   da_df_i<-da_df[code==idx,list(date,price),]
   df=xts(da_df_i$price,order.by=da_df_i$date)
   #df=df[-1,]  
   df=df[!is.na(df)]  
   df1=lag(df,1)#滞后一???  
   #df1=df1[-1,]  
   df1=df1[!is.na(df1)]  
   data=merge(df,df1) 
   #data1=merge(df,df1,by=c("date"))
   head(data)  
   arate=(df-df1)/df    # to define a kind of increment 
   ############this kind of definition
   rate=periodReturn(df,period="daily",type="log")  # type of returns: arithmetic (discrete) or log (continuous)
   # rate=periodReturn(df,period="daily",type="log") 
   names(rate)<-idx
   rate
 }
 
 rets<-lapply(fund_codes,cmp_ret)
 rets[[1]]
 
 # 0-risk benifit
 RF<-0.035/252
 # Sharpe 
 dat_ret=merge(rets[[1]],rets[[2]],rets[[3]],rets[[4]],rets[[5]],rets[[6]])
 head(dat_ret)
 #results<-table.AnnualizedReturns(dat_ret,Rf=Rf, scale = 4)
 #  number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4)
 #results
 
 #------------------------------------------------------
 
 #  
 #预期收益
 Erev<-mean(dat_ret[,c('X040008'),])
 Erev
 # 标准差 无偏估计
 fsd<-sd(dat_ret[,c('X040008'),])
 fsd
 #无风险收益
 RF<-0.035/252
 #夏普比率
 sharpe_v<-(Erev-RF)/fsd 
 sharpe_v
 
 #summary(dat_ret[,c('X040008'),])
 
  #------------------------------------------------------
 
 SharpeRatio(dat_ret,Rf=RF)
 #----------------------------------------------------------------------------------
 # the charts
 #chart.CumReturns(dat_ret,legend.loc='top')
 #chart.Correlation(dat_ret,legend.loc='top')
 #---------------------------------------------------------------------------------
 dat_ret[]
 
 #----------------------
 # how to choose the Rb,and how to set the cycle/period
 
 # 计算alpha
 alpha<-CAPM.alpha(dat_ret[1:46,1:5],dat_ret[1:46,6],Rf=RF);alpha
 #CAPM.alpha(Ra, Rb, Rf = 0)
 
 # 计算beta
 beta<-CAPM.beta(dat_ret[,1:5],dat_ret[,6],Rf=RF);beta
 #CAPM.beta(Ra, Rb, Rf = 0)
 
 
 