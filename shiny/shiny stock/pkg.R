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



#CAPM.beta.bear()
