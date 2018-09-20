setwd("D:/DataScience/Quant/fund_1")
source("fund_1.R")

#----------------------------------------

#1 to caculate/compute the rate of return
  #1.1  arithmetric mean of return-rate

fundcodes<-da_df[,.N,by=list(code)][,code]

  arithmetric_mr<-function(fndcode)
    {
      #fundcode=fundcodes[1]
      da_df[code==fundcode,,]
    }



  #1.2  geometric mean of return-rate 