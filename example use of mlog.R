mlog_init()


mlog_w(ml_list=lm_1A)

# First train the model using a customized function. And record many informations to a log file. 

lm_1A=boston_clean1%>%ML(method="lm",preProcess=c("zv","nzv"))%>%mlog_w() 

mlog_r()
