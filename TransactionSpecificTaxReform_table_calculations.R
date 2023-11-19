############## README ##############
# script to compute values of tables in
# "Transaction-Specific Tax Reform in Three Steps:
#  The Case of Constructive Ownership"
# by Thomas J. Brennan and David M. Schizer
# Columbia Journal of Tax Law, 2023
#
# The below commands generate the data for the tables in
# the paper.  Data frames containing the data are named
# dftab1, dftab2, etc.
#
# Calculations use the derivmkts library.
#
# The variables tab1fmt, tab2fmt, etc., put the data frames
# in kable/markdown format using the knitr library.
#
# The final lines of the file print the values of each of
# tab1fmt, tab2fmt, etc.
####################################

# load derivmkts library
library(derivmkts)

# load knitr library
library(knitr)

# Table 1
s=100; k=70; r=.05; v=c(.2,.6); d=0; tt=5;
ans1p=greeks2('bsput',list(s=s,k=k,r=r,v=v,d=d,tt=tt))
ans1=ans1p['Premium',]
dftab1 = data.frame(rbind(ans1))
colnames(dftab1) = paste(v*100,'%',sep='')
row.names(dftab1) = c('value')

# Table 2
s=100; kp1=95; kp2=100; kc1=100; kc2=115; r=.05; v=.2; d=0; tt=5;
ans2c1=greeks2('bscall',list(s=s,k=kc1,r=r,v=v,d=d,tt=tt))
ans2c2=greeks2('bscall',list(s=s,k=kc2,r=r,v=v,d=d,tt=tt))
ans2p1=greeks2('bsput',list(s=s,k=kp1,r=r,v=v,d=d,tt=tt))
ans2p2=greeks2('bsput',list(s=s,k=kp2,r=r,v=v,d=d,tt=tt))
ans2 = list(callsprd=ans2c1['Premium',]-ans2c2['Premium',],
            putsprd=ans2p1['Premium',]-ans2p2['Premium',])
ans2 = c(ans2,list(sum=ans2$callsprd+ans2$putsprd,
                   diffcontract=ans2$callsprd-ans2$putsprd))
dftab2 = data.frame(as.numeric(ans2))
colnames(dftab2) = c('value')
row.names(dftab2) = c('Call Spread','Put Spread','Sum',
                      'Difference Contract')

# Table 2 footnote
s=100; kp1=56; kp2=100; kc1=100; kc2=115; r=.05; v=.2; d=0; tt=5;
ans2fnc1=greeks2('bscall',list(s=s,k=kc1,r=r,v=v,d=d,tt=tt))
ans2fnc2=greeks2('bscall',list(s=s,k=kc2,r=r,v=v,d=d,tt=tt))
ans2fnp1=greeks2('bsput',list(s=s,k=kp1,r=r,v=v,d=d,tt=tt))
ans2fnp2=greeks2('bsput',list(s=s,k=kp2,r=r,v=v,d=d,tt=tt))
ans2fn = list(callsprd=ans2fnc1['Premium',]-ans2fnc2['Premium',],
            putsprd=ans2fnp1['Premium',]-ans2fnp2['Premium',])
ans2fn = c(ans2fn,list(sum=ans2fn$callsprd+ans2fn$putsprd,
                   diffcontract=ans2fn$callsprd-ans2fn$putsprd))
dftab2fn = data.frame(as.numeric(ans2fn))
colnames(dftab2fn) = c('value')
row.names(dftab2fn) = c('Call Spread','Put Spread','Sum',
                        'Difference Contract')

# Table 3
s=100; k=70; r=.05; v=c(.2,.6); d=0; tt=5;
ans3c=greeks2('bscall',list(s=s,k=k,r=r,v=v,d=d,tt=tt))
ans3=ans3c['Delta',]
dftab3 = data.frame(rbind(ans3))
colnames(dftab3) = paste(v*100,'%',sep='')
row.names(dftab3) = c('delta')

# Table 4
s=100; k=70; r=.05; v=c(.2,.6); d=0; tt=5; H=98;
ans4c=greeks2('dicall',list(s=s,k=k,r=r,v=v,d=d,tt=tt,H=H))
ans4=ans4c['Delta',]
dftab4 = data.frame(rbind(ans4))
colnames(dftab4) = paste(v*100,'%',sep='')
row.names(dftab4) = c('delta')

# Table 5
s=100; k=70; r=.05; v=c(.2,.6); d=0; tt=5; H=98;
ans5put=greeks2('bsput',list(s=s,k=k,r=r,v=v,d=d,tt=tt))
ans5diput=greeks2('diput',list(s=s,k=k,r=r,v=v,d=d,tt=tt,H=H))
ans5doasset=greeks2('docall',list(s=s,k=0,r=r,v=v,d=d,tt=tt,H=H))
ans5row1=ans5put['Premium',]/s*100
ans5row2=(ans5diput['Premium',]+ans5doasset['Premium',])/s*100
dftab5 = data.frame(rbind(ans5row1,ans5row2))
colnames(dftab5) = paste(v*100,'%',sep='')
row.names(dftab5) = c('diff pct for noncontingent call','diff pct for knock-in call')

# Table 6
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12;
ans6c=greeks2('bscall',list(s=s,k=k,r=r,v=v,d=d,tt=tt))
ans6=ans6c['Delta',]
dftab6 = data.frame(rbind(ans6))
colnames(dftab6) = paste(v*100,'%',sep='')
row.names(dftab6) = c('delta')

# Table 7
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=c(12,1);
ans7crow1=greeks2('bscall',list(s=s,k=k,r=r,v=v,d=d,tt=tt[1]))
ans7crow2=greeks2('bscall',list(s=s,k=k,r=r,v=v,d=d,tt=tt[2]))
ans7row1=ans7crow1['Delta',]
ans7row2=ans7crow2['Delta',]
dftab7 = data.frame(rbind(ans7row1,ans7row2))
colnames(dftab7) = paste(v*100,'%',sep='')
row.names(dftab7) = paste(tt,'years',sep=' ')

# Table 8
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ans8c=greeks2('docall',list(s=s,k=k,r=r,v=v,d=d,tt=tt,H=H))
ans8=ans8c['Delta',]
dftab8 = data.frame(rbind(ans8))
colnames(dftab8) = paste(v*100,'%',sep='')
row.names(dftab8) = c('delta')

# Table 9
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ans9dr=greeks2('drdeferred',list(s=s,r=r,v=v,d=d,tt=tt,H=H))
ans9=ans9dr['Premium',]*exp(tt*r)
dftab9 = data.frame(rbind(ans9))
colnames(dftab9) = paste(v*100,'%',sep='')
row.names(dftab9) = c('probability')

# Table 10
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12;
ans10p=greeks2('bsput',list(s=s,r=r,k=k,v=v,d=d,tt=tt))
ans10=ans10p['Premium',]
dftab10 = data.frame(rbind(ans10))
colnames(dftab10) = paste(v*100,'%',sep='')
row.names(dftab10) = c('value of diff contract')

# Table 11 (new line; for old line see table 10)
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=c(1,12);
ans11prow1=greeks2('bsput',list(s=s,r=r,k=k,v=v,d=d,tt=tt[1]))
ans11prow2=greeks2('bsput',list(s=s,r=r,k=k,v=v,d=d,tt=tt[2]))
ans11row1=ans11prow1['Premium',]
ans11row2=ans11prow2['Premium',]
dftab11 = data.frame(rbind(ans11row1,ans11row2))
colnames(dftab11) = paste(v*100,'%',sep='')
row.names(dftab11) = paste(tt,'year',sep=' ')

# Table 12
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ttshort=c(1,5)/252
ans12row1=c(0,0)
ans12prow2=greeks2('bsput',list(s=H,k=k,r=r,v=v,d=d,tt=ttshort[1]))
ans12prow3=greeks2('bsput',list(s=H,k=k,r=r,v=v,d=d,tt=ttshort[2]))
ans12hitprice=greeks2('dr',list(s=s,r=r,v=v,d=d,tt=tt,H=H))
ans12row2=ans12prow2['Premium',]*ans12hitprice['Premium',]/10*100
ans12row3=ans12prow3['Premium',]*ans12hitprice['Premium',]/10*100
dftab12 = data.frame(rbind(ans12row1,ans12row2,ans12row3))
colnames(dftab12) = paste(v*100,'%',sep='')
row.names(dftab12) = paste(c(0,ttshort)*252,'day',sep=' ')

# Table A1
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ansA1kicashrow1=k*greeks2('dr',list(s=s,r=r,v=v,d=d,tt=tt,H=H))
ansA1fincashrow2=k*greeks2('cashdocall',list(s=s,k=0,r=r,v=v,d=d,tt=tt,H=H))
ansA1row1=ansA1kicashrow1['Delta',]
ansA1row2=ansA1fincashrow2['Delta',]
ansA1row3=ansA1row1+ansA1row2
dftabA1 = data.frame(rbind(ansA1row1,ansA1row2,ansA1row3))
colnames(dftabA1) = paste(v*100,'%',sep='')
row.names(dftabA1) = c('knock-out cash','final cash','total cash')

# Table A2
ansA2row1=ansA1row3
ansA2row2=-ansA2row1
ansA2row3=ansA2row1+ansA2row2
dftabA2 = data.frame(rbind(ansA2row1,ansA2row2,ansA2row3))
colnames(dftabA2) = paste(v*100,'%',sep='')
row.names(dftabA2) = c('delta of final cash','delta of interest','total delta')

# Table A3
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ansA3kocrow1=greeks2('docall',list(s=s,k=k,r=r,v=v,d=d,tt=tt,H=H))
ansA3kirrow2=(H-k)*greeks2('dr',list(s=s,r=r,v=v,d=d,tt=tt,H=H))
ansA3row1=ansA3kocrow1['Delta',]
ansA3row2=ansA3kirrow2['Delta',]
ansA3row3=ansA3row1+ansA3row2
dftabA3 = data.frame(rbind(ansA3row1,ansA3row2,ansA3row3))
colnames(dftabA3) = paste(v*100,'%',sep='')
row.names(dftabA3) = c('delta of knock-out call','delta of knock-in rebate','delta of knock-out call and knock-in rebate')

# Table A4
ansA4row1=ansA3row1
ansA4row2=ansA3row2
ansA4row3=-ansA2row2
ansA4row4=ansA4row1+ansA4row2+ansA4row3
dftabA4 = data.frame(rbind(ansA4row1,ansA4row2,ansA4row3,ansA4row4))
colnames(dftabA4) = paste(v*100,'%',sep='')
row.names(dftabA4) = c('delta of knock-out call','delta of knock-in rebate','delta of interest','total delta')

# Table B1
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ansB1kicashrow1=k*greeks2('dr',list(s=s,r=r,v=v,d=d,tt=tt,H=H))
ansB1fincashrow2=k*greeks2('cashdocall',list(s=s,k=0,r=r,v=v,d=d,tt=tt,H=H))
ansB1row1=ansB1kicashrow1['Premium',]
ansB1row2=ansB1fincashrow2['Premium',]
ansB1row3=ansB1row1+ansB1row2
dftabB1 = data.frame(rbind(ansB1row1,ansB1row2,ansB1row3))
colnames(dftabB1) = paste(v*100,'%',sep='')
row.names(dftabB1) = c('knock-out cash','final cash','total cash')

# Table B2
ansB2row1=c(k,k)
ansB2row2=ansB1row3
ansB2row3=ansB2row1-ansB2row2
dftabB2 = data.frame(rbind(ansB2row1,ansB2row2,ansB2row3))
colnames(dftabB2) = paste(v*100,'%',sep='')
row.names(dftabB2) = c('price of contract','price of final cash','price of interest')

# Table B3
s=100; k=90; r=.05; v=c(.2,.6); d=0; tt=12; H=97;
ansB3kocrow1=greeks2('docall',list(s=s,k=k,r=r,v=v,d=d,tt=tt,H=H))
ansB3kirrow2=(H-k)*greeks2('dr',list(s=s,r=r,v=v,d=d,tt=tt,H=H))
ansB3row1=ansB3kocrow1['Premium',]
ansB3row2=ansB3kirrow2['Premium',]
ansB3row3=ansB3row1+ansB3row2
dftabB3 = data.frame(rbind(ansB3row1,ansB3row2,ansB3row3))
colnames(dftabB3) = paste(v*100,'%',sep='')
row.names(dftabB3) = c('price of knock-out call','price of knock-in rebate','price of knock-out call and knock-in rebate')

# Table B4
ansB4row1=ansB3row1
ansB4row2=ansB3row2
ansB4row3=-ansB2row3
ansB4row4=ansB4row1+ansB4row2+ansB4row3
dftabB4 = data.frame(rbind(ansB4row1,ansB4row2,ansB4row3,ansB4row4))
colnames(dftabB4) = paste(v*100,'%',sep='')
row.names(dftabB4) = c('price of knock-out call','price of rebate','price of interest','total contract price')

# Table B5
ansB5row1=2*ansB2row3
ansB5row2=c(0,0)
dftabB5 = data.frame(rbind(ansB5row1,ansB5row2))
colnames(dftabB5) = paste(v*100,'%',sep='')
row.names(dftabB5) = c('diff if underlying has up-front interest','diff if underlying has interest over time')

# Put the results in kable/markdown format using knitr
tab1fmt  =knitr::kable(dftab1,  format='markdown',caption='Table 1',digits=2)
tab2fmt  =knitr::kable(dftab2,  format='markdown',caption='Table 2', digits=2)
tab2fnfmt=knitr::kable(dftab2fn,format='markdown',caption='Table 2 Footnote',digits=2)
tab3fmt  =knitr::kable(dftab3,  format='markdown',caption='Table 3', digits=3)
tab4fmt  =knitr::kable(dftab4,  format='markdown',caption='Table 4', digits=3)
tab5fmt  =knitr::kable(dftab5,  format='markdown',caption='Table 5', digits=2)
tab6fmt  =knitr::kable(dftab6,  format='markdown',caption='Table 6', digits=3)
tab7fmt  =knitr::kable(dftab7,  format='markdown',caption='Table 7', digits=3)
tab8fmt  =knitr::kable(dftab8,  format='markdown',caption='Table 8', digits=2)
tab9fmt  =knitr::kable(dftab9,  format='markdown',caption='Table 9', digits=4)
tab10fmt =knitr::kable(dftab10, format='markdown',caption='Table 10',digits=2)
tab11fmt =knitr::kable(dftab11, format='markdown',caption='Table 11',digits=2)
tab12fmt =knitr::kable(dftab12, format='markdown',caption='Table 12',digits=2)
tabA1fmt =knitr::kable(dftabA1, format='markdown',caption='Table A1',digits=2)
tabA2fmt =knitr::kable(dftabA2, format='markdown',caption='Table A2',digits=2)
tabA3fmt =knitr::kable(dftabA3, format='markdown',caption='Table A3',digits=2)
tabA4fmt =knitr::kable(dftabA4, format='markdown',caption='Table A4',digits=2)
tabB1fmt =knitr::kable(dftabB1, format='markdown',caption='Table B1',digits=2)
tabB2fmt =knitr::kable(dftabB2, format='markdown',caption='Table B2',digits=2)
tabB3fmt =knitr::kable(dftabB3, format='markdown',caption='Table B3',digits=2)
tabB4fmt =knitr::kable(dftabB4, format='markdown',caption='Table B4',digits=2)
tabB5fmt =knitr::kable(dftabB5, format='markdown',caption='Table B5',digits=2)

# print the formatted tables
print(tab1fmt)
print(tab2fmt)
print(tab2fnfmt)
print(tab3fmt)
print(tab4fmt)
print(tab5fmt)
print(tab6fmt)
print(tab7fmt)
print(tab8fmt)
print(tab9fmt)
print(tab10fmt)
print(tab11fmt)
print(tab12fmt)
print(tabA1fmt)
print(tabA2fmt)
print(tabA3fmt)
print(tabA4fmt)
print(tabB1fmt)
print(tabB2fmt)
print(tabB3fmt)
print(tabB4fmt)
print(tabB5fmt)
