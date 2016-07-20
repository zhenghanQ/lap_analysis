##########################
###  LAP ERP ANALYSIS  ###
###  ZQ and SB         ###
###  July 2015         ###
##########################

# laperp.RDATA is the workspace

# let marginal be <.1
# local change made by zq
#  change by sb

##############################################
# repeated-measures ANOVA to define ERP ROIs

m1 = aov(amp~cond*laterality*anteriority+Error(subj/(cond*laterality*anteriority)),data=LAP_sem_eng_MA250500_anova)
summary(m1)
print(model.tables(m1,"means"),digits=4)  
ggplot() +
	geom_bar(aes(x = cond,y = amp),data=LAP_sem_eng_MA250500_anova,fun.data = mean_sdl,mult = 1,stat = 'summary') +
	facet_wrap(facets = ~laterality)+
	geom_errorbar(aes(y = amp,x = cond),data=LAP_sem_eng_MA250500_anova,size = 0.3,width = 0.2,fun.y = function(x) mean(x),fun.ymin = function(x) mean(x) - sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)) ,stat = 'summary')

m2 = aov(amp~cond*laterality*anteriority+Error(subj/(cond*laterality*anteriority)),data=LAP_syn_eng_MA250500_anova)
summary(m2)
print(model.tables(m2,"means"),digits=4)
ggplot() +
	geom_bar(aes(x = cond,y = amp),data=LAP_syn_eng_MA250500_anova,fun.data = mean_sdl,mult = 1,stat = 'summary') +
	facet_wrap(facets = ~anteriority)+
	geom_errorbar(aes(y = amp,x = cond),data=LAP_syn_eng_MA250500_anova,size = 0.3,width = 0.2,fun.y = function(x) mean(x),fun.ymin = function(x) mean(x) - sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)) ,stat = 'summary')

m3 = aov(amp~cond*laterality*anteriority+Error(subj/(cond*laterality*anteriority)),data=LAP_syn_eng_MA250500_anova)
summary(m3)
print(model.tables(m3,"means"),digits=4)
ggplot() +
	geom_bar(aes(x = cond,y = amp),data=LAP_syn_eng_MA250500_anova,fun.data = mean_sdl,mult = 1,stat = 'summary') +
	facet_wrap(facets = ~anteriority)+
	geom_errorbar(aes(y = amp,x = cond),data=LAP_syn_eng_MA250500_anova,size = 0.3,width = 0.2,fun.y = function(x) mean(x),fun.ymin = function(x) mean(x) - sd(x)/sqrt(length(x)),fun.ymax = function(x) mean(x) + sd(x)/sqrt(length(x)) ,stat = 'summary')


##########################################################################
# data = LAPERP_master.xslx ("LAPERP")

load("/Users/sarabeach/Dropbox (MIT)/ZQ_GABLAB/LAP/analysis/laperp.RDATA")

# independent variables = centralavg_bin7_250500, posterior5_bin8_250500, centralbackavg_bin8_5001000, RDI_bin7, RDI_bin8_a

ihist(var = LAPERP$centralavg_bin7_250500,binw = 1.0)      # roughly normal
ihist(var = LAPERP$posterior5_bin8_250500,binw = 1.0)      # roughly normal
ihist(var = LAPERP$centralbackavg_bin8_5001000,binw = 1.0) # roughly normal
ihist(var = LAPERP$RDI_bin7,binw = 1.0)                    # roughly normal
ihist(var = LAPERP$RDI_bin8_a,binw = 1.0)                  # roughly normal

# dependent variables = day1_voc, semantics_1, semantics_4, syntax_1, syntax_4

ihist(var = LAPERP$day1_voc,binw = 0.1)    # negative skew
ihist(var = LAPERP$semantics_1,binw = 0.1) # normal
ihist(var = LAPERP$semantics_4,binw = 0.1) # positive skew
ihist(var = LAPERP$syntax_1,binw = 0.1)    # positive skew
ihist(var = LAPERP$syntax_4,binw = 0.025)  # approximately trimodal

# covariates = KBIT_nonverbalstd, KBIT_verbalstd

ihist(var = LAPERP$KBIT_nonverbalstd,binw = 5.0) # negative skew
ihist(var = LAPERP$KBIT_verbalstd,binw = 5.0)    # normal-ish

# compute response dominance indices

LAPERP$RDI_bin7=(LAPERP$centralavg_bin7_250500+LAPERP$centralavg_bin7_5001000)/sqrt(2)

LAPERP$RDI_bin8=(LAPERP$posterior7_bin8_5001000+LAPERP$posterior7_bin8_250500)/sqrt(2) # old!

LAPERP$RDI_bin8_a=(LAPERP$centralbackavg_bin8_5001000+LAPERP$posterior5_bin8_250500)/sqrt(2) # new!


### spearman correlations
# note that when spearman>pearson, the correlation is monotonic but not linear

corr.mat<-cor.matrix(variables=d(centralavg_bin7_250500,posterior5_bin8_250500,centralbackavg_bin8_5001000),
	+ with.variables=d(day1_voc,semantics_1,syntax_1,semantics_4,syntax_4),
	+ data=LAPERP,
	+ test=cor.test,
	+ method='spearman',
	+ alternative="two.sided",
	+ exact=FALSE)
print(corr.mat)
rm('corr.mat')

corr.mat<-cor.matrix(variables=d(centralavg_bin7_250500,posterior5_bin8_250500,centralbackavg_bin8_5001000),
	+ with.variables=d(day1_voc,semantics_1,syntax_1,semantics_4,syntax_4),
	+ data=LAPERP,
	+ test=cor.test,
	+ method='pearson',
	+ alternative="two.sided")
print(corr.mat)
rm('corr.mat')

cor.test(LAPERP$KBIT_verbalstd,LAPERP$day1_voc,method="spearman",exact=FALSE)
cor.test(LAPERP$KBIT_verbalstd,LAPERP$vocab_ACC_4,method="spearman",exact=FALSE)

# partial correlations controlling for NVIQ

library(ppcor)

a1=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
a2=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
a3=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
a4=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
a5=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
a=rbind(a1,a2)
a=rbind(a,a3)
a=rbind(a,a4)
a=rbind(a,a5)

b1=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
b2=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
b3=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
b4=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
b5=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
b=rbind(b1,b2)
b=rbind(b,b3)
b=rbind(b,b4)
b=rbind(b,b5)

c1=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
c2=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
c3=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
c4=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
c5=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd")])
c=rbind(c1,c2)
c=rbind(c,c3)
c=rbind(c,c4)
c=rbind(c,c5)

d=rbind(a,b)
e=rbind(d,c) # combined stats for all 15 partial correlations

pvalue_half=e$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half,"fdr") # 1 and 9 survive. 2, 3, 4, 5, and 14 are marginal.

pvalue_full=e$p.value # for two-tailed correlation
p.adjust(pvalue_full,"fdr") # 9 survives. 1 is marginal.


# partial correlations controlling for NVIQ, Male, Age

a1=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
a2=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
a3=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
a4=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
a5=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
a=rbind(a1,a2)
a=rbind(a,a3)
a=rbind(a,a4)
a=rbind(a,a5)

b1=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
b2=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
b3=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
b4=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
b5=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
b=rbind(b1,b2)
b=rbind(b,b3)
b=rbind(b,b4)
b=rbind(b,b5)

c1=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
c2=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
c3=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
c4=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
c5=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
c=rbind(c1,c2)
c=rbind(c,c3)
c=rbind(c,c4)
c=rbind(c,c5)

d=rbind(a,b)
e=rbind(d,c) # combined stats for all 15 partial correlations

pvalue_half=e$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half,"fdr") # 1 and 9 survive. 2, 3, 4, 5, and 14 are marginal.

pvalue_full=e$p.value # for two-tailed correlation
p.adjust(pvalue_full,"fdr") # 9 survives. 1 is marginal.

# add the rest of the comparisons to complete the correlation matrix

p1=pcor.test(LAPERP$centralbackavg_bin8_5001000,LAPERP$posterior5_bin8_250500,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p2=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$posterior5_bin8_250500,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p3=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$centralbackavg_bin8_5001000,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p4=pcor.test(LAPERP$semantics_1,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p5=pcor.test(LAPERP$semantics_4,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p6=pcor.test(LAPERP$syntax_1,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p7=pcor.test(LAPERP$syntax_4,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p8=pcor.test(LAPERP$semantics_4,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p9=pcor.test(LAPERP$syntax_1,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p10=pcor.test(LAPERP$syntax_4,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p11=pcor.test(LAPERP$syntax_1,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p12=pcor.test(LAPERP$syntax_4,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p13=pcor.test(LAPERP$syntax_4,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
p=rbind(p1,p2)
p=rbind(p,p3)
p=rbind(p,p4)
p=rbind(p,p5)
p=rbind(p,p6)
p=rbind(p,p7)
p=rbind(p,p8)
p=rbind(p,p9)
p=rbind(p,p10)
p=rbind(p,p11)
p=rbind(p,p12)
p=rbind(p,p13)

r=rbind(e,p)

pvalue_half=r$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half,"fdr") # 1,2,4,9,16,19,20,22,23,28 survive.

pvalue_full=r$p.value # for two-tailed correlation
p.adjust(pvalue_full,"fdr") # 1,9,16,19,20,22 survive.


# partial correlations controlling for NVIQ and Day1Vocab
# leaving out P6 in favor of P4 because it did not predict learning

f1=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
f2=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
f3=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
f4=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
f=rbind(f1,f2)
f=rbind(f,f3)
f=rbind(f,f4)

g1=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
g2=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
g3=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
g4=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
g=rbind(g1,g2)
g=rbind(g,g3)
g=rbind(g,g4)

h=rbind(f,g) # combined stats for all 8 partial correlations

pvalue_half_=h$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_,"fdr") # 7 survives. 8 is marginal.

pvalue_full_=h$p.value # for two-tailed correlation
p.adjust(pvalue_full_,"fdr") # 7 survives.

# partial correlations controlling for NVIQ, Day1Vocab, Male, Age

f1=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
f2=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
f3=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
f4=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
f=rbind(f1,f2)
f=rbind(f,f3)
f=rbind(f,f4)

g1=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
g2=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
g3=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
g4=pcor.test(LAPERP$posterior5_bin8_250500,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
g=rbind(g1,g2)
g=rbind(g,g3)
g=rbind(g,g4)

h=rbind(f,g) # combined stats for all 8 partial correlations

pvalue_half_=h$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_,"fdr") # 7 survives. 8 is marginal.

pvalue_full_=h$p.value # for two-tailed correlation
p.adjust(pvalue_full_,"fdr") # 7 survives.

# add the rest of the comparisons to complete the correlation matrix

s1=pcor.test(LAPERP$centralavg_bin7_250500,LAPERP$posterior5_bin8_250500,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s2=pcor.test(LAPERP$semantics_4,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s3=pcor.test(LAPERP$syntax_1,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s4=pcor.test(LAPERP$syntax_4,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s5=pcor.test(LAPERP$syntax_1,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s6=pcor.test(LAPERP$syntax_4,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s7=pcor.test(LAPERP$syntax_4,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc","Male","Age")])
s=rbind(s1,s2)
s=rbind(s,s3)
s=rbind(s,s4)
s=rbind(s,s5)
s=rbind(s,s6)
s=rbind(s,s7)

t=rbind(h,s) 

pvalue_half_=t$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_,"fdr") #

pvalue_full_=t$p.value # for two-tailed correlation
p.adjust(pvalue_full_,"fdr") #



###############################################################################
### post-hoc correlations with RDIs and ERPs
# make sure that the new letter variable is a data frame, i.e. i=data.frame(i)
# make sure that the variable p.value is of type double

i1=cor.test(LAPERP$RDI_bin7,LAPERP$day1_voc,method="spearman",exact=FALSE)
i2=cor.test(LAPERP$RDI_bin7,LAPERP$semantics_1,method="spearman",exact=FALSE)
i3=cor.test(LAPERP$RDI_bin7,LAPERP$semantics_4,method="spearman",exact=FALSE)
i=rbind(i1,i2)
i=rbind(i,i3)
i=data.frame(i)

pvalue_half_rdi7=i$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_rdi7,"fdr") # 1 and 3 survive. 2 is marginal.

pvalue_full_rdi7=i$p.value # for two-tailed correlation
p.adjust(pvalue_full_rdi7,"fdr") # 1 and 3 are marginal.

j1=cor.test(LAPERP$RDI_bin8_a,LAPERP$syntax_1,method="spearman",exact=FALSE)
j2=cor.test(LAPERP$RDI_bin8_a,LAPERP$syntax_4,method="spearman",exact=FALSE)
j=rbind(j1,j2)
j=data.frame(j)

pvalue_half_rdi8=j$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_rdi8,"fdr") # 1 survives.

pvalue_full_rdi8=j$p.value # for two-tailed correlation
p.adjust(pvalue_full_rdi8,"fdr") # 1 is marginal.


# partial correlation controlling for NVIQ

k1=pcor.test(LAPERP$RDI_bin7,LAPERP$day1_voc,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
k2=pcor.test(LAPERP$RDI_bin7,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
k3=pcor.test(LAPERP$RDI_bin7,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
k=rbind(k1,k2)
k=rbind(k,k3)

pvalue_half_rdi7_=k$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_rdi7_,"fdr") # 1 and 3 survive. 2 is marginal.

pvalue_full_rdi7_=k$p.value # for two-tailed correlation
p.adjust(pvalue_full_rdi7_,"fdr") # 1 and 3 survive.

l1=pcor.test(LAPERP$RDI_bin8_a,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
l2=pcor.test(LAPERP$RDI_bin8_a,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","Male","Age")])
l=rbind(l1,l2)

pvalue_half_rdi8_=l$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_rdi8_,"fdr") # 1 survives. 2 is marginal.

pvalue_full_rdi8_=l$p.value # for two-tailed correlation
p.adjust(pvalue_full_rdi8_,"fdr") # 1 survives.


# partial correlation controlling for NVIQ and Day1Vocab

n1=pcor.test(LAPERP$RDI_bin7,LAPERP$semantics_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
n2=pcor.test(LAPERP$RDI_bin7,LAPERP$semantics_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
n=rbind(n1,n2)

pvalue_half_rdi7__=n$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_rdi7__,"fdr") # neither survives. 

pvalue_full_rdi7__=n$p.value # for two-tailed correlation
p.adjust(pvalue_full_rdi7__,"fdr") # neither survives.

o1=pcor.test(LAPERP$RDI_bin8_a,LAPERP$syntax_1,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
o2=pcor.test(LAPERP$RDI_bin8_a,LAPERP$syntax_4,method="spearman",LAPERP[,c("KBIT_nonverbalstd","day1_voc")])
o=rbind(o1,o2)

pvalue_half_rdi8__=o$p.value/2 # for one-tailed correlation
p.adjust(pvalue_half_rdi8__,"fdr") # both survive.

pvalue_full_rdi8__=o$p.value # for two-tailed correlation
p.adjust(pvalue_full_rdi8__,"fdr") # 1 survives. 2 is marginal.

###Plotting###
# Scatter plot of N400 and Day_1 vocabulary
ggplot() +
	geom_point(aes(x = centralavg_bin7_250500,y = day1_voc),data=LAPERP,size = 4.0) +
	theme_classic(base_size = 24.0) +
	xlab(label = 'N400 size (uV)') +
	ylab(label = 'MAL Vocabulary on Day1')

# Scatter plot of early p600 and Day 1 Syntax
ggplot() +
	geom_point(aes(x = posterior5_bin8_250500,y = syntax_1),data=LAPERP,size = 4.0) +
	theme_classic(base_size = 24.0) +
	xlab(label = 'Early P600 size (uV)') +
	ylab(label = 'MAL Syntax on Day1')

# Scatter plot of semantic RDI
ggplot() +
	geom_point(aes(x = centralavg_bin7_250500,y = centralavg_bin7_5001000),data=LAPERP,size = 4.0) +
	theme_classic(base_size = 24.0) +
	xlab(label = 'N400 size (uV)') +
	ylab(label = 'Semantic P600 Size (uV)') +
	geom_hline(data=LAPERP,linetype = 2,yintercept = 0.0) +
	geom_vline(data=LAPERP,linetype = 2,xintercept = 0.0)
# Scatter plot of syntactic RDI
ggplot() +
	geom_point(aes(x = posterior7_bin8_250500,y = posterior7_bin8_5001000),data=LAPERP,size = 4.0) +
	theme_classic(base_size = 24.0) +
	xlab(label = 'Syntactic N400 size (uV)') +
	ylab(label = 'P600 Size (uV)') +
	geom_hline(data=LAPERP,linetype = 2,yintercept = 0.0) +
	geom_vline(data=LAPERP,linetype = 2,xintercept = 0.0)

# Scatter plot of cross-validated predicted values and real values
pred=subset(LAPERP,select=c(1,3,20,21,22,27,28,29,34,35,98))
colnames(pred)[2]="n400"
colnames(pred)[3]="p400"

library(DAAG)
m1<-CVlm(df=pred,m=10,form.lm=formula(syntax_1~p400),seed=25,plotit="Residual")
#0.00646
sqrt(mean((m1$syntax_1-m1$cvpred)^2))
#0.0804
cor.test(m1$syntax_1,m1$cvpred,method="pearson")
#Pearson's product-moment correlation
#
#data:  m1$syntax_1 and m1$cvpred
#t = 2.1, df = 36, p-value = 0.04324
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.0112 0.5875
#sample estimates:
# cor 
#0.33 
ggplot() +
	geom_point(aes(x = syntax_1,y = cvpred,colour = p400),data=m1,size = 3.0) +
	theme_classic(base_size = 24.0) +
	geom_line(aes(x = syntax_1,y = syntax_1),linetype=4,data=m1)+
	xlab(label = 'Syntax Accuracy on Day 1') +
	ylab(label = 'Predicted Accuracy')

# add day1_voc as a predictor
m1<-CVlm(df=pred,m=10,form.lm=formula(syntax_1~p400+day1_voc),seed=25,plotit="Residual")
#0.00566  
sqrt(mean((m1$syntax_1-m1$cvpred)^2))
#0.0752
cor.test(m1$syntax_1,m1$cvpred,method="pearson")
#Pearson's product-moment correlation
#
#data:  m1$syntax_1 and m1$cvpred
#t = 3.19, df = 36, p-value = 0.002932
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.176 0.686
#sample estimates:
# cor 
#0.47 
ggplot() +
	geom_point(aes(x = syntax_1,y = cvpred,colour = p400),data=m1,size = 3.0) +
	theme_classic(base_size = 24.0) +
	geom_line(aes(x = syntax_1,y = syntax_1),linetype=4,data=m1)+
	xlab(label = 'Syntax Accuracy on Day 1') +
	ylab(label = 'Predicted Accuracy')

#non-parametric regression, cross-validated
model = glm(day1_voc~n400,pred,family=gaussian) 
summary(model)
val.10.fold<-cv.glm(data=pred,glmfit=model,K=10)
val.10.fold$delta
#0.0392
muhat=fitted(model)
m1=cbind(m1,muhat)
sqrt(mean((m1$day1_voc-m1$muhat)^2))
#0.185
cor.test(m1$day1_voc,m1$muhat,method="spearman")
#Spearman's rank correlation rho
#
#data:  m1$day1_voc and m1$muhat
#S = 5377, p-value = 0.01023
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.412 
#

ggplot() +
	geom_point(aes(x = day1_voc,y = muhat,colour = n400),data=m1,size = 3.0) +
	theme_classic(base_size = 24.0) +
	geom_line(aes(x = day1_voc,y = day1_voc),linetype=4,data=m1)+
	xlab(label = 'Vocabulary on Day 1') +
	ylab(label = 'Predicted Vocabulary')
