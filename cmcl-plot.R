setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Umich/Psych619/wd/CMCL2021")
library(ggplot2)


stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}

########################################################################
#        VL2003 (singular)
########################################################################


########################################################################
#        W2009 EXP2_3 (singular)
########################################################################

W2009_3<-read.csv('results/W2009_exp3.csv', sep=',',header=TRUE)
W2009_3_sing <-W2009_3[which(W2009_3$subject=="sing"),]
W2009_3_sing[which(W2009_3_sing$interference=='unint'),]$interference = 'non-int'

######## SURPRISAL
ggplot(aes(x = interference, y = surprisal), data = W2009_3_sing) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

######## ENTROPY
ggplot(aes(x = interference, y = entropy), data = W2009_3_sing) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue") +ylab('entropy_head43')

######## ATTENTION_TO_TARGET

ggplot(aes(x = interference, y = attn_to_target), data = W2009_3_sing) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=37) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

######## Stat

W2009_3_sing_ungram<-W2009_3_sing[which(W2009_3_sing$grammaticality=='ungram'),]
W2009_3_singng_gram<-W2009_3_sing[which(W2009_3_sing$grammaticality=='gram'),]

W2009_3_sing_gram_surprisal<- t.test(W2009_3_sing_gram[which(W2009_3_sing_gram$interference=='non-int'),]$surprisal,W2009_3_sing_sing_gram[which(W2009_3_sing_gram$interference=='int'),]$surprisal, paired =TRUE)
W2009_3_sing_ungram_surprisal<-t.test(W2009_3_sing_ungram[which(W2009_3_sing_ungram$interference=='non-int'),]$surprisal,W2009_3_sing_sing_ungram[which(W2009_3_sing_ungram$interference=='int'),]$surprisal, paired =TRUE )

W2009_3_sing_gram_entropy <-t.test(W2009_3_sing_gram[which(W2009_3_sing_gram$interference=='non-int'),]$entropy,W2009_3_sing_gram[which(W2009_3_sing_gram$interference=='int'),]$entropy, paired =TRUE)
W2009_3_sing_ungram_entropy <-t.test(W2009_3_sing_ungram[which(W2009_3_sing_ungram$interference=='non-int'),]$entropy,W2009_3_sing_ungram[which(W2009_3_sing_ungram$interference=='int'),]$entropy, paired =TRUE)

W2009_3_sing_gram_attn.T <-t.test(W2009_3_sing_gram[which(W2009_3_sing_gram$interference=='non-int'),]$attn_to_target,W2009_3_sing_gram[which(W2009_3_sing_gram$interference=='int'),]$attn_to_target, paired =TRUE)
W2009_3_sing_ungram_attn.T <-t.test(W2009_3_sing_ungram[which(W2009_3_sing_ungram$interference=='non-int'),]$attn_to_target,W2009_3_sing_ungram[which(W2009_3_sing_ungram$interference=='int'),]$attn_to_target, paired =TRUE)


########################################################################
#        W2009 EXP2_3 (plural)
########################################################################
W2009_3<-read.csv('results/W2009_exp3.csv', sep=',',header=TRUE)
W2009_3_pl <-W2009_23[which(W2009_3$subject=="pl"),]
W2009_3_pl[which(W2009_3_pl$interference=='unint'),]$interference='non-int'

######## SURPRISAL
ggplot(aes(x = interference, y = surprisal), data = W2009_3_pl) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

######## ENTROPY
ggplot(aes(x = interference, y = entropy), data = W2009_3_pl) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue") +ylab('entropy_head43')

######## ATTENTION_TO_TARGET
ggplot(aes(x = interference, y = attn_to_target), data = W2009_3_pl) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

W2009_3_pl_ungram<-W2009_3_pl[which(W2009_3_pl$grammaticality=='ungram'),]
W2009_3_pl_gram<-W2009_3_pl[which(W2009_3_pl$grammaticality=='gram'),]

W2009_3_pl_gram_surprisal <-t.test(W2009_3_pl_gram[which(W2009_3_pl_gram$interference=='non-int'),]$surprisal,W2009_3_pl_gram[which(W2009_3_pl_gram$interference=='int'),]$surprisal, paired=TRUE)
W2009_3_pl_ungram_surprisal<-t.test(W2009_3_pl_ungram[which(W2009_3_pl_ungram$interference=='non-int'),]$surprisal,W2009_3_pl_ungram[which(W2009_3_pl_ungram$interference=='int'),]$surprisal, paired=TRUE)

W2009_3_pl_gram_entropy<-t.test(W2009_3_pl_gram[which(W2009_3_pl_gram$interference=='non-int'),]$entropy,W2009_3_pl_gram[which(W2009_3_pl_gram$interference=='int'),]$entropy, paired=TRUE)
W2009_3_pl_ungram_entropy <-t.test(W2009_3_pl_ungram[which(W2009_3_pl_ungram$interference=='non-int'),]$entropy,W2009_3_pl_ungram[which(W2009_3_pl_ungram$interference=='int'),]$entropy, paired=TRUE)

W2009_3_pl_gram_attn.T <- t.test(W2009_3_pl_gram[which(W2009_3_pl_gram$interference=='non-int'),]$attn_to_target,W2009_3_pl_gram[which(W2009_3_pl_gram$interference=='int'),]$attn_to_target, paired=TRUE)
W2009_3_pl_ungram_attn.T <-t.test(W2009_3_pl_ungram[which(W2009_3_pl_ungram$interference=='non-int'),]$attn_to_target,W2009_3_pl_ungram[which(W2009_3_pl_ungram$interference=='int'),]$attn_to_target, paired=TRUE)

########################################################################
#        W2009 EXP4_6
########################################################################
W2009_4_5<-read.csv('results/W2009_exp45.csv', sep=',',header=TRUE)
head(W2009_4_5)
W2009_4_5[which(W2009_4_5$interference=='unint'),]$interference = 'non-int'
######## SURPRISAL
ggplot(aes(x = interference, y = surprisal), data = W2009_4_5) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

######## ENTROPY
ggplot(aes(x = interference, y = entropy), data = W2009_4_5) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue") +ylab('entropy_head43')

######## ATTENTION_TO_TARGET

ggplot(aes(x = interference, y = attn_to_target), data = W2009_4_5) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) +theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

W2009_4_5_ungram<-W2009_4_5[which(W2009_4_5$grammaticality=='ungram'),]
W2009_4_5_gram<-W2009_4_5[which(W2009_4_5$grammaticality=='gram'),]

W2009_4_5_gram_surprisal <-t.test(W2009_4_5_gram[which(W2009_4_5_gram$interference=='non-int'),]$surprisal,W2009_4_5_gram[which(W2009_4_5_gram$interference=='int'),]$surprisal,paired=TRUE )
W2009_4_5_ungram_surprisal <-t.test(W2009_4_5_ungram[which(W2009_4_5_ungram$interference=='non-int'),]$surprisal,W2009_4_5_ungram[which(W2009_4_5_ungram$interference=='int'),]$surprisal, paired =TRUE )

W2009_4_5_gram_entropy <-t.test(W2009_4_5_gram[which(W2009_4_5_gram$interference=='non-int'),]$entropy,W2009_4_5_gram[which(W2009_4_5_gram$interference=='int'),]$entropy, paired =TRUE )
W2009_4_5_ungram_entropy <-t.test(W2009_4_5_ungram[which(W2009_4_5_ungram$interference=='non-int'),]$entropy,W2009_4_5_ungram[which(W2009_4_5_ungram$interference=='int'),]$entropy, paired=TRUE )

W2009_4_5_gram_attn.T <-t.test(W2009_4_5_gram[which(W2009_4_5_gram$interference=='non-int'),]$attn_to_target,W2009_4_5_gram[which(W2009_4_5_gram$interference=='int'),]$attn_to_target, paired=TRUE )
W2009_4_5_ungram_attn.T <- t.test(W2009_4_5_ungram[which(W2009_4_5_ungram$interference=='non-int'),]$attn_to_target,W2009_4_5_ungram[which(W2009_4_5_ungram$interference=='int'),]$attn_to_target, paired =TRUE )


########################################################################
#        Dillon Agreement
########################################################################

D_agree<-read.csv('results/Dillon2013_agreement.csv', sep=',',header=TRUE)
D_agree[which(D_agree$interference=='unint'),]$interference = 'non-int'
head(D_agree)

######## SURPRISAL
ggplot(aes(x = interference, y = surprisal), data = D_agree) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

######## ENTROPY
ggplot(aes(x = interference, y = entropy), data = D_agree) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue") +ylab('entropy_head43')

######## ATTENTION_TO_TARGET

ggplot(aes(x = interference, y = attn_to_target), data = D_agree) +
  facet_grid(.~grammaticality)+
  geom_point() + xlab("") + #Verb agreement across four levels of clausal embedding in 'it' clefts") +
  theme_bw(base_size=20) + theme(legend.position = "bottom",text = element_text(size=40)) +theme(legend.position = "bottom") +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", colour="red", width=0.3) +
  stat_sum_single(mean, colour="blue")

dillon_gram<-D_agree[which(D_agree$grammaticality=='gram'),]
dillon_ungram<-D_agree[which(D_agree$grammaticality=='ungram'),]

d2013_gram_surprisal <-t.test(dillon_gram[which(dillon_gram$interference=='non-int'),]$surprisal,dillon_gram[which(dillon_gram$interference=='int'),]$surprisal,paired =TRUE)
d2013_ungram_surprisal<-t.test(dillon_ungram[which(dillon_ungram$interference=='non-int'),]$surprisal,dillon_ungram[which(dillon_ungram$interference=='int'),]$surprisal, paired=TRUE )

d2013_gram_entropy <-t.test(dillon_gram[which(dillon_gram$interference=='non-int'),]$entropy,dillon_gram[which(dillon_gram$interference=='int'),]$entropy, paired=TRUE)
d2013_ungram_entropy <-d2013_gram_attn.T <-t.test(dillon_ungram[which(dillon_ungram$interference=='non-int'),]$entropy,dillon_ungram[which(dillon_ungram$interference=='int'),]$entropy, paired=TRUE)

d2013_gram_attn.T <- t.test(dillon_gram[which(dillon_gram$interference=='non-int'),]$attn_to_target,dillon_gram[which(dillon_gram$interference=='int'),]$attn_to_target,paired =TRUE )
d2013_ungram_attn.T <-t.test(dillon_ungram[which(dillon_ungram$interference=='non-int'),]$attn_to_target,dillon_ungram[which(dillon_ungram$interference=='int'),]$attn_to_target,paired=TRUE )

