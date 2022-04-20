# analysis-of-aggregated-data 
## charging the libraries
library("readr")
library("dplyr")
library("janitor")
library("formattable")
library("tidyverse")
library("reshape2")
library(kableExtra)
library(knitr)
library(ggplot2)
library(scales)
library("dplyr")
library("cowplot")
library("VGAM")

#charge the function for tables
############################
# questions 
###########################
# Descriptive statistics- preparation tables

tab1<-function(G,A){
G<-filter(G, G[,1]!= "Prefer not to respond")
G<-filter(G, G[,2]!= "Prefer not to respond")
G<-filter(G, G[,2]!= "Not Applicable")
G<-filter(G, G[,2]!= "Does not apply")
G<-filter(G, G[,2]!= "I do not know")
G<-filter(G, G[,2]!= "*")
G<-filter(G, G[,1]!= "*")
G<-G[complete.cases(G[,2]), ]
G<-select(G, 1:3)
G<-spread(G, key = 1, value=3)
G[is.na(G)] <- 0
G<-mutate(G,  pct_F = round(Female / sum(Female), 3)*100,
              pct_M  = round(Male /sum(Male), 3)*100)

A<-filter(A, A[,1]!= "Prefer not to respond")
A<-filter(A, A[,2]!= "Prefer not to respond")
A<-filter(A, A[,2]!= "Not Applicable")
A<-filter(A, A[,2]!= "Does not apply")
A<-filter(A, A[,2]!= "I do not know")
A<-filter(A, A[,1]!= "*")
A<-filter(A, A[,2]!= "*")
A<-A[complete.cases(A[,2]), ]
A<-select(A, 1:3)
A<-spread(A, key = 1, value=3)
colnames(A)[2] <- "AFemale"
colnames(A)[3] <- "AMale"
A[is.na(A)] <- 0
A<-mutate(A,  Apct_F = round(AFemale / sum(AFemale), 3)*100,
          Apct_M  = round(AMale /sum(AMale), 3)*100)

t1<-merge(G,A, all=TRUE)
return(t1)
}

#function2: preparing tables for the graphs
tab2<- function(a){
  a<-select(a, 1,5,4,9,8)
  a<-gather(a, key ="Gender", value="pct", c(-1))
  a[3]<-a[3]/100
  a[4]<-rep(1:2, each=4)
  a[4]<-factor(a[,4], ordered = T, labels = c("General", "Africa"))
  a[5]<-rep(c("Male", "Female"), each=2)
  a<-select(a, 1,5,3,4)
  h<-as.character(group_indices(a, a[,2],a[,1]))
  a<-cbind(a,h)
  
  return(a)
}

## plot for question with three or more options

plt4<-function(df, x,y,z,w,Palette) {
  ggplot(df, aes(x= df[,x], y= df[,y], fill = df[,z])) + 
    facet_wrap(~ df[,w], strip.position = "bottom") +
    labs( x=" ", y="Percentage")  +
    geom_bar(position = "fill",stat = "identity",width=0.7) + 
    scale_y_continuous(labels = percent_format()) +
    theme_bw() +
    theme(legend.position="bottom",legend.box = "horizontal", axis.text.x=element_blank(), axis.ticks.x=element_blank())+
    geom_text(aes(label=scales::percent(pct,accuracy = 2L)),
              position=position_stack(vjust=0.5),
               size=3)+geom_text(aes(x=1,y=-0.05,label="Female"), size=2.5)+
    geom_text(aes(x=2,y=-0.05,label="Male"), size=2.5)
}

## Function for t-test on all the table 

tab_test<-function(L){
  L<- select(L, 1,2,3,6,7)
  result <- data.frame(matrix(nrow = nrow(L), ncol = 5))
  colnames(result) <- c( "  ","Female/Male", "Female_a/Male_a", "Female_g/Female_a", "Male_g/male_a")
  result[1]<-L[1]
  for (i in 1: nrow(L)) {
    result[i,2]<-prop.test(x = c(L[i,2], L[i,3]), n = c(colSums(L[, c(2,3)])), correct = F)$p.value
    result[i,3]<-prop.test(x = c(L[i,4], L[i,5]), n = c(colSums(L[, c(4,5)])), correct = F)$p.value
    result[i,4]<-prop.test(x = c(L[i,2], L[i,4]), n = c(colSums(L[, c(2,4)])), correct = F)$p.value
    result[i,5]<-prop.test(x = c(L[i,3], L[i,5]), n = c(colSums(L[, c(3,5)])), correct = F)$p.value
  }
  
  return(result)
}

tab_t2<-function(M){
  tbl5 <- data.frame(matrix(nrow = nrow(M), ncol = 5))
  colnames(tbl5) <- c( "  ","Female/Male", "Female/Male", "Female/Female", "Male/male")
  tbl5[1]<-M[1]
  for(i in 2:5){
    for (j in 1:nrow(M)) {
      if(M[j,i]< 0.01){
        tbl5[j,i]<-"****"
      }else{
        if(M[j,i]< 0.05){
          tbl5[j,i]<-"***" 
        }else{
          if(M[j,i]< 0.10){
            tbl5[j,i]<-" ** " 
          }else{
            if (M[j,i]== "NO"){
              tbl5[j,i]<-"NO"
            }else{
              tbl5[j,i]<-"--" 
            }
            }
          }
        }
      j=j+1
    }
    i=i+1
  }
  return(tbl5)
}

### Logistic model for aggragated data and results' presentation
P<- read.csv(".../q20s0/study.csv", sep = ";", na.strings=c("","NA"))
## creating tables for models
tab_c<-function(G){
        G<-filter(G, G[,2]!= "Prefer not to respond")
        G<-filter(G, G[,1]!= "Not Applicable")
        G<-filter(G, G[,2]!= "*")
        G<-G[complete.cases(G[ , 1]),]
        G<-G[complete.cases(G[ , 5]),]
        G<- select(G, -5)
        G<-filter(G, G[,3]!= "*")
        G[,"Gender"]<- (G[,2])
        G[,"Gender"]<-factor(G[,"Gender"])
        G[,"Gender"]<-relevel(G[,"Gender"], ref="Male")
        G<-spread(G, key = 1, value=4)
        return(G)
}
tab2<- function(a){
  a<-select(a, 1,5,4)
  a<-gather(a, key ="Gender", value="pct", c(-1))
  a[3]<-a[3]/100
  a[4]<-rep(1, each=4)
  a[4]<-factor(a[,4], ordered = T, labels = c("Africa"))
  a[5]<-rep(c("Male", "Female"), each=2)
  a<-select(a, 1,5,3,4)
  h<-as.character(group_indices(a, a[,2],a[,1]))
  a<-cbind(a,h)
  
  return(a)
}


P<- tab_c(P)

P[,"Field_"]<-ifelse(P[,2]=="Astronomy","Astronomy",
                          ifelse(P[,2]=="Biological and Related Sciences",'Biological',
                                 ifelse(P[,2]=="Chemistry",'Chemistry',
                                        ifelse(P[,2]== "Computer Science and Technology",'Computer',
                                               ifelse(P[,2]=="Mathematics", 'Mathematics',
                                                      ifelse(P[,2]=="History and Philosophy of Science", 'History',
                                                             ifelse(P[,2]=="Physics", 'Physics', 'Others')))))))
  
  
P[,"Field_"]<-factor(P[,"Field_"])
P[,"Field_"]<-relevel(P[,"Field_"], ref='Mathematics')
m_f<- glm(cbind(P[,5], P[,4]) ~ Gender+Field_, family = binomial("logit"), data = P)
summary(m_f)
p<-exp(cbind(coef(m_f), confint(m_f))) 
p1<-p[c(2),]
c_p<-m_f$coefficients
t_1<-as.data.frame(cbind(c_p,p))
t_1<-t_1 %>% mutate_if(is.numeric, round, digits = 3)
names(t_1)[1] <- "Coefficients"
names(t_1)[2] <- "Odds-ratio"
names(t_1)[3] <- "Lower"
names(t_1)[4] <- "Upper"

row.names(t_1)[2]<-'Female'

tabA<-kbl(t_1,"html", table.attr = "style='width:60%;'", align= 'lccc') %>%
  kable_classic() %>%
  add_header_above(c("variables " = 1, "Model" = 1, "Odds-ratio and Intervals" = 3))

l<- rbind(a1,h1)
l<-rbind(l,e1)
l<-rbind(l,p1)
l<-as.data.frame(l)
rownames(l)<-c("age","HDI","Employment","Field_study")
l$vars<-c("age","HDI","Employment","Field_study")

## plot the estimated odds ratio

p1<-ggplot(l, aes(x=vars, y=V1)) + 
  geom_point() + geom_errorbar(aes(ymin=l[,2], ymax=l[,3]), linetype="dotted",color="blue", width = 0.2)+
  xlab("Models")+ylab("Number of times women are more likely to have research resources than men")+
  ggtitle("Funding")+
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 8),plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=1, color="red")
  
