# Universidad de Valparaíso
## Facultad de ciencias
## IECD 424: Estadística no Paramétrica

# Trabajo práctico 2
## Bastián Barraza Morales

setwd('C:\\Users\\Bastian Barraza M\\OneDrive\\Documentos\\Semestre 8\\Nonparametric statistics\\Trabajo 2')

library(bootstrap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(boot)
library(gridExtra)

x <-c(576, 635, 558, 578, 666, 
    580, 555, 661, 651, 605,
    653, 575, 545, 572, 594)

y = c(3.39, 3.30, 2.81, 3.03, 3.44, 
    3.07, 3.00, 3.43, 3.36, 3.13,
    3.12, 2.74, 2.76, 2.88, 2.96)
png("test.png")
data = data.frame(x, y)
grid.table(data)
dev.off()

cor(x, y)
#correlation = 0.776

# BOOTSTRAP RHO
set.seed(1)
b3 <- boot(data, 
  statistic = function(data, i) {
    cor(data[i, "x"], data[i, "y"], method='pearson')
  },
  R = 500
)
boot.ci(b3, type = c("norm")) #bootstrapped CI

#Plotting RHO vs iteration
png(file="M1_IterationsR.png",
    width=1000, height=1000)
plot(b3$t,type="l",
      ylab="Coeficiente de correlación",
      xlab="Iteration",
      main = 'Iteraciones Bootstrap de los estimadores del coeficiente de correlación simulados desde la muestra 1.')
dev.off()

#Plotting RHO distribution with CI
quantile(b3$t, probs = c(0.025, 0.975))
b3_df = data.frame(b3$t)
ggplot(b3_df, aes(x=b3.t)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.20, fill="#FF6666")  +
 geom_vline(aes(xintercept=mean(b3.t)),
            color="blue", size=1.5) +
            ggtitle('Simulación Bootstrap para el coeficiente de correlación de la muestra 1') +
            xlab("Coeficiente de correlación") + 
            ylab("Densidad")+ 
  geom_vline(aes(xintercept=0.5302),
            color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1.0155),
            color="blue", linetype="dashed", size=1)

  
ggsave('M1_HistogramR.png')

# BOOTSTRAP ATANH(RHO)
set.seed(1)
b2 <- boot(data, 
  statistic = function(data, i) {
    atanh(cor(data[i, "x"], data[i, "y"], method='pearson'))
  },
  R = 500
)
b2
boot.ci(b2, type = c("norm", "perc")) #bootstrapped CI. 

#Plotting ATANH(RHO) vs iteration
png(file="M1_IterationsATANR.png",
    width=1000, height=1000)
plot(b2$t,type="l",
      ylab="Arctanh(r)",
      xlab="Iteration",
      main = 'Iteraciones Bootstrap del estimador arctanh(r) simulados desde la muestra 1.')
dev.off()

#Plotting ATANH(RHO) distribution with CI
quantile(b2$t, probs = c(0.025, 0.975))
b2_df = data.frame(b2$t)
ggplot(b2_df, aes(x=b2.t)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.20, fill="#FF6666")  +
 geom_vline(aes(xintercept=mean(b2.t)),
            color="blue", size=1.5) +
            ggtitle('Simulación Bootstrap para el coeficiente de correlación de la muestra 2') +
            xlab("Arctanh(r)") + 
            ylab("Densidad")+ 
  geom_vline(aes(xintercept=0.224),
            color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1.690),
            color="blue", linetype="dashed", size=1)

  
ggsave('M1_HistogramATANR.png')

################################
#### Ejercisio 2: #############
###############################

a <-c(35, 46, 36, 36, 49,
    41, 40, 41, 45, 44,
    33, 39, 49, 35, 36)

b = c(31, 35, 30, 22, 37,
    30, 21, 33, 34, 31,
    21, 15, 42, 25, 22)

df = data.frame(a, b)
df


# Jack RHO
jack = numeric(15)
for (i in 1:15){
  jack[i] = cor(a[-i], b[-i])
  jack[i] = 14*(cor(a,b) - jack[i])
}
mean(jack)
s_r = sqrt(var(jack)/15)
s_r
li_r = cor(a,b) - 1.96*s_r
li

# Jack R2
jack = numeric(15)
for (i in 1:15){
  jack[i] = cor(a[-i], b[-i])**2
  jack[i] = 14*(cor(a,b)**2 - jack[i])
}
mean(jack)
s_r = sqrt(var(jack)/15)
s_r
li_r = cor(a,b) - 1.96*s_r
li_r

# Bootstrap RHO
set.seed(1)
b_m2 <- boot(df, 
  statistic = function(df, i) {
    cor(df[i, "a"], df[i, "b"], method='pearson')
  },
  R = 500
)
b_m2
boot.ci(b_m2, type = c("norm", "perc")) #bootstrapped CI. 

#Plotting rho vs iteration
png(file="M2_IterationsRHO.png",
    width=1000, height=1000)
plot(b_m2$t,type="l",
      ylab="Coeficiente de correlación",
      xlab="Iteration",
      main = 'Iteraciones Bootstrap de los estimadores del coeficiente de correlación simulados desde la muestra 2.')
dev.off()

#Plotting rho distribution with CI
quantile(b_m2$t, probs = c(0.025, 0.975))
bm2_df = data.frame(b_m2$t)
ggplot(bm2_df, aes(x=b_m2.t)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.20, fill="#FF6666")  +
 geom_vline(aes(xintercept=mean(b_m2.t)),
            color="blue", size=1.5) +
            ggtitle('Simulación Bootstrap para el coeficiente de correlación de la muestra 2') +
            xlab("Coeficiente de correlación") + 
            ylab("Densidad")+ 
  geom_vline(aes(xintercept=0.4604),
            color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1.0175),
            color="blue", linetype="dashed", size=1)

  
ggsave('M2_HistogramR.png')


#### Bootstrap R2
set.seed(1)
b_m2R2 <- boot(df, 
  statistic = function(df, i) {
    cor(df[i, "a"], df[i, "b"], method='pearson')**2
  },
  R = 500
)
b_m2R2
boot.ci(b_m2R2, type = c("norm", "perc")) #bootstrapped CI. 

#Plotting rho^{2} vs iteration
png(file="M2_IterationsRHO2.png",
    width=1000, height=1000)
plot(b_m2R2$t,type="l",
      ylab="Coeficiente de regresión",
      xlab="Iteration",
      main = 'Iteraciones Bootstrap de los estimadores del coeficiente de regresión simulados desde la muestra 2.')

dev.off()

#Plotting rho^{2} distribution with CI
quantile(b_m2R2$t, probs = c(0.025, 0.975))
bm2R2_df = data.frame(b_m2R2$t)

ggplot(bm2R2_df, aes(x=b_m2R2.t)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.20, fill="#FF6666")  +
 geom_vline(aes(xintercept=mean(b_m2R2.t)),
            color="blue", size=1.5) +
            ggtitle('Simulación Bootstrap para el coeficiente de regresión de la muestra 2') +
            xlab("Coeficiente de correlación") + 
            ylab("Densidad")+ 
  geom_vline(aes(xintercept=0.1604270),
            color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=0.8637762),
            color="blue", linetype="dashed", size=1)

ggsave('M2_HistogramR2.png')
