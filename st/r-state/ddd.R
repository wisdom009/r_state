library(ggplot2)
library(dplyr)
library(stringr)

setwd("D:/workspace/r_state")
mergeUserDic(data.frame(readLines("영화_황해.txt"), "ncn"))

install.packages('rJava')
library(rJava)

install.packages('KoNLP')
library(KoNLP)


remove.packages('rJava')
remove.packages('KoNLP')

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_212/lib")
