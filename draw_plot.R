# first row contains variable names, comma is separator
# assign the variable id to row names
# note the / instead of \ on mswindows systems

library(vcd)
library(reshape2)
library(lattice)
library(ggplot2)
library(directlabels)
library(plyr)

# Prepare Data

mydata <- read.table("data.csv", header=TRUE, sep=",", row.names="Country",check.names = FALSE)

M <- melt(t(mydata))

for (i in 1: nrow(M)) {

  if (M[i,3]==0) {
     M[i,4] <- 0
     M[i,5] <- 0
     M[i,6] <- 1
     M[i,7] <- 0
     }

  if (M[i,3] == 1) {
     M[i,4] <- 1
     M[i,5] <- 0 
     M[i,6] <- 0
     M[i,7] <- 0
     }
      
  if (M[i,3] == 2) {
     M[i,4] <- 0
     M[i,5] <- 1 
     M[i,6] <- 0
     M[i,7] <- 0
     }

  if (M[i,3] == 3) {
     M[i,4] <- 0
     M[i,5] <- 0
     M[i,6] <- 0
     M[i,7] <- 1}
}


for (i in 2: nrow(M)) {
     if (M[i-1,3]!=M[i,3] && M[i-1,2] == M[i,2]) { M[i,8] <- 1 }
  }

colnames(M) <- c("year","country","value","FIT","GC","none","both","pol_change")

################################################################################
# First plots
M1 <- M[,!(colnames(M) =="value")] 
M2 <- melt(M1, id=c("year","country"))

p1 <- ggplot(M1, aes(x=year, y=FIT, fill=country))+ 
  geom_bar(stat="identity") 
p2 <- ggplot(M1, aes(x=year, y=GC, fill=country))+ 
  geom_bar(stat="identity") 

ggsave("p1.pdf", plot = p1)
ggsave("p2.pdf", plot = p2)

p3 <- ggplot(M2, aes(x=year, y=value, fill=country))+ 
  geom_bar(stat="identity") + geom_text(aes(label=country),angle=90,size=2,hjust=1,vjust=1) +facet_grid( variable ~.)
ggsave("p3.pdf", plot = p3)
p4 <- ggplot(M2, aes(x=year, y=value, fill=variable))+ 
  geom_bar(stat="identity") + facet_grid(country ~.)
ggsave("p4.pdf", plot = p4)

########################
# Set label coordinates 

M6 <- M[,!(colnames(M) %in% c("value","pol_change"))]
M6 <- melt(M6, id=c("year","country"))
M6 <- na.omit(M6)
M6 <- M6[which(M6$value != 0),]
M6$pos <- 0
for (y in 1990:2012){
  M6[which(M6$year==y) , ]$pos = cumsum(M6[which(M6$year==y),4 ] ) -0.5
  }

p6 <- ggplot(M6,aes(x=year,y=value,fill=variable)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label=country,y=pos),angle=90,hjust=1,vjust=1,size=2) +
    theme(legend.position = "none", axis.title.y= element_blank())

ggsave("p6.pdf", plot = p6)

########################
# Set label coordinates 

M8 <- M[,!(colnames(M) %in% c("value","pol_change"))]
M8 <- melt(M8, id=c("year","country"))
M8 <- na.omit(M8)
M8 <- M8[which(M8$value != 0),]
M8$pos <- 0
M8$pos_lab <- 0

for (y in 1990:2012){
  for (cat in c("FIT","GC","none","both")){
    M8[which(M8$year==y & M8$variable == cat), ]$pos = 0.05 * cumsum(M8[which(M8$year==y & M8$variable == cat),4 ] )
    M8[which(M8$year==y & M8$variable == cat), ]$pos_lab = M8[which(M8$year==y & M8$variable == cat), ]$pos + 0.01
  }}

p8 <- ggplot(M8,aes(x=year,y=pos, color=country),ylab="") + 
    geom_point(stat="identity") + 
    geom_text(aes(label=country,y=pos_lab),hjust=0,vjust=0,size=2) + 
    facet_grid(variable ~.) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.title.y= element_blank(),axis.ticks.y= element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("p8.pdf", plot = p8)

########################
# Set label coordinates 

M9 <- M[,!(colnames(M) %in% c("value"))]
M9 <- M9[which(M9$pol_change == 1), ]
M9 <- M9[,!(colnames(M9) %in% c("pol_change"))]
M9 <- melt(M9, id=c("year","country"))
M9 <- na.omit(M9)
M9 <- M9[which(M9$value != 0),]
M9$pos <- 0
M9$pos_lab <- 0

for (y in 1990:2012){
  for (cat in c("FIT","GC","none","both")){
    M9[which(M9$year==y & M9$variable == cat), ]$pos = 0.03 * cumsum(M9[which(M9$year==y & M9$variable == cat),4 ] )
    M9[which(M9$year==y & M9$variable == cat), ]$pos_lab = M9[which(M9$year==y & M9$variable == cat), ]$pos + 0.005
  }}

p9 <- ggplot(M9,aes(x=year,y=pos, color=country),ylab="") + 
    geom_point(stat="identity") + 
    geom_text(aes(label=country,y=pos_lab),hjust=0,vjust=0,size=2) + 
    facet_grid(variable ~.) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.title.y= element_blank(),axis.ticks.y= element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())

ggsave("p9.pdf", plot = p9)


