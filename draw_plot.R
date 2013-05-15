# first row contains variable names, comma is separator
# assign the variable id to row names
# note the / instead of \ on mswindows systems

library(vcd)
library(reshape2)
library(lattice)
library(ggplot2)
library(directlabels)
library(plyr)

mydata <- read.table("data.csv", header=TRUE, sep=",", row.names="Country",check.names = FALSE)

M <- melt(t(mydata))

#attach(t(mydata))

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
     if (M[i-1,3]!=M[i,3] && M[i-1,2] == M[i,2])
      {
       if (M[i,3]==1){M[i,8] <- 1}
       if (M[i,3]==2){M[i,8] <- 2}
      }
  }

colnames(M) <- c("year","country","value","FIT","GC","none","both","pol_change")

M1 <- M[,!(colnames(M) =="value")] 

M2 <- melt(M1, id=c("year","country"))

#filled.contour(M, main="Protein-Protein Interaction Potential",nlevels=2)

p1 <- ggplot(M1, aes(x=year, y=FIT, fill=country))+ 
  geom_bar(stat="identity") 
#  geom_text(aes(label = country), vjust = 1, size = 3)) 
p2 <- ggplot(M1, aes(x=year, y=GC, fill=country))+ 
  geom_bar(stat="identity") 

ggsave("p1.pdf", plot = p1)
ggsave("p2.pdf", plot = p2)

p3 <- ggplot(M2, aes(x=year, y=value, fill=country))+ 
  geom_bar(stat="identity") + geom_text(aes(label=country),angle=90,size=2,hjust=1,vjust=1) +facet_grid( variable ~.)
ggsave("p3.pdf", plot = p3)
p4 <- ggplot(M2, aes(x=year, y=value, fill=variable))+ 
  geom_bar(stat="identity") + facet_grid(country ~.)
p5 <- ggplot(M, aes(x=year, y=pol_change, shape=country))+
  geom_point(stat="identity")+
#  geom_text(aes(label=country))+
  geom_jitter(position = position_jitter(height = .1))
#direct.label(p5,"perpendicular.grid")
ggsave("p4.pdf", plot = p4)
ggsave("p5.pdf", plot = p5)

########################
# Set label coordinates 

M_shifted <- M[,!(colnames(M) %in% c("value","pol_change"))]
M_shifted <- melt(M_shifted, id=c("year","country"))
M_shifted <- na.omit(M_shifted)
M_shifted <- M_shifted[which(M_shifted$value != 0),]
M_shifted$pos <- 0
for (y in 1990:2012){
  M_shifted[which(M_shifted$year==y) , ]$pos = cumsum(M_shifted[which(M_shifted$year==y),4 ] )
  }

#M_shifted <- ddply(M_shifted, .(year), transform, pos = cumsum(FIT) - 0.5)
#M_shifted <- melt(M_shifted,id = c("year","country"))
p6 <- ggplot(M_shifted,aes(x=year,y=value,fill=variable)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label=country,y=pos),angle=90,hjust=1,vjust=1,size=2) +
    theme(legend.position = "none", axis.title.y= element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("p6.pdf", plot = p6)

########################
# Set shifted coordinates 

M_shifted <- M[,!(colnames(M) %in% c("value","FIT","GC","none","both"))]
M_shifted <- melt(M_shifted, id=c("year","country"))
M_shifted <- na.omit(M_shifted)
M_shifted <- M_shifted[which(M_shifted$value != 0),]
M_shifted$pos <- 0
M_shifted$pos_lab <- 0

for (y in 1990:2012){
  for (cat in 1:2){
    M_shifted[which(M_shifted$year==y & M_shifted$value == cat), ]$pos = cat + 0.05 * cumsum(M_shifted[which(M_shifted$year==y & M_shifted$value == cat),4 ] )
    M_shifted[which(M_shifted$year==y & M_shifted$value == cat), ]$pos_lab = M_shifted[which(M_shifted$year==y & M_shifted$value == cat), ]$pos + 0.01
  }}



#M_shifted <- ddply(M_shifted, .(year), transform, pos = cumsum(FIT) - 0.5)
#M_shifted <- melt(M_shifted,id = c("year","country"))
p7 <- ggplot(M_shifted,aes(x=year,y=pos, color=value)) + 
    geom_point(stat="identity") + 
    geom_text(aes(label=country,y=pos_lab),hjust=0,vjust=0,size=2) +
    theme(legend.position = "none", axis.text.y = element_blank(), axis.title.y= element_blank(),axis.ticks.y= element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave("p7.pdf", plot = p7)

########################
# Set label coordinates 

M_shifted <- M[,!(colnames(M) %in% c("value","pol_change"))]
M_shifted <- melt(M_shifted, id=c("year","country"))
M_shifted <- na.omit(M_shifted)
M_shifted <- M_shifted[which(M_shifted$value != 0),]
M_shifted$pos <- 0
M_shifted$pos_lab <- 0

for (y in 1990:2012){
  for (cat in c("FIT","GC","none","both")){
    M_shifted[which(M_shifted$year==y & M_shifted$variable == cat), ]$pos = 0.05 * cumsum(M_shifted[which(M_shifted$year==y & M_shifted$variable == cat),4 ] )
    M_shifted[which(M_shifted$year==y & M_shifted$variable == cat), ]$pos_lab = M_shifted[which(M_shifted$year==y & M_shifted$variable == cat), ]$pos + 0.01
  }}

p8 <- ggplot(M_shifted,aes(x=year,y=pos, color=country),ylab="") + 
    geom_point(stat="identity") + 
    geom_text(aes(label=country,y=pos_lab),hjust=0,vjust=0,size=2) + 
    facet_grid(variable ~.) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.title.y= element_blank(),axis.ticks.y= element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("p8.pdf", plot = p8)

########################
# Set label coordinates 

M_shifted <- M[,!(colnames(M) %in% c("value","FIT","GC","none","both"))]
M_shifted <- melt(M_shifted, id=c("year","country"))
M_shifted <- na.omit(M_shifted)
M_shifted <- M_shifted[which(M_shifted$value != 0),]
M_shifted$pos <- 0
M_shifted$pos_lab <- 0

for (y in 1990:2012){
  for (cat in c("FIT","GC","none","both")){
    M_shifted[which(M_shifted$year==y & M_shifted$variable == cat), ]$pos = 0.05 * cumsum(M_shifted[which(M_shifted$year==y & M_shifted$variable == cat),4 ] )
    M_shifted[which(M_shifted$year==y & M_shifted$variable == cat), ]$pos_lab = M_shifted[which(M_shifted$year==y & M_shifted$variable == cat), ]$pos + 0.01
  }}

p8 <- ggplot(M_shifted,aes(x=year,y=pos, color=country),ylab="") + 
    geom_point(stat="identity") + 
    geom_text(aes(label=country,y=pos_lab),hjust=0,vjust=0,size=2) + 
    facet_grid(variable ~.) + 
    theme(legend.position = "none", axis.text.y = element_blank(), axis.title.y= element_blank(),axis.ticks.y= element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("p9.pdf", plot = p9)


