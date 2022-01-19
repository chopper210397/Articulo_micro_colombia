######################################
########################################
#               data micro             #
########################################
########################################
# install.packages("ggpubr")
library(psych)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(ggpubr)

#------------------------------------------------------------#
#------------------ SHANNON INDEX DATABASE ------------------#
#------------------------------------------------------------#

datamicro<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\workfiles\\Shannonindex.xlsx")
# DESCRIPTIVE STATS
# str(datamicro)
# describe(datamicro)
# WORKING/formatting the data
datamicro$Shannon<-paste0(substr(datamicro$Shannon,start = 1,stop = 1),".",substr(datamicro$Shannon,start = 2,stop = 4))
datamicro$Shannon<-as.numeric(datamicro$Shannon)
datamicro$Shannon<-round(datamicro$Shannon,digits = 2)
# converting from number to labels in sample type
datamicro$`Sample Type`<-ifelse( datamicro$`Sample Type`==3,"BAL",ifelse(datamicro$`Sample Type`==4,"Saline from BAL","Induced Sputum"))
# converting from number to labels in time point
datamicro$`Time Point`<-ifelse(datamicro$`Time Point`==0,"Admission",ifelse(datamicro$`Time Point`==4,"Follow-up (6 months)","ND") )

# scaleFUN <- function(x) sprintf("%.1f", x)
# +scale_y_continuous(labels=scaleFUN)
# plotting with ggplot2
# los ggsave son para exportarlo en un formato de alta calidad

#--------------------- BOXPLOT + VIOLIN ENTRE GRUPOS ---------------------#
datamicro %>%
  ggplot(mapping = aes(x=Group,y=Shannon, fill=Group))+
  geom_violin(trim = FALSE, alpha=0.5)+
  geom_boxplot(width=0.3)+ labs(y="Shannon Index",x="")+theme_classic()

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "shannonbygroup.png")

#--------------------- BOXPLOT + VIOLIN ENTRE SAMPLE TYPE ---------------------#

datamicro %>%
  ggplot(mapping = aes(x=factor(`Sample Type`),y=Shannon,fill=factor(`Sample Type`)))+
  geom_violin(trim = FALSE, alpha=0.5)+
  geom_boxplot(width=0.3)+
  labs(x="",y= "Shannon Index")+
  guides(fill=guide_legend(title="Sample Type"))+theme_classic()+xlim("BAL","Induced Sputum")+
  scale_fill_discrete(breaks=c("BAL","Induced Sputum"))

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "shannonbysampletype.png")
#--------------------- BOXPLOT + VIOLIN ENTRE TIME POINT ---------------------#
datamicro %>%
  ggplot(mapping = aes(x=`Time Point`,y=Shannon,fill=`Time Point`))+
  geom_violin(trim = FALSE, alpha=0.5,color="black")+
  geom_boxplot(width=0.13)+
  labs(x="",y= "Shannon Index")+
  guides(fill=guide_legend(title="Time Point"))+theme_classic()

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "shannonbytimepoint.png")
# trying new things
# DENSITY FUNCTIONS
a<-datamicro %>%
  ggplot(aes(x=Shannon, fill=Group))+
  geom_density(alpha=0.6, color="black")+
  labs(x="Shannon Index",y="Density")+
  guides(fill=guide_legend(title="Group"))+theme_classic()+geom_vline(aes(xintercept=mean(Shannon)),
                                                                      color="black", linetype="dashed", size=0.5)

b<-datamicro %>%
  ggplot(aes(x=Shannon, fill=`Sample Type`))+
  geom_density(alpha=0.6, color="black")+
  labs(x="Shannon Index",y="Density")+
  guides(fill=guide_legend(title="Sample Type"))+theme_classic()+geom_vline(aes(xintercept=mean(Shannon)),
                                                                      color="black", linetype="dashed", size=0.5)+
  scale_fill_discrete(breaks=c("BAL","Induced Sputum"))

c<-datamicro %>%
  ggplot(aes(x=Shannon, fill=`Time Point`))+
  geom_density(alpha=0.6, color="black")+
  labs(x="Shannon Index",y="Density")+
  guides(fill=guide_legend(title="Time Point"))+theme_classic()+geom_vline(aes(xintercept=mean(Shannon)),
                                                                            color="black", linetype="dashed", size=0.5)
# CON ggarrange podemos unir los gráficos en uno solo
figure<-ggarrange(c, ggarrange(b, a, ncol = 2, labels = c("B", "C"),legend = "bottom"), 
                  nrow = 2, 
                  labels = "A"       # Label of the line plot
) 
figure

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 7, device='tiff', dpi=300,filename = "separated_densitybygroup_timepoint_sampletype.png")
#
datamicro %>%
  ggplot(aes(x=Shannon))+
  geom_density(alpha=0.6, color="black")+
  labs()+
  guides(fill=guide_legend(title="Time Point"))+theme_classic()+geom_vline(aes(xintercept=mean(Shannon)),
                                                                           color="black", linetype="dashed", size=0.5)+
  facet_grid(Group~ .)

# some histograms
datamicro %>% ggplot(aes(x=Shannon))+
  geom_histogram(color="black",fill="white", alpha=0.5, position="identity")+theme_classic()+
  facet_grid(`Sample Type`~ .)+
  geom_density(alpha=0.6)


#barplots
datamicro %>%filter(`Sample Type`!="Saline from BAL") %>% 
  ggplot(aes(x=Group,y=Shannon,fill=`Time Point`))+
  geom_bar(stat = "identity")+theme_classic()+facet_grid(`Sample Type`~ .)

# Density plot with facet grid, 3 variables in one plot
datamicro %>%filter(`Sample Type`!="Saline from BAL") %>% 
  ggplot(aes(x=Shannon, fill=Group))+
  geom_density(alpha=0.6, color="black")+
  labs(x="Shannon Index",y="Density")+
  guides(fill=guide_legend(title="Group"))+theme_classic()+
  facet_grid(vars(`Sample Type`),vars(`Time Point`))

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "densitybygroup_timepoint_sampletype.png")
# boxplots mas violines con p-values
#group
datamicro %>%
  ggplot(mapping = aes(x=Group,y=Shannon, fill=Group))+
  geom_violin(trim = FALSE, alpha=0.5)+
  geom_boxplot(width=0.3)+ labs(y="Shannon Index",x="")+theme_classic()+
  geom_segment(aes(x = 1, y = 6.4, xend = 2, yend = 6.4))+
  geom_segment(aes(x = 1, y = 6.3, xend = 1, yend = 6.4))+
  geom_segment(aes(x = 2, y = 6.3, xend = 2, yend = 6.4))+
  geom_segment(aes(x = 2.05, y = 6.3, xend = 3, yend = 6.3))+
  geom_segment(aes(x = 2.05, y = 6.2, xend = 2.05, yend = 6.3))+
  geom_segment(aes(x = 3, y = 6.2, xend = 3, yend = 6.3))+
  geom_segment(aes(x = 1, y = 6.9, xend = 3, yend = 6.9))+
  geom_segment(aes(x = 1, y = 6.9, xend = 1, yend = 6.8))+
  geom_segment(aes(x = 3, y = 6.9, xend = 3, yend = 6.8))+
  geom_text(aes(x=1.5,y=6.6),label="0.07")+
  geom_text(aes(x=2.5,y=6.5),label="0.28")+
  geom_text(aes(x=2,y=7.1),label="0.0039")

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "shannonbygrouppvalues.png")
#sample type

datamicro %>%
  ggplot(mapping = aes(x=`Sample Type`,y=Shannon,fill=`Sample Type`))+
  geom_violin(trim = FALSE, alpha=0.5)+
  geom_boxplot(width=0.3)+
  labs(x="",y= "Shannon Index")+
  guides(fill=guide_legend(title="Sample Type"))+theme_classic()+xlim("BAL","Induced Sputum")+
  geom_segment(aes(x = 1, y = 6.6, xend = 2, yend = 6.6))+
  geom_segment(aes(x = 1, y = 6.5, xend = 1, yend = 6.6))+
  geom_segment(aes(x = 2, y = 6.5, xend = 2, yend = 6.6))+
  geom_text(aes(x=1.5,y=6.8),label="0.09")+
  scale_fill_discrete(breaks=c("BAL","Induced Sputum"))

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "shannonbysampletypepvalues.png")
#TIME POINT
datamicro %>%
  ggplot(mapping = aes(x=`Time Point`,y=Shannon,fill=`Time Point`))+
  geom_violin(trim = FALSE, alpha=0.5,color="black")+
  geom_boxplot(width=0.13)+
  labs(x="",y= "Shannon Index")+
  guides(fill=guide_legend(title="Time Point"))+theme_classic()+
  geom_segment(aes(x = 1, y = 6.6, xend = 2, yend = 6.6))+
  geom_segment(aes(x = 1, y = 6.5, xend = 1, yend = 6.6))+
  geom_segment(aes(x = 2, y = 6.5, xend = 2, yend = 6.6))+
  geom_text(aes(x=1.5,y=6.8),label="0.02")

ggsave(path = "C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\plots",
       width = 7, height = 5, device='tiff', dpi=300,filename = "shannonbytimepointpvalues.png")
# working with kruskal wallis test
shannon<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\workfiles\\Shannonindex.xlsx")
shannon$Shannon<-paste0(substr(shannon$Shannon,start = 1,stop = 1),".",substr(shannon$Shannon,start = 2,stop = 4))
shannon$Shannon<-as.numeric(shannon$Shannon)
shannon$Shannon<-round(shannon$Shannon,digits = 2)
# converting from number to labels in sample type
shannon$`Sample Type`<-ifelse( shannon$`Sample Type`==3,"BAL",ifelse(shannon$`Sample Type`==4,"Saline from BAL","Induced Sputum"))
# converting from number to labels in time point
shannon$`Time Point`<-ifelse(shannon$`Time Point`==0,"Admission",ifelse(shannon$`Time Point`==4,"Follow-up (6 months)","ND") )
# general test
kruskal.test(Shannon ~ Group, data = shannon)
# by group
# install.packages("dunn.test")
library(dunn.test)
dunn.test(shannon$Shannon,g = shannon$Group)
pairwise.wilcox.test(x = shannon$Shannon, g = shannon$Group, p.adjust.method = "holm" )
#
faith<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\workfiles\\Faith_pd.xlsx")
observed<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\workfiles\\Observed_features.xlsx")
evenness<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\workfiles\\Pielou_evenness.xlsx")

braycurtis<-read_xlsx("C:\\Users\\LBarrios\\Desktop\\Articulo_micro_colombia\\workfiles\\bray_curtis_distance_matrix.xlsx")
install.packages("vegan")
install.packages("ape")
library(vegan)
library(ape)
pcoa <- wcmdscale(braycurtis, eig = TRUE)
data(varespec)
braycurtis<-braycurtis[,-1]

braycurtis %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")


PCoA.res<-capscale(my_pcoa~1,distance="bray")
scores(my_pcoa,display="sites")
plot(my_pcoa)

a<-pcoa(my_pcoa,correction="none")
biplot(my_pcoa)
pcoa <- cmdscale (eurodist, eig = TRUE)
PCA <- rda(my_pcoa, scale = FALSE)
plot(PCA)
ordiplot(my_pcoa,display = "sites",type = "point")
braycurtis %>% ggplot()+geom_point()

dist <- vegdist(my_pcoa,  method = "bray")
PCOA <- pcoa(dist)
barplot(PCOA$values$Relative_eig[1:10])
PCOA <- pcoa(dist, correction = "cailliez")
biplot.pcoa()
biplot.pcoa(PCOA, varespec)
#
library(stats)
distance_matrix <- as.dist(braycurtis[,c(2:48)])
my_pcoa <- stats:::cmdscale(distance_matrix)
my_pcoa <- as.data.frame(my_pcoa)
plot(my_pcoa)
text(my_pcoa[,1], my_pcoa[,2], labels=rownames(my_pcoa), cex= 1)
names(my_pcoa)[1:2] <- c('PC1', 'PC2')
my_pcoa %>% ggplot(aes(x = PC1, y = PC2))+geom_point(size=5)
#
PcoA2D_AT <- cmdscale(braycurtis, k =2)

PcoA2D_AT <- as.data.frame(PcoA2D_AT)
PcoA2D_AT$Group <- Zeller2014AbundTable$Groups
names(PcoA2D_AT)[1:2] <- c('PC1', 'PC2')
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")
Tr_PcoA <- ggplot(PcoA2D_AT, aes(x = PC1, y = PC2, 
                                 label = row.names(PcoA2D_AT)))
Tr_PcoA + geom_point(size =5) +
  scale_colour_manual(values = cbPalette[2:3]) +
  geom_text(col = 'black')
                   
#

