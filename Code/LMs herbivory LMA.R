#01.30.2023
#Models for understanding LMA and herbivory 
#R version: 3.6.1

require(ggplot2)
require(tidyverse)
require(cowplot)
require(lme4)
library(car) #needed for logit transformation
library(agricolae) #You need this for the Tukey test
library(EnvStats) #rosnerTest
require(wesanderson)
#needed to run sjPlot
library(insight)
library(performance) 
library(parameters)
library(effectsize)
library(sjPlot) #needed for making lmer summary tables
library(webshot) #needed to save sjPlot tables

options(scipen = 99) # turn off sci notation

#load data----
traitdata <- read.csv("plant.traits_divadded.csv", header=T, sep=",", 
                      na.strings=  "NA", dec=".", strip.white=TRUE)

#Data Transformation----
newdata <- traitdata %>%
  select(c(Quarry,Species,perc.dam:div.mine,LMA..mean.,LMA_sd)) #selecting the columns I want by name

#long data
longdata_rep2 <- gather(newdata, trait, value_rep, perc.dam:LMA..mean.,LMA_sd, factor_key=TRUE) #transforming data into long format

#taking averages to eliminate replicate data
longdata2 <- longdata_rep2 %>% 
  group_by(Quarry,Species,trait) %>% #grouped by Year variable
  summarise_each(funs(value=mean(., na.rm=TRUE))) %>%
  #when you specify the function, `summarise_each will applies the function 
  #(here it is mean) to each of the columns in the dataset or a subset of columns (if specified)
  mutate(Forest = case_when(Quarry %in% c("HF1901", "HF1902", "HF1903") ~ "Harvard Forest",
                            Quarry %in% c("LS1901", "LS1902", "LS1903") ~ "La Selva",
                            Quarry %in% c("MD1901", "MD1902", "MD1903") ~ "SERC"))
#now need to turn long data into wide
widedata2 <- spread(longdata2, trait, value)

#Harvard Forest
HFdata <- widedata2 %>%
  filter(Forest == "Harvard Forest")%>%
  rename(LMA = LMA..mean.)
HFdata <- HFdata %>%
  mutate(Facies = case_when(Quarry %in% "HF1901" ~ "Swamp",
                            Quarry %in% "HF1902" ~ "Tributary",
                            Quarry %in% "HF1903" ~ "Fluvial"))

#SERC
MDdata <- widedata2 %>%
  filter(Forest == "SERC") %>%
  rename(LMA = LMA..mean.)
MDdata <- MDdata %>%
  mutate(Facies = case_when(Quarry %in% "MD1901" ~ "Swamp",
                            Quarry %in% "MD1902" ~ "Tributary",
                            Quarry %in% "MD1903" ~ "Fluvial"))

#La Selva
LSdata <- widedata2 %>%
  filter(Forest == "La Selva")%>%
  rename(LMA = LMA..mean.)
LSdata <- LSdata %>%
  mutate(Facies = case_when(Quarry %in% "LS1901" ~ "Fluvial",
                            Quarry %in% "LS1902" ~ "Tributary",
                            Quarry %in% "LS1903" ~ "Swamp"))

#color palettes----
#HF = Chevalier1
#SERC = Zissou1
#Need 6 colors so here is the manual version
Zissou1 <- c("#219ebc", "#8ecae6", "#f4e409","#eeba0b", "#d00000","#6a040f")
#LS = Darjeeling1
#need 7 colors so doing this manually
Darjeeling1 <- c("#dc2f02","#bc6c25","#40916c","#eeba0b","#ff7b00","#4cc9f0",
                 "#8f2d56")

#setting seed for reproducibility
set.seed(888)

#HF herb vs. LMA----
#frequency
hf.tf <-  lm(perc.dam ~ LMA, data = HFdata)
summary(hf.tf)
hf.specf <- lm(perc.spec ~ LMA, data = HFdata)
summary(hf.specf)
hf.gf <- lm(perc.gall ~ LMA, data = HFdata) #very sig R2 = 0.45
summary(hf.gf)
hf.mf <- lm(perc.mine ~ LMA, data = HFdata)
summary(hf.mf)
#diversity 
hf.td <- lm(div.dam ~ LMA, data = HFdata) 
summary(hf.td)
hf.specd <- lm(div.spec ~ LMA, data = HFdata) 
summary(hf.specd)
hf.gd <- lm(div.gall ~ LMA, data = HFdata) #sig R2 = 0.36
summary(hf.gd)
hf.md <- lm(div.mine ~ LMA, data = HFdata)
summary(hf.md)

#output tables
tab_model(hf.tf,
          hf.specf,
          hf.gf,
          hf.mf,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/HF.lmaherbfreq.html")

tab_model(hf.td,
          hf.specd,
          hf.gd,
          hf.md,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/HF.lmaherbdiv.html")

#HF significant plots: 
#gall %
gallperc.LMA.HF <- ggplot(HFdata, aes(x=LMA, y=perc.gall*100, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd), width = 0.5)+
  scale_color_manual(values = wes_palette("Chevalier1"))+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Gall Damage Frequency (%)")+
  #facet_wrap(~Forest)+
  theme_cowplot()
gallperc.LMA.HF

HFlegend <- get_legend(gallperc.LMA.HF) #rerun with legend removed

#rerun without legend
gallperc.LMA.HF <- ggplot(HFdata, aes(x=LMA, y=perc.gall*100, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd), width = 0.5)+
  scale_color_manual(values = wes_palette("Chevalier1"))+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Gall Damage Frequency (%)")+
  #facet_wrap(~Forest)+
  theme_cowplot()+
  theme(legend.position="none")
gallperc.LMA.HF

#Gall diversity
galldiv.LMA.HF <- ggplot(HFdata, aes(x=LMA, y=div.gall, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd))+
  scale_color_manual(values = wes_palette("Chevalier1"))+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Gall Damage Diversity")+
  #facet_wrap(~Forest)+
  theme_cowplot()+
  theme(legend.position="none")
galldiv.LMA.HF


#SERC
#frequency 
md.tf <- lm(perc.dam ~ LMA, data = MDdata)
summary(md.tf)
md.specf <- lm(perc.spec ~ LMA, data = MDdata)
summary(md.specf)
md.gf <- lm(perc.gall ~ LMA, data = MDdata) 
summary(md.gf)
md.mf <- lm(perc.mine ~ LMA, data = MDdata)
summary(md.mf)
#diversity 
md.td <- lm(div.dam ~ LMA, data = MDdata)
summary(md.td)
md.specd <- lm(div.spec ~ LMA, data = MDdata) 
summary(md.specd)
md.gd <- lm(div.gall ~ LMA, data = MDdata) 
summary(md.gd)
md.md <-  lm(div.mine ~ LMA, data = MDdata) 
summary(md.md)
#No significance

#output tables
tab_model(md.tf,
          md.specf,
          md.gf,
          md.mf,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/MD.lmaherbfreq.html")

tab_model(md.td,
          md.specd,
          md.gd,
          md.md,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/MD.lmaherbdiv.html")

#LS
#frequency 
ls.tf<- lm(perc.dam ~ LMA, data = LSdata)
summary(ls.tf)
ls.specf <- lm(perc.spec ~ LMA, data = LSdata)
summary(ls.specf)
ls.gf <- lm(perc.gall ~ LMA, data = LSdata) #sig R2 = 0.42
summary(ls.gf)
ls.mf <- lm(perc.mine ~ LMA, data = LSdata) #sig R2 = 0.38
summary(ls.mf)

#diversity
ls.td <- lm(div.dam ~ LMA, data = LSdata) 
summary(ls.td)
ls.specd <- lm(div.spec ~ LMA, data = LSdata) 
summary(ls.specd)
ls.gd <- lm(div.gall ~ LMA, data = LSdata) #sig R2 = 0.38
summary(ls.gd)
ls.md <- lm(div.mine ~ LMA, data = LSdata) #sig R2 = 0.39
summary(ls.md)

#output tables
tab_model(ls.tf,
          ls.specf,
          ls.gf,
          ls.mf,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/LS.lmaherbfreq.html")

tab_model(ls.td,
          ls.specd,
          ls.gd,
          ls.md,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/LS.lmaherbdiv.html")

#LS significant plots: 
#Gall %
gallperc.LMA.LS <- ggplot(LSdata, aes(x=LMA, y=perc.gall*100, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd), width = 0.5)+
  scale_color_manual(values = Darjeeling1)+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Gall Damage Frequency (%)")+
  theme_cowplot()
gallperc.LMA.LS

LSlegend <- get_legend(gallperc.LMA.LS) #rerun with legend removed

#rerun figure after the legend is removed
gallperc.LMA.LS <- ggplot(LSdata, aes(x=LMA, y=perc.gall*100, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd), width = 0.5)+
  scale_color_manual(values = Darjeeling1)+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Gall Damage Frequency (%)")+
  theme_cowplot()+
  theme(legend.position="none")
gallperc.LMA.LS

#Gall diversity
galldiv.LMA.LS <- ggplot(LSdata, aes(x=LMA, y=div.gall, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd), width = 0.05)+
  scale_color_manual(values = Darjeeling1)+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Gall Damage Diversity")+
  theme_cowplot()+
  theme(legend.position="none")
galldiv.LMA.LS

#Mine %
mineperc.LMA.LS <- ggplot(LSdata, aes(x=LMA, y=perc.mine*100, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd))+
  scale_color_manual(values = Darjeeling1)+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Mine Damage Frequency (%)")+
  theme_cowplot()+
  theme(legend.position="none")
mineperc.LMA.LS

#Mine diversity
minediv.LMA.LS <- ggplot(LSdata, aes(x=LMA, y=div.mine, color=Species))+
  geom_point(aes(shape=Facies), size=6)+
  geom_errorbar(aes(xmin=LMA-LMA_sd, xmax=LMA+LMA_sd))+
  scale_color_manual(values = Darjeeling1)+
  xlab("Leaf Mass per Area (LMA; mean per species)")+
  ylab("Mine Damage Diversity")+
  theme_cowplot()+
  theme(legend.position="none")
minediv.LMA.LS

#Super plots----
HFLMA.super <- cowplot::plot_grid(gallperc.LMA.HF,
                                  galldiv.LMA.HF,
                                  HFlegend,
                                  nrow = 1, 
                                  ncol = 3,
                                  rel_widths = c(1,1,0.5),
                                  labels = c("A","B",""))
HFLMA.super

LSLMA.super <- cowplot::plot_grid(gallperc.LMA.LS,
                                  galldiv.LMA.LS,
                                  mineperc.LMA.LS,
                                  minediv.LMA.LS,
                                  LSlegend,
                                  nrow = 1, 
                                  ncol = 5,
                                  rel_widths = c(1,1,1,1,0.5),
                                  labels = c("A","B","C","D",""))
LSLMA.super

#Saving figures----
ggsave("figures/HFLMA.plot.pdf", HFLMA.super, width = 10, units = "in")
ggsave("figures/LSLMA.plot.pdf", LSLMA.super, width = 20, units = "in")


