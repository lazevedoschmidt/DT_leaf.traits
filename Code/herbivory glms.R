#01.27.2023
#Models for understanding plant traits influence on herbivory metrics
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
library(scales)
library(optimx)
library(lmerTest)

options(scipen = 99) # turn off sci notation
#load data
widedata <- read.csv("Cleaned_data/trait.wide.clean_data.csv")
newtraitHF <- read.csv("Cleaned_data/HFtrait.clean_data.csv")
newtraitMD <- read.csv("Cleaned_data/MDtrait.clean_data.csv")
newtraitLS <- read.csv("Cleaned_data/LStrait.clean_data.csv")
###############################################################################
#BELOW IS ALL PART OF STEP1_clean.data NOW
###############################################################################
# #load data
# #maybe use read_csv instead because it changes the dataframe into a tibble which is easier to deal with 
#   #in tidyverse
# traitdata <- read.csv("plant.traits_divadded.csv", header=T, sep=",", 
#                       na.strings=  "NA", dec=".", strip.white=TRUE)
# 
# #Data Transformation
# #Logit transformations for all percentage data
# #Herbivory
# 
# traitdata$perc.dam_logit <- logit(traitdata$perc.dam, percents = TRUE)
# traitdata$perc.spec_logit <- logit(traitdata$perc.spec, percents = TRUE)
# traitdata$perc.gall_logit <- logit(traitdata$perc.gall, percents = TRUE)
# traitdata$perc.mine_logit <- logit(traitdata$perc.mine, percents = TRUE)
# traitdata$perc.area.dam..mean._logit <- logit(traitdata$perc.area.dam..mean., percents = TRUE)
# 
# #Traits
# traitdata$wt.perc_C_logit <- logit(traitdata$wt.perc_C, percents = TRUE)
# traitdata$wt.perc_N_logit <- logit(traitdata$wt.perc_N, percents = TRUE)
# traitdata$perc.P_logit <- logit(traitdata$perc.P, percents = TRUE)
# traitdata$lignin.perc_logit <- logit(traitdata$lignin.perc., percents = TRUE)
# traitdata$tannin.perc.condenced_logit <- logit(traitdata$tannin.perc.condenced., 
#                                                percents = TRUE)
# traitdata$phenols.perc_logit <- logit(traitdata$phenols.perc, 
#                                       percents = TRUE)
# traitdata$cellulose.perc_logit <- logit(traitdata$cellulose.perc, 
#                                         percents =  TRUE)
# traitdata$trichomes..cm.2._logit <- logit(traitdata$trichomes..cm.2.,
#                                           percents = TRUE)
# traitdata$perc.water_logit <- logit(traitdata$perc.water, percents = TRUE)
# 
# #scale transform everything afterwards 
# #allows me to compare the beta distribution between all the variables.
# traitdata$perc.dam_scale <- scale(traitdata$perc.dam, center=TRUE, scale=TRUE)
# traitdata$perc.spec_scale <- scale(traitdata$perc.spec, center=TRUE, scale=TRUE)
# traitdata$perc.gall_scale <- scale(traitdata$perc.gall, center=TRUE, scale=TRUE)
# traitdata$perc.mine_scale <- scale(traitdata$perc.mine, center=TRUE, scale=TRUE)
# traitdata$perc.area.dam..mean._scale <- scale(traitdata$perc.area.dam..mean., center=TRUE, scale=TRUE)
# 
# #Traits
# traitdata$wt.perc_C_scale <- scale(traitdata$wt.perc_C, center=TRUE, scale=TRUE)
# traitdata$wt.perc_N_scale <- scale(traitdata$wt.perc_N, center=TRUE, scale=TRUE)
# traitdata$perc.P_scale <- scale(traitdata$perc.P, center=TRUE, scale=TRUE)
# traitdata$lignin.perc_scale <- scale(traitdata$lignin.perc., center=TRUE, scale=TRUE)
# traitdata$tannin.perc.condenced_scale <- scale(traitdata$tannin.perc.condenced., 
#                                                center=TRUE, scale=TRUE)
# traitdata$phenols.perc_scale <- scale(traitdata$phenols.perc, 
#                                       center=TRUE, scale=TRUE)
# traitdata$cellulose.perc_scale <- scale(traitdata$cellulose.perc, 
#                                         center=TRUE, scale=TRUE)
# traitdata$trichomes..cm.2._scale <- scale(traitdata$trichomes..cm.2.,
#                                           center=TRUE, scale=TRUE)
# traitdata$perc.water_scale <- scale(traitdata$perc.water, center=TRUE, scale=TRUE)
# 
# #Scaling non-percentage data using the 'scale' function
# #Herbivory
# traitdata$div.dam_scale <- scale(traitdata$div.dam, center= TRUE,scale=TRUE) 
# traitdata$div.spec_scale <- scale(traitdata$div.spec, center= TRUE,scale=TRUE)
# traitdata$div.gall_scale <- scale(traitdata$div.gall, center= TRUE,scale=TRUE)
# traitdata$div.mine_scale <- scale(traitdata$div.mine, center= TRUE,scale=TRUE)
# 
# #Traits 
# traitdata$CN_scale <- scale(traitdata$C.N, center= TRUE,scale=TRUE)
# traitdata$LMA_scale <- scale(traitdata$LMA..mean., center= TRUE,scale=TRUE)
# traitdata$trichomes_scale <- scale(traitdata$trichomes, center= TRUE,scale=TRUE)
# traitdata$margin_type_scale <- scale(traitdata$margin_type, center= TRUE,scale=TRUE)
# 
# #Cleaning data
# #Switch to saving cleaned data as a table so that you can load it back in in other scripts
# #without having to re-clean everything? 
# newtraitd <- traitdata %>% 
#   select(!c(Spec.Abrev,margin_type, trichomes, perc.dam:wt.perc_C, perc.P:perc.water, C.N, LMA..mean.,
#             SLA:leaf.touchness..N.,trichomes..cm.2.:perc.water_logit))
# #removing unnneeded columns so that we only have the transformed ones. 
# #Doing this by column name rather than position because that's a stupid way to do it. 
# 
# #long data
# longdata_rep <- gather(newtraitd, trait, value_rep, perc.dam_scale:margin_type_scale, factor_key=TRUE)
# longdata_rep <- longdata_rep %>%
#   select(!c(ID, Forest, Facies))
# 
# #taking averages to eliminate replicate data
# longdata <- longdata_rep %>% 
#   group_by(Quarry,Species,trait) %>% #grouped by Year variable
#   summarise_each(funs(value=mean(., na.rm=TRUE))) %>%
# #when you specify the function, `summarise_each will applies the function 
# #(here it is mean) to each of the columns in the dataset or a subset of columns (if specified)
#   mutate(Forest = case_when(Quarry %in% c("HF1901", "HF1902", "HF1903") ~ "Harvard Forest",
#                            Quarry %in% c("LS1901", "LS1902", "LS1903") ~ "La Selva",
#                            Quarry %in% c("MD1901", "MD1902", "MD1903") ~ "SERC"))
# #now need to turn long data into wide
# widedata <- spread(longdata, trait, value)
# 
# #Harvard Forest
# newtraitHF <- widedata %>%
#   filter(Forest == "Harvard Forest") %>%
#   select(!c(perc.water_scale,trichomes..cm.2._scale))
# newtraitHF[newtraitHF == "NaN"] <- NA #converting NaNs to NA so R can drop them later
# 
# #SERC
# newtraitMD <- widedata %>%
#   filter(Forest == "SERC") %>%
#   select(!c(lignin.perc_scale:phenols.perc_scale,cellulose.perc_scale))
# newtraitMD[newtraitMD == "NaN"]<- NA
# 
# #La Selva
# newtraitLS <- widedata %>%
#   filter(Forest == "La Selva")%>%
#   select(!c(perc.water_scale,trichomes..cm.2._scale))
# newtraitLS[newtraitLS == "NaN"] <- NA
###############################################################################
###############################################################################

#All forest models----
#All forests binned together so only using independent variables that are present/measured within each forest
#ID needs to be used as the random effect because it has replicates in it
#wrapping lme4 and lmerTest together so I can get p-values
#C:N is super correlated with perc. C and N but, I do want to know about the ratio and its influence
   #on herbivory so, maybe we should use it? 

#Use figure from landscape paper to show that we don't need to account for forest in some of the models but should if the forest does differ
#look at boxplot figure 
#Forest is accounted for in the model (lmer) when it has previously been shown to influence insect herbivory 
#and when it is not needed, simple glms were used. 

#wt.perc_N and C are highly correlatd with CN (obviously) so they were removed from the models but will be used in the random forests to see
#how the hierarchical structure in the regression tree plays out. 

#Setting seed for reproducability: 
set.seed(888)

tf <- glm(perc.dam_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
            perc.P_scale, data = widedata)
summary(tf)
AIC(tf)

td <- glm(div.dam_scale ~ CN_scale + LMA_scale+ wt.perc_N_scale +wt.perc_C_scale + 
            perc.P_scale, data = widedata)
summary(td) 

sf <- glm(perc.spec_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
            perc.P_scale, data = widedata)
summary(sf) #significant!!!!

#need to add Forest as random variable
sd <- lmerTest::lmer(div.spec_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale +  
                       (1|Forest), data = widedata)
summary(sd) #phosphorus was removed to fix singularity
AIC(sd)

gf <- lmerTest::lmer(perc.gall_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
            perc.P_scale + (1|Forest), data = widedata)
summary(gf) #LMA is very significant 

gd <- lmerTest::lmer(div.gall_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale +
            perc.P_scale + (1|Forest), data = widedata)
summary(gd) #model is significant when wt.perc. N and C are removed. Why? Is it because or correlation of fixed effects? 
AIC(gd)

mf <- lmerTest::lmer(perc.mine_scale ~ CN_scale + (1|Forest), data = widedata)
summary(mf) 

md <- lmerTest::lmer(div.mine_scale ~ CN_scale + (1|Forest), data = widedata)
summary(md) 

#All Forest area damaged----
area.all <- glm(perc.area.dam..mean._scale ~ CN_scale + LMA_scale + wt.perc_N_scale + wt.perc_C_scale + 
                             perc.P_scale, data = widedata)
summary(area.all)
AIC(area.all)

#All Forest Tables----
tab_model(tf,
          sf,
          gf,
          mf,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/alltab.freq.html")
tab_model(td,
          sd,
          gd,
          md,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/alltab.div.html")
tab_model(area.all,
          dv.labels = "% Area Damaged",
          show.intercept = FALSE,
          file = "tables/all.areaperc.html")

#HF specific models----
tf.hf <- glm(perc.dam_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale + trichomes_scale , data = newtraitHF)
summary(tf.hf) 
AIC(tf.hf) #28.32
#Best model, not singular, and secondary compounds do not improve

td.hf <- glm(div.dam_scale ~ CN_scale * LMA_scale+ wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale , data = newtraitHF)
summary(td.hf) 
AIC(td.hf) #34.95
#Best model, not singular, and secondary compounds do not improve

sf.hf <- glm(perc.spec_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale + trichomes_scale, data = newtraitHF)
summary(sf.hf) 
AIC(sf.hf) #15.10
#Best model, not singular, and secondary compounds do not improve 

sd.hf <- glm(div.spec_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale , data = newtraitHF)
summary(sd.hf) 
AIC(sd.hf) #35.18
#Best model, not singular, and secondary compounds do not improve

gf.hf <- glm(perc.gall_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale , data = newtraitHF)
summary(gf.hf) #sig
AIC(gf.hf) #11.67
#Best model, not singular, and secondary compounds do not improve

gd.hf <- glm(div.gall_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale + trichomes_scale, data = newtraitHF)
summary(gd.hf) #almost sig
AIC(gd.hf) #2.29
#Best model, not singular, and secondary compounds do not improve

mf.hf <- glm(perc.mine_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale + lignin.perc_scale, data = newtraitHF)
summary(mf.hf) 
AIC(mf.hf) #10.05
#Best model, not singular, and lignin improves this model! 

md.hf <- glm(div.mine_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale + trichomes_scale, data = newtraitHF)
summary(md.hf) #not singular and sig
AIC(md.hf) #22.17
#Best model, not singular, and secondary compounds do not improve

#HF area damaged----
area.hf <- glm(perc.area.dam..mean._scale ~ CN_scale * LMA_scale + wt.perc_N_scale *wt.perc_C_scale + 
                 perc.P_scale + trichomes_scale, data = newtraitHF)
summary(area.hf) #Higher AIC with CN + LMA instead of *  
AIC(area.hf) #44.58

#HF tables----
tab_model(tf.hf,
          sf.hf,
          gf.hf,
          mf.hf,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/HF.freq.html")
tab_model(td.hf,
          sd.hf,
          gd.hf,
          md.hf,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/HF.div.html")
tab_model(area.hf,
          dv.labels = "% Area Damaged",
          show.intercept = FALSE,
          file = "tables/HF.areaperc.html")

#MD specific models----
tf.md <- glm(perc.dam_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  margin_type_scale, data = newtraitMD)
summary(tf.md) #sig
AIC(tf.md) #8.64 
#Best model, not singular, and secondary compounds do not improve

td.md <- glm(div.dam_scale ~ CN_scale + LMA_scale+ wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  margin_type_scale, data = newtraitMD)
summary(td.md) #sig
AIC(td.md) #2.81
#Best model, not singular, and secondary compounds do not improve

sf.md <- glm(perc.spec_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  margin_type_scale, data = newtraitMD)
summary(sf.md) #sig
AIC(sf.md) #16.63
#Best model, not singular, and secondary compounds do not improve

sd.md <- glm(div.spec_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  perc.water_scale, data = newtraitMD)
summary(sd.md) 
AIC(sd.md) #17.18
#Best model, not singular, and water actually improves the model 

gf.md <- glm(perc.gall_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  margin_type_scale, data = newtraitMD)
summary(gf.md) #sig
AIC(gf.md) #-9.72
#Best model, not singular, and secondary compounds do not improve

gd.md <- glm(div.gall_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  margin_type_scale, data = newtraitMD)
summary(gd.md) 
AIC(gd.md) #15.06 
#Best model, not singular, and secondary compounds do not improve

mf.md <- glm(perc.mine_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  trichomes_scale, data = newtraitMD)
summary(mf.md)
AIC(mf.md) #31.73
#Best model, not singular, and secondary compounds do not improve

md.md <- glm(div.mine_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               perc.P_scale +  perc.water_scale, data = newtraitMD)
summary(md.md)  
AIC(md.md) #37.60
#Best model, not singular, and water improves!!

#MD area damaged----
area.md <- glm(perc.area.dam..mean._scale ~ CN_scale+ LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
                 perc.P_scale +  perc.water_scale, data = newtraitMD)
summary(area.md) #only model that can be run all others are singular  
AIC(area.md) #35.57

#MD tables----
tab_model(tf.md,
          sf.md,
          gf.md,
          mf.md,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/MD.freq.html")
tab_model(td.md,
          sd.md,
          gd.md,
          md.md,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/MD.div.html")
tab_model(area.md,
          dv.labels = "% Area Damaged",
          show.intercept = FALSE,
          file = "tables/MD.areaperc.html")

#LS specific models----
#can't use phosphorus because there isn't enough data
tf.ls <- glm(perc.dam_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale +
               trichomes_scale, data = newtraitLS)
summary(tf.ls) 
AIC(tf.ls) #51.87
#Best model, not singular, and secondary compounds do not improve

td.ls <- glm(div.dam_scale ~ CN_scale + LMA_scale+ wt.perc_N_scale +wt.perc_C_scale + 
               trichomes_scale, data = newtraitLS)
summary(td.ls) 
AIC(td.ls) #46.98
#Best model, not singular, and secondary compounds do not improve

sf.ls <- glm(perc.spec_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               trichomes_scale, data = newtraitLS)
summary(sf.ls) 
AIC(sf.ls) #33.78
#Best model, not singular, and secondary compounds do not improve

sd.ls <- glm(div.spec_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               trichomes_scale, data = newtraitLS)
summary(sd.ls)
AIC(sd.ls) #6.01
#Best model, not singular, and secondary compounds do not improve

gf.ls <- glm(perc.gall_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale, data = newtraitLS)
summary(gf.ls) #sig
AIC(gf.ls) #28.85
#Best model, not singular, and secondary compounds do not improve

gd.ls <- glm(div.gall_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               trichomes_scale, data = newtraitLS)
summary(gd.ls) #sig
AIC(gd.ls) #36.69
#Best model, not singular, and secondary compounds do not improve

mf.ls <- glm(perc.mine_scale ~ CN_scale + LMA_scale + wt.perc_N_scale +wt.perc_C_scale, data = newtraitLS)
summary(mf.ls) 
AIC(mf.ls) #25.88
#Best model, not singular, and secondary compounds do not improve

md.ls <- glm(div.mine_scale ~ CN_scale * LMA_scale + wt.perc_N_scale +wt.perc_C_scale + 
               trichomes_scale, data = newtraitLS)
summary(md.ls) #sig
AIC(md.ls) #28.03
#Best model, not singular, and secondary compounds do not improve

#LS area damaged----
area.ls <- glm(perc.area.dam..mean._scale ~ CN_scale + LMA_scale + wt.perc_N_scale * wt.perc_C_scale + 
                 trichomes_scale, data = newtraitLS)
summary(area.ls)   
AIC(area.ls) #46.60

#LS tables----
tab_model(tf.ls,
          sf.ls,
          gf.ls,
          mf.ls,
          dv.labels = c("Total Damage (%)", "Specialized Damage (%)", "Gall Damgae (%)",
                        "Mine Damage (%)"),
          show.intercept = FALSE,
          file = "tables/LS.freq.html")
tab_model(td.ls,
          sd.ls,
          gd.ls,
          md.ls,
          dv.labels = c("Total Damage Diversity", "Specialized Damage Diversity",
                        "Gall Damage Diversity", "Mine Damage Diversity"),
          show.intercept = FALSE,
          file = "tables/LS.div.html")
tab_model(area.ls,
          dv.labels = "% Area Damaged",
          show.intercept = FALSE,
          file = "tables/LS.areaperc.html")













