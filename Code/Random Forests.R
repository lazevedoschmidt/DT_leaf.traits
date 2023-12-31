#random forest analysis
#downside: 
  #black box of variable importance
  #relative importance
  #can get out prediction
#nice things: 
  #handels non-linear and interactions easily 
#Do them in addition to the linear models

#interactions between the variables fore example: LMA and nitrogen
  #For example do herbibores like thick leaves but only if they also have nitrogen in them? 

#Use forest as input varialbe (right hand side of the 'twiggle' ~ )

#takes regression trees and bootstraps them to make them more stable and produces a random forest

#packages
#tree or rpart to actualy print out the tree
#random forest package
  #can I get out a visualization for it. 

#Purpose: Random Forest analyses for herb ~ trait data
#Date: 02.27.2023
#Author: LAS
#R version: 4.2.2. 

#load packages
# library(tree)
# library(rpart)
#install.packages("randomForest")
library(randomForest) 
library(tidyverse)
library(ggplot2)
#install.packages("cowplot")
library(cowplot)
library(sjPlot) #need for SI tables 

#load in clean data
trait.data <- read_csv("Cleaned_data/trait.wide.clean_data_V2.csv")
str(trait.data) #structure looks fine for the dataset
hfdata <-  read_csv("Cleaned_data/HFtrait.clean_data_V2.csv")
mddata <-  read_csv("Cleaned_data/MDtrait.clean_data_V2.csv")
lsdata <-  read_csv("Cleaned_data/LStrait.clean_data_V2.csv")

#removing unneeded columns
trait.data2 <- trait.data %>%
  select(!c("Quarry", "Species", "Forest","trichomes..cm.2._scale", "perc.water_scale")) #removing columns
  #dataset can't have any NAs for randomForest analysis

hfdata <-  hfdata %>%
  select(!c("Quarry", "Species", "Forest")) %>%
  drop_na()

mddata <- mddata %>%
  select(!c("Quarry", "Species", "Forest")) %>%
  drop_na()

lsdata <- lsdata %>%
  select(!c("Quarry", "Species", "Forest")) %>%
  drop_na()

#All forest data----
#dataset = traitdata2

#subset each damage type on it's own so it can be compared to the trait data
#total damage
perc.dam <- trait.data2 %>%
  select(perc.dam_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale) %>%
  drop_na()
div.dam <- trait.data2 %>%
  select(div.dam_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#spec
perc.spec <- trait.data2 %>%
  select(perc.spec_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
div.spec <- trait.data2 %>%
  select(div.spec_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#gall
perc.gall <- trait.data2 %>%
  select(perc.gall_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
div.gall <- trait.data2 %>%
  select(div.gall_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#mine
perc.mine <- trait.data2 %>%
  select(perc.mine_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
div.mine <- trait.data2 %>%
  select(div.mine_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#area damaged
perc.area <- trait.data2 %>%
  select(perc.area.dam..mean._scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#HF datasets----
hfperc.dam <- hfdata %>%
  select(perc.dam_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale) %>%
  drop_na()
hfdiv.dam <- hfdata %>%
  select(div.dam_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#spec
hfperc.spec <- hfdata %>%
  select(perc.spec_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
hfdiv.spec <- hfdata %>%
  select(div.spec_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#gall
hfperc.gall <- hfdata %>%
  select(perc.gall_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
hfdiv.gall <- hfdata %>%
  select(div.gall_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#mine
hfperc.mine <- hfdata %>%
  select(perc.mine_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
hfdiv.mine <- hfdata %>%
  select(div.mine_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#area damaged
hfperc.area <- hfdata %>%
  select(perc.area.dam..mean._scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#MD datasets----
mdperc.dam <- mddata %>%
  select(perc.dam_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale) %>%
  drop_na()
mddiv.dam <- mddata %>%
  select(div.dam_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#spec
mdperc.spec <- mddata %>%
  select(perc.spec_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()
mddiv.spec <- mddata %>%
  select(div.spec_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#gall
mdperc.gall <- mddata %>%
  select(perc.gall_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()
mddiv.gall <- mddata %>%
  select(div.gall_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#mine
mdperc.mine <- mddata %>%
  select(perc.mine_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()
mddiv.mine <- mddata %>%
  select(div.mine_scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#area damaged
mdperc.area <- mddata %>%
  select(perc.area.dam..mean._scale,wt.perc_C_scale:perc.water_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#LS datasets----
lsperc.dam <- lsdata %>%
  select(perc.dam_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale) %>%
  drop_na()
lsdiv.dam <- lsdata %>%
  select(div.dam_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#spec
lsperc.spec <- lsdata %>%
  select(perc.spec_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
lsdiv.spec <- lsdata %>%
  select(div.spec_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#gall
lsperc.gall <- lsdata %>%
  select(perc.gall_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
lsdiv.gall <- lsdata %>%
  select(div.gall_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#mine
lsperc.mine <- lsdata %>%
  select(perc.mine_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()
lsdiv.mine <- lsdata %>%
  select(div.mine_scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#area damaged
lsperc.area <- lsdata %>%
  select(perc.area.dam..mean._scale,wt.perc_C_scale:cellulose.perc_scale, CN_scale:margin_type_scale)%>%
  drop_na()

#seeting seed so that the data is reproducible because we are using random numbers in the randomForest
set.seed(888)

#Creating impute file first
#total.perc.imputed <-  rfImpute(perc.dam_scale ~ ., data = perc.dam, inter=6) #perc.dam is predicted by all other columns in the dataset (.). Inter means the iteractions used (4-6 is usually fine)
#maybe I don't need to do this?? Why was the impute originally created? What does impute do? 

#% Var explained noted on each forest only if greater than 0

#All forest randomForests----
#creating randomForest for all forests
perc.dam.rF <-randomForest(perc.dam_scale ~ ., data = perc.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) #taking the damage data within the imputed dataframe created above
perc.dam.rF
div.dam.rF <-randomForest(div.dam_scale ~ ., data = div.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
div.dam.rF

perc.spec.rF <-randomForest(perc.spec_scale ~ ., data = perc.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
perc.spec.rF #explains 42.96% of the data
div.spec.rF <-randomForest(div.spec_scale ~ ., data = div.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
div.spec.rF #explains 27.1% of the data

perc.gall.rF <-randomForest(perc.gall_scale ~ ., data = perc.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
perc.gall.rF #explains 55.6% of the data
div.gall.rF <-randomForest(div.gall_scale ~ ., data = div.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
div.gall.rF #explains 47.6% of the data

perc.mine.rF <-randomForest(perc.mine_scale ~ ., data = perc.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
perc.mine.rF
div.mine.rF <-randomForest(div.mine_scale ~ ., data = div.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
div.mine.rF # explains 14.8% of the data 

perc.area.rF <-randomForest(perc.area.dam..mean._scale ~ ., data = perc.area, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
perc.area.rF

#HF randomForests----
hfperc.dam.rF <-randomForest(perc.dam_scale ~ ., data = hfperc.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) #taking the damage data within the imputed dataframe created above
hfperc.dam.rF # explains 49.4% of the data
hfdiv.dam.rF <-randomForest(div.dam_scale ~ ., data = hfdiv.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfdiv.dam.rF #explains 50.2% 


hfperc.spec.rF <-randomForest(perc.spec_scale ~ ., data = hfperc.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfperc.spec.rF #explains 75.5% 
hfdiv.spec.rF <-randomForest(div.spec_scale ~ ., data = hfdiv.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfdiv.spec.rF #explains 51.8% 


hfperc.gall.rF <-randomForest(perc.gall_scale ~ ., data = hfperc.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfperc.gall.rF # explains 71.3%
hfdiv.gall.rF <-randomForest(div.gall_scale ~ ., data = hfdiv.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfdiv.gall.rF #explains 60.3%


hfperc.mine.rF <-randomForest(perc.mine_scale ~ ., data = hfperc.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfperc.mine.rF
hfdiv.mine.rF <-randomForest(div.mine_scale ~ ., data = hfdiv.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfdiv.mine.rF #explains 3.24%

hfperc.area.rF <-randomForest(perc.area.dam..mean._scale ~ ., data = hfperc.area, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
hfperc.area.rF

#MD randomForests----
mdperc.dam.rF <-randomForest(perc.dam_scale ~ ., data = mdperc.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) #taking the damage data within the imputed dataframe created above
mdperc.dam.rF #explains 47.3%
mddiv.dam.rF <-randomForest(div.dam_scale ~ ., data = mddiv.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mddiv.dam.rF #explains 17.57% 


mdperc.spec.rF <-randomForest(perc.spec_scale ~ ., data = mdperc.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mdperc.spec.rF #explains 58.0%
mddiv.spec.rF <-randomForest(div.spec_scale ~ ., data = mddiv.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mddiv.spec.rF #explains 48.3%


mdperc.gall.rF <-randomForest(perc.gall_scale ~ ., data = mdperc.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mdperc.gall.rF
mddiv.gall.rF <-randomForest(div.gall_scale ~ ., data = mddiv.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mddiv.gall.rF

mdperc.mine.rF <-randomForest(perc.mine_scale ~ ., data = mdperc.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mdperc.mine.rF
mddiv.mine.rF <-randomForest(div.mine_scale ~ ., data = mddiv.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mddiv.mine.rF #explains 8.08%

mdperc.area.rF <-randomForest(perc.area.dam..mean._scale ~ ., data = mdperc.area, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
mdperc.area.rF

#LS randomForests----
lsperc.dam.rF <-randomForest(perc.dam_scale ~ ., data = lsperc.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) #taking the damage data within the imputed dataframe created above
lsperc.dam.rF
lsdiv.dam.rF <-randomForest(div.dam_scale ~ ., data = lsdiv.dam, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsdiv.dam.rF

lsperc.spec.rF <-randomForest(perc.spec_scale ~ ., data = lsperc.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsperc.spec.rF
lsdiv.spec.rF <-randomForest(div.spec_scale ~ ., data = lsdiv.spec, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsdiv.spec.rF

lsperc.gall.rF <-randomForest(perc.gall_scale ~ ., data = lsperc.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsperc.gall.rF #explains 6.35%
lsdiv.gall.rF <-randomForest(div.gall_scale ~ ., data = lsdiv.gall, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsdiv.gall.rF

lsperc.mine.rF <-randomForest(perc.mine_scale ~ ., data = lsperc.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsperc.mine.rF
lsdiv.mine.rF <-randomForest(div.mine_scale ~ ., data = lsdiv.mine, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsdiv.mine.rF

lsperc.area.rF <-randomForest(perc.area.dam..mean._scale ~ ., data = lsperc.area, ntree= 2000, keep.forest=FALSE, importance=TRUE) 
lsperc.area.rF

###############################################################################
#Combined data files----
#perc.dam data #all data regardless of significant variability explained
# ImpData1 <-  as.data.frame(importance(perc.dam.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData1$Var.Names <-  row.names(ImpData1)
# 
# ImpData10 <-  as.data.frame(importance(hfperc.dam.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData10$Var.Names <-  row.names(ImpData10)
# 
# ImpData20 <-  as.data.frame(importance(mdperc.dam.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData20$Var.Names <-  row.names(ImpData20)
# 
# ImpData30 <-  as.data.frame(importance(lsperc.dam.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData30$Var.Names <-  row.names(ImpData30)
# 
# perc.dam.ImpData <- bind_rows(ImpData1,ImpData10)
# perc.dam.ImpData <- bind_rows(perc.dam.ImpData,ImpData20)
# perc.dam.ImpData <- bind_rows(perc.dam.ImpData,ImpData30)

#only forests with greater than 5% variability explained
varex.ImpData10 <-  as.data.frame(importance(hfperc.dam.rF)) %>%
    add_column(Forest = "Harvard Forest") %>%
    drop_na() #removing the variables that have NaN's
varex.ImpData10$Var.Names <-  row.names(varex.ImpData10)

varex.ImpData20 <-  as.data.frame(importance(mdperc.dam.rF)) %>%
    add_column(Forest = "SERC")%>%
    drop_na() #removing the variables that have NaN's
varex.ImpData20$Var.Names <-  row.names(varex.ImpData20)

perc.dam.ImpData.varex <- bind_rows(varex.ImpData10,varex.ImpData20)

#div.dam data
# ImpData2 <-  as.data.frame(importance(div.dam.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData2$Var.Names <-  row.names(ImpData2)
# 
# ImpData11 <-  as.data.frame(importance(hfdiv.dam.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData11$Var.Names <-  row.names(ImpData11)
# 
# ImpData21 <-  as.data.frame(importance(mddiv.dam.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData21$Var.Names <-  row.names(ImpData21)
# 
# ImpData31 <-  as.data.frame(importance(lsdiv.dam.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData31$Var.Names <-  row.names(ImpData31)
# 
# div.dam.ImpData <- bind_rows(ImpData2,ImpData11)
# div.dam.ImpData <- bind_rows(div.dam.ImpData,ImpData21)
# div.dam.ImpData <- bind_rows(div.dam.ImpData,ImpData31)

varex.ImpData11 <-  as.data.frame(importance(hfdiv.dam.rF)) %>%
  add_column(Forest = "Harvard Forest") %>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData11$Var.Names <-  row.names(varex.ImpData11)

varex.ImpData21 <-  as.data.frame(importance(mddiv.dam.rF)) %>%
  add_column(Forest = "SERC")%>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData21$Var.Names <-  row.names(varex.ImpData21)

div.dam.ImpData.varex <- bind_rows(varex.ImpData11,varex.ImpData21)

#perc.spec
# ImpData3 <-  as.data.frame(importance(perc.spec.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData3$Var.Names <-  row.names(ImpData3)
# 
# ImpData12 <-  as.data.frame(importance(hfperc.spec.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData12$Var.Names <-  row.names(ImpData12)
# 
# ImpData22 <-  as.data.frame(importance(mdperc.spec.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData22$Var.Names <-  row.names(ImpData22)
# 
# ImpData32 <-  as.data.frame(importance(lsperc.spec.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData32$Var.Names <-  row.names(ImpData32)
# 
# perc.spec.ImpData <- bind_rows(ImpData3,ImpData12)
# perc.spec.ImpData <- bind_rows(perc.spec.ImpData,ImpData22)
# perc.spec.ImpData <- bind_rows(perc.spec.ImpData,ImpData32)

varex.ImpData3 <-  as.data.frame(importance(perc.spec.rF)) %>%
  add_column(Forest = "All")%>%
  drop_na()  #removing the variables that have NaN's 
varex.ImpData3$Var.Names <-  row.names(varex.ImpData3)

varex.ImpData12 <-  as.data.frame(importance(hfperc.spec.rF)) %>%
  add_column(Forest = "Harvard Forest") %>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData12$Var.Names <-  row.names(varex.ImpData12)

varex.ImpData22 <-  as.data.frame(importance(mdperc.spec.rF)) %>%
  add_column(Forest = "SERC")%>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData22$Var.Names <-  row.names(varex.ImpData22)

perc.spec.ImpData.varex <- bind_rows(varex.ImpData3,varex.ImpData12,varex.ImpData22)

#div.spec
# ImpData4 <-  as.data.frame(importance(div.spec.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData4$Var.Names <-  row.names(ImpData4)
# 
# ImpData13 <-  as.data.frame(importance(hfdiv.spec.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData13$Var.Names <-  row.names(ImpData13)
# 
# ImpData23 <-  as.data.frame(importance(mddiv.spec.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData23$Var.Names <-  row.names(ImpData23)
# 
# ImpData33 <-  as.data.frame(importance(lsdiv.spec.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData33$Var.Names <-  row.names(ImpData33)
# 
# div.spec.ImpData <- bind_rows(ImpData4,ImpData13)
# div.spec.ImpData <- bind_rows(div.spec.ImpData,ImpData23)
# div.spec.ImpData <- bind_rows(div.spec.ImpData,ImpData33)

varex.ImpData4 <-  as.data.frame(importance(div.spec.rF)) %>%
  add_column(Forest = "All")%>%
  drop_na()  #removing the variables that have NaN's 
varex.ImpData4$Var.Names <-  row.names(varex.ImpData4)

varex.ImpData13 <-  as.data.frame(importance(hfdiv.spec.rF)) %>%
  add_column(Forest = "Harvard Forest") %>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData13$Var.Names <-  row.names(varex.ImpData13)

varex.ImpData23 <-  as.data.frame(importance(mddiv.spec.rF)) %>%
  add_column(Forest = "SERC")%>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData23$Var.Names <-  row.names(varex.ImpData23)

div.spec.ImpData.varex <- bind_rows(varex.ImpData4,varex.ImpData13,varex.ImpData23)

#perc.gall
# ImpData5 <-  as.data.frame(importance(perc.gall.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData5$Var.Names <-  row.names(ImpData5)
# 
# ImpData14 <-  as.data.frame(importance(hfperc.gall.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData14$Var.Names <-  row.names(ImpData14)
# 
# ImpData24 <-  as.data.frame(importance(mdperc.gall.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData24$Var.Names <-  row.names(ImpData24)
# 
# ImpData34 <-  as.data.frame(importance(lsperc.gall.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData34$Var.Names <-  row.names(ImpData34)
# 
# perc.gall.ImpData <- bind_rows(ImpData5,ImpData14)
# perc.gall.ImpData <- bind_rows(perc.gall.ImpData,ImpData24)
# perc.gall.ImpData <- bind_rows(perc.gall.ImpData,ImpData34)

varex.ImpData5 <-  as.data.frame(importance(perc.gall.rF)) %>%
  add_column(Forest = "All")%>%
  drop_na()  #removing the variables that have NaN's 
varex.ImpData5$Var.Names <-  row.names(varex.ImpData5)

varex.ImpData14 <-  as.data.frame(importance(hfperc.gall.rF)) %>%
  add_column(Forest = "Harvard Forest") %>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData14$Var.Names <-  row.names(varex.ImpData14)

varex.ImpData34 <-  as.data.frame(importance(lsperc.gall.rF)) %>%
  add_column(Forest = "La Selva")%>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData34$Var.Names <-  row.names(varex.ImpData34)

perc.gall.ImpData.varex <- bind_rows(varex.ImpData5,varex.ImpData14,varex.ImpData34)

#div.gall
# ImpData6 <-  as.data.frame(importance(div.gall.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData6$Var.Names <-  row.names(ImpData6)
# 
# ImpData15 <-  as.data.frame(importance(hfdiv.gall.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData15$Var.Names <-  row.names(ImpData15)
# 
# ImpData25 <-  as.data.frame(importance(mddiv.gall.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData25$Var.Names <-  row.names(ImpData25)
# 
# ImpData35 <-  as.data.frame(importance(lsdiv.gall.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData35$Var.Names <-  row.names(ImpData35)
# 
# div.gall.ImpData <- bind_rows(ImpData6,ImpData15)
# div.gall.ImpData <- bind_rows(div.gall.ImpData,ImpData25)
# div.gall.ImpData <- bind_rows(div.gall.ImpData,ImpData35)
varex.ImpData6 <-  as.data.frame(importance(div.gall.rF)) %>%
  add_column(Forest = "All")%>%
  drop_na()  #removing the variables that have NaN's 
varex.ImpData6$Var.Names <-  row.names(varex.ImpData6)

varex.ImpData15 <-  as.data.frame(importance(hfdiv.gall.rF)) %>%
  add_column(Forest = "Harvard Forest") %>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData15$Var.Names <-  row.names(varex.ImpData15)

div.gall.ImpData.varex <- bind_rows(varex.ImpData6,varex.ImpData15)

# #perc.mine 
# ImpData7 <-  as.data.frame(importance(perc.mine.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's 
# ImpData7$Var.Names <-  row.names(ImpData7)
# 
# ImpData16 <-  as.data.frame(importance(hfperc.mine.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's 
# ImpData16$Var.Names <-  row.names(ImpData16)
# 
# ImpData26 <-  as.data.frame(importance(mdperc.mine.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData26$Var.Names <-  row.names(ImpData26)
# 
# ImpData36 <-  as.data.frame(importance(lsperc.mine.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's 
# ImpData36$Var.Names <-  row.names(ImpData36)
# 
# perc.mine.ImpData <- bind_rows(ImpData7,ImpData16)
# perc.mine.ImpData <- bind_rows(perc.mine.ImpData,ImpData26)
# perc.mine.ImpData <- bind_rows(perc.mine.ImpData,ImpData36)

#div.mine
# ImpData8 <-  as.data.frame(importance(div.mine.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's
# ImpData8$Var.Names <-  row.names(ImpData8)
# 
# ImpData17 <-  as.data.frame(importance(hfdiv.mine.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's
# ImpData17$Var.Names <-  row.names(ImpData17)
# 
# ImpData27 <-  as.data.frame(importance(mddiv.mine.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's
# ImpData27$Var.Names <-  row.names(ImpData27)
# 
# ImpData37 <-  as.data.frame(importance(lsdiv.mine.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's
# ImpData37$Var.Names <-  row.names(ImpData37)
# 
# div.mine.ImpData <- bind_rows(ImpData8,ImpData17)
# div.mine.ImpData <- bind_rows(div.mine.ImpData,ImpData27)
# div.mine.ImpData <- bind_rows(div.mine.ImpData,ImpData37)

varex.ImpData8 <-  as.data.frame(importance(div.mine.rF)) %>%
  add_column(Forest = "All")%>%
  drop_na()  #removing the variables that have NaN's 
varex.ImpData8$Var.Names <-  row.names(varex.ImpData8)

varex.ImpData27 <-  as.data.frame(importance(mddiv.mine.rF)) %>%
  add_column(Forest = "SERC")%>%
  drop_na() #removing the variables that have NaN's 
varex.ImpData27$Var.Names <-  row.names(varex.ImpData27)

div.mine.ImpData.varex <- bind_rows(varex.ImpData8,varex.ImpData27)

# #area damaged
# varex.ImpData9 <-  as.data.frame(importance(perc.area.rF)) %>%
#   add_column(Forest = "All")%>%
#   drop_na()  #removing the variables that have NaN's
# varex.ImpData9$Var.Names <-  row.names(varex.ImpData9)
# 
# varex.ImpData18 <-  as.data.frame(importance(hfperc.area.rF)) %>%
#   add_column(Forest = "Harvard Forest") %>%
#   drop_na() #removing the variables that have NaN's
# varex.ImpData18$Var.Names <-  row.names(varex.ImpData18)
# 
# varex.ImpData28 <-  as.data.frame(importance(mdperc.area.rF)) %>%
#   add_column(Forest = "SERC")%>%
#   drop_na() #removing the variables that have NaN's
# varex.ImpData28$Var.Names <-  row.names(varex.ImpData28)
# 
# varex.ImpData38 <-  as.data.frame(importance(lsperc.area.rF)) %>%
#   add_column(Forest = "La Selva")%>%
#   drop_na() #removing the variables that have NaN's
# varex.ImpData38$Var.Names <-  row.names(varex.ImpData38)
# # 
# perc.area.ImpData.varex <- bind_rows(varex.ImpData9,varex.ImpData18)
# perc.area.ImpData.varex <- bind_rows(perc.area.ImpData.varex,varex.ImpData28)
# perc.area.ImpData.varex <- bind_rows(perc.area.ImpData.varex,varex.ImpData38)

############################################################################
#Supplementary rF tables----
#all
capture.output(perc.dam.rF,
               div.dam.rF,
               perc.spec.rF,
               div.spec.rF,
               perc.gall.rF,
               div.gall.rF,
               perc.mine.rF,
               div.mine.rF,
               perc.area.rF,
               file = "rF.all.SI", 
               append = TRUE)

#HF
capture.output(hfperc.dam.rF,
               hfdiv.dam.rF,
               hfperc.spec.rF,
               hfdiv.spec.rF,
               hfperc.gall.rF,
               hfdiv.gall.rF,
               hfperc.mine.rF,
               hfdiv.mine.rF,
               hfperc.area.rF,
               file = "rF.HF.SI", 
               append = TRUE)

#MD
capture.output(mdperc.dam.rF,
               mddiv.dam.rF,
               mdperc.spec.rF,
               mddiv.spec.rF,
               mdperc.gall.rF,
               mddiv.gall.rF,
               mdperc.mine.rF,
               mddiv.mine.rF,
               mdperc.area.rF,
               file = "rF.MD.SI", 
               append = TRUE)

#LS
capture.output(lsperc.dam.rF,
               lsdiv.dam.rF,
               lsperc.spec.rF,
               lsdiv.spec.rF,
               lsperc.gall.rF,
               lsdiv.gall.rF,
               lsperc.mine.rF,
               lsdiv.mine.rF,
               lsperc.area.rF,
               file = "rF.LS.SI", 
               append = TRUE)

############################################################################
#Plots----
allforestcol <- c("#9f86c0", "#3a5a40", "#f4a261", "#457b9d")
  #shape number:  21             22          23        24
var.perc.dam.plot <- perc.dam.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
  ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#3a5a40", "#457b9d")) +
  scale_fill_manual(values = c("#3a5a40", "#457b9d")) +
  scale_shape_manual(values = c(22,24)) +
  scale_x_discrete("")+
  ggtitle("Frequency (%) Total Damage")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.perc.dam.plot

var.div.dam.plot <- div.dam.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
  ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#3a5a40", "#457b9d")) +
  scale_fill_manual(values = c("#3a5a40", "#457b9d")) +
  scale_shape_manual(values = c(22,24)) +
  scale_x_discrete("")+
  ggtitle("Total Damage Diversity")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.div.dam.plot

var.perc.spec.plot <- perc.spec.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#9f86c0", "#3a5a40", "#457b9d")) +
  scale_fill_manual(values = c("#9f86c0", "#3a5a40", "#457b9d")) +
  scale_shape_manual(values = c(21,22,24)) +
  scale_x_discrete("")+
  ggtitle("Frequency (%) Specialized Damage")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.perc.spec.plot

var.div.spec.plot <- div.spec.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
  ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#9f86c0", "#3a5a40", "#457b9d")) +
  scale_fill_manual(values = c("#9f86c0", "#3a5a40", "#457b9d")) +
  scale_shape_manual(values = c(21,22,24)) +
  scale_x_discrete("")+
  ggtitle("Specialized Damage Diversity")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.div.spec.plot

var.perc.gall.plot <- perc.gall.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
  ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#9f86c0", "#3a5a40", "#f4a261")) +
  scale_fill_manual(values = c("#9f86c0", "#3a5a40", "#f4a261")) +
  scale_shape_manual(values = c(21:23)) +
  scale_x_discrete("")+
  ggtitle("Frequency (%) Gall Damage")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.perc.gall.plot

var.div.gall.plot <- div.gall.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
  ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#9f86c0", "#3a5a40")) +
  scale_fill_manual(values = c("#9f86c0", "#3a5a40")) +
  scale_shape_manual(values = c(21,22)) +
  scale_x_discrete("")+
  ggtitle("Gall Damage Diversity")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.div.gall.plot

var.div.mine.plot <- div.mine.ImpData.varex%>%
  rename(Importance = "%IncMSE")%>%
  ggplot(aes(x = Var.Names, y = Importance, 
             shape = Forest, color = Forest, fill = Forest)) +
  geom_crossbar(inherit.aes = FALSE, 
                aes(x = Var.Names, y = Importance),
                stat = "summary", fun.min = min, fun.max = max, fun = mean,
                fill = "grey", color = "grey", alpha = 0.50) +
  geom_point(size = 6, alpha = 0.9) + 
  coord_flip() + 
  scale_color_manual(values = c("#9f86c0", "#457b9d")) +
  scale_fill_manual(values = c("#9f86c0", "#457b9d")) +
  scale_shape_manual(values = c(21,24)) +
  scale_x_discrete("")+
  ggtitle("Mine Damage Diversity")+
  labs(y = "Relative Importance", x = "Variable") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))
var.div.mine.plot

# legend.fig <- div.mine.ImpData.varex%>%
#   rename(Importance = "%IncMSE")%>%
#   ggplot(aes(x = Var.Names, y = Importance, 
#              shape = Forest, color = Forest, fill = Forest)) +
#   geom_crossbar(inherit.aes = FALSE, 
#                 aes(x = Var.Names, y = Importance),
#                 stat = "summary", fun.min = min, fun.max = max, fun = mean,
#                 fill = "grey", color = "grey", alpha = 0.50) +
#   geom_point(size = 6, alpha = 0.9) + 
#   coord_flip() + 
#   scale_color_manual(values = allforestcol) +
#   scale_fill_manual(values = allforestcol) +
#   scale_shape_manual(values = c(21:24)) +
#   scale_x_discrete("")+
#   #scale_y_continuous(breaks = -5,0,5,10,20)+
#   ggtitle("Mine Damage Diversity")+
#   labs(y = "Relative Importance", x = "Variable") +
#   theme_bw()
# legend.fig

#varlegend <- get_legend(legend.fig)


#super figure
varsuper.fig <- cowplot::plot_grid(var.perc.dam.plot,var.div.dam.plot,
                          var.perc.spec.plot,var.div.spec.plot,
                          var.perc.gall.plot,var.div.gall.plot,
                          varlegend,var.div.mine.plot,
                          nrow = 4,
                          ncol = 2)
varsuper.fig



#Saving figures----
ggsave("figures/rf.varsuper.figV2_corrected.pdf", varsuper.fig, height = 18, width = 15, units = "in")

