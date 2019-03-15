#####Fam and Rand vQTL FINAL#####
library("qtl")
library("vqtl")
sample <-read.cross(file ="familyrandomsample.csv")
sample <- drop.nullmarkers(sample)
#scan with variance
sample <- calc.genoprob(sample)

# turn looks like 'plotstructure' is categorical data -- in R, we represent that with a 'factor'
sample$pheno$plotstructure <- factor(sample$pheno$plotstructure)

# let's make a ggplot to have a look
library(tidyverse)
sample$pheno %>%
  ggplot(mapping = aes(x = plotstructure, y = PlantHeight)) +
  geom_jitter(width = 0.2)

# don't think there's enough data here to say...looks like similar mean and variance? - Robert

outv <- scanonevar(cross = sample,
                   mean.formula = PlantHeight ~ plotstructure + mean.QTL.add + mean.QTL.dom,
                   var.formula = ~ plotstructure + var.QTL.add + var.QTL.dom,
                   return.covar.effects = TRUE)

outv$result %>% glimpse()

# I deleted a bunch of stuff here -- I think this you just want outv$result
# maybe the previous user didn't know about this return.covar.effects argument to scanonevar
# maybe it's not well documented!

write.csv(outv$result, file = "2019_01_15_FamRand_Output.csv")
