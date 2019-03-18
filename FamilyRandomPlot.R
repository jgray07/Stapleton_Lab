library(qtl)
library(vqtl)
setwd("/work/06156/jg1994/stampede2/Stapleton_Lab")
sample <-read.cross(file ="familyandrandom.csv")
sample <- drop.nullmarkers(sample)
#scan with variance
sample <- calc.genoprob(sample)

head(sample$pheno)

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
write.csv(outv$result, file = "additive_model.csv")

outv <- scanonevar(cross = sample,
                   mean.formula = PlantHeight ~ plotstructure * (mean.QTL.add + mean.QTL.dom),
                   var.formula = ~ plotstructure * (var.QTL.add + var.QTL.dom),
                   return.covar.effects = TRUE)

outv$result %>% glimpse()
write.csv(outv$result, file = "interactive_model.csv")
