library(qtl)
setwd ("/work/06156/jg1994/stampede2/Stapleton_Lab")
samp=read.cross(file ="familyandrandom.csv")
ls()

summary(samp)

plot(samp)

samp = calc.genoprob(samp)
out.nocovar = scanone(samp,pheno.col=1:2)

plotstruc = samp$pheno$plotstructure
out.acovar = scanone(samp,pheno.col = 1:2, addcovar = plotstruc)

plot(out.nocovar)
plot(out.acovar)

out.icovar = scanone(samp, pheno.col = 1:2, addcovar = plotstruc, intcovar = plotstruc)
summary(out.icovar)

plot(out.acovar, out.icovar, chr=c(2,5), col=c("blue", "red"))
plot(out.acovar, out.icovar, chr=c(2,5), lodcolumn=2,
     col=c("blue", "red"))

out.plotint = out.icovar - out.acovar
plot(out.plotint, lodcolumn=1:2, chr=c(2,5), col=c("green", "purple"))

set.seed((1))
operm.acovar = scanone(samp, pheno.col= 1:2, addcovar=plotstruc,
                       method='hk', n.perm=100)
operm.icovar =  scanone(samp, pheno.col= 1:2, addcovar=plotstruc,
                       intcovar = plotstruc, method='hk', n.perm=100)

operm.plotint = operm.icovar - operm.acovar

summary(operm.plotint, alpha=c(0.05, 0.20))

summary(out.plotint, perms = operm.plotint, alpha=0.1,
        format = 'allpeaks',pvalues = TRUE)

