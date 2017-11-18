install.packages("FrF2")
library(FrF2)

#V
design = FrF2(nruns=16,nfactors=7,generators=c("ABD","ABC","BCD"))
design$y = c(1:nrow(design))

# 4 Is legnth of interaction wanted

alias_sets = aliases(lm(y~(.)^3,data=design))
class=design
alias_sets


#IX
design = FrF2(nruns=32,nfactors=8,generators=c("ABC","ABD","BCDE"))
design$y = c(1:nrow(design))


alias_sets = aliases(lm(y~(.)^4,data=design))
class=design
alias_sets

#X
#3
design = FrF2(nruns=16,nfactors=5,generators=c("ABCD"))
design$y = c(1:nrow(design))


alias_sets = aliases(lm(y~(.)^5,data=design))
class=design
alias_sets

#6
design = FrF2(nruns=8,nfactors=5,generators=c("AB","AC"))
design$y = c(1:nrow(design))

alias_sets = aliases(lm(y~(.)^5,data=design))
class=design
alias_sets

#VII

design = FrF2(nruns=16,nfactors=5,generators=c("ABCD"))
design$y = c(1:nrow(design))

alias_sets = aliases(lm(y~(.)^5,data=design))
class=design
alias_sets