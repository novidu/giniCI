data(bli)

# Goalpost normalization
bli.pol = c("neg", "pos", "pos", "pos", "pos", "neg",
            "pos", "pos", "pos", "neg", "pos")
bli.norm.2014 <- normalize(inds = bli[, 3:13], method = "goalpost",
                           ind.pol = bli.pol, time = bli$YEAR,
                           ref.time = 2014)

# Composite indices
ci.gini <- giniCI(bli.norm.2014, method = "gini",
                  ci.pol = "pos", time = bli$YEAR, ref.time = 2014,
                  only.ci = TRUE)
ci.reci <- giniCI(bli.norm.2014, method = "reci", agg = "geo",
                  ci.pol = "pos", time = bli$YEAR, ref.time = 2014,
                  only.ci = TRUE)

# Ranking comparison plots
ci.comp <- rankComp(ci.gini, ci.reci, id = bli$COUNTRY, time = bli$YEAR)
rankScatterPlot(ci.comp)$'2014'
rankShiftPlot(ci.comp)$'2015'
rankRankPlot(ci.comp)$'2016'

# Storing and printing
p.scatter <- rankScatterPlot(ci.comp, combine = TRUE, max.overlaps = 20)
print(p.scatter$'2017') # or: print(p.scatter[[4]])
print(p.scatter$'comb') # or: print(p.scatter[[5]])
