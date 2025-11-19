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

# Ranking comparison
ci.comp <- rankComp(ci.gini, ci.reci, id = bli$COUNTRY, time = bli$YEAR)
print(ci.comp)
summary(ci.comp)
