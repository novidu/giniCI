data(bli)

# Indicator polarity
bli.pol = c("neg", "pos", "pos", "pos", "pos", "neg",
            "pos", "pos", "pos", "neg", "pos")

# Goalpost normalization without using time factors
bli.norm <- normalize(inds = bli[, 3:13], method = "goalpost",
                      ind.pol = bli.pol)

# Goalpost normalization using time factors and a reference time
bli.norm.2014 <- normalize(inds = bli[, 3:13], method = "goalpost",
                           ind.pol = bli.pol, time = bli$YEAR,
                           ref.time = 2014)

# Adjusted Mazziotta-Pareto index
bli.ampi <- giniCI(bli.norm, ci.pol = "pos")
bli.ampi$ci

# Gini-based weighted arithmetic mean with reference time
bli.gini <- giniCI(bli.norm.2014, method = "gini", ci.pol = "pos",
                   time = bli$YEAR, ref.time = 2014)
bli.gini$ci
bli.gini$w

# Reciprocal Gini-based weighted geometric mean with reference time
bli.reci <- giniCI(bli.norm.2014, method = "reci", agg = "geo",
                   ci.pol = "pos", time = bli$YEAR, ref.time = 2014)
bli.reci$ci
bli.reci$w

