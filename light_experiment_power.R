#Hypothesis 1: Testing if high light and low light are the same
n=3 #3 per group
nrep=12 #12 groups
N=1000 #replications
m1 = 8
m2 = 4
s1 = .5
n = 30
m1 = -1.5
m2 = -1
npower <- function(n,m1,m2,s1,s2){
  x = rnorm(n,m1,s1)
  y = rnorm(n,m2,s2)
  t.test(x,y)$p.value
}

npowerrep <- function(nrep,n,m1,m2,s1,s2){
  replicate(nrep,npower(n,m1,m2,s1,s2))
}
test = npowerrep(12,3,8,4)
prop.table(table(test < 0.05))
###With 3 genotypes per population for 12 populations, replicated 1000 times  for difference of 4 and power of .8
###Need 10 leaves per genotype with 12 populations for a difference of .5 for power of .5 (with SD of 1)
###Need 2 leaves with SD of .5
###Need 2 leaves per genotype for difference of 2 and power of .8 (with SD of 1)

norm1000 <- function(N,nrep,n,m1,m2,s1,s2){
  replicate(N, npowerrep(nrep,n,m1,m2,s1,s2))
}
test1 = norm1000(1000,12,3,8,4)
prop.table(table(test1 < .05))
##Power 88%
#Hypothesis 2: Testing if slope ~ 0
slope = y - x
zero = rnorm(12)
t.test(slope,zero)$p.value

attempt <- data.frame(
  A1 = rnorm(n,m1),
  B1 = rnorm(n,m1),
  C1 = rnorm(n,m1),
  D1 = rnorm(n,m1),
  E1 = rnorm(n,m1),
  F1 = rnorm(n,m1),
  G1 = rnorm(n,m1),
  H1 = rnorm(n,m1),
  I1 = rnorm(n,m1),
  J1 = rnorm(n,m1),
  K1 = rnorm(n,m1),
  L1 = rnorm(n,m1),
  A2 = rnorm(n,m2),
  B2 = rnorm(n,m2),
  C2 = rnorm(n,m2),
  D2 = rnorm(n,m2),
  E2 = rnorm(n,m2),
  F2 = rnorm(n,m2),
  G2 = rnorm(n,m2),
  H2 = rnorm(n,m2),
  I2 = rnorm(n,m2),
  J2 = rnorm(n,m2),
  K2 = rnorm(n,m2),
  L2 = rnorm(n,m2)
)

attempt1 = data.frame(
  S1 = attempt$A1-attempt$A2,
  S2 = attempt$B1-attempt$B2,
  S3 = attempt$C1-attempt$C2,
  S4 = attempt$D1-attempt$D2,
  S5 = attempt$E1-attempt$E2,
  S6 = attempt$F1-attempt$F2,
  S7 = attempt$G1-attempt$G2,
  S8 = attempt$H1-attempt$H2,
  S9 = attempt$I1-attempt$I2,
  S10 = attempt$J1-attempt$J2,
  S11 = attempt$K1-attempt$K2,
  S12 = attempt$L1-attempt$L2
)

slope <- c(colMeans(attempt1[sapply(attempt1,is.numeric)]))
slope <- as.data.frame(slope)
t.test(slope,zero)$p.value

#Hypothesis 3: Testing if slopes differ from one another
attempt2 <- data.frame(
  light = c(attempt1$S1,attempt1$S2,attempt1$S3,attempt1$S4,attempt1$S5,attempt1$S6,attempt1$S7,attempt1$S8,attempt1$S9,attempt1$S10,attempt1$S11,attempt1$S12),
  population = c("S1","S1","S1","S2","S2","S2","S3","S3","S3","S4","S4","S4","S5","S5","S5","S6","S6","S6","S7","S7","S7","S8","S8","S8","S9","S9","S9","S10","S10","S10","S11","S11","S11","S12","S12","S12")
  )
pairwise.t.test(attempt2$light,attempt2$population,p.adj="none")

#Hypothesis 4: testing if slopes and genetic/physical distance are linearly related
attemptfull = data.frame(
  S1 = attempt$A1-attempt$A2,
  S2 = attempt$B1-attempt$B2,
  S3 = attempt$C1-attempt$C2,
  S4 = attempt$D1-attempt$D2,
  S5 = attempt$E1-attempt$E2,
  S6 = attempt$F1-attempt$F2,
  S7 = attempt$G1-attempt$G2,
  S8 = attempt$H1-attempt$H2,
  S9 = attempt$I1-attempt$I2,
  S10 = attempt$J1-attempt$J2,
  S11 = attempt$K1-attempt$K2,
  S12 = attempt$L1-attempt$L2,
  l1 = rnorm(n,29,s1),
  l2 = rnorm(n,30,s1),
  l3 = rnorm(n,31,s1),
  l4 = rnorm(n,32,s1),
  l5 = rnorm(n,33,s1),
  l6 = rnorm(n,34,s1),
  l7 = rnorm(n,35,s1),
  l8 = rnorm(n,36,s1),
  l9 = rnorm(n,37,s1),
  l10 = rnorm(n,38,s1),
  l11 = rnorm(n,39,s1),
  l12 = rnorm(n,40,s1)
)

attempt3 <- data.frame(
  population = c("S1","S1","S1","S2","S2","S2","S3","S3","S3","S4","S4","S4","S5","S5","S5","S6","S6","S6","S7","S7","S7","S8","S8","S8","S9","S9","S9","S10","S10","S10","S11","S11","S11","S12","S12","S12"),
  light = c(attempt1$S1,attempt1$S2,attempt1$S3,attempt1$S4,attempt1$S5,attempt1$S6,attempt1$S7,attempt1$S8,attempt1$S9,attempt1$S10,attempt1$S11,attempt1$S12),
  location = c(attemptfull$l1,attemptfull$l2,attemptfull$l3,attemptfull$l4,attemptfull$l5,attemptfull$l6,attemptfull$l7,attemptfull$l8,attemptfull$l9,attemptfull$l10,attemptfull$l11,attemptfull$l12)
  )
geography = aov(attempt3$light ~ attempt3$location)
