n_treat <- 84277
n_untreat <- 6508019

treat <- c(59135, 1010, 5234, 18898)
untreat <- c(4717678, 72675, 442638, 1275028)

prop_trt <- round(treat/n_treat * 100, 1)
prop_untrt <- round(untreat/n_untreat * 100, 1)

prop_trt
prop_untrt
sum(prop_trt)
sum(prop_untrt)