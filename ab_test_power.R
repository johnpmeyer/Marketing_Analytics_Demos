suppressMessages(library(pwr))
suppressMessages(library(optparse))
suppressMessages(library(tidyverse))


# This script is meant to give an analyst a quick estimate on samples needed for a simple A/B test 
# It should be run in conjunction with 'prop_test.R' to see what the potential stat sig level that this outputs.


option_list <- list(
  make_option(c("-c", "--ctr"), type = "double",
              default = 0.01,
              help = "ctr of ad you're testing against", metavar = "double")
)


opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

cat(paste0("\nRunning a standard test with differences of 0.2, 0.5, and 0.8 assuming a CTR of ", opt$ctr,"\n"))

anticipated_lift <- c(0.2,0.5,0.8) 
#anticipated lift in CTR we're expecting from the given ad.
#often should be 0.2, 0.5, or 0.8 (small, medium, large)
#more details here https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

base_prop <- opt$ctr #base proportion of CTR on given ad
new_prop_small_impact <- base_prop * (1+anticipated_lift[[1]])
new_prop_medium_impact <- base_prop * (1+anticipated_lift[[2]])
new_prop_large_impact <- base_prop * (1+anticipated_lift[[3]])

power_impact_small<-pwr.2p.test(h=ES.h(base_prop,new_prop_small_impact), 
                                sig.level = 0.05, power=0.8, alternative = "two.sided")

power_impact_medium<-pwr.2p.test(h=ES.h(base_prop,new_prop_medium_impact), 
                                 sig.level = 0.05, power=0.8, alternative = "two.sided")

power_impact_large<-pwr.2p.test(h=ES.h(base_prop,new_prop_large_impact), 
                                sig.level = 0.05, power=0.8, alternative = "two.sided")

cat("Impressions needed per variant for an 0.2 impact\n")
print(power_impact_small)

cat("Impressions needed per variant for an 0.5 impact\n")
print(power_impact_medium)

cat("Impressions needed per variant for an 0.8 impact\n")
print(power_impact_large)

#ref on running this from command line:
#navigate to directory, run Rscript ad_test.R -c 0.01