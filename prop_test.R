library(optparse)
library(tidyverse)

# This script is meant to be run from the command line using the output from ad_test.R as guidance. 
# For example, ad_test.R will give you a recommended sample of 42,606 to observe a 20% difference in 
# a given KPI with a CVR of 1%. 
# 
# Running this test, with trials of 42,606 and 42,606 and successes of
# 426 and 511, you would have a statistically significant result.
# 
# This ultimately helps quickly understand what sample size may be needed to see 
# stat-sig results and can be used easily by other analysts in the command line.

option_list <- list(
  make_option(c("-t", "--trials"), type = "character", action="store",
              help = paste0("vector of trials, depending on number of ad variants running. ",
                            "Format should be 1000,3000,etc. (comma separated)"), metavar = "character"),
  make_option(c("-s", "--successes"), type = "character", action="store",
              help = paste0("vector of successes, depending on number of ad variants running. ",
                            "Format should be 100,149,178 (comma separated)"), metavar = "character"),
  make_option(c("-a", "--alternative"), type = "character",
              default<-"t",
              help = paste0("alternative hypothesis. Will run as a two-sided test unless you put 'g' (greater) or 'l' (less). ",
                            "When using greater or less, you're saying you believed the first value ",
                            "would be greater than the second value."
              ), metavar = "character")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

trials <- strsplit(opt$trials, ",")
successes <- strsplit(opt$successes, ",")
alternative <- opt$alternative

trials <- as.double(trials[[1]])
successes <- as.double(successes[[1]])

prop_table <- tibble(successes, trials) %>%
  as.matrix()

if(is.null(alternative)) {
  prop.test(prop_table)
} else if (alternative=="g") {
  prop.test(prop_table, alternative="greater")
} else if (alternative=="l") {
  prop.test(prop_table, alternative = "less")
} else {
  stop("Input Error")
}
