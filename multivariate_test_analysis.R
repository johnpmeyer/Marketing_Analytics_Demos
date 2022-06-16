library(jtools)

# This is a simple script that analyzes elements of a 2d multivariate ad test.
# Link below helped me with this process.
# https://www.searchdiscovery.com/blog/demystifying-multivariate-testing/
# Note that this uses dummy data.

#Image that is in same directory as this shows estimated effect of given elements
#Video format negatively contributes to CTR, while the healthy value prop
#positively contributes. THe healthy+video together format is negative relative
#to static and free value prop.

test_data = data.frame(
  f1_format = c("static", "static", "video", "video"),
  f2_valprop = c("free", "healthy", "free", "healthy"),
  ctr = c(148/759935, 488/759845, 84/757935, 212/759921),
  impressions = c(759935, 759845, 757935, 759921)
)

test_analysis = glm(ctr ~ (f1_format + f2_valprop)^2, family = "binomial",
                    data = test_data, 
                    weights = impressions)

summ(test_analysis, model.info = FALSE)
plot_summs(test_analysis)

#https://www.searchdiscovery.com/blog/demystifying-multivariate-testing/
#Link above