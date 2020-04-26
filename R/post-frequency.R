# Modeling the number of posts by user

library(evd)

post_counts <- table(df$author)
post_counts <- sort(post_counts[post_counts > 1], decreasing = TRUE)

# Choose threshold: we found ~300 to be a good choice
tcplot(post_counts, tlim = quantile(post_counts, c(0.8, 0.95)))

# Fit extreme value distribution (GPD)
fit <- fpot(pc, threshold = 300)
plot(fit)