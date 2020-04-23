# Modeling the number of posts by user

library(evd)

post_counts <- table(df$author)
# usernames <- dimnames(post_counts)[[1]]
# post_counts <- as.numeric(post_counts)
# names(post_counts) <- usernames
post_counts <- sort(post_counts[post_counts > 2], decreasing = TRUE)

# Choose threshold: we found ~300 to be a good choice
tcplot(post_counts, tlim = quantile(post_counts, c(0.8, 0.95)))

# Fit extreme value distribution (GPD)
fit <- fpot(pc, threshold = 300)
plot(fit)