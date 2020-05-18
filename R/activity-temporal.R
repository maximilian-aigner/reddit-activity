# Studying activity on a (weekday x hour) scale

library(multiway) # Other choice: rTensor, but I found it difficult to use
library(lattice)
library(lubridate)
library(mixtools)

df <- within(df, {
     day_of_week <- factor(wday(created, label = TRUE))
     hour <- hour(created)
})

# Compute the three-way tensors
# TODO: Could speed this up by making sparse slices for each day (sparse = TRUE in xtabs())

subreddit_activity <- xtabs(~ day_of_week + hour + subreddit, df)
user_activity <- xtabs(~ day_of_week + hour + author, df)

# In both cases, we are interested more in proportion than absolute counts
# See also the remark in NNMF section

table_totals <- apply(subreddit_activity, 3, sum)
subreddit_props <- sweep(subreddit_activity, 3, table_totals, FUN = "/")

table_totals <- apply(user_activity, 3, sum)
user_props <- sweep(user_activity, 3, table_totals, FUN = "/")

##########################################
# Parallel Factors (PARAFAC) decomposition
##########################################
# This decomposes according to third-degree "diagonal" interactions;
# see help(parafac) for formula

subreddit_parafac <- parafac(as.array(subreddit_props),
                             nfac = 5, # Number of latent profiles
                             const = rep("nonneg", 3), # For interpretability (viz. NNMF)
                             nstart = 20)

user_parafac <- parafac(as.array(user_props),
                             nfac = 4,
                             const = rep("nonneg", 3),
                             nstart = 20)

plot_temporal_profiles <- function(factor1, factor2, ...) {
  profiles <- lapply(1:ncol(factor1), function(i) {
    factor1[,i] %*% t(factor2[,i])
  })
  invisible(lapply(profiles, function(p) {
    readline(prompt = "Press for next plot")
    levelplot(t(p), col.regions = hcl.colors(100))
    }))
}

###############################
# Multinomial mixture models
###############################
# Here we try various clustering methods on the profile images.
# In order to apply a multinomial distribution, the images are
# flattened into pixel arrays.

ntables <- dim(subreddit_activity)[3]
subreddit_activity_flattened <- matrix(subreddit_activity, 7*24, ntables)

# Multinomial mixture algorithm from mixtools
mixture_of_multinomials <- multmixEM(t(subreddit_activity_flattened), k = 10)

# LDA
class(subreddit_activity_flattened) <- "matrix"
sparse_activity <- as(subreddit_activity_flattened, "TsparseMatrix")
lda_activity <- lda_svi(t(sparse_activity), K = 5)

lda_beta <- dcast(lda_activity$beta, term ~ topic) %>% column_to_rownames('term')

# Re-stack the vectors into images
reshaped_mixture <- simplify2array(lapply(as.list(as.data.frame(t(mixture_of_multinomials$theta))), function(u) {
  matrix(u, nrow = 7, ncol = 24)
}))

reshaped_lda <- simplify2array(lapply(as.list(as.data.frame(lda_beta)), function(u) {
  matrix(u, nrow = 7, ncol = 24)
}))

dimnames(reshaped_mixture) <- list(
                                dimnames(subreddit_activity)[[1]],
                                dimnames(subreddit_activity)[[2]],
                                paste0("Class", 1:dim(reshaped_mixture)[3])
                              )

dimnames(reshaped_lda) <- list(
  dimnames(subreddit_activity)[[1]],
  dimnames(subreddit_activity)[[2]],
  paste0("Class", 1:dim(reshaped_lda)[3])
)


