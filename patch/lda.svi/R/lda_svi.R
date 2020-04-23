#' Fit a Latent Dirichlet Allocation model to a text corpus
#'
#' @param dtm This must be a DocumentTermMatrix (with term frequency weighting) from the tm package.
#' @param passes The number of passes over the whole corpus - how many times we update the local variables for each document.
#' @param batchsize The size of the minibatches.
#' @param maxiter The maximum iterations for the "E step" for each document (the updating of the per-document parameters within each minibatch). The default of 100 follows the reference implementation in python by the authors.
#' @param eta Dirichlet prior hyperparameter for the document-specific topic proportions. 
#' @param alpha Dirichlet prior hyperparameter for the topic-specific term proportions.
#' @param kappa learning rate parameter. Lower values give greater weight to later iterations. For guaranteed convergence to a local optimum, kappa must lie in the interval (0.5,1].
#' @param tau_0 learning rate parameter. Higher values reduce the influence of early iterations.
#' @param K The number of topics
#' @param tidy_output if true, the parameter estimates are returned as 'long' data frames; otherwise they are returned as matrices. 
#' @importFrom methods is
#' @details The implementation here is based on the python implementation by Matthew D. Hoffman accompanying the paper
#' @return A named list of length two. The element named 'beta' gives the proportions for the terms within the topics, while the element named 'theta' gives the proportions for the topics within the documents. If the tidy_output argument is true these are data frames in 'long' format; otherwise they are matrices.
#' @references Hoffman, M., Bach, FM., and Blei, DM. (2010) 'Online Learning for Latent Dirichlet Allocation', _Conference and Workshop on Neural Information Processing Systems_
#'
#'
#' Hoffman, M., Blei, DM., Wang, C, and Paisley, J. (2013) 'Stochastic Variational Inference', _Journal of Machine Learning Research_. Preprint: https://arxiv.org/abs/1206.7051_
#'
#' @examples
#' library(topicmodels)
#' data(AssociatedPress)
#' ap_lda_fit <- lda_svi(AssociatedPress,passes=1,K=50)
#' #I use a single pass because CRAN requires examples to run quickly; 
#' #generally one would use more. 20 often seems to be sufficient as a rule of thumb,
#' #but it might be worth experimenting with more or fewer
#' @export

lda_svi <- function(dtm,passes=10,batchsize=256,maxiter=100,K,eta=1/K,alpha=1/K,kappa=0.7,tau_0=1024,tidy_output=TRUE){

	if (is(dtm,"DocumentTermMatrix")){
		if (!any(attr(dtm,'weighting') %in% c('term frequency','tf'))){
			stop('The DocumentTermMatrix object must use term frequency weighting')
		}
	}

	doc_ids <- dtm$i - 1#the c++ code expects 0-indexed ids
	docs <- dtm$dimnames$Docs

	term_ids <- dtm$j - 1#the c++ code expect 0-indexed ids
	terms <- dtm$dimnames$Terms
	
	counts <- dtm$v

	res_list <- lda_online_cpp(doc_ids,term_ids,counts,K,passes,batchsize,maxiter=maxiter,eta=eta,alpha=alpha,tau_0=tau_0,kappa=kappa)

	gamma <- res_list$Gamma
	lambda <- res_list$Lambda
	
	colnames(gamma) <- seq(1:ncol(gamma))#topic labels
	rownames(gamma) <- unique(docs)

	colnames(lambda) <- unique(terms)
	rownames(lambda) <- seq(1:nrow(lambda))
	
	# convert variational parameters to model parameters 
	# (this follows from equation 2 in the NIPS paper)
	# Noting that the expectation of a Dirichlet(a) rv is a/sum(a)

	theta <- gamma
	beta <- lambda
	
	if (tidy_output){

	for (i in seq(1,nrow(gamma))){
	  theta[i,] <- theta[i,]/sum(theta[i,])
	}
	
	for (i in seq(1,nrow(lambda))){
	  beta[i,] <- lambda[i,]/sum(lambda[i,])
	}

	theta <- reshape2::melt(theta)
	beta <- reshape2::melt(beta)

	colnames(beta) <- c('topic','term','value')
	colnames(theta) <- c('document','topic','value')	
	}

	list('theta'=theta,'beta'=beta)
}
