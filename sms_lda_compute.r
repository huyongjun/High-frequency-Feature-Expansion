############ dtm_new: 词文档矩阵
############ PATH: 存储路径
############ 判断dtm_new是否行太少，太少就不抽样

	sms_lda_compute <- function(dtm_new,Topic,PATH)
  {
##############1、 Simulation using 10-fold cross-validation
	
	SEED <- 20080809

	D <- nrow(dtm_new)
	folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
	
	for(k in Topic)
	{
		for (chain in seq_len(10)) 
		{
			FILE <- paste("VEM_", k, "_", chain, ".rda", sep = "")
			training <- LDA(dtm_new[folding != chain,], k = k,
				control = list(verbose = 100))
			testing <- LDA(dtm_new[folding == chain,], model = training,
				control = list(estimate.beta = FALSE, seed = SEED))			
			save(training, testing, file = file.path(PATH, FILE))
			FILE <- paste("VEM_fixed_", k, "_", chain, ".rda", sep = "")
			training <- LDA(dtm_new[folding != chain,], k = k,
				control = list(seed = SEED, estimate.alpha = FALSE))
			testing <- LDA(dtm_new[folding == chain,], model = training,
				control = list(estimate.beta = FALSE, seed = SEED))
			save(training, testing, file = file.path(PATH, FILE))
			FILE <- paste("Gibbs_", k, "_", chain, ".rda", sep = "")
			training <- LDA(dtm_new[folding != chain,], k = k,
				control = list(seed = SEED, burnin = 1000, thin = 100,
				iter = 1000, best = FALSE), method = "Gibbs")
			best_training <- training@fitted[[which.max(logLik(training))]]
			testing <- LDA(dtm_new[folding == chain,],
				model = best_training, control = list(estimate.beta = FALSE,
				seed = SEED, burnin = 1000, thin = 100, iter = 1000, best = FALSE))
			save(training, testing, file = file.path(PATH, FILE))

		}
	}
###########2、 Summarizing the cross-validation simulation results
	AP_test <- AP_alpha <- list()
	for (method in c("VEM", "VEM_fixed", "Gibbs")) 
	{
		AP_alpha[[method]] <- AP_test[[method]] <- matrix(NA,
			nrow = length(Topic), ncol = 10, dimnames = list(Topic, seq_len(10)))
		for (fold in seq_len(10)) 
		{
			for (i in seq_along(Topic)) 
			{
				T <- Topic[i]
				FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
				load(file.path(PATH, FILE))
				AP_alpha[[method]][paste(T),fold] <-
					if (is(training, "Gibbs_list")) training@fitted[[1]]@alpha
					else training@alpha
				m <- nrow(dtm_new[folding == fold,])
				if( m < 2)
					AP_test[[method]][paste(T),fold] <- 0
				else
					AP_test[[method]][paste(T),fold]<-perplexity(testing,
						dtm_new[folding == fold,], use_theta = FALSE)
			}
		}
	}
 
	FILE <- paste(PATH,"/AP.rda",sep = "")
	save(AP_alpha, AP_test, file = FILE)
	AP_test

  }