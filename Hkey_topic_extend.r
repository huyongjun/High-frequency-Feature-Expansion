############ re : input matrix data 
############ output high freq words
	
	Hkey_topic_extend <- function (key_Vector,training_LDA,test_LDA,
		names_key_role,maxTopic,Sparse=TRUE,Folding,testChain)
	{
		tmp <- sms_dtm_sparse(key_Vector)
		if(Sparse==TRUE) 
			folding1 <- tmp$folding
		else
			folding1 <- rep(0,length(tmp$folding))
		key_Vector_lda <- key_Vector

		p_terms <- list()
		p_terms[[1]] <- posterior(training_LDA)$terms
		p_terms[[2]] <- posterior(test_LDA)$terms
	##  针对频率较高主题做处理
		for(i in 1 : nrow(key_Vector))
		{
			freq_tmp <- LDA_Vector
			key_tmp <- rep(0,ncol(key_Vector))
			
			if(folding1[i]==0)  #稀疏
			{
				key_3st <- Topic_3st <- list()
				M <- 1
				Weight <- 0
				for(s in 1:5)
				{
					T <- which.max(freq_tmp[i,])
			
					if(Folding[i]!=testChain)  ##训练集
						key_tmp <- LDA_HighWords_gen(p_terms[[1]],names_key_role,
							T,prob = 0.01,IsTrains=TRUE)
					else          ##测试集
						key_tmp <- LDA_HighWords_gen(p_terms[[2]],names_key_role,
							T,prob = 0.01,IsTrains=FALSE)

					if(freq_tmp[i,T] > 0.1 & M <= maxTopic)
					{
						tmp_3st <- key_tmp[order(-key_tmp$key_freq),][1:3,]
						key_3st[[M]] <- tmp_3st
						key_3st[[M]]$Topic <- T
						key_3st[[M]]$Topic_freq <- freq_tmp[i,T]
						key_3st[[M]]$freq <- freq_tmp[i,T] * key_3st[[M]]$key_freq
						freq_tmp[i,T] <- 0
						Weight <- Weight + key_3st[[M]]$Topic_freq[1]
						M <- M +1
					}
				}

				key_Vector_lda[i,] <- 0
				for(m in 1:(M-1))
				{
					if(M > 1 )
					{
						tmp_3st <- key_3st[[m]][key_3st[[m]]$freq>0.05*Weight,] #0.0143
						key_Vector_lda[i,as.character(tmp_3st$key_names)] <- 
							as.numeric(tmp_3st$key_freq)/(0.015*Weight) ###freq
					}
				}
							
			}
		}
		key_Vector_lda

	}
	
	



	
	