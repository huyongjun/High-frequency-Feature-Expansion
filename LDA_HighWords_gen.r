#########################################################################
############ vector_terms : input key words vector 
############ names_dict_role : input key words dict
############ prob  : input 关键词过滤门限  
############ K : input the topic No 
############ output gen high freq words by LDA 1st Topic
	
	LDA_HighWords_gen <- function (vector_terms,names_dict_role,
		K,prob=0.01,IsTrains)
	{
		if(is.list(vector_terms)==TRUE)
		{#training and test
			train_vector <- vector_terms[[1]]
			test_vector <- vector_terms[[2]]	
		}
		else
			train_vector <- test_vector <- vector_terms
		
		key_tmp <- rep(0,nrow(names_dict_role))
		if(IsTrains == TRUE) words_freq <- train_vector[K,]
		else words_freq <- test_vector[K,]

		words <- colnames(train_vector)
		words_names <- names(words_freq)
		words_length <- as.numeric(words_freq)
		words_df <- data.frame(words_names=words_names, 
			words_freq=words_freq,words_length=words_length)
		key <- words_df[words_df[,1] %in% names_dict_role$words_names, ]

		for(i in 1 : nrow(names_dict_role))
		{
			if(key$words_freq[i] > prob)
				key_tmp[i] <- key$words_freq[i] 
		}
		key_names <- key$words_names
		key <- data.frame(key_names=key_names,key_freq=key_tmp)
		key
	}
	
	



	
	