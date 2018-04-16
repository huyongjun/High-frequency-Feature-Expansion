#############################################################
#########   sms vector extensive by lda    ##################
############## 1、导入主要的软件包	#########################
#############################################################
	library(topicmodels)
	source("r_code/sms_bind_SR.r")
	source("r_code/sms_bind_S.r")
	source("r_code/sms_mmseg.r")
	source("r_code/sms_zwfc.r")
	source("r_code/sms_dtm_sparse.r")
	source("r_code/sms_lda_compute.r")
	source("r_code/word_Highfreq_extract.r")
	source("r_code/LDA_HighWords_gen.r")
	source("r_code/SVM_Vector_predict.r")
	source("r_code/sms_cut_role.r")
	source("r_code/Hkey_topic_extend.r")
	source("r_code/key_Dict_gen.r")
	library(network)
	library(RODBC)
	library(sqldf)
	library(tcltk)
	library(rmmseg4j)
	library(tm)

#################################################################################
###   一、  LDA 及关键词特征 TFIDF基本特征训练
##############################################################################
#### 1、对全部语料训练LDA,挑选关键词
	reuters <- as.matrix(dat_pre[,20])
	reuters <- Corpus(DataframeSource(reuters)) 
	dtm <- DocumentTermMatrix(reuters,
			control = list(wordLengths=c(2,10),weighting = weightTf))
	result <- sms_dtm_sparse(dtm)
	chain <- 0
	folding <- result$folding

	re1 <- reuters[folding != chain]
	re1 <- Corpus(VectorSource(re1))
	dtm_new <- dtm[folding != chain] 

## TFIDF
  ## LDA Vector
	training_all_TFIDF <- LDA(dtm_new, k = 80,control = list(verbose = 100))
	LDA_Vector_TFIDF <- posterior(training_all_TFIDF)$topics
  ## CTM Vector
	#ctm <- CTM(dtm_new, k = 80)
  ## TFIDF Vector 
	key_Vector_TFIDF <- inspect(DocumentTermMatrix(re1, list(wordLengths=c(2,10),
		weighting = weightTfIdf)))

	key_Vector_15Sogou_TFIDF <- inspect(DocumentTermMatrix(re1,list(wordLengths=c(2,10), 
		weighting = weightTfIdf,dictionary = dict_Sogou)))
	key_Vector_role_TFIDF <- inspect(DocumentTermMatrix(re1,list(wordLengths=c(2,10), 
		weighting = weightTfIdf,dictionary = dict_role)))
	key_Vector_role_100_TFIDF <- inspect(DocumentTermMatrix(re1,list(wordLengths=c(2,10), 
		weighting = weightTfIdf,dictionary = dict_role_100)))

	## 控制了词长度，包含2个及以上的词

#############################################################################
#### 2、仅对训练集语料训练LDA,挑选关键词，结合sms_training_LDA处理结果
	### 利用前期保存的抽样数据，生成训练词条矩阵
	Sample <- Training_Data_Role
	classes <- iRole
	dtm_pre <- dtm_new
	key_Vector_role_TFIDF_90_10 <- key_Vector_role_TFIDF
	classes <- classes[Sample[,"Role"] != 5] #去掉白领
	key_Vector_role_TFIDF_90_10 <- key_Vector_role_TFIDF_90_10[Sample[,"Role"] != 5,] #去掉白领
	dtm_pre <- dtm_pre[Sample[,"Role"] != 5,] #去掉白领
	Sample <- Sample[Sample[,"Role"] != 5,] #去掉白领
	key_Vector_role_TFIDF_90_10 <- key_Vector_role_TFIDF_90_10[Sample[,"Role"] != 0,]
	classes <- classes[Sample[,"Role"] != 0] #去掉角色未定者
	dtm_pre<- dtm_pre[Sample[,"Role"] != 0,] #去掉角色未定者
	Sample <- Sample[Sample[,"Role"] != 0,] #去掉角色未定者
	dtm_train <- dtm_pre[folding != 9,] # 得到抽样的训练集
	dtm_test <- dtm_pre[folding == 9,] # 得到抽样的训练集
	classes <- as.factor(classes)

	LDA_pre_TFIDF <- LDA(dtm_pre, k = 80,control = list(verbose = 100))
	training_90_TFIDF <- LDA(dtm_train, k = 80,control = list(verbose = 100))
	test_90_TFIDF <- LDA(dtm_test, model = training_90_TFIDF,
				control = list(estimate.beta = FALSE, seed = SEED))
	LDA_Vector_TFIDF_90_10 <- posterior(LDA_pre_TFIDF)$topics

	LDA_Vector_TFIDF_90_10[folding != 9,] <- posterior(training_90_TFIDF)$topics
	LDA_Vector_TFIDF_90_10[folding == 9,] <- posterior(test_90_TFIDF)$topics
	
#############################################################################
#### 3、训练江嘉欣提供的新闻标题语料，采用LDA平滑扩展关键词特征，结合sms_training_LDA处理结果
  ###(1) 导入新闻标题数据
	FILE <- paste("短信数据分析/江嘉欣处理/新闻标题数据/","train.txt", sep = "")
	news_train <- read.csv.sql(FILE,sql = "select * from file",header = TRUE)
	FILE <- paste("短信数据分析/江嘉欣处理/新闻标题数据/","test.txt", sep = "")
	news_test <- read.csv.sql(FILE,sql = "select * from file",header = TRUE)
	odbcCloseAll()
	reNews_train <- Corpus(DataframeSource(as.matrix(news_train)))
	reNews_test <- Corpus(DataframeSource(as.matrix(news_test)))
	jjx_news_train <- read.arff("短信数据分析/江嘉欣处理/新闻标题数据/lda高频词/train.arff")
	jjx_news_test <- read.arff("短信数据分析/江嘉欣处理/新闻标题数据/lda高频词/test.arff")
	classes_tr <- jjx_news_train[,571]
	classes_t <- jjx_news_test[,571]

  ### (2) 生成词条矩阵
	### 1) train 数据集
	reuters <- reNews_train 
	dtm <- DocumentTermMatrix(reuters,
			control = list(wordLengths=c(2,10),weighting = weightTf))
	### 2) test 数据集
	reuters <- reNews_test 
	dtm_test <- DocumentTermMatrix(reuters,
			control = list(wordLengths=c(2,10),weighting = weightTf))
	## 判断稀疏率
	result <- sms_dtm_sparse(dtm_test)
	chain <- 0
	folding <- result$folding
	result$rate

	#re1 <- reuters[folding != chain]
	#re1 <- Corpus(VectorSource(re1))
	#dtm_new <- dtm[folding != chain] 

  ### (3) 制作关键词字典
	cl <- length(table(class_tr))
	ncl <- rep(0,cl)
	tmp_key <- list()
	
	ceiling(length(unlist(strsplit(as.matrix(news_train), ' ')))*0.10/cl)

	for(i in 1:cl)
	{
		re_key <- unlist(strsplit(news_train[class_tr==i,], ' '))
		tmp_key[[i]] <- word_Highfreq_extract(re_key,0.12,5)
		ncl[i] <- nrow(tmp_key[[i]]) 
	}
####### 调整短词条类
  ##	1) 等距调整，取前100个
	ncl
	#i <- which.min(ncl)
	#tmp_key[[i]] <- word_Highfreq_extract(re_key,0.18,5)
	#ncl[i] <- nrow(tmp_key[[i]]) 
	tmp_key_ex <- list()
	for(i in 1:cl)
	{
		tmp_key_ex[[i]] <- tmp_key[[i]][1:100,]
	}

  ##	2) 等比例抽取，取前10%
	tmp_key_ex <- tmp_key
  ##  3) 提取前50个词	
	tmp_key_ex <- list()
	for(i in 1:cl)
	{
		tmp_key_ex[[i]] <- tmp_key[[i]][1:50,]
	}
  ##  4) 提取前80个词	
	tmp_key_ex <- list()
	for(i in 1:cl)
	{
		tmp_key_ex[[i]] <- tmp_key[[i]][1:50,]
	}

####### 合成关键词字典	
	 
	tt <- tmp_key_ex[[1]]
	for(i in 1:cl)
	{
		if(i>1)
		{
			tt <- merge (tt, tmp_key_ex[[i]], by='words_names', all=TRUE)
			if(i==cl) names_keyNews_role <- tt
			tt <- tt['words_names']
		}
	}
	key_role <- tt['words_names']

	c_words <- Corpus(DataframeSource(key_role))
	n_words <- length(c_words)
	re_tmp <- c_words[[1]]
	for(i in 1:n_words)
	{
		if(i>1) re_tmp <- paste(re_tmp,c_words[[i]],sep=" ")

	}
	dict_role_news <- Dictionary(unlist(strsplit(re_tmp,' ')))

############################################################################	
	### (4) 生成初始特征向量
	## TFIDF
  ## LDA Vector
	training_train_TFIDF <- LDA(dtm, k = 80,control = list(verbose = 100))
	LDA_Vector_train_TFIDF <- posterior(training_train_TFIDF)$topics
	training_test_TFIDF <- LDA(dtm_test, model = training_train_TFIDF,
				control = list(estimate.beta = FALSE, seed = SEED))
	LDA_Vector_test_TFIDF <- posterior(training_test_TFIDF)$topics

  ## TFIDF Vector 
	key_Vector_role_train_TFIDF <- inspect(DocumentTermMatrix(reNews_train,
		list(wordLengths=c(2,10), weighting = weightTfIdf,dictionary = dict_role_news)))
	key_Vector_role_test_TFIDF <- inspect(DocumentTermMatrix(reNews_test,
		list(wordLengths=c(2,10),weighting = weightTfIdf,dictionary = dict_role_news)))
	## 控制了词长度，包含2个及以上的词

#############################
	### (5) 制作lda平滑扩展特征所需的输入向量
	folding <- c(rep(0,nrow(news_train)),rep(1,nrow(news_test)))
	LDA_Vector_TFIDF_90_10 <- matrix(data=NA,length(folding),
		ncol(LDA_Vector_train_TFIDF))
	LDA_Vector_TFIDF_90_10[folding != 1] <- LDA_Vector_train_TFIDF
	LDA_Vector_TFIDF_90_10[folding == 1] <- LDA_Vector_test_TFIDF
	
	key_Vector_role_TFIDF_90_10 <- matrix(data=NA,length(folding),
		ncol(key_Vector_role_train_TFIDF))
	key_Vector_role_TFIDF_90_10[folding != 1] <- key_Vector_role_train_TFIDF
	key_Vector_role_TFIDF_90_10[folding == 1] <- key_Vector_role_test_TFIDF
	key_Vector_role_TFIDF_90_10[is.na(key_Vector_role_TFIDF_90_10)] <- 0
	colnames(key_Vector_role_TFIDF_90_10) <- colnames(key_Vector_role_train_TFIDF)

	classes <- rep(0,length(folding))
	classes[folding != 1] <- classes_tr
	classes[folding == 1] <- classes_t
	classes <- as.factor(classes)	
	classes_tr <- as.factor(classes_tr)	
	classes_t <- as.factor(classes_t)
#################################################################################
###   二、  LDA 及关键词特征扩展特征训练
##############################################################################
##############################################################################
### 1、扩展频率最高主题对应的高频词到稀疏特征文本中

	result <- sms_dtm_sparse(key_Vector_role_TFIDF)
	chain <- 0
	folding1 <- result$folding

	Sparse <- 1-folding1  ##稀疏因子标注
	key_Vector_role_lda <- key_Vector_role_TFIDF
	key_Vector_spare <- key_Vector_role_TFIDF[folding1==chain,]
	LDA_sparse <- LDA_Vector_TFIDF[folding1==chain,]
	p_terms <- posterior(training_all_TFIDF)$terms
	
	for(i in 1 : nrow(key_Vector_role_TFIDF))
	{
		if(folding1[i]==0)
		{
			T <- which.max(LDA_Vector_TFIDF[i,])
			words <- colnames(p_terms)
			words_freq <- p_terms[T,]
			words_names <- names(words_freq)
			words_length <- as.numeric(words_freq)
			words_df <- data.frame(words_names=words_names, words_freq=words_freq,words_length=words_length)
			#keyroleDic <- names_key_role[names_key_role[,1] %in% words_df$words_names, ]
			key <- words_df[words_df[,1] %in% names_key_role$words_names, ]
			#words_df2 <- merge(words_df, keyroleDic , by='words_names', all.x=T)
			for(j in 1 : ncol(key_Vector_role_TFIDF))
			{
				if(key$words_freq[j] > 0.1)
					key_Vector_role_lda[i,j] <- key$words_freq[j] 
			}
		}
	}
	sort(apply(key_Vector_role_lda[folding1==chain,],1,sum))

###################################################################
### 2、扩展频率最高前三个主题对应的高频词到稀疏特征文本中

	result <- sms_dtm_sparse(key_Vector_role_TFIDF)
	chain <- 0
	folding1 <- result$folding
	
	T <- rep(0,3)
	WT <- rep(0,3)  #权重
	Sparse <- 1-folding1  ##稀疏因子标注
	key_Vector_role_lda <- key_Vector_role_TFIDF
	key_Vector_spare <- key_Vector_role_TFIDF[folding1==chain,]
	LDA_sparse <- LDA_Vector_TFIDF[folding1==chain,]
	p_terms <- posterior(training_all_TFIDF)$terms
	
	for(i in 1 : nrow(key_Vector_role_TFIDF))
	{
		if(folding1[i]==0)
		{
			tmp <- LDA_Vector_TFIDF[i,]
			T[1] <- which.max(tmp)
			tmp[T[1]] <- 0
			T[2] <- which.max(tmp)
			tmp[T[2]] <- 0
			T[3] <- which.max(tmp)
		#前三主题概率分布权重
			if(sum(LDA_Vector_TFIDF[i,c(T)])!=0)
				WT <- LDA_Vector_TFIDF[i,c(T)]/sum(LDA_Vector_TFIDF[i,c(T)])
			else
				WT <- rep(0,3)
		#去掉概率 <0.2 的主题
			WT[WT<0.4]<-0
			if(sum(WT)!=0)
				WT <- WT/sum(WT)
			
			words <- colnames(p_terms)
			words_freq <- WT[1]*p_terms[T[1],]+ WT[2]*p_terms[T[2],] + WT[3]*p_terms[T[3],]
			words_names <- names(words_freq)
			words_length <- as.numeric(words_freq)
			words_df <- data.frame(words_names=words_names, words_freq=words_freq,words_length=words_length)
			#keyroleDic <- names_key_role[names_key_role[,1] %in% words_df$words_names, ]
			key <- words_df[words_df[,1] %in% names_key_role$words_names, ]
			#words_df2 <- merge(words_df, keyroleDic , by='words_names', all.x=T)
			for(j in 1 : ncol(key_Vector_role_TFIDF))
			{
				if(key$words_freq[j] > 0.3)
					key_Vector_role_lda[i,j] <- key$words_freq[j] 
			}
		}
	}
	sort(apply(key_Vector_role_lda[folding1==chain,],1,sum))

###################################################################
### 3、扩展频率最高主题对应的高频词到全部特征文本中

	result <- sms_dtm_sparse(key_Vector_role_TFIDF)
	chain <- 0
	folding1 <- result$folding

	Sparse <- 1-folding1  ##稀疏因子标注
	key_Vector_role_lda <- key_Vector_role_TFIDF
	key_Vector_spare <- key_Vector_role_TFIDF[folding1==chain,]
	LDA_sparse <- LDA_Vector_TFIDF[folding1==chain,]
	p_terms <- posterior(training_all_TFIDF)$terms
	
	for(i in 1 : nrow(key_Vector_role_TFIDF))
	{
		#if(folding1[i]==0)
		#{
			T <- which.max(LDA_Vector_TFIDF[i,])
			words <- colnames(p_terms)
			words_freq <- p_terms[T,]
			words_names <- names(words_freq)
			words_length <- as.numeric(words_freq)
			words_df <- data.frame(words_names=words_names, words_freq=words_freq,words_length=words_length)
			#keyroleDic <- names_key_role[names_key_role[,1] %in% words_df$words_names, ]
			key <- words_df[words_df[,1] %in% names_key_role$words_names, ]
			#words_df2 <- merge(words_df, keyroleDic , by='words_names', all.x=T)
			for(j in 1 : ncol(key_Vector_role_TFIDF))
			{
				if(key$words_freq[j] > 0.1)
					key_Vector_role_lda[i,j] <- key$words_freq[j] 
			}
		#}
	}
	sort(apply(key_Vector_role_lda[folding1==chain,],1,sum))

###################################################################
### 4、扩展频率最高主题对应的高频词到全部特征文本中，只针对训练集训练lda

	result <- sms_dtm_sparse(key_Vector_role_TFIDF_90_10)
	chain <- 0
	folding1 <- result$folding
	result$rate
	Sparse <- 1-folding1  ##稀疏因子标注
	key_Vector_role_lda_90_10 <- key_Vector_role_TFIDF_90_10
	#key_Vector_spare <- key_Vector_role_TFIDF_90_10[folding1==chain,]
	#LDA_sparse <- LDA_Vector_TFIDF_90_10[folding1==chain,]
	p_terms <- list()
	p_terms[[1]] <- posterior(training_90_TFIDF)$terms
	p_terms[[2]] <- posterior(test_90_TFIDF)$terms
	

	for(i in 1 : nrow(key_Vector_role_TFIDF_90_10))
	{
		#if(folding1[i]==0)
		#{
			T <- which.max(LDA_Vector_TFIDF_90_10[i,])
			words <- colnames(p_terms[[1]])
			if(folding[i]!=1) words_freq <- p_terms[[1]][T,]
			else words_freq <- p_terms[[2]][T,]
	
			words_names <- names(words_freq)
			words_length <- as.numeric(words_freq)
			words_df <- data.frame(words_names=words_names, words_freq=words_freq,words_length=words_length)
			#keyroleDic <- names_key_role[names_key_role[,1] %in% words_df$words_names, ]
			key <- words_df[words_df[,1] %in% names_key_role$words_names, ]
			#words_df2 <- merge(words_df, keyroleDic , by='words_names', all.x=T)
			for(j in 1 : ncol(key_Vector_role_TFIDF_90_10))
			{
				if(key$words_freq[j] > 0.1)
					key_Vector_role_lda_90_10[i,j] <- key$words_freq[j] 
			}
		#}
	}
	sort(apply(key_Vector_role_lda_90_10[folding1==chain,],1,sum))

##
	Sample <- matrix(data = NA,length(classes),(ncol(LDA_Vector_TFIDF_90_10)
		+ncol(key_Vector_role_lda_90_10)+1))
	
	Sample[,1:ncol(LDA_Vector_TFIDF_90_10)] <- LDA_Vector_TFIDF_90_10
	Sample[,((ncol(LDA_Vector_TFIDF_90_10)+1):(ncol(LDA_Vector_TFIDF_90_10)
		+ncol(key_Vector_role_lda_90_10)))] <- key_Vector_role_lda_90_10
	Sample[,ncol(Sample)] <- classes
	Sample <- as.data.frame(Sample)

	LDA_name <- rep(0,ncol(LDA_Vector_TFIDF_90_10))
	for(i in 1:ncol(LDA_Vector_TFIDF_90_10))
	{
		LDA_name[i] <- paste("Topic",i,sep = "")
	}
	key_names <- colnames(key_Vector_role_lda_90_10)
	Role_names <- "Role"
	Sample_names <- union(union(LDA_name,key_names),Role_names)
	
	colnames(Sample) <- Sample_names

###################################################################
### 5、扩展频率最高主题对应的高频词到全部特征文本中，训练新闻标题集
	result <- sms_dtm_sparse(key_Vector_role_TFIDF_90_10)
	chain <- 0
	folding1 <- result$folding
	result$rate
	Sparse <- 1-folding1  ##稀疏因子标注
	key_Vector_role_lda_90_10 <- key_Vector_role_TFIDF_90_10

	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("H_Freq","LDA","LDA + H_Freq"))

	#key_Vector_spare <- key_Vector_role_TFIDF_90_10[folding1==chain,]
	#LDA_sparse <- LDA_Vector_TFIDF_90_10[folding1==chain,]

	p_terms <- list()
	p_terms[[1]] <- posterior(training_train_TFIDF)$terms
	p_terms[[2]] <- posterior(training_test_TFIDF)$terms
	
## (1) 针对频率最高主题做处理
	for(i in 1 : nrow(key_Vector_role_TFIDF_90_10))
	{
		if(folding1[i]==0)
		{
			T <- which.max(LDA_Vector_TFIDF_90_10[i,])
			words <- colnames(p_terms[[1]])
			if(folding[i]!=1) words_freq <- p_terms[[1]][T,]
			else words_freq <- p_terms[[2]][T,]
	
			words_names <- names(words_freq)
			words_length <- as.numeric(words_freq)
			words_df <- data.frame(words_names=words_names, words_freq=words_freq,words_length=words_length)
			#keyroleDic <- names_key_role[names_key_role[,1] %in% words_df$words_names, ]
			key <- words_df[words_df[,1] %in% names_keyNews_role$words_names, ]
			#words_df2 <- merge(words_df, keyroleDic , by='words_names', all.x=T)
			for(j in 1 : ncol(key_Vector_role_TFIDF_90_10))
			{
				if(key$words_freq[j] > 0.1)
					key_Vector_role_lda_90_10[i,j] <- key$words_freq[j] 
			}
		}
	}
	sort(apply(key_Vector_role_lda_90_10[folding1==0,],1,sum))
	sort(apply(key_Vector_role_lda_90_10[folding1==1,],1,sum))
##   Single Topic LDA_Key which extents to sparse documents
	#source("r_code/SVM_Vector_predict.r")
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("SLH_Freq","LDA","LDA + SLH_Freq"))

## (2) 针对频率最高较高的几个主题做处理
	for(i in 1 : nrow(key_Vector_role_TFIDF_90_10))
	{
		freq_tmp <- LDA_Vector_TFIDF_90_10
		key_tmp <- rep(0,ncol(key_Vector_role_TFIDF_90_10))
		if(folding1[i]==0)
		{
			
			#T <- which.max(freq_tmp[i,])
			
			#if(folding[i]!=1) 
			#	key_tmp <- LDA_HighWords_gen(p_terms,names_keyNews_role,
			#		T,prob = 0.01,IsTrains=TRUE)
			#else 
			#	key_tmp <- LDA_HighWords_gen(p_terms,names_keyNews_role,
			#		T,prob = 0.01,IsTrains=FALSE)
			key_3st <- Topic_3st <- list()
			M <- 1
			Weight <- 0
			for(s in 1:3)
			{
				T <- which.max(freq_tmp[i,])
			
				if(folding[i]!=1) 
					key_tmp <- LDA_HighWords_gen(p_terms,names_keyNews_role,
						T,prob = 0.01,IsTrains=TRUE)
				else 
					key_tmp <- LDA_HighWords_gen(p_terms,names_keyNews_role,
						T,prob = 0.01,IsTrains=FALSE)
				

				if(freq_tmp[i,T] > 0.1)
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

			key_Vector_role_lda_90_10[i,] <- 0
			for(m in 1:(M-1))
			{
				if(M > 1)
				{
					tmp_3st <- key_3st[[m]][key_3st[[m]]$freq>0.015*Weight,] #0.0143
					key_Vector_role_lda_90_10[i,as.character(tmp_3st$key_names)] <- 
						as.numeric(tmp_3st$key_freq)/(0.015*Weight) ###freq
				}
			}
			#sort(key_Vector_role_lda_90_10[i,])[512:521]

		}
	}

	#colnames(key_Vector_role_lda_90_10) <- key_tmp$key_names
	sort(apply(key_Vector_role_lda_90_10[folding1==0,],1,sum))
	sort(apply(key_Vector_role_lda_90_10[folding1==1,],1,sum))
##   Mult Topic LDA_Key which extents to sparse documents
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("MLH_Freq","LDA","LDA + MLH_Freq"))

##
	Sample <- matrix(data = NA,length(classes),(ncol(LDA_Vector_TFIDF_90_10)
		+ncol(key_Vector_role_lda_90_10)+1))
	
	Sample[,1:ncol(LDA_Vector_TFIDF_90_10)] <- LDA_Vector_TFIDF_90_10
	Sample[,((ncol(LDA_Vector_TFIDF_90_10)+1):(ncol(LDA_Vector_TFIDF_90_10)
		+ncol(key_Vector_role_lda_90_10)))] <- key_Vector_role_lda_90_10
	Sample[,ncol(Sample)] <- classes
	Sample <- as.data.frame(Sample)

	LDA_name <- rep(0,ncol(LDA_Vector_TFIDF_90_10))
	for(i in 1:ncol(LDA_Vector_TFIDF_90_10))
	{
		LDA_name[i] <- paste("Topic",i,sep = "")
	}
	key_names <- colnames(key_Vector_role_lda_90_10)
	Role_names <- "Role"
	Sample_names <- union(union(LDA_name,key_names),Role_names)
	
	colnames(Sample) <- Sample_names

	### 训练数据集
	training <- Sample[1:length(classes_tr),]
			
	### 测试数据集
	testing <- Sample[(length(classes_tr)+1):length(classes),]

###################################################################
### 6、扩展频率最高主题对应的高频词到全部特征文本中，训练短信数据集
	result <- sms_dtm_sparse(key_Vector_role_TFIDF_90_10)
	chain <- 0
	folding1 <- result$folding
	result$rate
	Sparse <- 1-folding1  ##稀疏因子标注
	key_Vector_role_lda_90_10 <- key_Vector_role_TFIDF_90_10

	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("H_Freq","LDA","LDA + H_Freq"))

	#key_Vector_spare <- key_Vector_role_TFIDF_90_10[folding1==chain,]
	#LDA_sparse <- LDA_Vector_TFIDF_90_10[folding1==chain,]

	p_terms <- list()
	p_terms[[1]] <- posterior(training_90_TFIDF)$terms
	p_terms[[2]] <- posterior(test_90_TFIDF)$terms
	#sort(inspect(dtm_pre[folding1==0,])[3,])
##  针对频率较高主题做处理
	for(i in 1 : nrow(key_Vector_role_TFIDF_90_10))
	{
		freq_tmp <- LDA_Vector_TFIDF_90_10
		key_tmp <- rep(0,ncol(key_Vector_role_TFIDF_90_10))

		if(folding1[i]==0)
		{
			#T <- which.max(freq_tmp[i,])
			
			#if(folding[i]!=1) 
			#	key_tmp <- LDA_HighWords_gen(p_terms,names_key_role,
			#		T,prob = 0.01,IsTrains=TRUE)
			#else 
			#	key_tmp <- LDA_HighWords_gen(p_terms,names_key_role,
			#		T,prob = 0.01,IsTrains=FALSE)
			key_3st <- Topic_3st <- list()
			M <- 1
			Weight <- 0
			for(s in 1:3)
			{
				T <- which.max(freq_tmp[i,])
			
				if(folding[i]!=1) 
					key_tmp <- LDA_HighWords_gen(p_terms,names_key_role,
						T,prob = 0.01,IsTrains=TRUE)
				else 
					key_tmp <- LDA_HighWords_gen(p_terms,names_key_role,
						T,prob = 0.01,IsTrains=FALSE)
				

				if(freq_tmp[i,T] > 0.1)
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

			key_Vector_role_lda_90_10[i,] <- 0
			for(m in 1:(M-1))
			{
				if(M > 1)
				{
					tmp_3st <- key_3st[[m]][key_3st[[m]]$freq>0.05*Weight,] #0.0143
					key_Vector_role_lda_90_10[i,as.character(tmp_3st$key_names)] <- 
						as.numeric(tmp_3st$key_freq)/(0.015*Weight) ###freq
				}
			}
			#sort(key_Vector_role_lda_90_10[i,])[512:521]
			
		}
	}
	sort(apply(key_Vector_role_lda_90_10[folding1==0,],1,sum))
	#sort(apply(key_Vector_role_lda_90_10[folding1==1,],1,sum))

##   Single Topic LDA_Key which extents to sparse documents
	#source("r_code/SVM_Vector_predict.r")
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("SLH_Freq","LDA","LDA + SLH_Freq"))

##   Single Topic LDA_Key which extents to all documents
	#source("r_code/SVM_Vector_predict.r")
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("ASLH_Freq","LDA","LDA + ASLH_Freq"))


##   Mult Topic LDA_Key which extents to sparse documents
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("MLH_Freq","LDA","LDA + MLH_Freq"))

##   Mult Topic LDA_Key which extents to all documents
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("AMLH_Freq","LDA","LDA + AMLH_Freq"))

###################################################################
### 7、单独评价sms 关键词效果
### 高频词
	key_Vector_role_lda_90_10 <- key_Vector_role_TFIDF_90_10
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("H_Freq","LDA","LDA + H_Freq"))
### 搜狗全部词
	tmp_vector <- sms_cut_role(key_Vector_15Sogou_TFIDF,iRole,c(5,0))
	key_Vector_role_lda_90_10 <- tmp_vector$Sample
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("Sogou_w","LDA","LDA + Sogou_w"))

### 各角色取100个高频词
	tmp_vector <- sms_cut_role(key_Vector_role_100_TFIDF,iRole,c(5,0))
	key_Vector_role_lda_90_10 <- tmp_vector$Sample
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("H100_Freq","LDA","LDA + H100_Freq"))

### 各角色取100个高频词，包含other
	tmp_vector <- sms_cut_role(key_Vector_role_100_TFIDF,iRole,c(0))
	key_Vector_role_lda_90_10 <- tmp_vector$Sample

	tmp_dtm <- sms_cut_role(dtm_new,iRole,c(0))
	tmp_LDA <- LDA(tmp_dtm$Sample, k = 80,control = list(verbose = 100))
	LDA_Vector <- posterior(tmp_LDA)$topics
	
	SVM_Vector_predict(LDA_Vector,key_Vector_role_lda_90_10,
		tmp_vector$classes,tmp_vector$folding,1,
		c("H100_Freq_PL1","LDA_PL1","LDA + H100_Freq_PL1"))
## LDA 扩展稀疏关键词
	key_Vector_role_lda_90_10 <- Hkey_topic_extend(key_Vector_role_TFIDF_90_10,training_90_TFIDF,test_90_TFIDF,
		names_key_role,3,Sparse=TRUE)
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("MLH_Freq","LDA","LDA + MLH_Freq"))

	key_Vector_role_lda_90_10 <- Hkey_topic_extend(key_Vector_role_TFIDF_90_10,training_90_TFIDF,test_90_TFIDF,
		names_key_role,1,Sparse=TRUE)
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("SLH_Freq","LDA","LDA + SLH_Freq"))

	key_Vector_role_lda_90_10 <- Hkey_topic_extend(key_Vector_role_TFIDF_90_10,training_90_TFIDF,test_90_TFIDF,
		names_key_role,1,Sparse=FALSE)
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("ASLH_Freq","LDA","LDA + ASLH_Freq"))

	key_Vector_role_lda_90_10 <- Hkey_topic_extend(key_Vector_role_TFIDF_90_10,training_90_TFIDF,test_90_TFIDF,
		names_key_role,3,Sparse=FALSE)
	SVM_Vector_predict(LDA_Vector_TFIDF_90_10,key_Vector_role_lda_90_10,
		classes,folding,9,c("AMLH_Freq","LDA","LDA + AMLH_Freq"))

###########################################################################
### SMS LDA平滑稀疏文档的关键词矩阵  #########################################
##########################################################################
### input: dat 原始短信数据
###        dat_pre 合并同一发送消息为一文档后的短信数据集
###        dat_pre_sparse 去掉词条矩阵上稀疏数据后的短信数据集
###        dtm_new 对应dat_pre_sparse的词条矩阵
###        iRole  对应dat_pre_sparse的角色标注数据
###        Training_Data_Role 最初生成的对应dat_pre_sparse的用于svm的特征向量
#########################################################################
	testChain <- 9
	Topic_best <- 80
	Mult <- 3
	Single <- 1
	SEED <- 20080809

###########################################################################
## 1、生成关键词特征向量，制作关键词字典                            ############
##########################################################################
## 去掉other语料
	Role_raw <- dat[,7:11]
	x<-cbind(matrix(rep(c(1)),nrow(Role_raw)),matrix(rep(c(2)),nrow(Role_raw)),
		matrix(rep(c(3)),nrow(Role_raw)),matrix(rep(c(4)),nrow(Role_raw)),
		matrix(rep(c(5)),nrow(Role_raw)))
	iRole_raw <- apply(Role_raw*x,1,sum)

	tmpdat <- sms_cut_role(dat,iRole_raw,c(0))
### 制作词典
	tmpDict <- key_Dict_gen(tmpdat$Sample[,20],tmpdat$classes,50,wlen=5)
	#tmpDict <- key_Dict_gen(tmpdat$Sample[,18],tmpdat$classes,50,wlen=5)
### 应用做好的词典，产生高频词向量
	tmp_key_Vector <- inspect(DocumentTermMatrix(re1,list(wordLengths=c(2,10), 
		weighting = weightTfIdf,dictionary = tmpDict$dict)))
### 裁掉white和other两个角色语料
	tmp_vector <- sms_cut_role(tmp_key_Vector,iRole,c(5,0))
	dim(tmp_vector$Sample)
	length(tmp_vector$folding)
	key_Vector <- tmp_vector$Sample

###########################################################################
## 2、训练LDA主题模型及特征向量                                   ############
##########################################################################
	reuters <- as.matrix(dat_pre[,20])
	reuters <- Corpus(DataframeSource(reuters)) 
	dtm <- DocumentTermMatrix(reuters,
			control = list(wordLengths=c(2,10),weighting = weightTf))
	result <- sms_dtm_sparse(dtm)
	dtm_new_t <- dtm[result$folding != 0,]
	tmp_dtm <- sms_cut_role(dtm_new_t,iRole,c(5,0))
	#tmp_dtm$Sample
	#tmp_dtm$classes
	#tmp_dtm$folding
	dtm_train <- tmp_dtm$Sample[folding != testChain,] # 得到抽样的训练集
	dtm_test <- tmp_dtm$Sample[folding == testChain,] # 得到抽样的训练集
	train_LDA <- LDA(dtm_train, k = Topic_best,control = list(verbose = 100))
	test_LDA <- LDA(dtm_test, model = train_LDA,
				control = list(estimate.beta = FALSE, seed = SEED))
	LDA_Vector <- matrix(data=NA,length(tmp_dtm$classes),Topic_best)
	LDA_Vector[folding != testChain ,] <- posterior(train_LDA)$topics
	LDA_Vector[folding == testChain ,] <- posterior(test_LDA)$topics

###########################################################################
## 3、LDA模型平滑处理关键词向量，解决稀疏性                         ############
##########################################################################
###  (1) 多主题平滑稀疏文档
	key_Vector_lda <- Hkey_topic_extend(key_Vector,train_LDA,test_LDA,
		tmpDict$names,Mult,Sparse=TRUE)
### 预测 
	SVM_Vector_predict(LDA_Vector,key_Vector_lda,
		classes,folding,testChain ,c("MLH_Freq","LDA","LDA + MLH_Freq"))
###  (2) 单主题平滑稀疏文档
	key_Vector_lda <- Hkey_topic_extend(key_Vector,train_LDA,test_LDA,
		tmpDict$names,Single,Sparse=TRUE)
	SVM_Vector_predict(LDA_Vector,key_Vector_lda,
		classes,folding,testChain ,c("SLH_Freq","LDA","LDA + SLH_Freq"))
###  (3) 单主题平滑全部文档
	key_Vector_lda <- Hkey_topic_extend(key_Vector,train_LDA,test_LDA,
		tmpDict$names,Single,Sparse=FALSE)
	SVM_Vector_predict(LDA_Vector,key_Vector_lda,
		classes,folding,testChain ,c("ASLH_Freq","LDA","LDA + ASLH_Freq"))
###  (4) 多主题平滑全部文档
	key_Vector_lda <- Hkey_topic_extend(key_Vector,train_LDA,test_LDA,
		tmpDict$names,Mult,Sparse=FALSE)
	SVM_Vector_predict(LDA_Vector,key_Vector_lda,
		classes,folding,testChain ,c("AMLH_Freq","LDA","LDA + AMLH_Freq"))
###  (5) 直接高频词，不做LDA扩展
	SVM_Vector_predict(LDA_Vector,key_Vector,
		classes,folding,testChain ,c("H_Freq","LDA","LDA + H_Freq"))


############################################################
### 参考程序
	reuters <- as.matrix(dat_pre[,20])
	reuters <- Corpus(DataframeSource(reuters)) 
	dtm <- DocumentTermMatrix(reuters,
			control = list(wordLengths=c(2,10),weighting = weightTf))
	result <- sms_dtm_sparse(dtm)
	chain <- 0
	folding <- result$folding
	re1 <- reuters[folding != chain]
	re1 <- Corpus(VectorSource(re1))


