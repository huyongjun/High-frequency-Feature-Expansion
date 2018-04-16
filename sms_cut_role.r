############ smsTable : 短信数据表，包括序号、发送号、接收号、短信内容
############ 把同一对发送、接收者的短信内容整合到一个文本中
############ 
############ 
	sms_cut_role <- function(vector,label,cutRole)
	{
		SEED <- 20080809

		if(is.na(cutRole[1])==TRUE)
			cutRole <- c(5,0)

		tSample <- vector
		tclasses <- label
		if(length(cutRole)>1)
		{
			for(i in 1:length(cutRole))
			{
				tSample <- tSample[tclasses != cutRole[i],] #去掉白领
				tclasses <- tclasses[tclasses != cutRole[i]] #去掉白领
			}
		}
		else
		{
			tSample <- tSample[tclasses != cutRole,] #去掉白领
			tclasses <- tclasses[tclasses != cutRole] #去掉白领

		}		
		tclasses <- as.factor(tclasses)	

		D <- rep(0,length(as.numeric(table(tclasses))))
		tFolding <- list()
		for(i in 1:length(D))
		{
			D[i] <- as.numeric(table(tclasses)[i])
			tFolding[[i]] <- sample(rep(seq_len(10), ceiling(D[i]))[seq_len(D[i])])		
		}

		tfolding <- rep(0,nrow(tSample))
		count <- rep(1,length(D))
		for(i in 1:nrow(tSample))
		{
			cl <- as.numeric(tclasses[i])
			n <- count[cl]
			tfolding[i] <- tFolding[[cl]][n]
			count[cl] <- count[cl] + 1
		}
		result <- list(Sample=tSample,classes=tclasses,folding=tfolding)
		result
	}