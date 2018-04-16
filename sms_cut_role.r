############ smsTable : �������ݱ�������š����ͺš����պš���������
############ ��ͬһ�Է��͡������ߵĶ����������ϵ�һ���ı���
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
				tSample <- tSample[tclasses != cutRole[i],] #ȥ������
				tclasses <- tclasses[tclasses != cutRole[i]] #ȥ������
			}
		}
		else
		{
			tSample <- tSample[tclasses != cutRole,] #ȥ������
			tclasses <- tclasses[tclasses != cutRole] #ȥ������

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