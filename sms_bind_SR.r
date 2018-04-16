############ smsTable : �������ݱ�������š����ͺš����պš���������
############ ��ͬһ�Է��͡������ߵĶ����������ϵ�һ���ı���
############ 
############ 
	sms_bind_SR <- function(smsTable)
	{
		cc <- dim(smsTable)
		sendNo <- smsTable[1,2]
		receiveNo <- smsTable[1,3]
		sms_bind <- smsTable[,1:3]

		docs <- data.frame(docs = smsTable[,4])
		(ds <- DataframeSource(docs))
		ovid <- Corpus(ds)
		ovid_bind <- ovid
		
		n <- cc[1]
		j <- 1
		for(i in 2:n)
		{
			if(receiveNo == smsTable[i,3])
			{
				ovid_bind[[j]] <- paste(ovid_bind[[j]],ovid[[i]])
			}
			else
			{
				j <- (j+1)
				sendNo <- smsTable[i,2]
				receiveNo <- smsTable[i,3]
				ovid_bind[[j]] <- ovid[[i]]
				sms_bind[j,1] <- j
				sms_bind[j,2:3] <- smsTable[i,2:3]
			}
		}

		corpus <- 0
		for(i in 1:j)
		{
			corpus[[i]] <- ovid_bind[[i]]
		}
		result <- list(corpus = corpus,sms=sms_bind[1:j,],n=j)
		result
	}