############ dtm : ��Ƶ�ĵ�����
############ output: û�п��е��ĵ�ռ���ĵ��ı���
	sms_dtm_sparse <- function (dtm)
	{
		n <- nrow(dtm)
		nullText <- matrix(0,n)
		folding <- matrix(0,n)
		j <- 1
		for(i in 1:n)
		{
			if(sum(dtm[i,])==0)
			{
				nullText[j] <- i
				j <- (j+1)	
			}
			else
			{
				folding[i] <- 1
			}			
		}			
		result <- list(folding = folding, rate = sum(folding)/n)
		result
	}