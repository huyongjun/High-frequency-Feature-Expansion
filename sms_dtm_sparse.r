############ dtm : 词频文档矩阵
############ output: 没有空行的文档占总文档的比率
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