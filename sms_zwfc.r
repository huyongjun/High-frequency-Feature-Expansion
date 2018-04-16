############ txtTable : table格式的原始预料
############ n: 预料长度
############ output: 分词处理后的中文预料
	sms_zwfc <- function (txtTable,n,zj)
	{

		source("r_code/zwfc.r")	
	
		re <- 0
		for (i in 1:n) 
		{
    			re[[i]]<-  zwfc(PlainTextDocument(txtTable)[[i]],zj)
    		}
###  生成新的文集  ### 
		reuters <- Corpus(VectorSource(re))
		reuters
	}