############ txtTable : table��ʽ��ԭʼԤ��
############ n: Ԥ�ϳ���
############ output: �ִʴ���������Ԥ��
	sms_zwfc <- function (txtTable,n,zj)
	{

		source("r_code/zwfc.r")	
	
		re <- 0
		for (i in 1:n) 
		{
    			re[[i]]<-  zwfc(PlainTextDocument(txtTable)[[i]],zj)
    		}
###  �����µ��ļ�  ### 
		reuters <- Corpus(VectorSource(re))
		reuters
	}