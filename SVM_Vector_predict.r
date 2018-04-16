############ p_result : 分类结果
############ 
############ 
############ output: 召回率、准确率等
	
	SVM_Vector_predict <- function(LDA_Vector,key_Vector,classes,folding,NoTest,ModelName)
	{
		library(e1071)
		source("r_code/class_evaluation.r")
		
		result <- list()
		if(ModelName[1]=="")
		{
			ModelName <- c("Key","LDA","LDA + Key")
		}

		Sample <- matrix(data = NA,length(classes),(ncol(LDA_Vector)
			+ncol(key_Vector)+1))
	
		Sample[,1:ncol(LDA_Vector)] <- LDA_Vector
		Sample[,((ncol(LDA_Vector)+1):(ncol(LDA_Vector)
			+ncol(key_Vector)))] <- key_Vector
		Sample[,ncol(Sample)] <- classes
		Sample <- as.data.frame(Sample)

		LDA_name <- rep(0,ncol(LDA_Vector))
		for(i in 1:ncol(LDA_Vector))
		{
			LDA_name[i] <- paste("Topic",i,sep = "")
		}
		key_names <- colnames(key_Vector)
		Role_names <- "Role"
		Sample_names <- union(union(LDA_name,key_names),Role_names)
	
		colnames(Sample) <- Sample_names
		#sort(apply(Sample[folding1==0,1:(ncol(Sample)-1)],1,sum))

	### 训练数据集
		classes_tr <- classes[folding != NoTest]
		training <- Sample[folding != NoTest,]
			
	### 测试数据集
		classes_t <- classes[folding == NoTest]
		testing <- Sample[folding == NoTest,]
	
		model <- svm(classes_tr~ ., training[,(ncol(LDA_Vector)+1):(ncol(training)-1)], kernel = "linear")
		pred <- predict(model, testing[,((ncol(LDA_Vector)+1):(ncol(training)-1))])
		p_result <- table(pred,t(classes_t))
		result[[ModelName[1]]] <- class_evaluation(p_result)

		model <- svm(classes_tr~ ., training[,1:ncol(LDA_Vector)], kernel = "linear")
		pred <- predict(model, testing[,1:ncol(LDA_Vector)])
		p_result <- table(pred,t(classes_t))
		result[[ModelName[2]]] <- class_evaluation(p_result)

		model <- svm(classes_tr~ ., training[,1:(ncol(training)-1)], kernel = "linear")
		pred <- predict(model, testing[,1:(ncol(training)-1)])
		p_result <- table(pred,t(classes_t))
		result[[ModelName[3]]] <- class_evaluation(p_result)
		
		result[["Length of Key_Vector"]] <- ncol(key_Vector)
		result[["Best Model"]] <- model
		result
	}