require(RWeka);require(caret);require(data.table)
require(Rgraphviz)

j48_weka.fn <- function(train,test,campo,cf) # train <- read.arff('CentroSalud_limpio_train_75r2.arff');cf <- 0.5
{
  # dep <- eval(parse(text=paste('train$',campo,sep=''))) # cf <- 0.5
  dep <- paste('train$',campo,'~.',sep='') # campo <- 'gender'
  tri <- J48(as.formula(dep),data=train,na.action=NULL
             ,control = Weka_control(C=cf)) # cf <- 0.5
  
  a <- capture.output(tri)
  la <- length(a)
  n <- as.numeric(gsub('\\D','',a[la-1]))
  h <- as.numeric(gsub('\\D','',a[la-3]))
  
  train.perf <- evaluate_Weka_classifier(tri)
  train.perf <- round(train.perf$details[1],2)
  
  test.pred <- predict(tri,test)
  test.tab <- table(eval(parse(text=paste('test$',campo,sep=''))), test.pred)
  test.perf <- round(sum(test.tab[1,1],test.tab[2,2])/length(test.pred)*100,2)
  
  return(c(cf,n,h,train.perf,test.perf))
  # return(test.pred)
}
