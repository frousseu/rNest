calendarprop <-
function(x,jul=1:365,column){
	dates<-x[,column] # change quantile !!!
	r<-range(dates,na.rm=T)
	p<-sapply(jul,function(i){
		sum(dates<=i,na.rm=T)/sum(!is.na(dates))
	})
	p
}
