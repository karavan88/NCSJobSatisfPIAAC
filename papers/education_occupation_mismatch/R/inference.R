repinfo <- function(d) {
 r<-unique(d$VENREPS); method<-unique(trimws(as.character(d$VEMETHOD)))
 stopifnot(length(r)==1,length(method)==1,method %in% c('FAY','JK2'))
 k<-unique(d$VEFAYFAC)
 factor<-if(method=='JK2') 1 else 1/(r*(1-k)^2)
 w<-as.matrix(d[paste0('SPFWT',0:r)])
 stopifnot(all(is.finite(w)),all(w>=0),all(colSums(w)>0))
 list(w=w,factor=factor,r=r,method=method)
}
pool <- function(b,u,r) {
 m<-ncol(b); est<-rowMeans(b); within<-Reduce('+',u)/m
 between<-if(m==1) matrix(0,nrow(b),nrow(b)) else stats::cov(t(b))
 total<-within+(1+1/m)*between
 se<-sqrt(pmax(diag(total),0)); imp<-(1+1/m)*diag(between)
 df<-ifelse(imp>0,pmin(r-1,(m-1)*(1+diag(within)/imp)^2),r-1)
 df[!is.finite(df)]<-r-1
 data<-data.frame(term=rownames(b),estimate=est,se=se,df=df,
  lower=est-qt(.975,df)*se,upper=est+qt(.975,df)*se,p=2*pt(-abs(est/se),df))
 list(table=data,cov=total)
}
fit_model <- function(d,cc,id,y,predictors,pv='PVNUM',joint=character(),center=FALSE) {
 need<-c(y,setdiff(predictors,c('cognition',grep(':',predictors,value=TRUE))),paste0(pv,1:10))
 # Explicit constituents of the interaction terms are included separately.
 s<-droplevels(d[complete.cases(d[unique(need)]),,drop=FALSE])
 if(nrow(s)<200 || length(unique(s[[y]]))<2) return(NULL)
 omitted<-predictors[vapply(predictors,function(v) v %in% names(s) && is.factor(s[[v]]) && nlevels(s[[v]])<2,logical(1))]
 predictors<-setdiff(predictors,omitted)
 if(center) for(v in traits) s[[v]]<-s[[v]]-wm(s[[v]],s$SPFWT0)
 ri<-repinfo(s); f<-reformulate(predictors,response=y)
 bs<-list(); us<-list(); outbounds<-numeric(10); ranks<-numeric(10)
 for(j in 1:10) {
  s$cognition<-(s[[paste0(pv,j)]]-250)/50
  X<-model.matrix(f,s); Y<-s[[y]]
  if(j==1) {
   qr0<-qr(X*sqrt(ri$w[,1]/mean(ri$w[,1])))
   keep<-sort(qr0$pivot[seq_len(qr0$rank)])
   omitted<-c(omitted,setdiff(colnames(X),colnames(X)[keep]))
   stopifnot(all(c('cognition',joint) %in% colnames(X)[keep]))
  }
  X<-X[,keep,drop=FALSE]
  W<-ri$w/mean(ri$w[,1])
  # Full column rank is required in every fit, including all replicate weights.
  fit<-function(w) {
   A<-crossprod(X,X*w); z<-crossprod(X,Y*w)
   drop(solve(A,z))
  }
  estimates<-vapply(seq_len(ncol(W)),function(k) fit(W[,k]),numeric(ncol(X)))
  rownames(estimates)<-colnames(X)
  b<-estimates[,1]; delta<-sweep(estimates[,-1,drop=FALSE],1,b)
  bs[[j]]<-b; us[[j]]<-ri$factor*tcrossprod(delta)
  pred<-drop(X%*%b);outbounds[j]<-wm(as.numeric(pred<0 | pred>1),W[,1]);ranks[j]<-ncol(X)
 }
 p<-pool(do.call(cbind,bs),us,ri$r); t<-p$table;t$country<-cc;t$model<-id;t$n<-nrow(s)
 ji<-match(joint,t$term);ji<-ji[!is.na(ji)]
 block<-NULL
 if(length(ji)) {
  b<-t$estimate[ji]; stat<-drop(t(b)%*%solve(p$cov[ji,ji,drop=FALSE],b))
  block<-data.frame(country=cc,model=id,n=nrow(s),statistic=stat,df=length(ji),p=pchisq(stat,length(ji),lower.tail=FALSE))
 }
 list(table=t,cov=p$cov,block=block,diagnostic=data.frame(country=cc,model=id,n=nrow(s),
  events=sum(s[[y]]==1),parameters=ranks[1],reps=ri$r,method=ri$method,
  outside=if(y=='satisfaction_score') NA_real_ else mean(outbounds),
  omitted=paste(omitted,collapse=';')))
}
