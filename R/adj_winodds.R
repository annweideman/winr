#' Adjusted win odds
#'
#' Randomization-based adjustment of the win odds for covariates and
#' strata.
#'
#' @param data a dataframe or matrix containing the analysis data. Must be in
#' wide format such that a participant's repeated responses are in a single row,
#' and each response is in a separate column.
#'
#' @param pid a string indicating the name of the variable corresponding to
#' participant ID.
#'
#' @param baseline a string indicating the name of the outcome measured at
#' baseline. If not specified, defaults to NULL, and no baseline adjustment is
#' employed.
#'
#' @param outcome a vector of strings indicating the names of the outcomes
#' measured at each visit. Baseline, if specified, will be concatenated to this
#' vector within the code. The outcomes must have at least an ordinal
#' measurement scale with larger values being better than smaller values.
#' Thus, the outcome can be ordered categories or continuous measurements.
#'
#' @param covars a vector of strings indicating the names of the covariates
#' (measured at baseline) used for adjustment. These covariates must be numeric
#' and can  be measured on a binary, categorical, ordered categorical, or
#' continuous scale. If not specified, defaults to NULL and no covariate
#' adjustment is employed.
#'
#' @param strata a string indicating the name of the variable used for
#' stratification. If not specified, defaults to NULL and no stratification is
#' utilized.
#'
#' @param arm a string indicating the name of the variable for treatment arm.
#' Treatment arm must be a positive integer such that the test treatment arm is
#' ALWAYS higher in value than the control arm.
#'
#' @param method a string "small" or "large" used to denote the method employed.
#' The small sample size method is recommended unless within-stratum sample size
#' is reasonably large (e.g., >= 50), number of visits is small (e.g., <=6), and
#' number of covariates is small (e.g., <=4). Defaults to "small."
#'
#' @param sig.level significance level (Type I error probability). Defaults to
#' 0.05.
#'
#' @return
#'
#' A  dataframe containing:
#' \item{logWR}{natural log-transformed win ratio}
#' \item{SE_logWR}{standard error of log-transformed win ratio}
#' \item{Var_logWR}{sample variance of log-transformed win ratio}
#' \item{Chi_Square}{Pearson's Chi-squared test statistic corresponding to
#' logWR}
#' \item{p_value}{p-value corresponding to the Pearson's Chi-squared test}
#' \item{WR}{win ratio}
#' \item{LCL_WR}{lower bound of \eqn{(1-\alpha/2)\times 100\%} CI for WR}
#' \item{UCL_WR}{upper bound of \eqn{(1-\alpha/2)\times 100\%} CI for WR}
#'
#' @examples
#'
#' #--------------------------
#' # Respiratory example
#' #--------------------------
#'
#' # Since IDs repeat at centers 1 and 2, create a new, unique ID
#' resp$new_ID<-resp$Center*100+resp$ID
#'
#' # Convert treatment arm to binary
#' resp$Treatment<-1*(resp$Treatment=="T")
#'
#' # Indicator for male
#' resp$Sex<-1*(resp$Sex=="M")
#'
#' adj_winodds(data=resp,
#'             pid="new_ID",
#'             baseline="Baseline",
#'             outcome=c("Visit1","Visit2","Visit3","Visit4"),
#'             covars= c("Sex","Age"),
#'             strata="Center",
#'             arm="Treatment",
#'             method="small",
#'             sig.level=0.05)
#'
#' #----------------------
#' # Skin example
#' #----------------------
#'
#' # Add column for patient IDs
#' skin$ID<-1:nrow(skin)
#'
#' # Remove missing rows
#' skin<-skin[complete.cases(skin), ]
#'
#' adj_winodds(data=skin,
#'             pid="ID",
#'             baseline=NULL,
#'             outcome=c("R1","R2","R3"),
#'             covars= c("Stage4","Stage5"),
#'             strata="center2",
#'             arm="TRT",
#'             method="small",
#'             sig.level=0.05)
#'
#' @export

adj_winodds<-function(data, pid, baseline=NULL, outcome, covars=NULL,
                      strata=NULL, arm, method="small", sig.level=0.05){

  # check arguments
  if (!inherits(data, c("data.frame","matrix"))){
    stop("data must be of class \"data.frame\" or \"matrix\"")
  }

  if (length(pid)>1){
    stop("pid must be of length one")
  }

  if (!is.character(pid)){
    stop("pid must be of type \"character\"")
  }

  if (!(pid %in% colnames(data))){
    stop("data must contain column \"pid\"")
  }

  if (!is.null(baseline)){
    if (!is.character(baseline)){
      stop("baseline must be of type \"character\" since it is specified")
    }
  }

  if (!is.null(baseline)){
    if(!(baseline %in% colnames(data))){
      stop("data must contain column \"baseline\" since it is specified")
    }
  }

  if (length(baseline)>1){
    stop("baseline must be of length one")
  }

  if (!is.character(outcome)){
    stop("outcome must be of type \"character\"")
  }

  if (!all(outcome %in% colnames(data))){
    stop("data must contain variable names in \"outcome\" since it is specified")
  }

  if (!is.null(covars)){
    if (!is.character(covars)){
      stop("covars must be of type \"character\" since it is specified")
    }
  }

  if (!is.null(covars)){
    if(!all(covars %in% colnames(data))){
      stop("data must contain variable names in \"covars\" since it is specified")
    }
  }

  if (!is.null(strata)){
    if (!is.character(strata)){
      stop("strata must be of type \"character\" since it is specified")
    }
  }

  if (is.null(strata)){
    if(!(strata %in% data)){
      stop("data must contain column \"strata\" since it is specified")
    }
  }

  if (length(strata)>1){
    stop("strata must be of length one")
  }

  if (!(arm %in% colnames(data))){
    stop("data must contain column \"arm\"")
  }

  if (any((data[,eval(arm)]-floor(data[,eval(arm)]))!=0)){
    stop("arm must only contain integer values")
  }

  if (length(arm)>1){
    stop("arm must be of length one")
  }

  if (!(method %in% c("small","large"))){
    stop("method must be either \"small\" or \"large\"")
  }

  if (length(method)>1){
    stop("method must be of length one")
  }

  if (!is.numeric(sig.level)){
    stop("sig.level must be of class numeric")
  }

  outcome<-c(baseline, outcome)

  # Convert data from wide format to long format
  data_long<-tidyr::gather(data, visit, outcome, outcome, factor_key=TRUE)

  # Order by ID
  data<-data[order(data[,pid]),]
  data_long<-data_long[order(data_long[,pid]),]

  if(length(covars)>0){

    # Split covariates by treatment arm
    dataTx<-data[which(data[arm]==max(data[arm])),][,c(covars,strata)]
    dataCx<-data[which(data[arm]==min(data[arm])),][,c(covars,strata)]

    # Convert all covariates to numeric
    dataTx[] <- apply(dataTx, 2, function(x) as.numeric(as.character(x)))
    dataCx[] <- apply(dataCx, 2, function(x) as.numeric(as.character(x)))

    # Split dataset into test treatment and control
    dataT<-data_long[which(data_long[arm]==max(data_long[arm])),]
    dataC<-data_long[which(data_long[arm]==min(data_long[arm])),]

    # Number of unique IDs in each arm
    nT<-nrow(unique(dataT[pid]))
    nC<-nrow(unique(dataC[pid]))

    # Number of visits - length of baseline (0 if NULL)
    r<-length(outcome)-length(baseline)
    # Number of visits + length of baseline (0 if NULL)
    rplusbase<-r+length(baseline)

    # Number of covariates
    s<-length(covars)
    # Number of covariates + length of baseline (0 if NULL)
    splusbase<-length(covars)+length(baseline)

    #------------------------------------------------------------------------
    # Stratification
    #------------------------------------------------------------------------

    if(length(strata)>0){

      dataT_split<-split(dataT,dataT[strata])
      dataC_split<-split(dataC,dataC[strata])
      dataTx_split<-split(dataTx,dataTx[strata])
      dataCx_split<-split(dataCx,dataCx[strata])

      # Remove stratum from covariates
      dataTx_split<-lapply(dataTx_split,"[", covars)
      dataCx_split<-lapply(dataCx_split,"[", covars)

      # Number of strata
      n_strata<-nrow(unique(data_long[strata]))

      # Sample size in each stratum
      n_hT<-sapply(1:n_strata, function(h) nrow(unique(dataT_split[[h]][pid])))
      n_hC<-sapply(1:n_strata, function(h) nrow(unique(dataC_split[[h]][pid])))

      # Mantel-Haenszel weights by stratum
      w<-((n_hT*n_hC)/(n_hT+n_hC))/sum((n_hT*n_hC)/(n_hT+n_hC))

    }

    # Function for U_h where h is the stratum
    Uh_fun<-function(dataTx, dataCx, dataT, dataC){

      #Uxi.
      Uxi.<-sweep(dataTx, 2, colMeans(dataCx), FUN="-")

      #Ux.i'
      Ux.ip<-sweep(-dataCx, 2, colMeans(dataTx), FUN="+")

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y<dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )
      l1<-rapply(l1, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #Ux..
      Ux..<-colMeans(dataTx)-colMeans(dataCx)

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y>dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U2i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l2[[x]])))

      #Ui.
      Ui.<-t(cbind(Uxi.,do.call(cbind.data.frame,U1i..),
                   do.call(cbind.data.frame,U2i..)))

      #U1.i'.
      U1.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2.i'.
      U2.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l2[[x]])))

      #U.i'
      U.ip<-t(cbind(Ux.ip,do.call(cbind.data.frame,U1.ip.),
                    do.call(cbind.data.frame,U2.ip.)))

      #U
      U<-rowMeans(Ui.)
      return(U)
    }

    # Function for V_h where h is the stratum
    Vh_fun<-function(dataTx, dataCx, dataT, dataC){

      nT<-nrow(unique(dataT[pid]))
      nC<-nrow(unique(dataC[pid]))

      #Uxi.
      Uxi.<-sweep(dataTx, 2, colMeans(dataCx), FUN="-")
      #Ux.i'
      Ux.ip<-sweep(-dataCx, 2, colMeans(dataTx), FUN="+")

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y<dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )

      l1<-rapply(l1, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #Ux..
      Ux..<-colMeans(dataTx)-colMeans(dataCx)

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y>dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U2i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l2[[x]])))

      #Ui.
      Ui.<-t(cbind(Uxi.,do.call(cbind.data.frame,U1i..),
                   do.call(cbind.data.frame,U2i..)))

      #U1.i'.
      U1.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2.i'.
      U2.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l2[[x]])))

      #U.i'
      U.ip<-t(cbind(Ux.ip,do.call(cbind.data.frame,U1.ip.),
                    do.call(cbind.data.frame,U2.ip.)))

      #U
      U<-rowMeans(Ui.)

      #V
      Ui._minus_U<-as.matrix(sweep(Ui., 1, U, FUN="-"))
      U.ip_minus_U<-as.matrix(sweep(U.ip, 1, U, FUN="-"))
      Ui._minus_U[is.na(Ui._minus_U)] <- 0
      U.ip_minus_U[is.na(U.ip_minus_U)] <- 0
      V<-Ui._minus_U%*%t(Ui._minus_U)/(nT*(nT-1)) + U.ip_minus_U%*%t(U.ip_minus_U)/(nC*(nC-1))
      return(V)

    }

    F_fun<-function(U){

      #indices
      x_ind<-1:s
      U_ind<-(s+1):length(U)

      #A
      A=cbind(diag(rplusbase),-diag(rplusbase))

      #F
      F<-c(U[x_ind],A%*%matrix(log(U[U_ind])))

      return(F)

    }

    VF_fun<-function(U, V){

      #A
      A<-cbind(diag(rplusbase),-diag(rplusbase))

      #D
      D<-diag(U[-(1:s)])

      #Matrix of zeros
      Zeros<-matrix(0,nrow=(s+rplusbase),ncol=s+2*(rplusbase))

      #M1
      Zeros[1:s,1:s]<-diag(s)
      Zeros[(s+1):nrow(Zeros),(s+1):ncol(Zeros)]<-A%*%solve(D)

      #VF
      VF<-Zeros%*%V%*%t(Zeros)

      return(VF)
    }

    b_fun<-function(VF,F){

      #L
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #b
      b<-solve(t(L)%*%solve(VF)%*%L)%*%t(L)%*%solve(VF)%*%F

      return(b)

    }

    Vb_fun<-function(VF){

      #L
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #VF
      VF<-solve(t(L)%*%solve(VF)%*%L)

      return(VF)

    }

    if(length(strata)>0){

      Uh_list<-lapply(1:n_strata, function(h)
        w[h]*matrix(Uh_fun(dataTx_split[[h]], dataCx_split[[h]],
                           dataT_split[[h]], dataC_split[[h]])))

      Vh_list<-lapply(1:n_strata, function(h)
        w[h]^2*Vh_fun(dataTx_split[[h]], dataCx_split[[h]],
                      dataT_split[[h]], dataC_split[[h]]))

      U<-  Reduce("+", Uh_list)
      V<-  Reduce("+", Vh_list)

    }else{

      U<- matrix(Uh_fun(dataTx, dataCx, dataT, dataC))

      V<-Vh_fun(dataTx, dataCx, dataT, dataC)

    }

    F<-F_fun(U)
    VF<-VF_fun(U,V)
    b<-b_fun(VF,F)
    Vb<-Vb_fun(VF)

    #generate output dataframe
    logWO<-b
    Var_logWO<-diag(Vb)
    SE_logWO<-sqrt(diag(Vb))
    Chi_Square<-(b/sqrt(diag(Vb)))^2
    p_value<-pchisq(Chi_Square, 1, lower.tail = FALSE)
    WO<-exp(b)
    UCL_WO<-exp(b+1.96*SE_logWO)
    LCL_WO<-exp(b-1.96*SE_logWO)

    df_WO<-data.frame(logWO, SE_logWO, Var_logWO, Chi_Square, p_value, WO,
                      LCL_WO, UCL_WO)
    rownames(df_WO)<-if(length(baseline)==0){outcome}else{outcome[-1]}

    return(df_WO)}

  if(length(covars)==0){

    # Split dataset into test treatment and control
    dataT<-data_long[which(data_long[arm]==max(data_long[arm])),]
    dataC<-data_long[which(data_long[arm]==min(data_long[arm])),]

    # Number of unique IDs in each arm
    nT<-nrow(unique(dataT[pid]))
    nC<-nrow(unique(dataC[pid]))

    # Number of visits - length of baseline (0 if NULL)
    r<-length(outcome)-length(baseline)
    # Number of visits + length of baseline (0 if NULL)
    rplusbase<-r+length(baseline)

    # Number of covariates
    s<-length(covars)
    # Number of covariates + length of baseline (0 if NULL)
    splusbase<-length(covars)+length(baseline)

    #------------------------------------------------------------------------
    # Stratification
    #------------------------------------------------------------------------

    if(length(strata)>0){

      dataT_split<-split(dataT,dataT[strata])
      dataC_split<-split(dataC,dataC[strata])

      # Number of strata
      n_strata<-nrow(unique(data_long[strata]))

      # Sample size in each stratum
      n_hT<-sapply(1:n_strata, function(h) nrow(unique(dataT_split[[h]][pid])))
      n_hC<-sapply(1:n_strata, function(h) nrow(unique(dataC_split[[h]][pid])))

      # Mantel-Haenszel weights by stratum
      w<-((n_hT*n_hC)/(n_hT+n_hC))/sum((n_hT*n_hC)/(n_hT+n_hC))

    }


    # Function for U_h where h is the stratum
    Uh_fun<-function(dataT, dataC){

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y<dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )

      l1<-rapply(l1, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y>dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U2i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l2[[x]])))

      #Ui.
      Ui.<-t(cbind(do.call(cbind.data.frame,U1i..),
                   do.call(cbind.data.frame,U2i..)))

      #U1.i'.
      U1.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2.i'.
      U2.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l2[[x]])))

      #U.i'
      U.ip<-t(cbind(do.call(cbind.data.frame,U1.ip.),
                    do.call(cbind.data.frame,U2.ip.)))

      #U
      U<-rowMeans(Ui.)
      return(U)
    }

    # Function for V_h where h is the stratum
    Vh_fun<-function(dataT, dataC){

      nT<-nrow(unique(dataT[pid]))
      nC<-nrow(unique(dataC[pid]))

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y<dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )
      l1<-rapply(l1, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                                    function(y) {v<-1*(y>dataT[which(dataT$visit==v),]$outcome)+
                                      0.5*(y==dataT[which(dataT$visit==v),]$outcome)
                                    return(v)
                                    }
                 )
      )
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0.5),
                 classes = c("numeric", "numeric"), how="replace")

      U2i..<-lapply(1:length(outcome),function(x)
        rowMeans(do.call(cbind.data.frame,l2[[x]])))

      #Ui.
      Ui.<-t(cbind(do.call(cbind.data.frame,U1i..),
                   do.call(cbind.data.frame,U2i..)))

      #U1.i'.
      U1.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2.i'.
      U2.ip.<-lapply(1:length(outcome),function(x)
        colMeans(do.call(cbind.data.frame,l2[[x]])))

      #U.i'
      U.ip<-t(cbind(do.call(cbind.data.frame,U1.ip.),
                    do.call(cbind.data.frame,U2.ip.)))

      #U
      U<-rowMeans(Ui.)

      #V
      Ui._minus_U<-as.matrix(sweep(Ui., 1, U, FUN="-"))
      U.ip_minus_U<-as.matrix(sweep(U.ip, 1, U, FUN="-"))
      Ui._minus_U[is.na(Ui._minus_U)] <- 0
      U.ip_minus_U[is.na(U.ip_minus_U)] <- 0
      V<-Ui._minus_U%*%t(Ui._minus_U)/(nT*(nT-1)) + U.ip_minus_U%*%t(U.ip_minus_U)/(nC*(nC-1))
      return(V)

    }

    F_fun<-function(U){

      #A
      A=cbind(diag(rplusbase),-diag(rplusbase))

      #F
      F<-A%*%matrix(log(U))

      return(F)

    }

    VF_fun<-function(U, V){

      #A
      A<-cbind(diag(rplusbase),-diag(rplusbase))

      #D
      D<-diag(as.numeric(U))

      #M1
      Zeros<-A%*%solve(D)

      #VF
      VF<-Zeros%*%V%*%t(Zeros)

      return(VF)
    }

    b_fun<-function(VF,F){

      #L
      #L<-diag(r)
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #b
      b<-solve(t(L)%*%solve(VF)%*%L)%*%t(L)%*%solve(VF)%*%F

      return(b)

    }

    Vb_fun<-function(VF){

      #L
      #L<-diag(r)
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #VF
      VF<-solve(t(L)%*%solve(VF)%*%L)

      return(VF)

    }

    if(length(strata)>0){

      Uh_list<-lapply(1:n_strata, function(h)
        w[h]*matrix(Uh_fun(dataT_split[[h]], dataC_split[[h]])))

      Vh_list<-lapply(1:n_strata, function(h)
        w[h]^2*Vh_fun(dataT_split[[h]], dataC_split[[h]]))

      U<-  Reduce("+", Uh_list)
      V<-  Reduce("+", Vh_list)

    }else{

      U<- matrix(Uh_fun(dataT, dataC))

      V<-Vh_fun(dataT, dataC)

    }

    F<-F_fun(U)
    VF<-VF_fun(U,V)
    b<-b_fun(VF,F)
    Vb<-Vb_fun(VF)

    #generate output dataframe
    logWO<-b
    Var_logWO<-diag(Vb)
    SE_logWO<-sqrt(diag(Vb))
    Chi_Square<-(b/sqrt(diag(Vb)))^2
    p_value<-pchisq(Chi_Square, 1, lower.tail = FALSE)
    WO<-exp(b)
    UCL_WO<-exp(b+qnorm(p=sig.level/2, lower.tail=F)*SE_logWO)
    LCL_WO<-exp(b+qnorm(p=sig.level/2, lower.tail=T)*SE_logWO)

    df_WO<-data.frame(logWO, SE_logWO, Var_logWO, Chi_Square, p_value, WO,
                      LCL_WO, UCL_WO)
    rownames(df_WO)<-if(length(baseline)==0){outcome}else{outcome[-1]}

    return(df_WO)}
}

