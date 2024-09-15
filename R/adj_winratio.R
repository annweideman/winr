#' Adjusted win ratio
#'
#' Randomization-based adjustment of the win ratio for baseline covariates and
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
#' stratification. If not specified, defaults to NULL and no stratification adjustment
#' is utilized.
#'
#' @param arm a string indicating the name of the variable for treatment arm.
#' Treatment arm must be a positive integer such that the test treatment arm is
#' ALWAYS higher in value than the control arm.
#'
#' @param method a string "small" or "large" used to denote the sample size
#' method employed. The small sample size method is recommended unless
#' within-stratum sample size is reasonably large (e.g., >= 50), number of
#' visits is small (e.g., <=6), and number of covariates is small (e.g., <=4).
#' Defaults to "small."
#'
#' @param sig.level significance level (Type I error probability). Defaults to
#' 0.05.
#'
#'@return
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
#' resp$UniqID<-resp$Center*100+resp$ID
#'
#' # Convert treatment arm to binary
#' resp$Trt<-1*(resp$Treatment=="T")
#'
#' # Indicator for male
#' resp$SexNum<-1*(resp$Sex=="M")
#'
#' adj_winratio(data=resp,
#'             pid="UniqID",
#'             baseline="Baseline",
#'             outcome=c("Visit1","Visit2","Visit3","Visit4"),
#'             covars= c("SexNum","Age"),
#'             strata="Center",
#'             arm="Trt",
#'             method="small",
#'             sig.level=0.05)
#'
#' #----------------------
#' # Dermatology example
#' #----------------------
#'
#' #Generate indicators for stage 4 and 5
#' skin$Stage4 = (skin$STAGE == 4)*1
#' skin$Stage5 = (skin$STAGE == 5)*1
#'
#' # Generate treatment center
#' skin$center<-ifelse(skin$INV==5,1,
#'   ifelse(skin$INV==6,2,
#'     ifelse(skin$INV==8,3,
#'       ifelse(skin$INV==9,4,
#'         ifelse(skin$INV==10,5,6)))))
#'
#' # Generate treatment center that pools centers 3 and 4 due to small sample size
#' skin$center2 = skin$center
#' skin$center2<-ifelse(skin$center == 4, 3, skin$center)
#'
#' # Generate participant IDs
#' skin$ID<-1:nrow(skin)
#'
#' adj_winratio(data=skin,
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

adj_winratio<-function(data, pid, baseline=NULL, outcome, covars=NULL,
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

  if(any(duplicated(data[, eval(pid), drop = TRUE]))){
    stop("pid must be unique for each row of the input dataset")
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
    if(any(!(baseline %in% colnames(data)))){
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

  if (any(!sapply(data[covars], is.numeric))){
    stop("covars must ony contain covariates of type \"numeric\"")
  }

  if (!is.null(strata)){
    if (!is.character(strata)){
      stop("strata must be of type \"character\" since it is specified")
    }
  }

  if (!is.null(strata)){
    if(!(strata %in% colnames(data))){
      stop("data must contain column \"strata\" since it is specified")
    }
  }

  if (length(strata)>1){
    stop("strata must be of length one")
  }

  if (!(arm %in% colnames(data))){
    stop("data must contain column \"arm\"")
  }

  if (!is.numeric(data[, eval(arm), drop = T])){
    stop("arm must only contain integer values")
  }

  if (any((data[,eval(arm)]-floor(data[,eval(arm)]))!=0)){
    stop("arm must only contain integer values")
  }

  if (length(unique((data[, eval(arm), drop = T])))!=2){
    stop("arm must only contain only two levels")
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

  if (sig.level<0 | sig.level>1){
    stop("sig.level must be between 0 and 1")
  }

  outcome<-c(baseline, outcome)

  # Convert data from wide format to long format
  data_long<-tidyr::gather(data=data, key="visit", outcome, outcome,
                           factor_key=TRUE)

  # Order by ID
  data <- dplyr::arrange(data, pid)
  data_long<-dplyr::arrange(data_long,pid)

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

      if((any(n_hT<50) | any(n_hC<50)) & method=="large"){
        warning("Minimum within-stratum sample size is less 50. Consider using
                METHOD = SMALL instead.")
      }

      # Mantel-Haenszel weights by stratum
      w<-((n_hT*n_hC)/(n_hT+n_hC))/sum((n_hT*n_hC)/(n_hT+n_hC))

    }else{w=1}

    # Function for U_h where h is the stratum
    Uh_fun1<-function(dataTx, dataCx, dataT, dataC){

      #Uxi.
      Uxi.<-sweep(dataTx, 2, colMeans(dataCx), FUN="-") #added

      #Ux.i'
      Ux.ip<-sweep(-dataCx, 2, colMeans(dataTx), FUN="+") #added

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y<dataT[which(dataT$visit==v),]$outcome))
      l1<-rapply(l1, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
             rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #Ux..
      Ux..<-colMeans(dataTx)-colMeans(dataCx) #added

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y>dataT[which(dataT$visit==v),]$outcome))
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

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
    Vh_fun1<-function(dataTx, dataCx, dataT, dataC){

      nT<-nrow(unique(dataT[pid]))
      nC<-nrow(unique(dataC[pid]))

      if(nT<=1 | nC<=1){
        stop("Within-stratum sample size in each arm must be greater than 1")
      }

      #Uxi.
      Uxi.<-sweep(dataTx, 2, colMeans(dataCx), FUN="-") #added
      #Ux.i'
      Ux.ip<-sweep(-dataCx, 2, colMeans(dataTx), FUN="+") #added

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y<dataT[which(dataT$visit==v),]$outcome))
      l1<-rapply(l1, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
             rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #Ux..
      Ux..<-colMeans(dataTx)-colMeans(dataCx)

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y>dataT[which(dataT$visit==v),]$outcome))
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

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

    F_fun<-function(U, w){

      #A
      A=cbind(diag(rplusbase),-diag(rplusbase))

      if(method=="small"){

        #indices
        x_ind<-1:s
        U_ind<-(s+1):length(U[[1]])

        #F
        F<-c(U[[1]][x_ind],A%*%matrix(log(U[[1]][U_ind])))

      }else{

        #indices
        x_ind<-1:s
        U_ind<-(s+1):length(U[[1]])

        #F
        F<-lapply(1:length(w), function(y) c(U[[y]][x_ind], A%*%matrix(log(U[[y]][U_ind]))))
      }

      return(F)

    }

    # Function to check if the matrix is invertible (full rank)
    is_invertible <- function(matrix) {
      qr_decomp <- qr(matrix)
      rank_matrix <- qr_decomp$rank
      full_rank <- ncol(matrix) == rank_matrix
      return(full_rank)
    }

    VF_fun<-function(U, V, w){

      #A
      A<-cbind(diag(rplusbase),-diag(rplusbase))

      if(method=="small"){

        #D
        D<-diag(U[[1]][-(1:s)])

        if (!is_invertible(D)) {
          stop("METHOD=SMALL must be utilized because there is perfect collinearity
             between covariates within one or more strata.")
        }

        #Matrix of zeros
        Zeros<-matrix(0,nrow=(s+rplusbase),ncol=s+2*(rplusbase))

        #M1
        Zeros[1:s,1:s]<-diag(s)
        Zeros[(s+1):nrow(Zeros),(s+1):ncol(Zeros)]<-A%*%solve(D)

        #VF
        VF<-Zeros%*%V[[1]]%*%t(Zeros)

      }else{
        VF<-lapply(1:length(w), function(y){
          #D
          D<-diag(U[[y]][-(1:s)])

          if (!is_invertible(D)) {
            stop("METHOD=SMALL must be utilized because there is perfect collinearity
             between covariates within one or more strata.")
          }

          #Matrix of zeros
          Zeros<-matrix(0,nrow=(s+rplusbase),ncol=s+2*(rplusbase))

          #M1
          Zeros[1:s,1:s]<-diag(s)
          Zeros[(s+1):nrow(Zeros),(s+1):ncol(Zeros)]<-A%*%solve(D)

          #VF
          VF<-Zeros%*%V[[y]]%*%t(Zeros)
        })
      }

      return(VF)

    }

    b_fun<-function(VF,F,w){

      #L
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #b
      if(method=="small"){

        if (!is_invertible(VF)) {
          stop("METHOD=SMALL must be utilized because there is perfect collinearity
             between covariates within one or more strata.")
        }

        b<-solve(t(L)%*%solve(VF)%*%L)%*%t(L)%*%solve(VF)%*%F
      }else{
        bh_list<-lapply(1:length(w), function(x) {
          if (!is_invertible(VF[[x]])) {
            stop("METHOD=SMALL must be utilized because there is perfect collinearity
             between covariates within one or more strata.")
          }
          w[x]*solve(t(L)%*%solve(VF[[x]])%*%L)%*%t(L)%*%solve(VF[[x]])%*%F[[x]]
          })
        b<-Reduce("+", bh_list)
      }

      return(b)

    }

    Vb_fun<-function(VF,w){

      #L
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #VF
      if(method=="small"){
        VF<-solve(t(L)%*%solve(VF)%*%L)
      }else{
        VFh_list<-lapply(1:length(w), function(x) w[x]^2*solve(t(L)%*%solve(VF[[x]])%*%L))
        VF<-Reduce("+", VFh_list)
      }

      return(VF)

    }

    if(length(strata)>0 & method=="small"){

      Uh_list<-lapply(1:n_strata, function(h)
               w[h]*matrix(Uh_fun1(dataTx_split[[h]], dataCx_split[[h]],
               dataT_split[[h]], dataC_split[[h]])))

      Vh_list<-lapply(1:n_strata, function(h)
               w[h]^2*Vh_fun1(dataTx_split[[h]], dataCx_split[[h]],
               dataT_split[[h]], dataC_split[[h]]))

      U<-  list(Reduce("+", Uh_list))
      V<-  list(Reduce("+", Vh_list))

    }else if (length(strata)>0 & method=="large"){

      Uh_list<-lapply(1:n_strata, function(h)
        matrix(Uh_fun1(dataTx_split[[h]], dataCx_split[[h]],
                      dataT_split[[h]], dataC_split[[h]])))

      Vh_list<-lapply(1:n_strata, function(h)
        Vh_fun1(dataTx_split[[h]], dataCx_split[[h]],
                       dataT_split[[h]], dataC_split[[h]]))

      U<-  Uh_list
      V<-  Vh_list
    }else{

      U<- list(matrix(Uh_fun1(dataTx, dataCx, dataT, dataC)))

      V<- list(Vh_fun1(dataTx, dataCx, dataT, dataC))

    }

    F<-F_fun(U,w)
    VF<-VF_fun(U,V,w)
    b<-b_fun(VF,F,w)
    Vb<-Vb_fun(VF,w)

    #generate output dataframe
    logWR<-b
    Var_logWR<-diag(Vb)
    SE_logWR<-sqrt(diag(Vb))
    Chi_Square<-(b/sqrt(diag(Vb)))^2
    p_value<-stats::pchisq(Chi_Square, 1, lower.tail = FALSE)
    WR<-exp(b)
    UCL_WR<-exp(b+stats::qnorm(1-sig.level/2)*SE_logWR)
    LCL_WR<-exp(b-stats::qnorm(1-sig.level/2)*SE_logWR)

    df_WR<-data.frame(logWR, SE_logWR, Var_logWR, Chi_Square, p_value, WR,
                      LCL_WR, UCL_WR)
    rownames(df_WR)<-if(length(baseline)==0){outcome}else{outcome[-1]}

    return(df_WR)
  }

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

      if((any(n_hT<50) | any(n_hC<50)) & method=="large"){
        warning("Minimum within-stratum sample size is less 50. Consider using
                METHOD = SMALL instead.")
      }

      # Mantel-Haenszel weights by stratum
      w<-((n_hT*n_hC)/(n_hT+n_hC))/sum((n_hT*n_hC)/(n_hT+n_hC))

    }else{w=1}


    # Function for U_h where h is the stratum
    Uh_fun2<-function(dataT, dataC){

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y<dataT[which(dataT$visit==v),]$outcome))
      l1<-rapply(l1, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
             rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y>dataT[which(dataT$visit==v),]$outcome))
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

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
    Vh_fun2<-function(dataT, dataC){

      nT<-nrow(unique(dataT[pid]))
      nC<-nrow(unique(dataC[pid]))

      if(nT<=1 | nC<=1){
        stop("within-stratum sample size in each arm must be greater than 1.")
      }

      #U1i..
      l1<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y<dataT[which(dataT$visit==v),]$outcome))
      l1<-rapply(l1, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

      U1i..<-lapply(1:length(outcome),function(x)
             rowMeans(do.call(cbind.data.frame,l1[[x]])))

      #U2i..
      l2<-lapply(outcome,
                 function(v) lapply(dataC[which(dataC$visit==v),]$outcome,
                 function(y) y>dataT[which(dataT$visit==v),]$outcome))
      l2<-rapply(l2, function(x) replace(x, is.na(x), 0),
                 classes = c("logical", "integer"), how="replace")

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

    F_fun<-function(U,w){

      #A
      A=cbind(diag(rplusbase),-diag(rplusbase))

      if(method=="small"){
        #F
        F<-A%*%matrix(log(U[[1]]))
      }else{
        #F
        F<-lapply(1:length(w), function(x) A%*%matrix(log(U[[x]])))
      }

      return(F)

    }

    # Function to check if the matrix is invertible (full rank)
    is_invertible <- function(matrix) {
      qr_decomp <- qr(matrix)
      rank_matrix <- qr_decomp$rank
      full_rank <- ncol(matrix) == rank_matrix
      return(full_rank)
    }

    VF_fun<-function(U, V, w){

      #A
      A<-cbind(diag(rplusbase),-diag(rplusbase))

      if(method=="small"){
        #D
        D<-diag(as.numeric(U[[1]]))

        if (!is_invertible(D)) {
          stop("METHOD=SMALL must be utilized because there is perfect collinearity
             between covariates within one or more strata.")
        }

        #M1
        Zeros<-A%*%solve(D)

        #VF
        VF<-Zeros%*%V[[1]]%*%t(Zeros)
      }else{
      VF<-lapply(1:length(w), function(x){
        #D
        D<-diag(as.numeric(U[[x]]))

        if (!is_invertible(D)) {
          stop("METHOD=SMALL must be utilized because there is perfect collinearity
             between covariates within one or more strata.")
        }

        #M1
        Zeros<-A%*%solve(D)

        #VF
        Zeros%*%V[[x]]%*%t(Zeros)
        })
      }

      return(VF)
    }

    b_fun<-function(VF,F,w){

      #L
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #b
      if(method=="small"){
        b<-solve(t(L)%*%solve(VF)%*%L)%*%t(L)%*%solve(VF)%*%F
      }else{
        bh_list<-lapply(1:length(w), function(x) w[x]*solve(t(L)%*%solve(VF[[x]])%*%L)%*%t(L)%*%solve(VF[[x]])%*%F[[x]])
        b<-Reduce("+", bh_list)
      }

      return(b)

    }

    Vb_fun<-function(VF,w){

      #L
      L<-rbind(matrix(0,nrow=splusbase,ncol=r),diag(r))

      #VF
      if(method=="small"){
        VF<-solve(t(L)%*%solve(VF)%*%L)
      }else{
        VFh_list<-lapply(1:length(w), function(x) w[x]^2*solve(t(L)%*%solve(VF[[x]])%*%L))
        VF<-Reduce("+", VFh_list)
      }
      return(VF)

    }

    if(length(strata)>0 & method=="small"){

      Uh_list<-lapply(1:n_strata, function(h)
        w[h]*matrix(Uh_fun2(dataT_split[[h]], dataC_split[[h]])))

      Vh_list<-lapply(1:n_strata, function(h)
        w[h]^2*Vh_fun2(dataT_split[[h]], dataC_split[[h]]))

      U<-  list(Reduce("+", Uh_list))
      V<-  list(Reduce("+", Vh_list))

    } else if (length(strata)>0 & method=="large"){

      Uh_list<-lapply(1:n_strata, function(h)
        matrix(Uh_fun2(dataT_split[[h]], dataC_split[[h]])))

      Vh_list<-lapply(1:n_strata, function(h)
        Vh_fun2(dataT_split[[h]], dataC_split[[h]]))

      U<-  Uh_list
      V<-  Vh_list

    }else{

      U<- list(matrix(Uh_fun2(dataT, dataC)))

      V<- list(Vh_fun2(dataT, dataC))

    }

    F<-F_fun(U,w)
    VF<-VF_fun(U,V,w)
    b<-b_fun(VF,F,w)
    Vb<-Vb_fun(VF,w)

    #generate output dataframe
    logWR<-b
    Var_logWR<-diag(Vb)
    SE_logWR<-sqrt(diag(Vb))
    Chi_Square<-(b/sqrt(diag(Vb)))^2
    p_value<-stats::pchisq(Chi_Square, 1, lower.tail = FALSE)
    WR<-exp(b)
    UCL_WR<-exp(b+stats::qnorm(p=1-sig.level/2)*SE_logWR)
    LCL_WR<-exp(b-stats::qnorm(p=1-sig.level/2)*SE_logWR)

    df_WR<-data.frame(logWR, SE_logWR, Var_logWR, Chi_Square, p_value, WR,
                      LCL_WR, UCL_WR)
    rownames(df_WR)<-if(length(baseline)==0){outcome}else{outcome[-1]}

    return(df_WR)}
}

