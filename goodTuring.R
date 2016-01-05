calc.nfreq <- function(data,type="unk") {
        nfreq <- data%>%count(freq)%>%rename(n_r=n)
        nfreq$type <-type
        setkey(nfreq,freq)
        nfreq
}
calc.nrz.estimate <- function(data){
        r <- data[,freq]
        nr <- data[,n_r]
        d <- c(1,diff(r))
        dr <- c(0.5*(d[-1] + d[- length(d)]),d[length(d)] )
        return(nr/dr)
}

calc.nrz.estimate(nfreq.uni)
