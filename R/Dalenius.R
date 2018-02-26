Dalenius<-function(variable,L=3, plot=T) {

  { # Clases
    var_sort<-sort(na.omit(variable))
    n<-length(var_sort)
    x1<-var_sort[1];x1
    xn<-var_sort[n];xn
    J<-min(L*10,n);J
    p<-(xn-x1)/J;p

    cortes<-cut(var_sort,breaks=seq(x1,xn,p),RIGHT=TRUE,
                include.lowest=TRUE)
    Fr<-table(cortes)
    C<-cumsum(sqrt(table(cortes)));C
    Q<-C[J]/L;Q

    v<-(1:(L-1))*Q
    Clase <- NA
  } # Clases

  { # Cortes
    Corte<-c()
    for (i in 1:(L-1)) {
      Clase<-names(which.min(abs(C-v[i])))
      Corte[i] <- as.numeric(stringr::str_sub(Clase,stringr::str_locate(Clase,",")[1]+1,
                                              stringr::str_length(Clase)-1))
    }

    Frecuencias<-data.frame(table(cut(variable,breaks=c(min(variable,na.rm = T),Corte,max(variable,na.rm = T)),
                                      include.lowest = F, right = T)))
    Frecuencias$Porc <- round(Frecuencias$Freq/sum(Frecuencias$Freq)*100,2)
    names(Frecuencias)<-c("Clase","Freq","Freq %")
    Cortes<-cbind.data.frame("Clase"=1:(L-1), Corte)

    ct<-as.numeric(c(stringr::str_sub(names(Fr),2,stringr::str_locate(names(Fr),",")[,1]-1),
                     stringr::str_sub(names(Fr)[J],stringr::str_locate(names(Fr)[J],",")[1]+1,
                                      stringr::str_length(names(Fr)[J])-1)))

    df<-cbind.data.frame(data.frame(Fr),"Lower"=ct[1:J],"Upper"=ct[-1])
    df$mids=(ct[1:J]+ct[-1])/2
    df$raiz_freq=sqrt(df$Freq)
    df$csqrtf <- cumsum(df$raiz_freq)
    df$Estrato<-NA
  } # Cortes

  { # Estratos
    Estratos <- rep(NA, length(variable))

    for (i in 1:(L - 1)) {
      tmp <- which(abs(df$csqrtf - v[i]) == min(abs(df$csqrtf - v[i])))
      df$Estrato[c(1:J) <= tmp & is.na(df$Estrato)] <- i
      Estratos[is.na(Estratos) & variable <= Cortes$Corte[i]]<-i
    }
    df$Estrato[is.na(df$Estrato)] <- L
    Estratos[is.na(Estratos)] <- L
    Estratos[is.na(variable)] <- NA
  } # Estratos

  { # Gráfica
    if (plot) {
      require(ggplot2)
      p <- ggplot2::ggplot(df) +
        ggplot2::geom_bar(aes(x = mids, y = Freq, fill = factor(Estrato)), color = "black",
                 stat = "identity") +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = NULL),
                                   title = "Estratos")) + ggplot2::theme_bw() +
        ggplot2::xlab("Estratificación de Variable") +
        ggplot2::ylab("Frecuencias") + ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent", color = NA),
                                    panel.background = ggplot2::element_rect(fill = "transparent",color = NA))
      plot(p)
    }
  } # Gráfica

  return(list("Clases"=df[,c(1,3,4,5,2,6,7,8)],"Cortes"=Cortes,"Estratos"=Estratos ,"Frecuencias"=Frecuencias))
}
