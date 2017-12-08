gg.gauge <- function(pos,breaks=c(0,14,28,42,57,71,85,100)) {
    require(ggplot2)
    get.poly <- function(start, end, height = 8) {
        margin <- 0.1
        x        <- c(start + margin, start + margin, end - margin, end - margin)
        y        <- c(-height/2 + margin, height/2 - margin, height/2 - margin, -height/2 + margin)
        return(data.frame(x,y))
    }
    g <- ggplot()+ 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red") +
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="coral2") +
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="lightsalmon") +
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="khaki2") +
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#CEEEA0") +
        geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill="darkolivegreen3") +
        geom_polygon(data=get.poly(breaks[7],breaks[8]),aes(x,y),fill="forestgreen") +
        geom_polygon(data=get.poly(pos*100/3.5-1.5, pos*100/3.5+1.5, 10),aes(x,y), fill="grey36")+ 
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank()) 
    return(g)
}