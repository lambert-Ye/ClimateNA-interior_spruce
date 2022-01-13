library(CEMT);library(plotrix)

#modification on sglFit function to test if slope = 1 , and if intercept = 0
sglFit_test <- function(x,y,order=1,plot=T,rmse=F, ...){
  xy <- data.frame(x,y);head(xy)
  xy2 <- na.omit(xy)
  x <- xy2$x; y <- xy2$y
  if(order==1){lm0 <- lm(y~x)}
  if(order==2){lm0 <- lm(y~x+I(x^2))}
  if(order==3){lm0 <- lm(y~x+I(x^2)++I(x^3))}
  r2p <- r2pFun(lm0);r2p
  if(plot==T){
    plot(y~x,...)
    # plot(y~x)
    x0 <- data.frame(x=seq(min(x),max(x),len=100))
    p <- predict(lm0,x0)
    lm1 <- lm(y~x)
    fit_stats <- summary(lm1)
    t1_stats <- (1-fit_stats$coefficients[2,1])/fit_stats$coefficients[2,2]
    s <- fit_stats$coefficients[2,1]
    p1 <- 2 * pt(abs(t1_stats), df = df.residual(lm1), lower.tail = FALSE)
    if(p1<0.0001){
      ch_s <- '<0.0001)'
    }else{
      ch_s <- paste("=",as.character(round(p1,digits = 4)),")",sep = "")
    }
    t2_stats <- (1-fit_stats$coefficients[1,1])/fit_stats$coefficients[1,2]
    i <- fit_stats$coefficients[1,1]
    p2 <- 2 * pt(abs(t2_stats), df = df.residual(lm1), lower.tail = FALSE)
    if(p2<0.0001){
      ch_i <- "<0.0001)"
    }else{
      ch_i <- paste("=",as.character(round(p2,digits = 4)),")",sep = "")
    }
    if(order==1){abline(lm0,col='blue',lwd=2)}
    if(order > 1){
      x0 <- data.frame(x=seq(min(x),max(x),len=100))
      p <- predict(lm0,x0)
      px <- data.frame(x0,p)
      lines(px$p~px$x, col='blue',lwd=2)
    }
    if(p[1] <= p[100]){legPos='topleft'}else{legPos='topright'}
    legendTxt <- c(bquote(italic(R^2)==.(format(r2p[1],digits=3))),bquote(italic(p)==.(format(r2p[2],digits=3))))
    if(rmse==T){legendTxt <- c(bquote(italic(R^2)==.(format(r2p[1],digits=3))* ' (' * italic(p)* '<0.0001)'),
                               bquote(italic(s)==.(s)* ' (' * italic(p)* .(ch_s)),
                               bquote(italic(i)==.(i)* ' (' * italic(p)* .(ch_i)),
                               bquote(italic(RMSE)==.(format(r2p[3],digits=3))))}
    legend(legPos,legend=as.expression(legendTxt),bty='n',inset =- .015)
  }
  return(lm0)
}


legendTxt <- c(bquote(italic(R^2)==.(0.55)* ' (' * italic(P)* '<0.0001)'),bquote(italic(s)==.(0.99)* ' (' * italic(P)* '<0.031)'));legendTxt
plot(2);legend("bottom",legend=as.expression(legendTxt),bty='n',inset =- .015)
#adjustment
sglFit <- function(x,y,order=2,plot=T,rmse=F, ...){
  xy <- data.frame(x,y);head(xy)
  xy2 <- na.omit(xy)
  x <- xy2$x; y <- xy2$y
  if(order==1){lm0 <- lm(y~x)}
  if(order==2){lm0 <- lm(y~x+I(x^2))}
  if(order==3){lm0 <- lm(y~x+I(x^2)++I(x^3))}
  r2p <- r2pFun(lm0);r2p
  if(plot==T){
    plot(y~x,...)
    # plot(y~x)
    x0 <- data.frame(x=seq(min(x),max(x),len=100))
    p <- predict(lm0,x0)
    if(order==1){abline(lm0,col='blue',lwd=2)}
    if(order > 1){
      x0 <- data.frame(x=seq(min(x),max(x),len=100))
      p <- predict(lm0,x0)
      px <- data.frame(x0,p)
      lines(px$p~px$x, col='blue',lwd=2)
    }
    if(p[1] <= p[100]){legPos='topleft'}else{legPos='topright'}
    legendTxt <- c(bquote(italic(r^2)==.(format(r2p[1],digits=3))),bquote(italic(p)==.(format(r2p[2],digits=3))))
    if(rmse==T){legendTxt <- c(bquote(italic(r^2)==.(format(r2p[1],digits=3))),
                               bquote(italic(p) < 0.001),bquote(italic(RMSE)==.(format(r2p[3],digits=3))))}
    legend(legPos,legend=as.expression(legendTxt),bty='n',inset =- .015)
  }
  return(lm0)
}


dblFit <- function(x,y,z,plot=T,rmse=F, ...){
  xy <- data.frame(x,y);head(xy)
  xy2 <- na.omit(xy)
  x <- xy2$x; y <- xy2$y
  lm0 <- lm(y~x)
  r2p <- r2pFun(lm0);
  plot(y~x,pch = 21,bg = "gold",...)
  # plot(y~x,pch = 21,bg = "gold")
  fit_stats <- summary(lm0)
  t1_stats <- (1-fit_stats$coefficients[2,1])/fit_stats$coefficients[2,2]
  s0 <- fit_stats$coefficients[2,1]
  p10 <- 2 * pt(abs(t1_stats), df = df.residual(lm0), lower.tail = FALSE)
  if(p10<0.0001){
    ch_s0 <- "<0.0001)"
  }else{
    ch_s0 <- paste("=",as.character(round(p10,digits = 4)),")",sep = "")
  }
  t2_stats <- (1-fit_stats$coefficients[1,1])/fit_stats$coefficients[1,2]
  i0 <- fit_stats$coefficients[1,1]
  p20 <- 2 * pt(abs(t2_stats), df = df.residual(lm0), lower.tail = FALSE)
  if(p20<0.0001){
    ch_i0 <- "<0.0001)"
  }else{
    ch_i0 <- paste("=",as.character(round(p20,digits = 4)),")",sep = "")
  }
  # plot(y,x,pch = 21,bg = "gold")
  x0 <- data.frame(x=seq(min(x),max(x),len=100))
  p <- predict(lm0,x0)
  abline(lm0,col='gold4',lwd=2)
  xz <- data.frame(x,z);head(xz)
  xz2 <- na.omit(xz)
  x <- xz2$x; y <- xz2$z
  lm1 <- lm(z~x)
  r2p1 <- r2pFun(lm1);r2p1
  points(z~x,pch = 21,bg = "cyan",...)
  #points(z~x,pch = 21,bg = "cyan")
  abline(lm1,col='cyan4',lwd=2)
  fit_stats <- summary(lm1)
  t1_stats <- (1-fit_stats$coefficients[2,1])/fit_stats$coefficients[2,2]
  s1 <- fit_stats$coefficients[2,1]
  p11 <- 2 * pt(abs(t1_stats), df = df.residual(lm1), lower.tail = FALSE)
  if(p11<0.0001){
    ch_s1 <- "<0.0001)"
  }else{
    ch_s1 <- paste("=",as.character(round(p11,digits = 4)),")",sep = "")
  }
  t2_stats <- (1-fit_stats$coefficients[1,1])/fit_stats$coefficients[1,2]
  i1 <- fit_stats$coefficients[1,1]
  p21 <- 2 * pt(abs(t2_stats), df = df.residual(lm1), lower.tail = FALSE)
  if(p21<0.0001){
    ch_i1 <- "<0.0001)"
  }else{
    ch_i1 <- paste("=",as.character(round(p21,digits = 4)),")",sep = "")
  }
  lengendTxt_o <- c("Onsite observations",bquote(italic(R^2)==.(format(r2p[1],digits=3))* ' (' * italic(p)* '<0.0001)'),
                                          bquote(italic(s)==.(s0)* ' (' * italic(p)* .(ch_s0)),
                                          bquote(italic(i)==.(i0)* ' (' * italic(p)* .(ch_i0)),
                                          bquote(italic(RMSE)==.(format(r2p[3],digits=3))))
  legend("topleft",inset = .01,legend = as.expression(lengendTxt_o)
  ,col=c("black","white","white","white","white"),pt.bg=c("gold","white","white","white","white"),pch=21)
  lengendTxt_c <- c("ClimateNA predictions",bquote(italic(R^2)==.(format(r2p1[1],digits=3))* ' (' * italic(p)* '<0.0001)'),
                                            bquote(italic(s)==.(s1)* ' (' * italic(p)* .(ch_s1)),
                                            bquote(italic(i)==.(i1)* ' (' * italic(p)* .(ch_i1)),
                                            bquote(italic(RMSE)==.(format(r2p1[3],digits=3))))
  legend("bottomright",inset = .01,legend = as.expression(lengendTxt_c)
         ,col=c("black","white","white","white","white"),pt.bg=c("cyan","white","white","white","white"),pch=21)
}

##-----Jpeg---------------
#==========Figure 2==========
x <- "terrace"
y <- "Terr"
x1 <- "duncanlake"
y1 <- "Dunc"
jpeg(filename = "FIG2.jpeg",height = 1350, width = 1880, res = 170)
par(mfrow=c(2,3),cex.lab=1.25,mai=c(0.35,0.55,0.55,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x1,c(15:26)])),order=1, xlab="", ylab="Duncan Lake",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "Monthly minimum temperature (°C)")
abline(b=1,a=0,lty=2)
corner.label("a",x=1,y=-1)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x1,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "Monthly maximum temperature (°C)")
abline(b=1,a=0,lty=2)
corner.label("b",x=1,y=-1)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == "Dunc",c(39:50)])),as.vector(as.matrix(site_month[site_month$site == "duncanlake",c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="Monthly precipitation (mm)")
abline(b=1,a=0,lty=2)
corner.label("c",x=1,y=-1)
par(mai=c(0.9,0.55,0,0))
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x,c(15:26)])),order=1, xlab="", ylab="Terrace",rmse=T,xlim=c(-12,15),ylim=c(-12,15),cex.main = 1.5,main = "")
abline(b=1,a=0,lty=2)
corner.label("d",x=1,y=-1)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x,c(3:14)])),order=1, xlab="",ylab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main="")
abline(b=1,a=0,lty=2)
corner.label("e",x=1,y=-1)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == "Terr",c(39:50)])),as.vector(as.matrix(site_month[site_month$site == "terrace",c(39:50)])),rmse=T,order=1, ylab="", xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)
corner.label("f",x=1,y=-1)
mtext("       ClimateNA prediction", 1, -4.5, cex = 1.2, outer=TRUE)
mtext(" Onsite observation", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()


#====Figure 3===========
jpeg(filename = "FIG3.jpeg",height = 1350, width = 1880, res = 170)
par(mfrow=c(2,3),cex.lab=1.25,mai=c(0.35,0.55,0.55,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
dblFit(as.vector(as.matrix(Dunc_month[,c(15:26)])),as.vector(as.matrix(site_month[c(2,13,24,35),c(15:26)])),as.vector(as.matrix(model_month[c(2,13,24,35),c(15:26)])),order=1, rmse=T, xlab="",ylab="Duncan Lake",cex=1.3,xlim=c(-10,15),ylim=c(-10,15),main = "Monthly minimum temperature (°C)")
abline(b=1,a=0,lty=2)
corner.label("a",x=1,y=-1)
dblFit(as.vector(as.matrix(Dunc_month[,c(3:14)])),as.vector(as.matrix(site_month[c(2,13,24,35),c(3:14)])),as.vector(as.matrix(model_month[c(2,13,24,35),c(3:14)])),order=1, rmse=T, xlab="",ylab="",cex=1.3,xlim=c(-5,25),ylim=c(-5,25),main = "Monthly maximum temperature (°C)")
abline(b=1,a=0,lty=2)
corner.label("b",x=1,y=-1)
dblFit(as.vector(as.matrix(Dunc_month[,c(39:50)])),as.vector(as.matrix(site_month[c(2,13,24,35),c(39:50)])),as.vector(as.matrix(model_month[c(2,13,24,35),c(39:50)])),order=1, rmse=T, xlab="",ylab="",lab.cex = 1.2, cex=1.3,xlim=c(0,250),ylim=c(0,250),main = "Monthly precipitation (mm)")
abline(b=1,a=0,lty=2)
corner.label("c",x=1,y=-1)
par(mai=c(0.9,0.55,0,0))
dblFit(as.vector(as.matrix(Terr_month[,c(15:26)])),as.vector(as.matrix(site_month[c(9,20,31,42),c(15:26)])),as.vector(as.matrix(model_month[c(9,20,31,42),c(15:26)])),order=1, rmse=T, xlab="",ylab="Terrace",cex=1.3,xlim=c(-10,15),ylim=c(-10,15), main = "",)
abline(b=1,a=0,lty=2)
corner.label("d",x=1,y=-1)
dblFit(as.vector(as.matrix(Terr_month[,c(3:14)])),as.vector(as.matrix(site_month[c(9,20,31,42),c(3:14)])),as.vector(as.matrix(model_month[c(9,20,31,42),c(3:14)])),order=1, rmse=T, xlab="",ylab="",cex=1.3,xlim=c(-5,25),ylim=c(-5,25))
abline(b=1,a=0,lty=2)
corner.label("e",x=1,y=-1)
dblFit(as.vector(as.matrix(Terr_month[,c(39:50)])),as.vector(as.matrix(site_month[c(9,20,31,42),c(39:50)])),as.vector(as.matrix(model_month[c(9,20,31,42),c(39:50)])),order=1, rmse=T, xlab="",ylab="",cex=1.3,xlim=c(0,250),ylim=c(0,250),main = "")
abline(b=1,a=0,lty=2)
corner.label("f",x=1,y=-1)
mtext("       Onsite observation and ClimateNA prediction", 1, -4.5, cex = 1.2, outer=TRUE)
mtext(" Environment Canada observation", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()

a <- lm(as.vector(as.matrix(site_month[c(2,13,24,35),c(39:50)]))~as.vector(as.matrix(Dunc_month[,c(39:50)])))
summary(a)
#======Figure 4=========
x2 <- model_2$P3.4.5;x1 <- site_1$P3.4.5;x4 <- model_2$MAT; x3 <- site_2$MAT;x22=x2/1.3 - 6
y1 <- dat4[which(dat4$seedlot == 348),]$rHT;y2 <- dat4[which(dat4$seedlot == 405),]$rHT;y3 <- dat4[which(dat4$seedlot == 406),]$rHT


jpeg(filename = "FIG4.jpeg",height = 1350, width = 1880, res = 170)
par(mfrow=c(2,3),cex.lab=1.25,mai=c(0.4,0.45,0.5,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
lm1 <- lm(y1~x3+I(x3^2))
plot(y1~x3,bg="gold",col = "black",pch=21,xlab = "", main = "N Rossland", ylab="",ylim=c(0.25,1.3),xlim=c(-1.5,7.5))
x0 <- data.frame(x3=seq(min(x3),max(x3),len=100))
p <- predict(lm1,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x3, col='gold2',lwd=2)
lm2 <- lm(y1~x4+I(x4^2))
points(y1~x4,bg="cyan",pch=21)
names(x0) <- "x4"
p <- predict(lm2,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x4, col='cyan2',lwd=2)
legend("bottom",inset = .01,legend = c("Onsite observation","ClimateNA prediction"),pt.bg=c("gold","cyan"), col = "black",pch=21)
corner.label("a",x=1,y=-1)

lm1 <- lm(y2~x3+I(x3^2))
plot(y2~x3,bg="gold",pch=21,xlab = "", main = "Willow Creek", ylab="",yaxt="n",ylim=c(0.25,1.3),xlim=c(-1.5,7.5))
x0 <- data.frame(x3=seq(min(x3),max(x3),len=100))
p <- predict(lm1,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x3, col='gold2',lwd=2)
lm2 <- lm(y2~x4+I(x4^2))
points(y2~x4,bg="cyan",pch=21)
names(x0) <- "x4"
p <- predict(lm2,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x4, col='cyan2',lwd=2)
axis(2,c(0.2,0.4,0.6,0.8,1.0,1.2),col.ticks="black",col.axis="white")
legend("bottom",inset = .01,legend = c("Onsite observation","ClimateNA prediction"),pt.bg=c("gold","cyan"), col = "black",pch=21)
corner.label("b",x=1,y=-1)

lm1 <- lm(y3~x3+I(x3^2))
plot(y3~x3,bg="gold",pch=21,xlab = "", main = "Lower Canjilon", ylab="",yaxt="n",ylim=c(0.25,1.3),xlim=c(-1.5,7.5))
x0 <- data.frame(x3=seq(min(x3),max(x3),len=100))
p <- predict(lm1,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x3, col='gold2',lwd=2)
lm2 <- lm(y3~x4+I(x4^2))
points(y3~x4,bg="cyan",pch=21)
names(x0) <- "x4"
p <- predict(lm2,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x4, col='cyan2',lwd=2)
axis(2,c(0.2,0.4,0.6,0.8,1.0,1.2),col.ticks="black",col.axis="white")
legend("bottom",inset = .01,legend = c("Onsite observation","ClimateNA prediction"),pt.bg=c("gold","cyan"), col = "black",pch=21)
corner.label("c",x=1,y=-1)

par(mai=c(0.75,0.45,0.15,0))

lm1 <- lm(y1~x2+I(x2^2))
plot(y1~x2,bg="cyan",pch=21,ylab = "", xlab="",xlim=c(20,310),ylim=c(0.25,1.3))
x0 <- data.frame(x2=seq(min(x2),max(x2),len=100))
p <- predict(lm1,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x2, col='cyan2',lwd=2)
lm2 <- lm(y1~x1+I(x1^2))
points(y1~x1,bg="gold",pch=21)
x0 <- data.frame(x1=seq(min(x1),max(x1),len=100))
p <- predict(lm2,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x1, col='gold2',lwd=2)
legend("bottom",inset = .01,legend = c("Onsite observation","ClimateNA prediction"),pt.bg=c("gold","cyan"), col = "black",pch=21)
corner.label("d",x=1,y=-1)

lm1 <- lm(y2~x2+I(x2^2))
plot(y2~x2,bg="cyan",pch=21,ylab = "", xlab="",xlim=c(20,310),yaxt="n",ylim=c(0.25,1.3))
x0 <- data.frame(x2=seq(min(x2),max(x2),len=100))
p <- predict(lm1,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x2, col='cyan2',lwd=2)
lm2 <- lm(y2~x1+I(x1^2))
points(y2~x1,bg="gold",pch=21)
x0 <- data.frame(x1=seq(min(x1),max(x1),len=100))
p <- predict(lm2,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x1, col='gold2',lwd=2)
axis(2,c(0.2,0.4,0.6,0.8,1.0,1.2),col.ticks="black",col.axis="white")
legend("bottom",inset = .01,legend = c("Onsite observation","ClimateNA prediction"),pt.bg=c("gold","cyan"), col = "black",pch=21)
corner.label("e",x=1,y=-1)

lm1 <- lm(y3~x2+I(x2^2))
plot(y3~x2,bg="cyan",pch=21,ylab = "", xlab="",yaxt="n",xlim=c(20,310),ylim=c(0.25,1.3))
x0 <- data.frame(x2=seq(min(x2),max(x2),len=100))
p <- predict(lm1,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x2, col='cyan2',lwd=2)
lm2 <- lm(y3~x1+I(x1^2))
points(y3~x1,bg="gold",pch=21)
x0 <- data.frame(x1=seq(min(x1),max(x1),len=100))
p <- predict(lm2,x0)
px <- data.frame(x0,p);px
lines(px$p~px$x1, col='gold2',lwd=2)
legend("bottom",inset = .01,legend = c("Onsite observation","ClimateNA prediction"),pt.bg=c("gold","cyan"), col = "black",pch=21)
axis(2,c(0.2,0.4,0.6,0.8,1.0,1.2),col.ticks="black",col.axis="white")
corner.label("f",x=1,y=-1)
mtext("       Mean annual temperature (°C)", 1, -31.0, cex = 1.2, outer=TRUE)
mtext("       Precipitation in spring (mm)", 1, -3.5, cex = 1.2, outer=TRUE)
mtext(" Tree height increment", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()
#==== Figure 5=====

model_1$PPT_sp <- model_2$P3.4.5
site_1$PPT_sp <- site_1$P3.4.5
site_346 <- site_1[c(-7,-10),]
model_346 <- model_1[c(-7,-10),]

modFit(site_1,y1,varList=c(which(names(site_1) == "MAT"),which(names(site_1) == "PPT_sp")),IR=F,points=F,zVar="N Rossland")
modFit(model_1,y1,varList=c(which(names(model_1) == "MAT"),which(names(model_1) == "PPT_sp")),IR=F,points=F,zVar="")

modFit(site_1,y3,varList=c(which(names(site_1) == "MAT"),which(names(site_1) == "PPT_sp")),IR=F,points=F,zVar="Lower Canjilon")
modFit(model_1,y3,varList=c(which(names(model_1) == "MAT"),which(names(model_1) == "PPT_sp")),IR=F,points=F,zVar="")

modFit(site_346,y_346,varList=c(which(names(site_1) == "MAT"),which(names(site_1) == "PPT_sp")),IR=F,points=F,zVar="McGregor")
modFit(model_346,y_346,varList=c(which(names(model_1) == "MAT"),which(names(model_1) == "PPT_sp")),IR=F,points=F,zVar="")



#====Figure S1=================
x1 <- "cranbrook"
y1 <- "Cran"
x2 <- "duncanlake"
y2 <- "Dunc"
x3 <- "highlevel"
y3 <- "High"
x4 <- "nakusp"
y4 <- "Naku"
x5 <- "parsnip"
y5 <- "Pars"
x6 <- "pinepass"
y6 <- "Pine"
x7 <- "revelstoke"
y7 <- "Reve"
x8 <- "skimikin"
y8 <- "Skim"
x9 <- "terrace"
y9 <- "Terr"
x10 <- "tetejaune"
y10 <- "Tete"
x11 <- "Wells"
y11 <- "Well"
png(filename = "Figure_S1_monthly_comparisons_all_sites_1.png",height = 2050, width = 1500, res = 170)
par(mfrow=c(4,3),cex.lab=1.25,mai=c(0.38,0.55,0.25,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x1,c(15:26)])),order=1, xlab="", ylab="Cranbrook",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "Monthly minimum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x1,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "Monthly maximum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x1,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="Monthly precipitation (mm)")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y2,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x2,c(15:26)])),order=1, xlab="", ylab="Duncan Lake",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y2,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x2,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y2,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x2,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y3,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x3,c(15:26)])),order=1, xlab="", ylab="High Level",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y3,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x3,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y3,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x3,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y4,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x4,c(15:26)])),order=1, xlab="", ylab="Nakusp",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y4,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x4,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y4,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x4,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)
mtext("       ClimateNA prediction", 1, -1.2, cex = 1.2, outer=TRUE)
mtext(" Onsite observation", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()

png(filename = "Figure_S1_monthly_comparisons_all_sites_2.png",height = 2050, width = 1500, res = 170)
par(mfrow=c(4,3),cex.lab=1.25,mai=c(0.38,0.55,0.25,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y5,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x5,c(15:26)])),order=1, xlab="", ylab="Parsnip",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "Monthly minimum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y5,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x5,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "Monthly maximum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y5,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x5,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="Monthly precipitation (mm)")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y6,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x6,c(15:26)])),order=1, xlab="", ylab="Pine Pass",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y6,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x6,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y6,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x6,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y7,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x7,c(15:26)])),order=1, xlab="", ylab="Reveltoke",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y7,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x7,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y7,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x7,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y8,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x8,c(15:26)])),order=1, xlab="", ylab="Skimikin",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y8,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x8,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y8,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x8,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)
mtext("       ClimateNA prediction", 1, -1.2, cex = 1.2, outer=TRUE)
mtext(" Onsite observation", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()


png(filename = "Figure_S1_monthly_comparisons_all_sites_3.png",height = 2050, width = 1500, res = 170)
par(mfrow=c(4,3),cex.lab=1.25,mai=c(0.38,0.55,0.25,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y9,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x9,c(15:26)])),order=1, xlab="", ylab="Terrace",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y9,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x9,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y9,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x9,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y10,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x10,c(15:26)])),order=1, xlab="", ylab="Tete Jaune",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y10,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x10,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y10,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x10,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y11,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x11,c(15:26)])),order=1, xlab="", ylab="Wells",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y11,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x11,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y11,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x11,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)
mtext("       ClimateNA prediction", 1, -22.5, cex = 1.2, outer=TRUE)
mtext("                                                          Onsite observation", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()

# combined supplements ======
png(filename = "Fig6_SI.png",height = 5550, width = 1500, res = 170)
par(mfrow=c(11,3),cex.lab=1.25,mai=c(0.38,0.55,0.25,0), oma = c(0, 0, 0, 0))
par(mgp = c(1.5, 0.5, 0))
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x1,c(15:26)])),order=1, xlab="", ylab="Cranbrook",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "Monthly minimum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x1,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "Monthly maximum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y1,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x1,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="Monthly precipitation (mm)")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y2,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x2,c(15:26)])),order=1, xlab="", ylab="Duncan Lake",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y2,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x2,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y2,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x2,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y3,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x3,c(15:26)])),order=1, xlab="", ylab="High Level",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y3,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x3,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y3,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x3,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y4,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x4,c(15:26)])),order=1, xlab="", ylab="Nakusp",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y4,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x4,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y4,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x4,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)


sglFit_test(as.vector(as.matrix(model_month[model_month$site == y5,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x5,c(15:26)])),order=1, xlab="", ylab="Parsnip",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "Monthly minimum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y5,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x5,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "Monthly maximum temperature (°C)")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y5,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x5,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="Monthly precipitation (mm)")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y6,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x6,c(15:26)])),order=1, xlab="", ylab="Pine Pass",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y6,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x6,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y6,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x6,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y7,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x7,c(15:26)])),order=1, xlab="", ylab="Reveltoke",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y7,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x7,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y7,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x7,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y8,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x8,c(15:26)])),order=1, xlab="", ylab="Skimikin",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y8,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x8,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y8,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x8,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)



sglFit_test(as.vector(as.matrix(model_month[model_month$site == y9,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x9,c(15:26)])),order=1, xlab="", ylab="Terrace",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y9,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x9,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y9,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x9,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y10,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x10,c(15:26)])),order=1, xlab="", ylab="Tete Jaune",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y10,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x10,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y10,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x10,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)

sglFit_test(as.vector(as.matrix(model_month[model_month$site == y11,c(15:26)])),as.vector(as.matrix(site_month[site_month$site == x11,c(15:26)])),order=1, xlab="", ylab="Wells",rmse=T,xlim=c(-12,15),ylim=c(-12,15),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y11,c(3:14)])),as.vector(as.matrix(site_month[site_month$site == x11,c(3:14)])),order=1, ylab="",xlab="",rmse=T,xlim=c(-5,27),ylim=c(-5,27),main = "")
abline(b=1,a=0,lty=2)
sglFit_test(as.vector(as.matrix(model_month[model_month$site == y11,c(39:50)])),as.vector(as.matrix(site_month[site_month$site == x11,c(39:50)])),rmse=T,order=1, ylab="",lab.cex = 1.2, xlab="",xlim=c(0,250),ylim=c(0,250), main="")
abline(b=1,a=0,lty=2)
mtext("       ClimateNA prediction", 1, -1, cex = 1.2, outer=TRUE)
mtext("                                                          Onsite observation", 2, -1.7, cex = 1.2, outer=TRUE)
dev.off()