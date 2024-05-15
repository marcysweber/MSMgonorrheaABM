library(ggplot2)
library(tidyverse)
library(ggrepel)

#copied from cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#early code:
###################
df <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.12_20_17.csv")
df2 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.13_15_11.csv")

a <- ggplot(data = df, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.5, Recovery 3 weeks, Pop 2,000/100,000",
       x = "Year",
       y = "Prevalence (%)") +
  ylim(0,25) + 
  theme(plot.title = element_text(size=8))


aa <- ggplot(data = df, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.5, Recovery 3 weeks, Pop 2,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,1000000) + 
  theme(plot.title = element_text(size=8))


b <- ggplot(data = df2, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.25, Recovery 5 weeks, Pop 2,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,25) + 
  theme(plot.title = element_text(size=8))

bb <- ggplot(data = df2, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.25, Recovery 5 weeks, Pop 2,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,1000000) + 
  theme(plot.title = element_text(size=8))

multiplot(a, aa, b, bb, cols = 2)




df3 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.13_43_22.csv")


c <- ggplot(data = df3, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.2, Recovery 6 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,15) + 
  theme(plot.title = element_text(size=8))

cc <- ggplot(data = df3, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.2, Recovery 6 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,200000) + 
  theme(plot.title = element_text(size=8))


df4 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.13_46_54.csv")

d <- ggplot(data = df4, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.15, Recovery 8 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,15) + 
  theme(plot.title = element_text(size=8))

dd <- ggplot(data = df4, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.15, Recovery 8 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,200000) + 
  theme(plot.title = element_text(size=8))



multiplot(c, cc, d, dd, cols = 2)





df5 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.13_56_11.csv")

e <- ggplot(data = df5, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.3, Recovery 4 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,15) + 
  theme(plot.title = element_text(size=8))

ee <- ggplot(data = df5, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.3, Recovery 4 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,100000) + 
  theme(plot.title = element_text(size=8))

multiplot(e, ee, cols = 2)


df6 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.14_04_38.csv")

f <- ggplot(data = df6, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.45, Recovery 3 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,15) + 
  theme(plot.title = element_text(size=8))

ff <- ggplot(data = df6, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.45, Recovery 3 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,200000) + 
  theme(plot.title = element_text(size=8))

multiplot(f, ff, cols = 2)

df7 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.15_03_23.csv")
  
g <- ggplot(data = df7, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.1, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,15) + 
  theme(plot.title = element_text(size=8))

gg <- ggplot(data = df7, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.1, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,200000) + 
  theme(plot.title = element_text(size=8))


multiplot(g, gg, cols = 2)

df8 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.15_07_05.csv")

h <- ggplot(data = df8, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.08, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,15) + 
  theme(plot.title = element_text(size=8))

hh <- ggplot(data = df8, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.08, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,10000) + 
  theme(plot.title = element_text(size=8))

multiplot(h, hh, cols = 2)



df9 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.15_08_37.csv")

i <- ggplot(data = df9, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.09, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,10) + 
  theme(plot.title = element_text(size=8))

ii <- ggplot(data = df9, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.09, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(i, ii, cols = 2)


df10 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.13.15_16_51.csv")

j <- ggplot(data = df10, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: ProbSeekContact 0.09, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  ylim(0,10) + 
  theme(plot.title = element_text(size=8))

jj <- ggplot(data = df10, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: ProbSeekContact 0.09, Recovery 12 weeks, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(j, jj, cols = 2)





df11 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.14_54_36.csv")

k <- ggplot(data = df11, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 0.75, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
 # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

kk <- ggplot(data = df11, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 0.75, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
#  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(k, kk, cols = 2)
 




df12 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.15_02_14.csv")

l <- ggplot(data = df12, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 0.75, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

ll <- ggplot(data = df12, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 0.75, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(l, ll, cols = 2)





df13 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.15_03_57.csv")

m <- ggplot(data = df13, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 0.5, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

mm <- ggplot(data = df13, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 0.5, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(m, mm, cols = 2)




df14 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.15_09_05.csv")

n <- ggplot(data = df14, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 0.7, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

nn <- ggplot(data = df14, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 0.7, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(n, nn, cols = 2)




df15 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.15_14_53.csv")

o <- ggplot(data = df15, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 0.72, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

oo <- ggplot(data = df15, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 0.72, multi = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(o, oo, cols = 2)




#trying to create two identical runs
df16 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.15_51_18.csv")
df17 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.16.15_52_25.csv")





#calibrating with new params
df18 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.10_52_45.csv")

p <- ggplot(data = df18, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 1, contacts = 13, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

pp <- ggplot(data = df18, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 1, contacts = 13, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(p, pp, cols = 2)




df19 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_02_11.csv")

q <- ggplot(data = df19, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 1.25, contacts = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

qq <- ggplot(data = df19, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 1.25, contacts = 10, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(q, qq, cols = 2)




df20 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_07_43.csv")

r <- ggplot(data = df20, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 1.5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

rr <- ggplot(data = df20, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 1.5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(r, rr, cols = 2)



df21 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_13_02.csv")

s <- ggplot(data = df21, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 3, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

ss <- ggplot(data = df21, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 3, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(s,ss, cols = 2)




df22 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_17_03.csv")

t <- ggplot(data = df22, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

tt <- ggplot(data = df22, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(t,tt, cols = 2)




df23 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_19_55.csv")

u <- ggplot(data = df23, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 4, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

uu <- ggplot(data = df23, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 4, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(u,uu, cols = 2)




df24 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_21_30.csv")

v <- ggplot(data = df24, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 3.5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

vv <- ggplot(data = df24, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 3.5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(v,vv, cols = 2)







df25 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_23_36.csv")

w <- ggplot(data = df25, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 4.5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

ww <- ggplot(data = df25, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 4.5, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(w,ww, cols = 2)




df26 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_25_46.csv")

x <- ggplot(data = df26, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 4.75, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
  # ylim(0,10) + 
  theme(plot.title = element_text(size=8))

xx <- ggplot(data = df26, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 4.75, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(x,xx, cols = 2)





df27 <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/ObserverOutput.2023.Oct.18.11_27_54.csv")

y <- ggplot(data = df27, aes(x = tick / 52, y = Prevalence)) + 
  geom_line() +
  labs(title = "Prevalence: Lambda 4.6, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Prevalence (%)")+
   ylim(0,7) + 
  theme(plot.title = element_text(size=8))

yy <- ggplot(data = df27, aes(x = tick / 52, y = Incidence)) + 
  geom_line() +
  labs(title = "Incidence: Lambda 4.6, contacts = 5, Pop 5,000/100,000",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8))

multiplot(y,yy, cols = 2)




#first real batch run
#different values of annual contacts
batchdf <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/output/ObserverOutput.2023.Oct.18.12_47_00.csv")
batchmap <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/output/ObserverOutput.2023.Oct.18.12_47_00.batch_param_map.csv")
batchdf <- cbind(annual_contacts = 0, batchdf)

for (i in 1:length(batchdf$annual_contacts)) { #for each row in batchdf
  thisrun <- batchdf$run[i]
  batchdf$annual_contacts[i] <- batchmap[batchmap$run==thisrun,2]
}


data_ends <- batchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = batchdf, aes(x = tick / 52, y = Prevalence, group = run)) + 
  geom_line() +
  labs(title = "Batch: Prevalence: Lambda 4.6",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) +
  geom_text_repel(aes(label = annual_contacts), data = data_ends)

aa <- ggplot(data = batchdf, aes(x = tick / 52, y = Incidence, group = run)) + 
  geom_line() +
  labs(title = "Batch: Incidence: Lambda 4.6",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) +
  geom_text_repel(aes(label = annual_contacts), data = data_ends)

multiplot(a,aa, cols = 2)


##batch with replicates
batchdf <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/output/ObserverOutput.2023.Oct.18.15_10_30.csv")
batchmap <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/output/ObserverOutput.2023.Oct.18.15_10_30.batch_param_map.csv")
batchdf <- cbind(annual_contacts = 0, batchdf)

for (i in 1:length(batchdf$annual_contacts)) { #for each row in batchdf
  thisrun <- batchdf$run[i]
  batchdf$annual_contacts[i] <- batchmap[batchmap$run==thisrun,2]
}


data_ends <- batchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = batchdf, aes(x = tick / 52, y = Prevalence, group = run)) + 
  geom_line() +
  labs(title = "Batch: Prevalence: Lambda 4.6",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) +
  geom_text_repel(aes(label = annual_contacts), data = data_ends)

aa <- ggplot(data = batchdf, aes(x = tick / 52, y = Incidence, group = run)) + 
  geom_line() +
  labs(title = "Batch: Incidence: Lambda 4.6",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) +
  geom_text_repel(aes(label = annual_contacts), data = data_ends)

multiplot(a,aa, cols = 2)




batchdf <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/output/ObserverOutput.2023.Oct.19.14_27_20.csv")
batchmap <- read.csv("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/output/ObserverOutput.2023.Oct.19.14_27_20.batch_param_map.csv")
batchdf <- cbind(annual_contacts = 0, batchdf)

for (i in 1:length(batchdf$annual_contacts)) { #for each row in batchdf
  thisrun <- batchdf$run[i]
  batchdf$annual_contacts[i] <- batchmap[batchmap$run==thisrun,2]
}


data_ends <- batchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = batchdf, aes(x = tick / 52, y = Prevalence, group = run)) + 
  geom_line() +
  labs(title = "Batch: Prevalence: Lambda 4.6",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
  #geom_text_repel(aes(label = annual_contacts), data = data_ends)

aa <- ggplot(data = batchdf, aes(x = tick / 52, y = Incidence, group = run)) + 
  geom_line() +
  labs(title = "Batch: Incidence: Lambda 4.6",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
  #geom_text_repel(aes(label = annual_contacts), data = data_ends)

multiplot(a,aa, cols = 2)





library(rrepast)
library(foreach)
library(digest)
library(sensitivity)
library(lhs)
library(rJava)
Sys.setenv(JAVA_HOME = "/Library/Java")
Sys.unsetenv("JAVA_HOME")
system("java -version")




modeldir <- "/Applications/SimpleSIR/"
e <- Model(modeldir, maxtime = 1300, dataset = "data_set_1", TRUE)
Load(e)


all.params <- GetSimulationParameters(e)
param <- AddFactor(name="annual_contacts", min = 3, max = 6)
exp.design.matrix <- AoE.RandomSampling(n=200, factors = param)

param.set <- BuildParameterSet(exp.design.matrix, all.params)

Easy.Setup(modeldir)
Easy.Run(modeldir, "data_set_1")


library(ggplot2)
library(ggrepel)

custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output.csv")

custombatchdf$RunNumber <- as.numeric(custombatchdf$RunNumber)
custombatchdf$AnnualContacts <- as.numeric(custombatchdf$AnnualContacts)
custombatchdf$RecoveryLambda <- as.numeric(custombatchdf$RecoveryLambda)
custombatchdf$tick <- as.numeric(custombatchdf$tick)
custombatchdf$Prevalence <- as.numeric(custombatchdf$Prevalence)
custombatchdf$Incidence <- as.numeric(custombatchdf$Incidence)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Prevalence, group = AnnualContacts)) + 
  geom_line() +
  labs(title = "Custom Parameter Sweep: Prevalence: Lambda 4.5",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Incidence, group = AnnualContacts)) + 
  geom_line() +
  labs(title = "Custom Parameter Sweep: Incidence: Lambda 4.5",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)





custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_Oct25.csv")

custombatchdf$RunNumber <- as.numeric(custombatchdf$RunNumber)
custombatchdf$AnnualContacts <- as.numeric(custombatchdf$AnnualContacts)
custombatchdf$RecoveryLambda <- as.numeric(custombatchdf$RecoveryLambda)
custombatchdf$tick <- as.numeric(custombatchdf$tick)
custombatchdf$Prevalence <- as.numeric(custombatchdf$Prevalence)
custombatchdf$Incidence <- as.numeric(custombatchdf$Incidence)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Prevalence, group = AnnualContacts)) + 
  geom_line(size = 0.05) +
  labs(title = "Custom Parameter Sweep: Prevalence: Lambda 4.5",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Incidence, group = AnnualContacts)) + 
  geom_line(size = 0.05) +
  labs(title = "Custom Parameter Sweep: Incidence: Lambda 4.5",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)



##
dbinom(500, 10000, 0.05)
dbinom(5, 10, 0.5)
dbinom(5, 100, 0.25)
dbinom(5, 100, 0.05)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)
data_ends <- cbind(likelihood = 0, data_ends)

binom_likelihood = function(x){
  return(dbinom(5000, 100000, x / 100))
}

binom_likelihood(3)

data_ends$likelihood <- binom_likelihood(data_ends$Prevalence)

likeplot <- ggplot(data_ends, aes(x = AnnualContacts, y = log(likelihood))) +
  geom_line()+
  xlim(4.8, 5.15)
likeplot

hist(log(data_ends$likelihood))


AnnualContactsResample <- sample(data_ends$AnnualContacts, size = 1000, prob = log(data_ends$likelihood), replace = TRUE)

histogram <- ggplot(AnnualContactsResample) +
  geom_histogram()
hist(AnnualContactsResample, xlim = range(4,6))



library(readr)
library(tidyverse)

custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_Oct25.csv")

custombatchdf$RunNumber <- as.numeric(custombatchdf$RunNumber)
custombatchdf$AnnualContacts <- as.numeric(custombatchdf$AnnualContacts)
custombatchdf$RecoveryLambda <- as.numeric(custombatchdf$RecoveryLambda)
custombatchdf$tick <- as.numeric(custombatchdf$tick)
custombatchdf$Prevalence <- as.numeric(custombatchdf$Prevalence)
custombatchdf$Incidence <- as.numeric(custombatchdf$Incidence)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Prevalence, group = AnnualContacts)) + 
  geom_line(size = 0.05) +
  labs(title = "Custom Parameter Sweep: Prevalence: Lambda 4.5",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Incidence, group = AnnualContacts)) + 
  geom_line(size = 0.05) +
  labs(title = "Custom Parameter Sweep: Incidence: Lambda 4.5",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)



##LIKELIHOOD - ANNUAL CONTACTS
data_ends <- custombatchdf %>% filter(tick == 25 * 52)
data_ends <- cbind(likelihood = 0, data_ends)

binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100)) #values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}


data_ends$likelihood <- binom_likelihood(data_ends$Prevalence)

likeplot <- ggplot(data_ends, aes(x = AnnualContacts, y = log(likelihood))) +
  geom_line()
  #xlim(4.8, 5.15)
likeplot

hist(log(data_ends$likelihood))


AnnualContactsResample <- sample(data_ends$AnnualContacts, size = 1000, prob = data_ends$likelihood, replace = TRUE)

histogram <- ggplot(AnnualContactsResample) +
  geom_histogram()
hist(AnnualContactsResample, xlim = range(4,6))
custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_Oct25.csv")






custombatchdf2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_Oct26.csv")

custombatchdf2$RunNumber <- as.numeric(custombatchdf2$RunNumber)
custombatchdf2$AnnualContacts <- as.numeric(custombatchdf2$AnnualContacts)
custombatchdf2$RecoveryLambda <- as.numeric(custombatchdf2$RecoveryLambda)
custombatchdf2$tick <- as.numeric(custombatchdf2$tick)
custombatchdf2$Prevalence <- as.numeric(custombatchdf2$Prevalence)
custombatchdf2$Incidence <- as.numeric(custombatchdf2$Incidence)

data_ends2 <- custombatchdf2 %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf2, aes(x = tick / 52, y = Prevalence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Custom Parameter Sweep: Prevalence: Contacts 4.95",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf2, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Custom Parameter Sweep: Incidence: Contacts 4.95",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)



##

data_ends2 <- custombatchdf2 %>% filter(tick == 25 * 52)
data_ends2 <- cbind(likelihood = 0, data_ends2)

binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}


data_ends2$likelihood <- binom_likelihood(data_ends2$Prevalence)

likeplot2 <- ggplot(data_ends2, aes(x = RecoveryLambda, y = likelihood)) +
  geom_line() +
  xlim(4.3, 4.7)
likeplot2

hist(log(data_ends$likelihood))


RecoveryLambdaResample <- sample(data_ends2$RecoveryLambda, size = 1000, prob = (data_ends2$likelihood), replace = TRUE)

histogram <- ggplot(RecoveryLambdaResample) +
  geom_histogram()
hist(RecoveryLambdaResample, xlim = range(4,5))



library(data.table)
fwrite(list(AnnualContactsResample), file = "/Users/me597/Documents/annual_contacts_resample.txt")
fwrite(list(RecoveryLambdaResample), file = "/Users/me597/Documents/recovery_lambda_resample.txt")





#plot the 100 most likely trajectories

#sort data ends by likelihood
sorted_ends <- data_ends[order(data_ends$likelihood, decreasing = TRUE),]

#take the top 100 rows
best_100_ends<-sorted_ends[1:100,]

best_100_runnumbers <- unique(best_100_ends$RunNumber)

#use RunNumber of those in data ends to pull up all the rows in custombatchdf with those RunNumbers
best_100 <- custombatchdf %>% filter(RunNumber %in% best_100_runnumbers)

a <- ggplot(data = best_100, aes(x = tick / 52, y = Prevalence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Likeliest 100 Trajectories: Prevalence",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = best_100, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Likeliest 100 Trajectories: Incidence",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)










custombatchdf3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_OCTOBER_27_2023_1.csv")

custombatchdf3$RunNumber <- as.numeric(custombatchdf3$RunNumber)
custombatchdf3$AnnualContacts <- as.numeric(custombatchdf3$AnnualContacts)
custombatchdf3$RecoveryLambda <- as.numeric(custombatchdf3$RecoveryLambda)
custombatchdf3$tick <- as.numeric(custombatchdf3$tick)
custombatchdf3$Prevalence <- as.numeric(custombatchdf3$Prevalence)
custombatchdf3$Incidence <- as.numeric(custombatchdf3$Incidence)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf3, aes(x = tick / 52, y = Prevalence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Calibrated for AnnualContacts: Prevalence",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf3, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Calibrated for AnnualContacts: Incidence",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)







custombatchdf4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_OCTOBER_27_2023_2.csv")

custombatchdf4$RunNumber <- as.numeric(custombatchdf4$RunNumber)
custombatchdf4$AnnualContacts <- as.numeric(custombatchdf4$AnnualContacts)
custombatchdf4$RecoveryLambda <- as.numeric(custombatchdf4$RecoveryLambda)
custombatchdf4$tick <- as.numeric(custombatchdf4$tick)
custombatchdf4$Prevalence <- as.numeric(custombatchdf4$Prevalence)
custombatchdf4$Incidence <- as.numeric(custombatchdf4$Incidence)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf4, aes(x = tick / 52, y = Prevalence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Calibrated for RecoveryLambda: Prevalence",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf4, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Calibrated for RecoveryLambda: Incidence",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)






### both parameters varying at once
custombatchdf5 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_OCTOBER_27_2023_5.csv")

custombatchdf5$RunNumber <- as.numeric(custombatchdf5$RunNumber)
custombatchdf5$AnnualContacts <- as.numeric(custombatchdf5$AnnualContacts)
custombatchdf5$RecoveryLambda <- as.numeric(custombatchdf5$RecoveryLambda)
custombatchdf5$tick <- as.numeric(custombatchdf5$tick)
custombatchdf5$Prevalence <- as.numeric(custombatchdf5$Prevalence)
custombatchdf5$Incidence <- as.numeric(custombatchdf5$Incidence)

a <- ggplot(data = custombatchdf5, aes(x = tick / 52, y = Prevalence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Sweeping Both Parameters: Prevalence",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf5, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Sweeping Both Parameters: Incidence",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)



data_ends5 <- custombatchdf5 %>% filter(tick == 25 * 52)
data_ends5 <- cbind(likelihood = 0, data_ends5)

binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}


data_ends5$likelihood <- binom_likelihood(data_ends5$Prevalence)

likeplot5 <- ggplot(data_ends5, aes(x = RecoveryLambda, y = likelihood)) +
  geom_line() 
likeplot5

combinedResample<-data_ends5[sample(nrow(data_ends5), size = 1000, prob = (data_ends5$likelihood), replace = TRUE),]

combinedResampleAnnualContacts <- combinedResample$AnnualContacts

combinedResampleRecoveryLambda <- combinedResample$RecoveryLambda

hist(combinedResampleAnnualContacts)
hist(combinedResampleRecoveryLambda)



library(data.table)
fwrite(list(combinedResampleAnnualContacts), file = "/Users/me597/Documents/annual_contacts_resample2.txt")
fwrite(list(combinedResampleRecoveryLambda), file = "/Users/me597/Documents/recovery_lambda_resample2.txt")


data_ends5 <- custombatchdf5 %>% filter(tick == 25 * 52)
data_ends5 <- cbind(likelihood = 0, data_ends5)


custombatchdf6 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_OCTOBER_27_2023_6.csv")

custombatchdf6$RunNumber <- as.numeric(custombatchdf6$RunNumber)
custombatchdf6$AnnualContacts <- as.numeric(custombatchdf6$AnnualContacts)
custombatchdf6$RecoveryLambda <- as.numeric(custombatchdf6$RecoveryLambda)
custombatchdf6$tick <- as.numeric(custombatchdf6$tick)
custombatchdf6$Prevalence <- as.numeric(custombatchdf6$Prevalence)
custombatchdf6$Incidence <- as.numeric(custombatchdf6$Incidence)

data_ends <- custombatchdf %>% filter(tick == 25 * 52)

a <- ggplot(data = custombatchdf6, aes(x = tick / 52, y = Prevalence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Calibrated for both Parmas: Prevalence",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf6, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line(size = 0.05) +
  labs(title = "Calibrated for both Params: Incidence",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)










#using the double sweep to calibrate for both prevalence and incidence

prev_binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100, log=TRUE)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}

#is incidence bionmial or continuous???
inc_binom_likelihood = function(x){
  return(dbinom(100, 1000, x / 100000, log=TRUE)) # values made up for now
}

data_ends5 <- custombatchdf5 %>% filter(tick == 25 * 52)
data_ends5 <- cbind(likelihood_prev = 0, likelihood_inc = 0, combined_log_likelihood = 0, data_ends5)


data_ends5$likelihood_prev <- prev_binom_likelihood(data_ends5$Prevalence)
data_ends5$likelihood_inc <- inc_binom_likelihood(data_ends5$Incidence)

data_ends5$combined_log_likelihood <- data_ends5$likelihood_prev + data_ends5$likelihood_inc

sum_likelihood <- sum(exp(data_ends5$combined_log_likelihood))

data_ends5$weights <- exp(data_ends5$combined_log_likelihood)/sum_likelihood

weightplot <- ggplot(data_ends5, aes(x = AnnualContacts, y = weights)) +
  geom_line() 
weightplot

combinedResample<-data_ends5[sample(nrow(data_ends5), size = 1000, prob = (data_ends5$weights), replace = TRUE),]

combinedResampleAnnualContacts <- combinedResample$AnnualContacts

combinedResampleRecoveryLambda <- combinedResample$RecoveryLambda

hist(combinedResampleAnnualContacts)
hist(combinedResampleRecoveryLambda)






#######################




###strain
#################
custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_OCTOBER_31_2023_2.csv")

custombatchdf$RunNumber <- as.numeric(custombatchdf$RunNumber)
custombatchdf$AnnualContacts <- as.numeric(custombatchdf$AnnualContacts)
custombatchdf$RecoveryLambda <- as.numeric(custombatchdf$RecoveryLambda)
custombatchdf$tick <- as.numeric(custombatchdf$tick)
custombatchdf$Prevalence <- as.numeric(custombatchdf$Prevalence)
custombatchdf$Incidence <- as.numeric(custombatchdf$Incidence)
custombatchdf$StrainPrevalence <- as.numeric(custombatchdf$StrainPrevalence)
custombatchdf$StrainIncidence <- as.numeric(custombatchdf$StrainIncidence)

a <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_line(aes(y=StrainPrevalence), color="red")
  labs(title = "Calibrated for both Parmas: Prevalence",
       x = "Year",
       y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Incidence, group = RunNumber)) + 
  geom_line( aes(y = Prevalence),size = 0.05, color="black") +
  geom_line(aes(y=StrainIncidence), color = "red")
  labs(title = "Calibrated for both Params: Incidence",
       x = "Year",
       y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

multiplot(a,aa, cols = 2)




custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_NOVEMBER_16_2023_overnight.csv", skip = 1)

a <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") 
  #geom_line(aes(y=StrainPrevalence), color="red", size = 0.05)
labs(title = "Calibrated for both Parmas: Prevalence",
     x = "Year",
     y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") 
labs(title = "Calibrated for both Params: Incidence",
     x = "Year",
     y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aaa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  labs(x = "Year",
       y = "Proportion symptomatic of detected")

aaaa <- ggplot(data=custombatchdf, aes(x= tick/52, y = SymptomProportion, group = RunNumber))+
  geom_line(color = "red", size = 0.05) +
  labs(x = "Year",
       y = "Proportion symptomatic")

multiplot(a,aa,aaa, aaaa, cols = 2)



data_ends <- custombatchdf %>% filter(tick == 25 * 52)



#is there a relationship between the size of the initial drop and the parameter for symptoms?

each_run <- custombatchdf %>% filter(tick == 0)
each_run2 <- custombatchdf %>% filter(tick == 4)

each_run$initialfall <- each_run$Prevalence - each_run2$Prevalence

plot(each_run2$ProbSymptomatic, each_run2$Prevalence)
plot(each_run$ProbSymptomatic, each_run$initialfall)

##################


prev_binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100, log=TRUE)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}

#is incidence bionmial or continuous???
inc_norm_likelihood = function(x){
  return(dnorm(6500, x, 6500/5, log=TRUE)) # values made up for now
}

data_ends <- custombatchdf %>% filter(tick == 25 * 52)
data_ends <- cbind(likelihood_prev = 0, likelihood_inc = 0, combined_log_likelihood = 0, data_ends)


data_ends$likelihood_prev <- prev_binom_likelihood(data_ends$Prevalence)
data_ends$likelihood_inc <- inc_binom_likelihood(data_ends$Detected)

data_ends$combined_log_likelihood <- data_ends$likelihood_prev + data_ends$likelihood_inc

sum_likelihood <- sum(exp(data_ends$combined_log_likelihood))

data_ends$weights <- exp(data_ends$combined_log_likelihood)/sum_likelihood

weightplotcontact <- ggplot(data_ends, aes(x = AnnualContacts, y = weights)) +
  geom_line() 
weightplotrecovery <- ggplot(data_ends, aes(x = RecoveryLambda, y = weights)) +
  geom_line() 
weightplotsympt <- ggplot(data_ends, aes(x = ProbSymptomatic, y = weights)) +
  geom_line() 
weightplotscreen <- ggplot(data_ends, aes(x = ScreenInterval, y = weights)) +
  geom_line() 

multiplot(weightplotcontact, weightplotrecovery, weightplotsympt, weightplotscreen, cols = 2)

combinedResample<-data_ends[sample(nrow(data_ends), size = 1000, prob = (data_ends$weights), replace = TRUE),]

resampleAnnualContacts <- combinedResample$AnnualContacts
resampleRecoveryLambda <- combinedResample$RecoveryLambda
resampleProbSymptomatic <- combinedResample$ProbSymptomatic
resampleScreenInterval <- combinedResample$ScreenInterval

hist(resampleAnnualContacts)
hist(resampleRecoveryLambda)
hist(resampleProbSymptomatic)
hist(resampleScreenInterval)

library(data.table)
fwrite(list(resampleAnnualContacts), file = "/Users/me597/Documents/calibrated_params/annual_contacts_resample.txt")
fwrite(list(resampleRecoveryLambda), file = "/Users/me597/Documents/calibrated_params/recovery_lambda_resample.txt")
fwrite(list(resampleProbSymptomatic), file = "/Users/me597/Documents/calibrated_params/prob_symptomatic_resample.txt")
fwrite(list(resampleScreenInterval), file = "/Users/me597/Documents/calibrated_params/screen_interval_resample.txt")




#as an alternative to resampling with replacement,
#we could take the 500 best parameter combos (according to weight)

data_ends[order(data_ends$weights),]

best_of_sweep <- data_ends[1:500,]

hist(best_of_sweep$AnnualContacts)
hist(best_of_sweep$RecoveryLambda)
hist(best_of_sweep$ProbSymptomatic)
hist(best_of_sweep$ScreenInterval)







custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_NOVEMBER_15_2023_2.csv")

a <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") 
#geom_line(aes(y=StrainPrevalence), color="red", size = 0.05)
labs(title = "Calibrated for both Parmas: Prevalence",
     x = "Year",
     y = "Prevalence (%)")+
  #ylim(0,7) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") 
labs(title = "Calibrated for both Params: Incidence",
     x = "Year",
     y = "Incidence per 100,000")+
  #  ylim(0,25000) + 
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

aaa <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  labs(x = "Year",
       y = "Proportion symptomatic of detected")

aaaa <- ggplot(data=custombatchdf, aes(x= tick/52, y = SymptomProportion, group = RunNumber))+
  geom_line(color = "red", size = 0.05) +
  labs(x = "Year",
       y = "Proportion symptomatic")

multiplot(a,aa,aaa, aaaa, cols = 2)


ggplot(data=data_ends, aes(x=ProbSymptomatic, y = DetectedAndSymptoms / Detected)) + 
  geom_point()

ggplot(data=data_ends, aes(x=ScreenInterval, y = DetectedAndSymptoms / Detected)) + 
  geom_point()


library(ggplot2)

## new figure code

#observed value and confidence interval shown by lines/dashed lines
prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_line(aes(y=4.5), color="red", size = 0.5) +
  geom_line(aes(y=3.6), color="red", size = 0.5, linetype = "dashed") +
  geom_line(aes(y=5.4), color="red", size = 0.5, linetype = "dashed") +
  labs(title = "",
     x = "Year",
     y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) 
prev

#observed value and confidence interval shown by dot w/ error bars
#Reza prefers this one, provided the x values are the years used in the calibration process
prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) 



inc <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
labs(title = "",
     x = "Year",
     y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) 
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

library(Hmisc)
binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected")
#symptomatic


multiplot(prev,inc,symptomatic, cols = 2)



#likelihood functions with the real observed values for three targets
prev_binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100, log=TRUE)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}

#incidence
inc_norm_likelihood = function(x){
  return(dnorm(6508, x, 6500/5, log=TRUE)) # values from 2018 CDC report
}

sympt_binom_likelihood = function(x){
  return(dbinom(233, 343, x, log=TRUE))
}

library(tidyverse)
target_data <- custombatchdf %>% filter(tick %% 5 * 52 == 0)
target_data <- target_data %>% filter(tick != 0)


target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)

target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)

target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt

target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)

sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))

data_ends <- target_data %>% filter(tick == 25 * 52)

# to use proper weights for resampling
for (i in 1:nrow(data_ends)){
  thisrun <- data_ends$RunNumber[i]
  data_ends$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun]))
}


weightplotcontact <- ggplot(data_ends, aes(x = AnnualContacts, y = weight)) +
  geom_line() 
weightplotrecovery <- ggplot(data_ends, aes(x = RecoveryLambda, y = weight)) +
  geom_line() 
weightplotsympt <- ggplot(data_ends, aes(x = ProbSymptomatic, y = weight)) +
  geom_line() 
weightplotscreen <- ggplot(data_ends, aes(x = ScreenInterval, y = weight)) +
  geom_line() 

multiplot(weightplotcontact, weightplotrecovery, weightplotsympt, weightplotscreen, cols = 2)


# to use best "n" runs
for (i in 1:nrow(data_ends)){
  thisrun <- data_ends$RunNumber[i]
  data_ends$combined_log_likelihood[i] <- sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun])
}

data_ends <- data_ends[order(data_ends$combined_log_likelihood, decreasing = TRUE),]

best_of_sweep <- data_ends[1:100,]

a <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0,9) + 
  theme_bw()

r <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
  xlim(0, 2.5)+ 
  theme_bw()

p <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
  xlim(0, 0.8)+ 
  theme_bw()

s <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0, 5.5)+ 
  theme_bw()

multiplot(a, r, p, s, cols = 2)

resampleSeed <- best_of_sweep$seed
resampleAnnualContacts <- best_of_sweep$AnnualContacts
resampleRecoveryLambda <- best_of_sweep$RecoveryLambda
resampleProbSymptomatic <- best_of_sweep$ProbSymptomatic
resampleScreenInterval <- best_of_sweep$ScreenInterval



library(data.table)
fwrite(list(resampleAnnualContacts), file = "/Users/me597/Documents/calibrated_params/annual_contacts_resample.txt")
fwrite(list(resampleRecoveryLambda), file = "/Users/me597/Documents/calibrated_params/recovery_lambda_resample.txt")
fwrite(list(resampleProbSymptomatic), file = "/Users/me597/Documents/calibrated_params/prob_symptomatic_resample.txt")
fwrite(list(resampleScreenInterval), file = "/Users/me597/Documents/calibrated_params/screen_interval_resample.txt")
fwrite(list(resampleSeed), file = "/Users/me597/Documents/calibrated_params/seed_resample.txt")


library(ggplot2)
custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_NOVEMBER_22_2023_1.csv")


prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic


multiplot(prev,symptomatic, inc, cols = 2)



#extracting parameter ranges
min(custombatchdf$AnnualContacts)
max(custombatchdf$AnnualContacts)

min(custombatchdf$RecoveryLambda)
max(custombatchdf$RecoveryLambda)

min(custombatchdf$ProbSymptomatic)
max(custombatchdf$ProbSymptomatic)

min(custombatchdf$ScreenInterval)
max(custombatchdf$ScreenInterval)


library(tidyverse)
target_data <- custombatchdf %>% filter(tick %% 5 * 52 == 0)
target_data <- target_data %>% filter(tick != 0)


target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)

target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)

target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt

target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)

sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))

data_ends <- target_data %>% filter(tick == 25 * 52)

# to use proper weights for resampling
for (i in 1:nrow(data_ends)){
  thisrun <- data_ends$RunNumber[i]
  data_ends$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun]))
}


weightplotcontact <- ggplot(data_ends, aes(x = AnnualContacts, y = weight)) +
  geom_line() 
weightplotrecovery <- ggplot(data_ends, aes(x = RecoveryLambda, y = weight)) +
  geom_line() 
weightplotsympt <- ggplot(data_ends, aes(x = ProbSymptomatic, y = weight)) +
  geom_line() 
weightplotscreen <- ggplot(data_ends, aes(x = ScreenInterval, y = weight)) +
  geom_line() 

multiplot(weightplotcontact, weightplotrecovery, weightplotsympt, weightplotscreen, cols = 2)

combinedResample<-data_ends[sample(nrow(data_ends), size = 1000, prob = (data_ends$weight), replace = TRUE),]

a <- ggplot(combinedResample) + 
  geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0,9) + 
  theme_bw()

r <- ggplot(combinedResample) + 
  geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
  xlim(0, 2.5)+ 
  theme_bw()

p <- ggplot(combinedResample) + 
  geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
  xlim(0, 0.8)+ 
  theme_bw()

s <- ggplot(combinedResample) + 
  geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0, 5.5)+ 
  theme_bw()

multiplot(a, r, p, s, cols = 2)


custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_NOVEMBER_22_2023_overnight.csv")


prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic


multiplot(prev,symptomatic, inc, cols = 2)

library(tidyverse)
target_data <- custombatchdf %>% filter(tick %% 5 * 52 == 0)
target_data <- target_data %>% filter(tick != 0)


target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)

target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)

target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt

target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)

sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))

data_ends <- target_data %>% filter(tick == 25 * 52)

# to use proper weights for resampling
for (i in 1:nrow(data_ends)){
  thisrun <- data_ends$RunNumber[i]
  data_ends$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun]))
}


weightplotcontact <- ggplot(data_ends, aes(x = AnnualContacts, y = weight)) +
  geom_line() 
weightplotrecovery <- ggplot(data_ends, aes(x = RecoveryLambda, y = weight)) +
  geom_line() 
weightplotsympt <- ggplot(data_ends, aes(x = ProbSymptomatic, y = weight)) +
  geom_line() 
weightplotscreen <- ggplot(data_ends, aes(x = ScreenInterval, y = weight)) +
  geom_line() 

multiplot(weightplotcontact, weightplotrecovery, weightplotsympt, weightplotscreen, cols = 2)

data_ends <- data_ends[order(data_ends$combined_log_likelihood, decreasing = TRUE),]
best_of_sweep <- data_ends[1:10,]

a <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0,9) + 
  theme_bw()

r <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
  xlim(0, 2.5)+ 
  theme_bw()

p <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
  xlim(0, 0.8)+ 
  theme_bw()

s <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0, 5.5)+ 
  theme_bw()

multiplot(a, r, p, s, cols = 2)

combinedResample<-data_ends[sample(nrow(data_ends), size = 1000, prob = (data_ends$weight), replace = TRUE),]

a <- ggplot(combinedResample) + 
  geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0,9) + 
  theme_bw()

r <- ggplot(combinedResample) + 
  geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
  xlim(0, 2.5)+ 
  theme_bw()

p <- ggplot(combinedResample) + 
  geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
  xlim(0, 0.8)+ 
  theme_bw()

s <- ggplot(combinedResample) + 
  geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0, 5.5)+ 
  theme_bw()

multiplot(a, r, p, s, cols = 2)


best_traj <- custombatchdf %>% filter(RunNumber %in% best_of_sweep$RunNumber)
prev <- ggplot(data = best_traj, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = best_traj, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = best_traj, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic


multiplot(prev,symptomatic, inc, cols = 2)

2 + (9 * 1) + (9*1)
2.5/52


custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_NOVEMBER_29_2023_overnight.csv", skip = 1)


prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic


multiplot(prev,symptomatic, inc, cols = 2) 



target_data <- custombatchdf %>% filter(tick %% 5 * 52 == 0)
target_data <- target_data %>% filter(tick != 0)


target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)

target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)

target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt

target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)

sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))

data_ends <- target_data %>% filter(tick == 25 * 52)

# to use proper weights for resampling
for (i in 1:nrow(data_ends)){
  thisrun <- data_ends$RunNumber[i]
  data_ends$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun]))
}


weightplotcontact <- ggplot(data_ends, aes(x = AnnualContacts, y = weight)) +
  geom_line() 
weightplotrecovery <- ggplot(data_ends, aes(x = RecoveryLambda, y = weight)) +
  geom_line() 
weightplotsympt <- ggplot(data_ends, aes(x = ProbSymptomatic, y = weight)) +
  geom_line() 
weightplotscreen <- ggplot(data_ends, aes(x = ScreenInterval, y = weight)) +
  geom_line() 

multiplot(weightplotcontact, weightplotrecovery, weightplotsympt, weightplotscreen, cols = 2)

data_ends <- data_ends[order(data_ends$combined_log_likelihood, decreasing = TRUE),]
best_of_sweep <- data_ends[1:10,]

a <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0,20) + 
  theme_bw()

r <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
  xlim(0, 3)+ 
  theme_bw()

p <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
  xlim(0, 1)+ 
  theme_bw()

s <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0, 5.5)+ 
  theme_bw()

multiplot(a, r, p, s, cols = 2)

best_traj <- custombatchdf %>% filter(RunNumber %in% best_of_sweep$RunNumber)
prev <- ggplot(data = best_traj, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = best_traj, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = best_traj, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic


multiplot(prev,symptomatic, inc, cols = 2)




custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_NOVEMBER_30_2023_overnight.csv")


prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_line(aes(y = StrainPrevalence), size = 0.05, color = "red")+
  geom_point(aes(y=4.5, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "gray")+
  geom_point(aes(y=4.5, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "gray")+
  geom_point(aes(y=4.5, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "gray")+
  geom_point(aes(y=4.5, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "gray")+
  geom_point(aes(y=4.5, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "gray")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "gray")+
  geom_point(aes(y=6508, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "gray")+
  geom_point(aes(y=6508, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "gray")+
  geom_point(aes(y=6508, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "gray")+
  geom_point(aes(y=6508, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "gray")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "gray")+
  geom_point(aes(y=0.679, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "gray")+
  geom_point(aes(y=0.679, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "gray")+
  geom_point(aes(y=0.679, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "gray")+
  geom_point(aes(y=0.679, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "gray")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic

treatments <- ggplot(data=custombatchdf, aes(x=tick / 52, group=RunNumber))+
  geom_line(aes(y = Treatments), size = 0.05, color = "black") +
  geom_line(aes(y = FailedTreatments), size = 0.05, color = "red")+
  labs(x = "Year",
       y = "Count Treatments (annually)") +
  theme_bw()




multiplot(prev,symptomatic, inc, treatments, cols = 2) 

library(tidyverse)
target_data <- custombatchdf %>% filter(tick %% 5 * 52 == 0)
target_data <- target_data %>% filter(tick != 0)


target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)

target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)

target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt

target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)

sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))

data_ends <- target_data %>% filter(tick == 25 * 52)

# to use proper weights for resampling
for (i in 1:nrow(data_ends)){
  thisrun <- data_ends$RunNumber[i]
  data_ends$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun]))
}


weightplotcontact <- ggplot(data_ends, aes(x = AnnualContacts, y = weight)) +
  geom_line() 
weightplotrecovery <- ggplot(data_ends, aes(x = RecoveryLambda, y = weight)) +
  geom_line() 
weightplotsympt <- ggplot(data_ends, aes(x = ProbSymptomatic, y = weight)) +
  geom_line() 
weightplotscreen <- ggplot(data_ends, aes(x = ScreenInterval, y = weight)) +
  geom_line() 

multiplot(weightplotcontact, weightplotrecovery, weightplotsympt, weightplotscreen, cols = 2)

data_ends <- data_ends[order(data_ends$combined_log_likelihood, decreasing = TRUE),]
best_of_sweep <- data_ends[1:100,]

a <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0,20) + 
  theme_bw()

r <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
  xlim(0, 3)+ 
  theme_bw()

p <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
  xlim(0, 1)+ 
  theme_bw()

s <- ggplot(best_of_sweep) + 
  geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
  xlim(0, 5.5)+ 
  theme_bw()

multiplot(a, r, p, s, cols = 2)

resampleSeed <- best_of_sweep$seed
resampleAnnualContacts <- best_of_sweep$AnnualContacts
resampleRecoveryLambda <- best_of_sweep$RecoveryLambda
resampleProbSymptomatic <- best_of_sweep$ProbSymptomatic
resampleScreenInterval <- best_of_sweep$ScreenInterval

library(data.table)
fwrite(list(resampleAnnualContacts), file = "/Users/me597/Documents/calibrated_params/annual_contacts_resample.txt")
fwrite(list(resampleRecoveryLambda), file = "/Users/me597/Documents/calibrated_params/recovery_lambda_resample.txt")
fwrite(list(resampleProbSymptomatic), file = "/Users/me597/Documents/calibrated_params/prob_symptomatic_resample.txt")
fwrite(list(resampleScreenInterval), file = "/Users/me597/Documents/calibrated_params/screen_interval_resample.txt")
fwrite(list(resampleSeed), file = "/Users/me597/Documents/calibrated_params/seed_resample.txt")


best_traj <- custombatchdf %>% filter(RunNumber %in% best_of_sweep$RunNumber)
prev <- ggplot(data = best_traj, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
  geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
  geom_point(aes(y=4.5, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "red")+
  geom_point(aes(y=4.5, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "red")+
  geom_point(aes(y=4.5, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = best_traj, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red")+
  geom_point(aes(y=6508, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
  geom_point(aes(y=6508, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "red")+
  geom_point(aes(y=6508, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "red")+
  geom_point(aes(y=6508, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "red")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,20000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = best_traj, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red")+
  geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
  geom_point(aes(y=0.679, x = 15), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "red")+
  geom_point(aes(y=0.679, x = 20), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "red")+
  geom_point(aes(y=0.679, x = 25), color="red", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "red")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic

treatments <- ggplot(data=best_traj, aes(x=tick / 52, group=RunNumber))+
  geom_line(aes(y = Treatments), size = 0.05, color = "black") +
  geom_line(aes(y = FailedTreatments), size = 0.05, color = "red")+
  labs(x = "Year",
       y = "Count Treatments (annually)") +
  ylim(0,16000)+
  theme_bw()



multiplot(prev,symptomatic, inc, cols = 2)


#rerunning the 100 best above with resistance turned on
custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_1_2023_2.csv")


prevR <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_line(aes(y = StrainPrevalence), size = 0.05, color = "red")+
  geom_point(aes(y=4.5, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "gray")+
  geom_point(aes(y=4.5, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "gray")+
  geom_point(aes(y=4.5, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "gray")+
  geom_point(aes(y=4.5, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "gray")+
  geom_point(aes(y=4.5, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "gray")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



incR <- ggplot(data = custombatchdf, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "gray")+
  geom_point(aes(y=6508, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "gray")+
  geom_point(aes(y=6508, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "gray")+
  geom_point(aes(y=6508, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "gray")+
  geom_point(aes(y=6508, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "gray")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,20000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomaticR <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "gray")+
  geom_point(aes(y=0.679, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "gray")+
  geom_point(aes(y=0.679, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "gray")+
  geom_point(aes(y=0.679, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "gray")+
  geom_point(aes(y=0.679, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "gray")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic

treatmentsR <- ggplot(data=custombatchdf, aes(x=tick / 52, group=RunNumber))+
  geom_line(aes(y = Treatments), size = 0.05, color = "black") +
  geom_line(aes(y=KnownFailedTreatments), size = 0.05, color="darkgreen")+
  geom_line(aes(y = FailedTreatments), size = 0.05, color = "red")+
  labs(x = "Year",
       y = "Count Treatments (annually)") + 
  ylim(0,16000)+
  theme_bw()




multiplot(prev, inc, symptomatic, treatments, prevR, incR, symptomaticR, treatmentsR, cols = 2) 


custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_4_2023_1.csv")
library(ggplot2)
surveillance <- ggplot(data = custombatchdf, aes(x=tick/52, group = RunNumber))+
  geom_line(aes(y=SurveillanceEstPropResist), linewidth = 0.05, color = "blue")+
  geom_line(aes(y=TruePropResist), linewidth = 0.05, color = "red")+
  theme_bw()
surveillance




custombatchdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_6_2023_overnight.csv")


prev <- ggplot(data = custombatchdf, aes(x = tick / 52, group = RunNumber)) + 
  geom_line(aes(y = Prevalence),size = 0.05, color = "black") +
  geom_line(aes(y = StrainPrevalence), size = 0.05, color = "red")+
  geom_point(aes(y=4.5, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "gray")+
  geom_point(aes(y=4.5, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "gray")+
  geom_point(aes(y=4.5, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 15), color = "gray")+
  geom_point(aes(y=4.5, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 20), color = "gray")+
  geom_point(aes(y=4.5, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 25), color = "gray")+
  labs(title = "",
       x = "Year",
       y = "Prevalence (%)") +
  theme(plot.title = element_text(size=8)) +
  ylim(0,10)+
  theme_bw()



inc <- ggplot(data = custombatchdf[custombatchdf$tick!=0,], aes(x = tick / 52, y = Detected, group = RunNumber)) + 
  geom_line( aes(y = Detected),size = 0.05, color="black") +
  geom_point(aes(y=6508, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "gray")+
  geom_point(aes(y=6508, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "gray")+
  geom_point(aes(y=6508, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 15), color = "gray")+
  geom_point(aes(y=6508, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 20), color = "gray")+
  geom_point(aes(y=6508, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 25), color = "gray")+
  labs(title = "",
       x = "Year",
       y = "Incidence per 100,000")+
  theme(plot.title = element_text(size=8)) +
  ylim(0,30000)+
  theme_bw()
#geom_text_repel(aes(label = AnnualContacts), data = data_ends)

#library(Hmisc)
#binconf(233, 343, method = "wilson")

symptomatic <- ggplot(data = custombatchdf, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
  geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
  geom_point(aes(y=0.679, x = 5), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "gray")+
  geom_point(aes(y=0.679, x = 10), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "gray")+
  geom_point(aes(y=0.679, x = 15), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 15), color = "gray")+
  geom_point(aes(y=0.679, x = 20), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 20), color = "gray")+
  geom_point(aes(y=0.679, x = 25), color="gray", size = 1) +
  geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 25), color = "gray")+
  labs(x = "Year",
       y = "Proportion symptomatic of detected") +
  ylim(0.4, 0.9) +
  theme_bw()

#symptomatic

treatments <- ggplot(data=custombatchdf[custombatchdf$tick!=0,], aes(x=tick / 52, group=RunNumber))+
  geom_line(aes(y = Treatments), size = 0.05, color = "black") +
  geom_line(aes(y = FailedTreatments), size = 0.05, color = "red")+
  labs(x = "Year",
       y = "Count Treatments (annually)") +
  theme_bw()




multiplot(prev,symptomatic, inc, treatments, cols = 2) 

cost <- ggplot(data=custombatchdf[custombatchdf$tick!=0,], aes(x=tick/52, group = RunNumber))+
  geom_line(aes(y=AnnualMonetaryCost/1000000), size = 0.05, color = "black")+
  labs(x = "Year",
       y = "Cost in Millions of Dollars (annually)") +
  theme_bw()
cost
multiplot(prev, symptomatic, inc, cost, cols = 2)







