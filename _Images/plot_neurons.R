library(ggplot2)

# plot neurone 1 
# senza stima

load("/home/laura/Documents/Dottorato/2.06 Calcium imaging/Review/real_data/data/g.RData")
load("/home/laura/Documents/Dottorato/2.06 Calcium imaging/Review/real_data/data/int.Rdata")

data <- read.csv("/home/laura/Documents/Dottorato/2.06 Calcium imaging/Review/real_data/data/cellula2.csv", header = FALSE)
y_real = c(data$V1)
length(y_real)
rm(list = ("data"))
n = length(y_real)
df = data.frame(x = 1:n, y = y_real, g = g, interval = int)

df_rect = data.frame(start = sapply(unique(df$interval)[-1], function(x) min(df$x[df$interval==x])),
                     end = sapply(unique(df$interval)[-1], function(x) max(df$x[df$interval==x])),
                     Stimulus = as.factor(c("Static grating","Natural scene","Natural scene",
                                            "Static grating","Natural movie","Natural scene",
                                            "Static grating")) )


cols = c("#91ff00", "#00fffb","#ff3700")

df$x = df$x/30
df_rect$start = df_rect$start/30
df_rect$end = df_rect$end/30


plot1 = ggplot(data = df) +
  geom_rect(data = df_rect, inherit.aes = FALSE,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf, fill = Stimulus), alpha = 0.12 ) +
  scale_fill_manual(values = cols) +
  geom_line(aes(x = x, y = y)) +
  theme_bw() +
  theme(legend.position = "bottom",
        rect = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous(name = "Time (seconds)") +
  scale_y_continuous(name = "Calcium level") 
# geom_line(aes(x = x, y = AA), col = "gold")

pdf("neuron1_trace.pdf", width=8, height=3.5)
print(plot1)     
dev.off() 



load("/home/laura/Documents/Dottorato/2.06 Calcium imaging/Review/real_data/out/AA.RData")
AA = colMeans(AA_gMFM)
rm(AA_gMFM)

plot2 = ggplot(data = df) +
  geom_rect(data = df_rect, inherit.aes = FALSE,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf, fill = Stimulus), alpha = 0.12 ) +
  scale_fill_manual(values = cols) +
  geom_line(aes(x = x, y = y)) +
  theme_bw() +
  theme(legend.position = "bottom",
        rect = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous(name = "Time (seconds)") +
  scale_y_continuous(name = "Calcium level") +
 geom_line(aes(x = x, y = AA), col = "gold")

pdf("neuron1_trace+estimate.pdf", width=8, height=3.5)
print(plot2)     
dev.off() 













# plot neurone experiment 2 paper SIS
data <- read.csv("/home/laura/Documents/Dottorato/3.11 Conferenze/1_ SIS2021/Analisi/Data/SIScellula517444286_dep350_exp2.csv", header = FALSE)

y_real = c(data$V1)
length(y_real)
rm(list = ("data"))
n = length(y_real)

g = rep(4, n)
J = 4
int = g

drift_grat <- read.csv("/home/laura/Documents/Dottorato/3.11 Conferenze/1_ SIS2021/Analisi/Data/SISdrifting_grating.csv", header = TRUE)

g[min(drift_grat$start[drift_grat$start < 20000]):max(drift_grat$end[drift_grat$end < 20000])] = 1
int[min(drift_grat$start[drift_grat$start < 20000]):max(drift_grat$end[drift_grat$end < 20000])] = 11

g[min(drift_grat$start[(drift_grat$start > 20000) & 
                         (drift_grat$start < 80000)]):max(drift_grat$end[(drift_grat$end > 20000) & 
                                                                           (drift_grat$end < 80000)])] = 1
int[min(drift_grat$start[(drift_grat$start > 20000) & 
                           (drift_grat$start < 80000)]):max(drift_grat$end[(drift_grat$end > 20000) & 
                                                                             (drift_grat$end < 80000)])] = 12
g[min(drift_grat$start[drift_grat$start > 80000]):max(drift_grat$end[drift_grat$end > 80000])] = 1
int[min(drift_grat$start[drift_grat$start > 80000]):max(drift_grat$end[drift_grat$end > 80000])] = 13

nat_movie_one <- read.csv("/home/laura/Documents/Dottorato/3.11 Conferenze/1_ SIS2021/Analisi/Data/SISnatural_movie_one2.csv", header = TRUE)
g[min(nat_movie_one$start):max(nat_movie_one$end)] = 2
int[min(nat_movie_one$start):max(nat_movie_one$end)] = 2

nat_movie_three <- read.csv("/home/laura/Documents/Dottorato/3.11 Conferenze/1_ SIS2021/Analisi/Data/SISnatural_movie_three.csv", header = TRUE)
g[min(nat_movie_three$start[nat_movie_three$start < 40000]):max(nat_movie_three$end[nat_movie_three$start < 40000])] = 3
int[min(nat_movie_three$start[nat_movie_three$start < 40000]):max(nat_movie_three$end[nat_movie_three$start < 40000])] = 31

g[min(nat_movie_three$start[nat_movie_three$start > 40000]):max(nat_movie_three$end[nat_movie_three$start > 40000])] = 3
int[min(nat_movie_three$start[nat_movie_three$start > 40000]):max(nat_movie_three$end[nat_movie_three$start > 40000])] = 32

rm(list=c("drift_grat","nat_movie_three","nat_movie_one"))


#------ plot fluorescence trace ------#

df = data.frame(x = 1:n, y = y_real, g = g, interval = int)

df_rect = data.frame(start = sapply(unique(df$interval)[-1], function(x) min(df$x[df$interval==x])),
                     end = sapply(unique(df$interval)[-1], function(x) max(df$x[df$interval==x])),
                     Stimulus = as.factor(c("Drifting grating","Natural movie 2","Natural movie",
                                            "Drifting grating","Natural movie 2", "Drifting grating")) )


df$x = df$x/30
df_rect$start = df_rect$start/30
df_rect$end = df_rect$end/30

# cols = c("#ffa500", "#91ff00", "#007600")
cols = c("hotpink","#91ff00", "blue")

plot3 = ggplot(data = df) +
  geom_rect(data = df_rect, inherit.aes = FALSE,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf, fill = Stimulus), alpha = 0.12 ) +
  scale_fill_manual(values = cols) +
  geom_line(aes(x = x, y = y)) +
  theme_bw() +
  theme(legend.position = "bottom",
        rect = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous(name = "Time (seconds)") +
  scale_y_continuous(name = "Calcium level") 
# geom_line(aes(x = x, y = AA), col = "gold")

pdf("neuron2_trace.pdf", width=8, height=3.5)
print(plot3)     
dev.off() 



load("/home/laura/Documents/Dottorato/3.11 Conferenze/1_ SIS2021/Analisi/estspikes_SIS_exp2_cell517444286.Rdata")
times = which(est_spikes>0)
#----------------# plot data + activity #----------------# 
df$AA = est_spikes


plot4 = ggplot(data = df) +
  geom_rect(data = df_rect, inherit.aes = FALSE,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf, fill = Stimulus), alpha = 0.12 ) +
  scale_fill_manual(values = cols) +
  geom_line(aes(x = x, y = y)) +
  theme_bw() +
  theme(legend.position = "bottom",
        rect = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous(name = "Time (seconds)") +
  scale_y_continuous(name = "Calcium level") +
geom_line(aes(x = x, y = AA), col = "gold")

pdf("neuron2_trace+estimate.pdf", width=8, height=3.5)
print(plot4)     
dev.off() 
