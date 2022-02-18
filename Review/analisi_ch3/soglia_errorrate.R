len=1000
seqq = seq(0.05, 0.95, length.out = len)

errseq = matrix(NA, 50, len)

for(nsim in 1:50)
{
  filename = paste0("~/Documents/Dottorato/2.06 Calcium imaging/calcium_imaging_recap/Simulazioni_fCAM/Scen1/data/data_scen1_seed", 
                    nsim, ".Rdata")
  load(file = filename)
  s = out$s
  
  filename = paste0("/home/laura/Documents/Dottorato/2.06 Calcium imaging/calcium_imaging_recap/Simulazioni_fCAM/Scen1/gMFM_par8/scen1_run_gMFM_gammapar8_sim", 
                    nsim, ".Rdata")
  load(file = filename)
     
  spike_prob = apply(run_gMFM$clusterO, 1, function(x) mean(x>0) )
  rm(run_gMFM)
  rm(out)
  
  for(th in 1:length(seqq)) {
    spike_yes = (spike_prob > seqq[th])*1
    errseq[nsim, th] = sum(spike_yes != s)/length(s)
  }
  
}
str(outf)
                   



df = data.frame(th = rep(seqq, 50),
                id = sort(rep(1:50, length(seqq))),
                value = c(t(errseq)) )
df$id = as.factor(df$id)

library(ggplot2)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
  
cols = gg_color_hue(4)[3]


ggplot() +
  geom_line(data = df, aes(x=th, y=value, group=id), lwd = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom",
        rect = element_rect(fill="transparent", colour=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "Threshold") +
  scale_y_continuous(name = "Error rate") +
  geom_line(data = data.frame(t = seqq, yy = apply(errseq, 2, median)), aes(x = t, y = yy), col = cols, lwd = 1) +
  ggtitle("Scenario 1")

