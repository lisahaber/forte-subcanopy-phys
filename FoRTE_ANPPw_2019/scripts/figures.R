# This script uses ggplot2 to create all the plots for the Grigri et al. 2020 
# manuscript. Respictive script must be run before plotting their data (i.e. cohort.R
# must be run in order to graph in the cohort data)

# forte colors for severity
forte <- c("#000000", "#009E73", "#0072B2", "#D55E00")

# create text grobs for months to then annotate them onto our figures. These are for
# the time series figures 
april <- textGrob("April", gp=gpar(fontsize=22))
may <- textGrob("May", gp=gpar(fontsize=22))
june <- textGrob("June", gp=gpar(fontsize=22))
july <- textGrob("July", gp=gpar(fontsize=22))
aug <- textGrob("Aug", gp=gpar(fontsize=22))
sept <- textGrob("Sept", gp=gpar(fontsize=22))
oct <- textGrob("Oct", gp=gpar(fontsize=22))
nov <- textGrob("Nov", gp=gpar(fontsize=22))

####################################################################################
## Disturbance severity time series; includes CANOPY only (Figure 3A)
sev_time <- ggplot(severity_time, aes(x=week, y=NPP, color = factor(severity))) +
  scale_colour_manual(values = forte, name = "Disturbance\nSeverity (%)", labels = 
                        c("0", "45", "65", "85")) +
  theme_bw() +
  theme(axis.text.x= element_blank(), axis.text.y= element_text(size=22), 
        axis.title.x = element_blank(), axis.title.y  = element_blank(), 
        legend.title = element_text("Disturbance\nSeverity", size = 22), legend.position = c(0.8, 0.6), 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(0.5,1,0,1, "cm")) +
  geom_point(size = 4) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,60),
                     sec.axis = sec_axis(~ .,labels = NULL, breaks = c(0, 10, 20, 30, 40, 50)))+
  geom_path(aes(group = factor(severity)), size = .9) +
  geom_errorbar(aes(ymin = NPP-SE, ymax = NPP+SE), width = 1.9, size = 0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-21")), linetype = 2, 
             color = "firebrick3", size = 1) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%j",
               limits = as.Date(c("2019-04-02", "2019-11-11")))+
  annotate(geom="text", x = as.Date("2019-06-02", "%Y-%m-%d"), y = 19, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-06-09", "%Y-%m-%d"), y = 33, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-06-30", "%Y-%m-%d"), y = 43, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-07-07", "%Y-%m-%d"), y = 42, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-07-14", "%Y-%m-%d"), y = 51, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-07-21", "%Y-%m-%d"), y = 41.5, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-07-28", "%Y-%m-%d"), y = 40, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-08-04", "%Y-%m-%d"), y = 28, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = as.Date("2019-08-11", "%Y-%m-%d"), y = 22, label = c("*"),
           color="black", size = 12)+
  annotate(geom="text", as.Date("2019-04-7", "%Y-%m-%d"), y = 55, 
           label = c("A"), color="black", size = 7.5) 

######## treatment time series (Figure 3B)
tx_time <- ggplot(treatment, aes(x = week, y = NPP, color = treatment)) +
  scale_colour_manual(values = c("#C4961A", "#293352"), name = "Disturbance\n  Type", labels = 
                        c("Bottom-Up", "Top-Down")) +
  geom_path(aes(group = treatment), size = .9) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = NPP-SE, ymax = NPP+SE), width = 1.9, size = 0.9) +
  theme_bw() +
  scale_x_date(date_breaks = "20 days", date_labels = "%j",
               limits = as.Date(c("2019-04-02", "2019-11-11")))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0,60), 
                     sec.axis = sec_axis(~ .,labels = NULL, breaks = c(0, 10, 20, 30, 40, 50)))+
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_blank(), axis.title.y  = element_blank(), 
        legend.title = element_text(size = 22), legend.position = c(0.8, 0.6), 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(-0.1,1,1.5,1, "cm")) +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-21")), linetype = 2, 
             color = "firebrick3", size = 0.9) +
  annotate(geom="text", x = as.Date("2019-11-09", "%Y-%m-%d"), y = 60, 
           label = c("n.s."),
           color="black", size = 10) +
  annotation_custom(april, xmin = as.Date("2019-04-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-04-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(may, xmin = as.Date("2019-05-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-05-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(june, xmin = as.Date("2019-06-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-06-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(july, xmin = as.Date("2019-07-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-07-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(aug, xmin = as.Date("2019-08-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-08-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(sept, xmin = as.Date("2019-09-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-09-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(oct, xmin = as.Date("2019-10-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-10-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  annotation_custom(nov, xmin = as.Date("2019-11-15", "%Y-%m-%d"),
                    xmax = as.Date("2019-11-15", "%Y-%m-%d"), ymin=-15,ymax=-15) +
  coord_cartesian(clip = "off")+
  annotate(geom="text", as.Date("2019-04-7", "%Y-%m-%d"), y = 55, 
           label = c("B"), color="black", size = 7.5) 

# creating a grob so I can paste in a common y-axis and join both of the above plots
g <- arrangeGrob(sev_time, tx_time, ncol=1, 
            left = grid::textGrob(expression(paste("Daily ANPP" [w], " ( ",kgC," ",ha^-1," ",day^-1,")")), 
                                  x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                                  rot=90, gp=gpar(fontsize = 30)))
grid.newpage()
grid.draw(g)

# save the plot to figures folder
ggsave(filename = "figures/sev_tx_time.jpeg", dpi = 400, height = 8, width = 10, units = "in", g)

##################################################################################
######## Cumulative NPP histogram of all canopy strata (Figure A1)
ggplot(annual_NPP_fig, aes(x = factor(severity), y = annual_NPP, fill = canopy_strata)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  theme_bw() +
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_text(size=30), axis.title.y  = element_text(size = 30), 
        legend.title = element_blank(), legend.position = c(0.2, 0.9), 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(1.5,1,1.5,1, "cm")) +
  scale_fill_manual(values = c("lightgreen", "darksalmon", "goldenrod1"), name = "", 
                    labels = c("Canopy", "Subcanopy", "Seedling/Sapling")) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000), 
                     sec.axis = sec_axis(~ .,labels = NULL, breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000))) +
  labs(x = "Disturbance Severity (%)",
       y = expression(paste("Annual ANPP" [w], " ( ",kgC," ",ha^-1," ",yr^-1,")"))) +
  geom_errorbar(aes(ymin = errorbars-SE, ymax = errorbars+SE), width = 0.1) +
  annotate(geom="text", x = "0", y = 1700, label = c("a"),
           color="black", size = 7.5) +
  annotate(geom="text", x = "45", y = 1700, label = c("a"),
           color="black", size = 7.5) +
  annotate(geom="text", x = "65", y = 1700, label = c("a"),
           color="black", size = 7.5) +
  annotate(geom="text", x = "85", y = 1700, label = c("a"),
           color="black", size = 7.5) 

ggsave("figures/cumulative_NPP.jpeg", height = 8, width = 11, units = "in", 
       dpi = 400 ,plot = last_plot())

####################################################################################
############### Live-Girdled Stacked Histogram of Annual ANPPw

# Figure 3A
sev <- ggplot(NPP_fate_mean, aes(x = factor(severity), y = NPP_mean, fill = fate)) +
  geom_bar(stat = "identity", position = "stack", width = 0.4) +
  geom_line(aes(x= factor(severity), y = annual_NPP), group = 0) +
  geom_point(aes(x = factor(severity), y = annual_NPP), size = 3, show.legend = FALSE) +
  theme_bw() +
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_text(size=30), axis.title.y  = element_blank(), 
        legend.title = element_blank(), legend.position = c(0.24, 0.87), 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(1.5,1,.5,1, "cm")) +
  scale_fill_manual(values = c("orange3","palegreen3"), labels = c("Girdled", "Healthy"))+
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000), 
                     sec.axis = sec_axis(~ .,labels = NULL, breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000))) +
  labs(x = "Disturbance Severity (%)",
       y = expression(paste("Annual ANPP" [w], " ( ",kgC," ",ha^-1," ",yr^-1,")"))) +
  geom_errorbar(aes(ymin = errorbars-SE, ymax = errorbars+SE), width = 0.1) +
  annotate(geom="text", x = 4.35, y = 3000, label = c("n.s."),
           color="black", size = 10) +
  annotate(geom="text", x = 0.6, y = 2700, 
           label = c("A"), color="black", size = 7.5) 

############## Treatment stacked histogram (Figure 3B)
tx <- ggplot(NPP_fate_tx) +
  geom_bar(aes(x = treatment, y = NPP_mean, fill = fate), stat = "identity", position = "stack", width = 0.4) +
  geom_bracket(xmin = "Bottom-Up",
               xmax = "Top-Down", y.position = 2900, label = "n.s.",
               label.size = 10, tip.length = c(0.09, .02), size = 0.6) +
  theme_bw() +
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_text(size=30), axis.title.y  = element_blank(), 
        legend.title = element_blank(), legend.position = "none", 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(0,1,2,1, "cm")) +
  scale_fill_manual(values = c("orange3","palegreen3"), labels = c("Girdled", "Ungirdled"))+
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000),limits = c(0,3000),
                     sec.axis = sec_axis(~ .,labels = NULL, breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000))) +
  labs(x = "Disturbance Type",
       y = "") +
  geom_errorbar(aes(x = treatment, y = NPP_mean, ymin = errorbars-SE, ymax = errorbars+SE), 
                width = 0.1) +
  annotate(geom="text", x = 0.51, y = 2700,
           label = c("B"), color="black", size = 7.5) + 
  annotate(geom="text", x=1.5, y=850, label = c("P=0.06"), color = "black",
           size = 10)

# creating a grob so I can paste in a common y-axis and join both of the above plots
g <- arrangeGrob(sev, tx, ncol=1, 
                 left = grid::textGrob(expression(paste("Annual ANPP" [w], " ( ",kgC," ",ha^-1," ",yr^-1,")")), 
                                       x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                                       rot=90, gp=gpar(fontsize = 30)))
grid.newpage()
grid.draw(g)

#save to figures folder
ggsave(filename = "figures/fate_tx_sev.jpeg", dpi = 400, height = 11, width = 8, units = "in", g)

##################################################################################
############## PFT early-late successional side by side figure (Figure 5)
ggplot(annual_PFT, aes(x = factor(severity), y = NPP_canopy, fill = PFT)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  theme_bw() +
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_text(size=30), axis.title.y  = element_text(size = 30), 
        legend.title = element_blank(), legend.position = c(0.3, 0.9), 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(1.5,1,1.5,1, "cm")) +
  scale_fill_manual(values = c("greenyellow","chartreuse4"), 
                    labels = c("Early Successional", "Middle/Late Succesional"))+
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000), 
                     sec.axis = sec_axis(~ .,labels = NULL, breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000))) +
  labs(x = "Disturbance Severity (%)",
       y = expression(paste("Annual ANPP" [w], " ( ",kgC," ",ha^-1," ",yr^-1,")"))) + 
  geom_errorbar(position = "dodge", aes(ymin = NPP_canopy-SE, ymax = NPP_canopy+SE), width = 0.4) +
  annotate(geom="text", x = "65", y = 2130, label = c("*"),
           color="black", size = 12) +
  annotate(geom="text", x = "0", y = 1650, label = c("n.s."),
           color="black", size = 7.5) +
  annotate(geom="text", x = "45", y = 1690, label = c("n.s."),
           color="black", size = 7.5) +
  annotate(geom="text", x = "85", y = 1800, label = c("n.s."),
           color="black", size = 7.5) 

ggsave(filename = "figures/PFT_NPP.jpeg", dpi = 400, height = 8, width = 11, units = "in",plot = last_plot())

##################################################################################
########LAI FIGures by severity and type (treatment)

# Figure 2A
LAI_sev <- ggplot(LAI_severity, aes(x = factor(severity), y = LAI_mean, fill = factor(severity))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  theme_bw() +
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_text(size=30), axis.title.y  = element_blank(), 
        legend.title = element_blank(), legend.position = c(0.5, 1.06), 
        legend.text = element_text(size = 22), legend.direction = "horizontal", 
        panel.grid = element_blank(), axis.line = element_line(size = 0.5), 
        plot.margin = margin(1.5,1,0.5,1, "cm")) +
  scale_y_continuous(limits = c(0,3.6), sec.axis = sec_axis(~ .,labels = NULL)) +
  # scale_x_discrete(position = "top") +
  # scale_x_discrete(position = "bottom")+
  scale_fill_manual(aesthetics = "fill", values = forte)+
  guides(fill = FALSE) +
  labs(x = "Disturbance Severity (%)",
       y = "LAI") +
  #theme(axis.title = element_text(size = 10)) +
  geom_errorbar(position = position_dodge(width = 0.4), 
                aes(ymin = LAI_mean-SE, ymax = LAI_mean+SE), width = 0.1, 
                color = "grey32", size = 1) +
  annotate(geom="text", x = 1, y = 3.5, label = c("a"), color="black", size = 10) +
  annotate(geom="text", x = 2, y = 3.6, label = c("a"), color="black", size = 10) +
  annotate(geom="text", x = 3, y = 3.15, label = c("ab"), color="black", size = 10) +
  annotate(geom="text", x = 4, y = 2.9, label = c("b"), color="black", size = 10) +
  annotate(geom="text", x = 0.6, y = 3.5, label = c("A"), color="black", size = 7.5) 

# and for disturbance type (Figure 2B)
LAI_tx <- ggplot(LAI_type, aes(x = factor(treatment), y = LAI_mean, fill = factor(treatment))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  theme_bw() +
  theme(axis.text.x= element_text(size = 22), axis.text.y= element_text(size=22), 
        axis.title.x = element_text(size=30), axis.title.y  = element_blank(), 
        legend.title = element_blank(), legend.position = "blank", 
        legend.text = element_text(size = 22), panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), plot.margin = margin(0,1,1.5,1, "cm")) +
  scale_y_continuous(limits = c(0,3.6), sec.axis = sec_axis(~ .,labels = NULL)) +
  scale_fill_manual(values = c("#C4961A", "#293352"), 
                    labels = c("DOY: 186-190", "DOY: 213-215"))+
  labs(x = "Disturbance Type",
       y = "LAI") +
  geom_errorbar(position = position_dodge(0.4), aes(ymin = LAI_mean-SE, ymax = LAI_mean+SE), 
                width = 0.1, color = "grey32", size = 1) +
  annotate(geom="text", x = 2.4, y = 3.5, label = c("n.s."),
           color="black", size = 10) +
  annotate(geom="text", x = 0.5, y = 3.5, label = c("B"), color="black", size = 7.5) 

# creating a grob so I can paste in a common y-axis and join both of the above plots
g <- arrangeGrob(LAI_sev, LAI_tx, ncol=1, 
                 left = grid::textGrob("LAI (dimensionless)", 
                                       x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                                       rot=90, gp=gpar(fontsize = 30)))
grid.newpage()
grid.draw(g)

# save to figures folder
ggsave(filename = "figures/LAI_tx_sev.jpeg", dpi = 400, height = 11, width = 9, units = "in", g)
