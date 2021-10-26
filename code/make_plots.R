# load required packages
require(ggplot2)
require(gridExtra)
require(cowplot)
require(latex2exp)
require(lemon)

# read in raw data
fig_2a_data <- read.csv(file.path("data", "Fig2a_data.csv"))
fig_2b_data <- read.csv(file.path("data", "Fig2b_data.csv"))
fig_2c_data <- read.csv(file.path("data", "Fig2c_data.csv"))
fig_2d_data <- read.csv(file.path("data", "Fig2d_data.csv"))
fig_3_data  <- read.csv(file.path("data", "Fig3_data.csv"))

# Figure 2a plot
fig_2a_data$effort_level <- factor(fig_2a_data$effort_level)
fig_2a_plot <- ggplot(data = fig_2a_data, mapping = aes(x=effort_level, y=proportion_hr, group=effort_level, fill=effort_level)) +
  geom_boxplot(fill=c("#EFF3FE", "#C4DBEF", "#97CADA", "#60AED5", "#1383B9", "#005299"), size=1) +
  geom_hline(yintercept = 0.5, colour="grey60", linetype="dashed") +
  scale_y_continuous(name = "Pr(HR)", limits = c(0,1)) +
  scale_x_discrete(name = "Effort Level") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size=16),
    axis.ticks = element_line(size=1, colour="black"),
    axis.text = element_text(size=13, colour="black"),
    plot.margin = unit(c(1,0.5,0.5,0.5), units="cm"),
    legend.pos = "none"
  )

fig_2a_plot <- ggdraw() + 
  draw_plot(fig_2a_plot) + 
  draw_label("(a)", color="black", size=16, x=0, y=0.95, vjust=0, hjust=0)

# Figure 2b
fig_2b_data$reward_level <- factor(fig_2b_data$reward_level)
fig_2b_plot <- ggplot(data = fig_2b_data, mapping = aes(x=reward_level, y=proportion_hr, group=reward_level, fill=reward_level)) +
  geom_boxplot(fill=c("#ECF8E9", "#B4E3B8", "#64C37C", "#00A25C", "#006B34"), size=1) +
  geom_hline(yintercept = 0.5, colour="grey60", linetype="dashed") +
  scale_y_continuous(name = "Pr(HR)", limits = c(0,1)) +
  scale_x_discrete(name = "Reward") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size=16),
    axis.ticks = element_line(size=1, colour="black"),
    axis.text = element_text(size=13, colour="black"),
    plot.margin = unit(c(1,0.5,0.5,0.5), units="cm"),
    legend.pos = "none"
  )
fig_2b_plot <- ggdraw() + 
  draw_plot(fig_2b_plot) + 
  draw_label("(b)", color="black", size=16, x=0, y=0.95, vjust=0, hjust=0)

# Figure 2c
fig_2c_data$effort_level <- factor(fig_2c_data$effort_level)
fig_2c_plot <- ggplot(data = fig_2c_data, mapping = aes(x=effort_level, y=proportion_hr, group=effort_level, fill=effort_level)) +
  geom_boxplot(fill=c("#EFF3FE", "#C4DBEF", "#97CADA", "#60AED5", "#1383B9", "#005299"), size=1) +
  geom_hline(yintercept = 0.5, colour="grey60", linetype="dashed") +
  scale_y_continuous(name = "Pr(Info)", limits=c(0,1), breaks = seq(from=0, to=1, length.out=5)) +
  scale_x_discrete(name = "Effort Level") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size=16),
    axis.ticks = element_line(size=1, colour="black"),
    axis.text = element_text(size=13, colour="black"),
    plot.margin = unit(c(1,0.5,0.5,0.5), units="cm"),
    legend.pos = "none"
  )
fig_2c_plot <- ggdraw() + 
  draw_plot(fig_2c_plot) + 
  draw_label("(c)", color="black", size=16, x=0, y=0.95, vjust=0, hjust=0)

# Figure 2d
nvise_by_pwin <- as.data.frame.table(by(fig_2d_data$pr_info, INDICES=list(fig_2d_data$prior_pr_win), FUN=mean, na.rm=T))
names(nvise_by_pwin) <- c("p_win", "pr_info")
nvise_by_pwin$pr_info_se <- as.vector(by(fig_2d_data$pr_info, INDICES=list(fig_2d_data$prior_pr_win), FUN=function(x){sd(x, na.rm=T)/sqrt(sum(!is.na(x)))}))
nvise_by_pwin$p_win <- as.numeric(as.character(nvise_by_pwin$p_win))

fig_2d_plot <- ggplot(data = nvise_by_pwin, mapping = aes(x=p_win, y=pr_info)) +
  geom_hline(yintercept = 0.5, colour="grey60", linetype="dashed") +
  geom_errorbar(aes(ymin = pr_info - pr_info_se, ymax = pr_info + pr_info_se), width=0.02, size=0.5) +
  geom_point(colour="blue", size=2, shape=17) +
  scale_y_continuous(name = "Pr(Info)", limits=c(0,1), breaks = seq(from=0, to=1, length.out=5)) +
  scale_x_continuous(name = "Prior Pr(win)", breaks=seq(from=0, to=1, length.out=5), labels = c("0", "0.25", "0.5", "0.75", "1")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size=16),
    axis.ticks = element_line(size=1, colour="black"),
    axis.text = element_text(size=13, colour="black"),
    plot.margin = unit(c(1,0.5,0.5,0.5), units="cm"),
    legend.pos = "none"
  ) +
  NULL
fig_2d_plot <- ggdraw() + 
  draw_plot(fig_2d_plot) + 
  draw_label("(d)", color="black", size=16, x=0, y=0.95, vjust=0, hjust=0)

# Figure 2 - combine subplots
fig_2_plot <- grid.arrange(fig_2a_plot, fig_2b_plot, fig_2c_plot, fig_2d_plot,nrow=2)

# Figure 3
fig_3_plot <- ggplot(data=fig_3_data, mapping=aes(x=k_w, y=k_i)) + 
  geom_smooth(method="lm", fullrange=T) +
  geom_point(size=2) +
  scale_x_continuous(limits=c(-0.4,2.5), breaks=c(0,1,2), name=TeX('\\textit{k}_\\textit{w}')) +
  scale_y_continuous(limits=c(0, 10), breaks=c(0,3,6,9), name=TeX('\\textit{k}_\\textit{i}')) +
  theme_bw() + 
  theme(legend.position = "right",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, angle=0, vjust=0.5, face="bold"),
        axis.text = element_text(size=12, colour="black")
  ) +
  coord_capped_cart(left="both", bottom="both")
