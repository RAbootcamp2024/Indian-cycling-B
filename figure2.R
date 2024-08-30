#load packages

install.packages("pacman")
pacman::p_load(tidyverse, haven, gridExtra)


#read data

lest <- read_dta("C:/Users/Owner/Indian-cycling-B/replication data/113676-V1/ddd_long.dta")

#create variables

dd10<-lest$dd10
dd10.l<-lest$ci_low10
dd10.h<-lest$ci_high10
dd20<-lest$dd20
dd20.l<-lest$ci_low20
dd20.h<-lest$ci_high20
diff<-lest$diff
diff.l<-lest$ci_low30
diff.h<-lest$ci_high30
tempdist<-lest$longdistgroup

# plot figures

ggplot(data, aes(x = tempdist, y = dd10)) +
  # Add the shaded area
  geom_ribbon(aes(ymin = dd10.l, ymax = dd10.h), fill = "lightblue1", alpha = 0.5) +
  # Add the line plot
  geom_line() +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Set axis limits
  coord_cartesian(ylim = c(-0.1, 0.3)) +
  # Add labels and title
  labs(
    x = "Distance to Secondary School (KM)",
    y = "Double Difference\nChange in Girls' Enrollment -\nChange in Boys' Enrollment",
    title = expression(underline("Panel A: Bihar Double Difference by Distance to Secondary School"))
  ) +
  # Customize theme
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# new

plot_a <- ggplot(data, aes(x = tempdist, y = dd10)) +
  geom_ribbon(aes(ymin = dd10.l, ymax = dd10.h), fill = "lightblue1", alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = c(-0.1, 0.3)) +
  labs(
    x = "Distance to Secondary School (KM)",
    y = "Double Difference\nChange in Girls' Enrollment -\nChange in Boys' Enrollment",
    title = expression(underline("Panel A: Bihar Double Difference by Distance to Secondary School"))
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )

plot_b <- ggplot(data, aes(x = tempdist, y = dd20)) +
  geom_ribbon(aes(ymin = dd20.l, ymax = dd20.h), fill = "lightblue1", alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = c(-0.1, 0.2)) +
  labs(
    x = "Distance to Secondary School (KM)",
    y = "Double Difference\nChange in Girls' Enrollment -\nChange in Boys' Enrollment",
    title = expression(underline("Panel B: Jharkhand Double Difference by Distance to Secondary School"))
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )

plot_c <- ggplot(data, aes(x = tempdist, y = diff)) +
  geom_ribbon(aes(ymin = diff.l, ymax = diff.h), fill = "lightblue1", alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = c(-0.25, 0.25)) +
  labs(
    x = "Distance to Secondary School (KM)",
    y = "Triple Difference\nDouble Difference in Bihar -\nDouble Difference in Jharkhand",
    title = expression(underline("Panel C: Triple Difference by Distance to Secondary School"))
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )

gridExtra::grid.arrange(plot_a,plot_b,plot_c, ncol=1)

#nonpara check

data_non <- read_dta("C:/Users/Owner/Indian-cycling-B/replication data/113676-V1/data-for-fig-2.dta")
