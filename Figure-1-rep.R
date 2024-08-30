#パッケージの読み込み
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra,grid,foreign)

setwd("C:/Users/Owner/Indian-cycling-B")
dist.data<-read.dta("C:/Users/Owner/Indian-cycling-B/replication data/113676-V1/schooldist.dta")
dist.india<-dist.data$highschool_india
dist.india.male<-dist.data$highschool_india_male
dist.india.female<-dist.data$highschool_india_female
dist.bihar<-dist.data$highschool_bihar
dist.bihar.male<-dist.data$highschool_bihar_male
dist.bihar.female<-dist.data$highschool_bihar_female
dist<-dist.data$secondarydist


age.data<-read.dta("C:/Users/Owner/Indian-cycling-B/replication data/113676-V1/schoolage.dta")
age.india<-age.data$inschool_india
age.india.male<-age.data$inschool_india_male
age.india.female<-age.data$inschool_india_female
age.bihar<-age.data$inschool_bihar
age.bihar.male<-age.data$inschool_bihar_male
age.bihar.female<-age.data$inschool_bihar_female
age<-age.data$age


# to percentage
age.data <- data.frame(
  age = age,
  India = 100 * age.india,
  India_male = 100 * age.india.male,
  India_female = 100 * age.india.female,
  Bihar = 100 * age.bihar,
  Bihar_male = 100 * age.bihar.male,
  Bihar_female = 100 * age.bihar.female
)

dist.data <- data.frame(
  dist = dist,
  India = 100 * dist.india,
  India_male = 100 * dist.india.male,
  India_female = 100 * dist.india.female,
  Bihar = 100 * dist.bihar,
  Bihar_male = 100 * dist.bihar.male,
  Bihar_female = 100 * dist.bihar.female
)

# Create the plots
p1 <- ggplot(age.data, aes(x = age)) +
  geom_line(aes(y = India, color = "All"), size = 1) +
  geom_line(aes(y = India_male, color = "Male"), linetype = "dashed", size = 1) +
  geom_line(aes(y = India_female, color = "Female"), linetype = "dotted", size = 1) +
  scale_color_manual(values = c("All" = "black", "Male" = "blue", "Female" = "red")) +
  ylim(40, 100) +
  labs(x = "Age (Years)", y = "Percent", title = "India") +
  theme_classic() +
  theme(aspect.ratio=1.5, 
        legend.position = c(0.4, 0.25),
        legend.text=element_text(size=6),
        legend.key.size=unit(0.8,"lines")) +
  guides(color=guide_legend(title=NULL),linetype=guide_legend(title=NULL))

p2 <- ggplot(age.data, aes(x = age)) +
  geom_line(aes(y = Bihar, color = "All"), size = 1) +
  geom_line(aes(y = Bihar_male, color = "Male"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Bihar_female, color = "Female"), linetype = "dotted", size = 1) +
  scale_color_manual(values = c("All" = "black", "Male" = "blue", "Female" = "red")) +
  ylim(40, 100) +
  labs(x = "Age (Years)", y = "Percent", title = "Bihar") +
  theme_classic() +
  theme(aspect.ratio=1.5) +
  theme(legend.position = "none")

p3 <- ggplot(dist.data, aes(x = dist)) +
  geom_line(aes(y = India, color = "All"), size = 1) +
  geom_line(aes(y = India_male, color = "Male"), linetype = "dashed", size = 1) +
  geom_line(aes(y = India_female, color = "Female"), linetype = "dotted", size = 1) +
  scale_color_manual(values = c("All" = "black", "Male" = "blue", "Female" = "red")) +
  ylim(10, 70) +
  labs(x = "Distance to Secondary School (KM)", y = "Percent", title = "India") +
  theme_classic() +
  theme(aspect.ratio=1.5, 
        legend.position = c(0.4, 0.25),
        legend.text=element_text(size=6),
        legend.key.size=unit(0.8,"lines"))+
  guides(color=guide_legend(title=NULL),linetype=guide_legend(title=NULL))

p4 <- ggplot(dist.data, aes(x = dist)) +
  geom_line(aes(y = Bihar, color = "All"), size = 1) +
  geom_line(aes(y = Bihar_male, color = "Male"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Bihar_female, color = "Female"), linetype = "dotted", size = 1) +
  scale_color_manual(values = c("All" = "black", "Male" = "blue", "Female" = "red")) +
  ylim(10, 70) +
  labs(x = "Distance to Secondary School (KM)", y = "Percent", title = "Bihar") +
  theme_classic() +
  theme(aspect.ratio=1.5) +
  theme(legend.position = "none")

# Arrange the plots in a 2x2 grid
p5 <- grid.arrange(
  p1, p2,
  ncol = 2,
  top = "Panel A: Enrollment in School by Age and Gender"
  )
p6<- grid.arrange(
  p3, p4,
  ncol = 2,
  top = "Panel B: 16 and 17 Year Olds Enrolled in OR Completed Grade 9 by Distance and Gender"
)

p7<- grid.arrange(
  p5, p6,
  ncol = 1
)



