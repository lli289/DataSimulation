library(readxl)
library(dplyr)
library(ggplot2)

Students <- read_excel("Students.xlsx")
colnames(Students)
students_cnt <- Students %>%
  count(Semester, Gender)
students_cnt

ggplot(students_cnt,
       aes(x = Semester,
           y = n,
           fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("#a1a9d0", "#f0988c"),
    name   = "Gender"
  )+
  labs(
    x        = "Semester",
    y        = "Number of Students",
    fill     = "Gender"
  ) +
  theme_classic(base_size = 25)

Level_cnt <- Students %>%
  count(Semester, Level = `Class Level`)

Level_cnt

ggplot(Level_cnt,
       aes(x    = Semester,
           y    = n,
           fill = Level)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("#80cba4", "#f36f43", "#e9f4a3", "#fbda83"),
    name   = "Class Level"
  ) +
  labs(
    x     = "Semester",
    y     = "Number of Students"
  ) +
  theme_classic(base_size = 25)

