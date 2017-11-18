#homework 1

#1
#Define blocks
women_younger = c("F18", "F19", "F21", "F18")
women_older = c( "F41", "F39",  "F44", "F38", "F38")
men_older = c("M54", "M58", "M74", "M62", "M51")
men_younger = c("M18", "M31", "M35", "M34", "M38")

#Sample from block
wy_exp = women_younger
(wo_exp = sample(women_older,4, replace=FALSE))
(my_exp = sample(men_younger,4, replace=FALSE))
(mo_exp = sample(men_older,4, replace=FALSE))

#Assign to treatment
(wy_1 = sample(wy_exp,2, replace=FALSE))
(wo_1 = sample(wo_exp,2, replace=FALSE))
(my_1 = sample(my_exp,2, replace=FALSE))
(mo_1 = sample(mo_exp,2, replace=FALSE))

#4
choose(24,6)*choose(18,6)*choose(12,6)*choose(6,6)

choose(24,6)*choose(18,5)*choose(13,7)*choose(6,6)