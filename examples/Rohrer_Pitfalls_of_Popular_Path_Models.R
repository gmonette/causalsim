# From: Rohrer et al., That’s a lot to Process! Pitfalls of Popular Path Models
# https://psyarxiv.com/paeb7/

library(dagitty)

################################################################
### DEFINE PROCESS MODELS
################################################################
# Models according to appendix A in Hayes (2018, ed. 2)

tmp <- vector(mode="list", length=58)

m_1 <- dagitty("dag{
               Y <- X ;
               Y <- W }")
tmp[[1]] <- impliedConditionalIndependencies(m_1)

m_2 <- dagitty("dag{
               Y <- X ;
               Y <- W ;
               Y <- Z }")
tmp[[2]] <- impliedConditionalIndependencies(m_2)

m_3 <- dagitty("dag{
               Y <- X ;
               Y <- W ;
               Y <- Z }")
tmp[[3]] <- impliedConditionalIndependencies(m_3)

m_4 <- dagitty("dag{
               Y <- X ;
               M <- X ;
               Y <- M }")
(tmp[[4]] <- impliedConditionalIndependencies(m_4))

m_5 <- dagitty("dag{
               Y <- X ;
               M <- X ;
               Y <- M ;
               Y <- W }")
(tmp[[5]] <- impliedConditionalIndependencies(m_5))

m_6 <- dagitty("dag{
               Y <- X ;
               M2 <- X ;
               M1 <- X ;
               M2 <- M1 ;
               Y <- M2 ;
               Y <- M1 }")
tmp[[6]] <- impliedConditionalIndependencies(m_6)

m_6_1 <- dagitty("dag{
               Y <- X ;
               M2 <- X ;
               M1 <- X ;
               M2 <- M1 ;
               Y <- M2 ;
               Y <- M1 }")
tmp[[7]] <- impliedConditionalIndependencies(m_6_1)

m_6_2 <- dagitty("dag{
                 Y <- X ;
                 M1 <- X ;
                 M2 <- X ;
                 M3 <- X ;
                 M2 <- M1 ;
                 M3 <- M2 ;
                 Y <- M1 ;
                 Y <- M2 ;
                 Y <- M3 }")
tmp[[8]] <- impliedConditionalIndependencies(m_6_2)

m_6_3 <- dagitty("dag{
                 Y <- X ;
                 M1 <- X ;
                 M2 <- X ;
                 M3 <- X ;
                 M4 <- X ;
                 M2 <- M1 ;
                 M3 <- M2 ;
                 M4 <- M3 ;
                 Y <- M1 ;
                 Y <- M2 ;
                 Y <- M3
                 Y <- M4 }")
tmp[[9]] <- impliedConditionalIndependencies(m_6_3)

m_7 <- dagitty("dag{
               Y <- X ;
               M <- X ;
               Y <- M ;
               M <- W }")
tmp[[10]] <- impliedConditionalIndependencies(m_7)

m_8 <- dagitty("dag{
               Y <- X ;
               M <- X ;
               Y <- M ;
               M <- W ;
               Y <- W }")
tmp[[11]] <- impliedConditionalIndependencies(m_8)

m_9 <- dagitty("dag{
               Y <- X ;
               M <- X ;
               Y <- M ;
               M <- W ;
               M <- Z }")
tmp[[12]] <- impliedConditionalIndependencies(m_9)

m_10 <- dagitty("dag{
               Y <- X ;
               M <- X ;
               Y <- M ;
               M <- W ;
               Y <- W ;
               M <- Z ;
               Y <- Z }")
tmp[[13]] <- impliedConditionalIndependencies(m_10)

m_11 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                M <- Z }")
tmp[[14]] <- impliedConditionalIndependencies(m_11)

m_12 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[15]] <- impliedConditionalIndependencies(m_12)

m_13 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z }")
tmp[[16]] <- impliedConditionalIndependencies(m_13)

m_14 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W }")
tmp[[17]] <- impliedConditionalIndependencies(m_14)

m_15 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W }")
tmp[[18]] <- impliedConditionalIndependencies(m_15)

m_16 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W ;
                Y <- Z}")
tmp[[19]] <- impliedConditionalIndependencies(m_16)

m_17 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W ;
                Y <- Z}")
tmp[[20]] <- impliedConditionalIndependencies(m_17)

m_18 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W ;
                Y <- Z}")
tmp[[21]] <- impliedConditionalIndependencies(m_18)

m_19 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W ;
                Y <- Z}")
tmp[[22]] <- impliedConditionalIndependencies(m_19)

m_20 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W ;
                Y <- Z}")
tmp[[23]] <- impliedConditionalIndependencies(m_20)

m_21 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- Z}")
tmp[[24]] <- impliedConditionalIndependencies(m_21)

m_22 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                Y <- Z}")
tmp[[25]] <- impliedConditionalIndependencies(m_22)

m_28 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- Z}")
tmp[[25]] <- impliedConditionalIndependencies(m_28)

m_29 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                Y <- Z}")
tmp[[26]] <- impliedConditionalIndependencies(m_29)

m_58 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W }")
tmp[[27]] <- impliedConditionalIndependencies(m_58)

m_59 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W }")
tmp[[28]] <- impliedConditionalIndependencies(m_59)

m_60 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z }")
tmp[[29]] <- impliedConditionalIndependencies(m_60)

m_61 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z }")
tmp[[30]] <- impliedConditionalIndependencies(m_61)

m_62 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[31]] <- impliedConditionalIndependencies(m_62)

m_63 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[32]] <- impliedConditionalIndependencies(m_63)

m_64 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z }")
tmp[[33]] <- impliedConditionalIndependencies(m_64)

m_65 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z }")
tmp[[34]] <- impliedConditionalIndependencies(m_65)

m_66 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[35]] <- impliedConditionalIndependencies(m_66)

m_67 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[36]] <- impliedConditionalIndependencies(m_67)

m_68 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z }")
tmp[[37]] <- impliedConditionalIndependencies(m_68)

m_69 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[38]] <- impliedConditionalIndependencies(m_69)

m_70 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                Y <- Z }")
tmp[[39]] <- impliedConditionalIndependencies(m_70)

m_71 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                Y <- Z }")
tmp[[40]] <- impliedConditionalIndependencies(m_71)

m_72 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[41]] <- impliedConditionalIndependencies(m_72)

m_73 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[42]] <- impliedConditionalIndependencies(m_73)

m_74 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                Y <- W }")
tmp[[43]] <- impliedConditionalIndependencies(m_74)

m_75 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[44]] <- impliedConditionalIndependencies(m_75)

m_76 <- dagitty("dag{
                Y <- X ;
                M <- X ;
                Y <- M ;
                M <- W ;
                Y <- W ;
                M <- Z ;
                Y <- Z }")
tmp[[45]] <- impliedConditionalIndependencies(m_76)

m_80 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Mk <- X ;
                Mk1 <- X ;
                Y <- M1 ;
                Mk <- M1 ;
                Y <- Mk ;
                Mk <- Mk1 ;
                Y <- Mk1 ;
                M1 -- Mk1 }")
tmp[[46]] <- impliedConditionalIndependencies(m_80)

m_81 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                M2 <- X ;
                Mk <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Mk <- M1 ;
                Y <- M2 ;
                Y <- Mk ;
                M2 -- Mk }")
tmp[[47]] <- impliedConditionalIndependencies(m_81)

m_82 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                M2 <- X ;
                M3 <- X ;
                M4 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                Y <- M3 ;
                M4 <- M3 ;
                Y <- M4 }")
tmp[[48]] <- impliedConditionalIndependencies(m_82)

m_83 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                M1 <- W }")
tmp[[49]] <- impliedConditionalIndependencies(m_83)

m_84 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                M1 <- W ;
                M2 <- W }")
tmp[[50]] <- impliedConditionalIndependencies(m_84)

m_85 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                M1 <- W ;
                M2 <- W ;
                Y <- W }")
tmp[[51]] <- impliedConditionalIndependencies(m_85)

m_86 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                M1 <- W ;
                Y <- W }")
tmp[[52]] <- impliedConditionalIndependencies(m_86)

m_87 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                Y <- W }")
tmp[[53]] <- impliedConditionalIndependencies(m_87)

m_88 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                Y <- W }")
tmp[[54]] <- impliedConditionalIndependencies(m_88)

m_89 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                Y <- W }")
tmp[[55]] <- impliedConditionalIndependencies(m_89)

m_90 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                Y <- W }")
tmp[[56]] <- impliedConditionalIndependencies(m_90)

m_91 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                M2 <- W }")
tmp[[57]] <- impliedConditionalIndependencies(m_91)

m_92 <- dagitty("dag{
                Y <- X ;
                M1 <- X ;
                Y <- M1 ;
                M2 <- M1 ;
                Y <- M2 ;
                Y <- W ;
                M1 <- W ;
                M2 <- W }")
tmp[[58]] <- impliedConditionalIndependencies(m_92)

################################################################
### CHECK OVERLAP IN MODELS 
################################################################
# How many models in PROCESS overlap with other models in terms 
# of their testable implications?

overlap <- vector(mode="logical", length=58)

for (i in 1:58) {
  ### Form intersection of element i with rest of list, if non-empty this 
  ### means that element i is overlapping with another and thus PROCESS
  ### model i has a twin with exactly the same testable implications
  overlap[i] <- (length(intersect(tmp[i], tmp[-i])) > 0)
}

summary(overlap)
mean(overlap)
