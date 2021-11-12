#' # Wageningen - June 2019
#' ## Prediction of Soil Properties
#' ## I Artificial Neural Network
#' ## II Partial Least Square Regression 
#' ## III Random Forest
#' ## IV Cubist
#' ### Yuri Gelsleichter
date()
#    ___   _  ___  __    ___  __   _______      ___  ____    _______  _____  ______________
#   / _ | / |/ / |/ /___/ _ \/ /  / __/ _ \____/ _ \/ __/___/ ___/ / / / _ )/  _//__/_  __/
#  / __ |/    /    /___/ ___/ /___\ \/ , _/___/ , _/ _//___/ /__/ /_/ / _  |/ / _\ \ / /   
# /_/ |_/_/|_/_/|_/   /_/  /____/___/_/|_|   /_/|_/_/      \___/\____/____/___//___//_/    
# figlet -f smslant ANN-PLSR-RF-CUBIST
# figlet -f smslant ANN-PLSR-RF-CB
# showfigfonts
# /*
### Save console output in external file and run silent console ### NOT WORK IN Rmd
### https://stackoverflow.com/questions/5571591/how-do-i-get-all-the-output-from-script-i-am-running-in-rstudio
### the code generating file console output:
# */
#' ### Auto setwd
library(rstudioapi); current_path <- getActiveDocumentContext()$path; setwd(dirname(current_path)) ### https://eranraviv.com/r-tips-and-tricks-working-directory/
getwd()
sink(file='sink') # open sink in getwd()

#############################################################################################################################
#' ### 1 General script adjusts, data mangement, data selection
# library(knitr)
# # Command to criate a folder called 'figure' and save all figures in it (used when convert to Rmarkdown) 
# knitr::opts_chunk$set(fig.path = 'figure/silk-')

# empty memory and workspace
gc(); rm(list=ls())

### Load data with correspnding spectrum ###
ld_spc <- read.csv("/home/yuri/MEGAsync/PNI_CHRIS_2019/20190414_PNI_data_management_adjust/data/dados_PNI_sc_spctr_data.csv", sep = ";", dec = ".", skip = 0)

### Observations
### indicação de linhas ld_spc (lab data + spectral)
### de 1 a 335: dados de solo + dados espectrais espectrorrdiômetro ASD
### de 336 a 359: dados Paula Soares, pode ser usado na validação de mapas
### de 360 a 376: Rock outcrops
### de 377 a 431: pontos do google que Elias utilizou para validação de classes de solo
### de 432 a 476: calibração com spectralon (white reference)
### de 477 a 479: calibração absoluta com spectralon da caixa de madeira
### de 480 a 482: espectros PNI lido em vazio (pni00164.asd sem amostra de solo), e pni00167.asd e pni00168.asd não foi feita analise quimica das amostra de solo
### Observação: Elias quando foi devolver (unificar) amostras PNI utilizadas por Yuri para a embalagem principal verificou a possibilidade de ter errado nas etiquetas do P77. Ficando então a sugestão: 		
### pni00216.asd	P77 	O1 ; pni00219.asd	P77 	O2 ; pni00220.asd	P77 	OB ; pni00217.asd	P77 	Bi1 ; pni00218.asd	P77 	Bi2
### sugestão minha, talvez remover o P77 das análises espectrais

### Rename some columns
# ld_spc[1:4, 1:45]
# names(ld_spc[, 1:45])
# names(ld_spc[, 1:23])
# colnames(ld_spc)[23] <- c("TC")

### Managing soil data and spectrum ### Selecting data
ld_spc[1:4, 1:45]
# names(ld_spc[1:45])
# names(ld_spc[1200:NCOL(ld_spc)])
### Phisical + Chemical data
# as.data.frame(names(ld_spc[1:45]))
# as.data.frame(ld_spc[1:2, c(1:7, 15:30, 41:42)])
# vis_nm <- as.data.frame(ld_spc[1:2, c(1:7, 15:30, 41:42)])
# as.data.frame(names(vis_nm))
# ld_spc <- ld_spc[1:335, c(1:7, 15:30, 41:NCOL(ld_spc))]
### these 4 lines were used only to check the sequence and comparing with the first database (Hungary 2017)
# ld_spc[, c(3, 16, 25)][order(ld_spc[, 16]), ] 
# ld_spc[1:190, c(3:5,16, 25)]
# ld_spc[190:NROW(ld_spc) , c(3:5,16, 25)]
# txtStop() # finaliza a anotação

### Chemical data only 
as.data.frame(names(ld_spc[1:45]))
as.data.frame(ld_spc[1:2, c(3:4, 15:27, 42:45)])
vis_nm <- as.data.frame(ld_spc[1:2, c(3:4, 15:27, 42)])
as.data.frame(names(vis_nm))

### Subset data
ld_spc <- ld_spc[1:335, c(3:4, 15:27, 42:NCOL(ld_spc))]
ld_spc[1:12, 1:16]

### Removing lines without most of data and spectrum ### http://www.datasciencemadesimple.com/drop-variables-columns-r-using-dplyr/
# ld_spc$X350
# ld_spc <- ld_spc[-c(3, 10, 24, 35, 39, 56, 67, 69, 143, 145, 208, 226, 261, 327), ]
# ld_spc <- ld_spc[-c(3, 10, 24, 35, 39, 56, 67, 69, 143, 145, 208, 226, 261, 327, 166), ]
# (262, 23 ,226 ,22 ,4 ,21 ,144 ,166 ,227 ,260 ,20 ,2 ,228 ,70 ,207 ,225 ,142 ,68) ### possible trouble samples (checked with first dataset Hungary 2017)

### out all in this list plus 166 (same sample dataset used in the Rio Congress) # Best results here
# ld_spc <- ld_spc[-c(3, 10, 24, 35, 39, 56, 67, 69, 143, 145,
#                     208, 226, 261, 327, 262, 23, 22, 4, 21, 144, 166,
#                     227, 260, 20, 2, 228, 70, 207, 225, 142, 68), ]
### out all in this list, but keep 166
ld_spc <- ld_spc[-c(3, 10, 24, 35, 39, 56, 67, 69, 143, 145,
                    208, 226, 261, 327, 262, 23, 22, 4, 21, 144,
                    227, 260, 20, 2, 228, 70, 207, 225, 142, 68), ]

    # asd <- c(3, 10, 24, 35, 39, 56, 67, 69, 143, 145, 208, 226, 261, 327, 262, 23 ,226 ,22 ,4 ,21 ,144 ,166 ,227 ,260 ,20 ,2 ,228 ,70 ,207 ,225 ,142 ,68)
    # sort(asd)
    # # [1]   2   3   4  10  20  21  22  23  24  35  39  56  67  68  69  70
    # [17] 142 143 144 145 166 207 208 225 226 226 227 228 260 261 262 327
    # 2,3,4 P2 O Ab Bi (Full)
    # 10 P4   O2
    # 20,21,22,23,24 P7 O A1 A2 A3 Cr (Full)
    # 35 P10 O2
    # 39 P12 O2
    # 56 P18   O2
    # 67 P22   O2
    # 68,69,70 P23   O1 O2 OC (FULL)
    # 142,143,144,145 P41   O1 O2 Or1 Or2 (FULL)
    # 166 P46   A2
    # 207,208 P59 O1 O2 (full)
    # 225,226,227,228 P63   O1 OC1 OC2 O3 (full)
    # 260,261,262 P72   O1  Ar bir (full)
    # 327 V1  Cr 
    # 
    # Possivelmente não remover (pois o perfil é completo) 
    # 166

# ld_spc[1:12, 1:16]
# table(is.na(ld_spc$X350))
### check
# table(is.na(ld_spc)) #460
# table(is.na(ld_spc$clay)) #118
# table(is.na(ld_spc$sand)) #118
# table(is.na(ld_spc$silt)) #118
# table(is.na(ld_spc$BD)) #118
# table(is.na(ld_spc$SM)) #118
# 118*3+53+53

library(dplyr) 
### Removing P77
ld_spc <- ld_spc %>% filter(id != "P77")
# write.csv(ld_spc, file = "../data/ld_spc.csv")
# ld_spc[100:280, 1:5]

### Look the index
# as.data.frame(names(vis_nm))
# {

#' ### 1.1 Selecting the soil properties
# sp_spc <- ld_spc[ , c( 3, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- "" # pH
# sp_spc <- ld_spc[ , c( 4, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- "(ppm)" # P
# sp_spc <- ld_spc[ , c( 5, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((cmol[c]~kg^-1)) # Na
# sp_spc <- ld_spc[ , c( 6, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((cmol[c]~kg^-1)) # K
# sp_spc <- ld_spc[ , c( 7, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((cmol[c]~kg^-1)) # Ca
# sp_spc <- ld_spc[ , c( 8, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((cmol[c]~kg^-1)) # Mg
# sp_spc <- ld_spc[ , c( 9, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((cmol[c]~kg^-1)) # Al
### sp_spc <- ld_spc[ , c(10, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((cmol[c]~kg^-1)) # H_AL
sp_spc <- ld_spc[ , c(11, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- "(%)" # C
# sp_spc <- ld_spc[ , c(12, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- "(%)" # H
# sp_spc <- ld_spc[ , c(13, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- "(%)" # N
### sp_spc <- ld_spc[ , c(14, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- "(%)" # SM
# sp_spc <- ld_spc[ , c(15, 16:NCOL(ld_spc))] ; sp_spc[1:4, 1:4] ; unit <- bquote((g~dm^-3)) # BD

### check completeness (NA)
# summary(complete.cases(sp_spc))
# check if incomplete observations
# sp_spc[complete.cases(sp_spc),1:3]
# sp_spc[!complete.cases(sp_spc),1:3]
# length(sp_spc[!complete.cases(sp_spc),1])
# exclude incomplete observations
sp_spc <- sp_spc[complete.cases(sp_spc),]

# pH = (unitless)
# P = Phosphorus (ppm) or (mg.dm-3)
# Na = Sodium (cmolc/kg) / (cmolc kg−1)
# K = Potassium (cmolc/kg)
# Ca = Calcium (cmolc/kg)
# Mg = Magnesium (cmolc/kg)
# Al = Aluminum (cmolc/kg)
# H_AL = Hydrogen + Aluminum (cmolc/kg)
# C = Soil Total Carbon (%) 
# H = Hydrogen (%)
# N = Nitrogen (%)
# SM = Soil Moisture (%)
# BD = Bulk density (g.dm-3) / (g dm−3)

### soil properties unites: test plot units
# unit <- ('cmolc kg-¹') ### not used
# unit <- ('cmolc kg−¹') ### not used 
# unit <- bquote(cmol[c]~kg^-1) ### no parentesis 
# unit <- "(%)" ### ok
# unit <- bquote((cmol[c]~kg^-1)) ### great! 
# plot(1,1, main=expression('title'[2]), ylab= bquote(paste("Observed", " ", .(names(subset(df_sc_cal_sp_spc, select = 1))), " ", .(unit))))
# plot(1,1, main=expression('title'[2]), ylab= bquote(paste("Observed", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))))

#' ### 1.2 ################################### pre treatment of spectrums -- start

#' ### 1.2.1 ###### eliminate high Correlation manually ######
# {
# sp <- sp_spc[1] ### this is the soil propertie (NOT USED)
# spc <- sp_spc[,2:NCOL(sp_spc)] ### These are the spectrum data
# res <- cor(spc)
# # round(res, 2)
# # library(corrplot)
# ### corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# # corrplot(res, method = "color", cl.pos = "n", tl.pos = "n", addgrid.col = NA)
# 
# # Heatmap
# # col<- colorRampPalette(c("blue", "white", "red"))(20)
# # heatmap(x = res, col = col, symm = TRUE)
# 
# ### library(devtools)
# ### install_github("jokergoo/ComplexHeatmap")
# 
# # library(ComplexHeatmap)
# # Heatmap(res, name = "corr", #title of legend
# #         column_title = "Variables", row_title = "Samples",
# #         row_names_gp = gpar(fontsize = 7)) # Text size for row names
# 
# library(caret)
# hc = findCorrelation(res, cutoff=0.999999) # gets high correlated 931
# # hc = findCorrelation(res, cutoff=0.99999) # gets high correlated 293
# ### hc = findCorrelation(res, cutoff=0.9999) # gets high correlated 50
# hc = sort(hc)
# ### reduced_Data = spc[, -c(hc)] ### give the high correlated
# reduced_Data = spc[, c(hc)] ### give the non high correlated
# dim(reduced_Data)
# 
# ### join soil propertie and spectra
# sp_spc_bind <- cbind(sp, reduced_Data)
# colnames(sp_spc_bind)[1] <- names(sp_spc[1]) ### restoring the propertie name
# sp_spc <- sp_spc_bind
# }

#' ### 1.2.2 ###### Continuum Removal ######
# {
# sp <- sp_spc[1] ### this is the soil propertie (NOT USED)
# spc <- sp_spc[,2:NCOL(sp_spc)] ### These are the spectrum data
# library(prospectr) ### install.packages("prospectr")
# # # ### plot of the 10-30 first Reflectance spectra
# # matplot(1:NCOL(spc), t(spc[1:10,]),type='l', ylim=c(0, .45), xlab='Wavelength (nm)', ylab='Reflectance')
# # png(file= "plot_spectra_30.png", height=12, width=24, units="cm", res=120)
# # matplot(1:NCOL(spc), t(spc[1:30,]),type='l', ylim=c(0, .7), xlab='Wavelength (nm)', ylab='Reflectance')
# # matlines(1:NCOL(spc), t(spc[1:30,]))
# # dev.off()
#   # matplot(wav,t(spc),type='l', ylim=c(0, .8), xlab='Wavelength (nm)', ylab='Reflectance')
#   # # NOT WORKED ### type = 'A' is used for absorbance spectra
#   # # NOT WORKED # a_spc = log(1/spc) # conversion to Absorbance
#   # # NOT WORKED # matplot(wav,t(a_spc[1:30,]),type='l', ylim=c(0, 4), xlab='Wavelength (nm)', ylab='Absorbance')
#   # # NOT WORKED # cr <- continuumRemoval(a_spc, wav, type='A')
#   # spc <- 1/10^spc # conversion to reflectance
# # ### type = 'R' is used for reflectance spectra
# # wav <- 350:2500 ### just index to be used in plot
# # cr <- continuumRemoval(spc, wav, type='R') ### old way
# cr <- continuumRemoval(spc, 1:NCOL(spc), type='R') ### new way
# # ### plot cr
# # png(file= "plot_Continuum_Removal_30.png", height=12, width=24, units="cm", res=120)
# # matplot(1:NCOL(cr), t(cr[1:30,]),type='l', ylim=c(0.42, 1.15), xlab='Wavelength (nm)', ylab='Reflectance (Continuum Removal)')
# # matlines(1:NCOL(cr), t(cr[1:30,]))
# # dev.off()
# # ### plot both spectra and continuum removal
# # png(file= "plot_Continuum_Removal_spectra_30.png", height=12, width=24, units="cm", res=120)
# # opar <- par(no.readonly = TRUE)
# # par(mfrow=c(2,1),mar=c(4,4,2,2))
# # matplot(1:NCOL(spc), t(spc[1:30,]),type='l',ylim=c(0,.7),xlab='Wavelength (nm)',ylab='Reflectance') # graphic from continum removal example
# # matlines(1:NCOL(spc), t(spc[1:30,]))
# # matplot(1:NCOL(cr), t(cr[1:30,]),type='l', ylim=c(0.42, 1.15), xlab='Wavelength (nm)', ylab='Reflectance (Continuum Removal)')
# # matlines(1:NCOL(cr), t(cr[1:30,]))
# # dev.off()
# # ### data mang
# # # class(cr); names(cr); colnames(cr); rownames(cr)
# df_cr <- as.data.frame(as.matrix(cr))
# # class(df_cr)
# names(df_cr) <- names(spc)
# # colnames(df_cr)
# # rownames(df_cr)
# 
# ### join soil propertie and spectra
# sp_spc_bind <- cbind(sp, df_cr)
# colnames(sp_spc_bind)[1] <- names(sp_spc[1]) ### restoring the propertie name
# sp_spc <- sp_spc_bind
# # sp_spc[1:4, 1:12]
# # sp_spc[1:2, 500:1200]
# 
# ### check the effet of cr
# ### with the soil propertie
# ##### sp_spc <- as.matrix(sp_spc)
# # matplot(1:ncol(sp_spc), t(sp_spc[1:20,]),type='l', ylim=c(0.42, 1.15), xlab='Wavelength (nm)', ylab='Reflectance (Continuum Removal)')
# ### without the soil propertie
# # matplot(2:ncol(sp_spc), t(sp_spc[1:20,2:NCOL(sp_spc)]),type='l', ylim=c(0.42, 1.15), xlab='Wavelength (nm)', ylab='Reflectance (Continuum Removal)')
# 
# ### see the result of X350 and X2500, became 1
# # matplot(1:20,t(cr[1:30,1:20]),type='l', ylim=c(0.42, 1.15), xlab='Wavelength (nm)', ylab='Reflectance (Continuum Removal)')
# # matplot(1:200,t(cr[1:30,1:200]),type='l', ylim=c(0.42, 1.15), xlab='Wavelength (nm)', ylab='Reflectance (Continuum Removal)')
# 
# ### remove possibles NA
# table(is.na(sp_spc))
# sp_spc <- sp_spc[complete.cases(sp_spc),]
# 
# ### # ### remove coloumn (X350, X2500) because they became NaN later (in for ANN only)
# # table(is.na(sp_spc))
# sp_spc$X350 <- NULL
# sp_spc$X2500 <- NULL
# # sp_spc[1:5, 1:5]
# # sp_spc[, 2140:NCOL(sp_spc)]
# } ###### End CR ######

#' ### 1.2.3 ###### savitzkyGolay ######
# {
# library(prospectr)
# sp <- sp_spc[1] ### this is the soil propertie (NOT USED)
# spc <- sp_spc[,2:NCOL(sp_spc)] ### These are the spectrum data
# svg_spc <- savitzkyGolay(X = spc, 1, 2, 11, delta.wav=2) ### R2 0.94 RF C  SVG-1-2-11_RF__R2_094__C_2019-06-20 2
# # svg_spc <- savitzkyGolay(X = spc, 1, 3, 11, delta.wav=2) ### R2 0.9 CB C   SVG-1-3-11_RF__R2_09__C_2019-06-20 22:
# # svg_spc <- savitzkyGolay(X = spc, 0, 1, 9, delta.wav=2) ### R2 0.89 CB C   SVG-0-1-9_CB__R2_089__C_2019-06-20 22:
# # svg_spc <- savitzkyGolay(X = spc, 1, 1, 9, delta.wav=2) ### SVG-1-1-9_RF__R2_089__
# # svg_spc <- savitzkyGolay(X = spc, 1, 2, 9, delta.wav=2) ### SVG-1-2-9_RF__R2_088__C_
# # svg_spc <- savitzkyGolay(X = spc, 2, 2, 11, delta.wav=2) ### SVG-2-2-11_RF__R2_088__C_
# svg_spc[1:5,1:5]
# # ### savitzkyGolay() removes few columns from begging
# # png(file= "plot_Savitzky_Golay_20.png", height=12, width=24, units="cm", res=120)
# # par(mar=c(6,6,4,2)) # (left, bottom, top, right)
# # matplot(1:NCOL(svg_spc), t(svg_spc[1:20,]),type='l', xlab='Wavelength (nm)', ylab= bquote(Reflectance ~ (Savitzky ~ Golay ~ 1^st ~ derivative))) # ok 350:2490
# # matlines(1:NCOL(svg_spc),t(svg_spc[1:20,]))
# # # mtext(bquote(Reflectance ~ (Savitzky ~ Golay ~ 1^st ~ derivative)))
# # dev.off()
# #
# # # ### plot both spectra and continuum removal
# # png(file= "plot_Savitzky_Golay_spectra_20.png", height=12, width=24, units="cm", res=120)
# # opar <- par(no.readonly = TRUE)
# # par(mfrow=c(2,1), mar=c(4,6,2,2))
# # matplot(1:NCOL(spc),t(spc[1:20,]),type='l',ylim=c(0,.7),xlab='Wavelength (nm)',ylab='Reflectance') # graphic from continum removal example
# # matlines(1:NCOL(spc),t(spc[1:20,]))
# # matplot(1:ncol(svg_spc), t(svg_spc[1:20,]),type='l', xlab='Wavelength (nm)', ylab= bquote(Reflectance ~ (Savitzky ~ Golay ~ 1^st ~ derivative))) # ok 350:2490
# # matlines(1:ncol(svg_spc),t(svg_spc[1:20,]))
# # dev.off()
# 
# # ### data mang
# # class(svg_spc); names(svg_spc); colnames(svg_spc); rownames(svg_spc)
# df_svg_spc <- as.data.frame(as.matrix(svg_spc))
# 
# # df_svg_spc[1:4,1:4]
# # df_svg_spc[1:4,2140:NCOL(df_svg_spc)]
# #### names(df_svg_spc) <- names(spc)
# 
# ### join soil propertie and spectra
# sp_spc_bind <- cbind(sp, df_svg_spc)
# colnames(sp_spc_bind)[1] <- names(sp_spc[1]) ### restoring the propertie name
# sp_spc <- sp_spc_bind
# # sp_spc[1:4,1:4]
# # sp_spc[1:4,2140:NCOL(df_svg_spc)]
# 
# ### remove possibles NA
# # table(is.na(sp_spc))
# sp_spc <- sp_spc[complete.cases(sp_spc),]
# 
# ### check the effet of cr
# ### with the soil propertie
# #### matplot(1:NCOL(sp_spc), t(sp_spc[1:20,]),type='l', xlab='Wavelength (nm)', ylab='Reflectance (Savitzky Golay - Soil Propertie + Spectrum)')
# ### without the soil propertie
# # matplot(2:NCOL(sp_spc), t(sp_spc[1:20,2:NCOL(sp_spc)]),type='l', xlab='Wavelength (nm)', ylab='Reflectance (Savitzky Golay)')
# # dim(svg_spc)
# }


#' ### 1.2.4 ####### PCA ####### PCA ####### PCA ####### PCA #######
# ### using PCA to leave out outlier based on standard deviation
# ### get pca
# sp_spc_pca <- prcomp(sp_spc)
# scores <- sp_spc_pca$x
# loadings <- sp_spc_pca$rotation
# spc.recons <- scores %*% t(loadings)
# obs.spc <- scale(sp_spc, scale = FALSE)
# ## plot(spc.recons[1,], type = "l")
# ## lines(obs.spc[1,], col= "red")
# 
# spc.res <- (spc.recons - obs.spc)^2
# spc.res <- sqrt(rowSums(spc.res))
# for(i in 1:length(spc.res)){
#   sample.fratio <- (length(spc.res)-1) * spc.res^2/sum((spc.res[-i])^2)
# }
# 
# ok <- pf(sample.fratio, 1, length(spc.res))
# ### Select the interval
# rows <- which(ok > 0.99)
# rows
# # plot(rows)
# ## png('Insert Your Directory Path Here/savings.png')
# # plot(scores)
# # points(scores[rows,], col="red", pch=16)
# # with(sp_spc, text(scores, labels = row.names(sp_spc), pos = 4))
# 
# ## library(devtools)
# ## install_github("vqv/ggbiplot")
# # library(ggbiplot)
# # ggbiplot(sp_spc_pca)
# # ggbiplot(sp_spc_pca, circle=TRUE, ellipse=TRUE, obs.scale = 1, var.scale = 1,  labels=rownames(sp_spc_pca))
# 
# ### Remove the PCA pointed
# rows
# dim(sp_spc)
# sp_spc[1:12,1:4]
# sp_spc <- sp_spc[-rows, ] ### controling the inclusion/remove by PCA: commented (#) not remove. Uncommented remove.
# dim(sp_spc)
# sp_spc[1:12,1:4]
# ####### PCA ####### PCA ####### PCA #######

#' ### 1.2.4 ###### using stepAIC to eliminate similar covariates
# require(MASS)
### build the scope formula for the stepAIC function
### scope.f <- as.formula(paste(names(sp_spc[1]), " ~ ", paste(paste0("X", 350:2500), collapse="+")))
### build the scope formula for the stepAIC function
# allVars <- colnames(sp_spc)
# predictorVars <- allVars[!allVars%in%names(sp_spc[1])]
# predictorVars <- paste(predictorVars, collapse = "+")
# scope.f = as.formula(paste(names(sp_spc[1]), "~", predictorVars, collapse = "+")) # form: C ~ X350 + X351 + X352 + X353 ...X2500
# 
# # create null model
# sp_spc.lm.null <- lm(sp_spc[[1]] ~ 1, data = sp_spc)
# sp_spc.lm.full <- lm(formula = scope.f, data = sp_spc)
# 
# sp_spc.forward <- stepAIC(sp_spc.lm.null, scope=list(lower=sp_spc.lm.null, upper=sp_spc.lm.full), direction="forward") ### smaller AIC is better
# 
# # do stepwise model selection based on the AIC criterion
# sp_spc.forward$coefficients
# sp_spc.forward$residuals
# sp_spc.forward$effects
# sp_spc.forward$rank
# sp_spc.forward$fitted.values
# sp_spc.forward$assign
# sp_spc.forward$qr
# sp_spc.forward$df.residual
# sp_spc.forward$xlevels
# sp_spc.forward$call ###
# sp_spc.forward$terms ###
# sp_spc.forward$model
# sp_spc.forward$anova
# save.image(file = paste0("../model/", paste(names(sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_stepAIC_"), ".RData", sep = "")) # save the output in model folder
# save.image(file = "_stepAIC_.RData") # save the output in model folder
# load("/home/yuri/_stepAIC_.RData")
# sp_spc <- cbind(sp_spc[1], sp_spc[, c("X1199","X2207","X2161","X1880","X1421","X2494","X2021","X1476","X2155","X2470","X1395","X1486","X548","X2481","X2449","X1945","X1929",
#                                       "X2103","X2209","X2029","X2037","X2496","X2019","X2450","X2055","X1584","X1453","X1507","X1582","X1583","X2471","X2406","X2415","X2320",
#                                       "X2051","X1198","X1131","X1073","X1175","X2049","X2047","X2046","X1061","X1064","X2101","X2105","X2443","X1062","X1473","X1543","X2127",
#                                       "X1078","X2211","X1887","X1283","X1933","X2035","X2054","X2213","X2210","X1292","X1578","X2446","X2214","X1133","X2124","X2444","X2445",
#                                       "X1084","X2129","X2455","X1534","X1531","X1535","X1538","X1539","X1456","X2424","X2414","X2409","X2418","X1935","X1930","X1502","X1500",
#                                       "X1187","X2493","X1134","X1267","X353","X363","X2389","X1577","X2390","X2113","X1942","X2005","X2004","X2006","X2388","X2392","X2394",
#                                       "X2393","X1075","X1451","X2057","X2452","X1430","X1220","X2479","X2118","X2011","X1455","X2024","X2115","X1203","X1201","X2119","X1483",
#                                       "X1189","X1128","X2003","X1271","X1532","X2412","X2326","X1135","X1137","X2048","X355","X2299","X1448","X2013","X2014","X2478","X2300",
#                                       "X1576","X366","X370","X372","X378","X389","X2302","X2304","X383","X2160","X1449","X2050","X2408","X354","X2159","X1664","X1129",
#                                       "X2429","X374","X367","X1659","X2421","X2423","X1957","X371","X1158","X1963","X1956","X2023","X1482","X1210","X1979","X2368","X1149",
#                                       "X1057","X1397","X2396","X1511","X2107","X1474","X1256","X1435","X2348","X1293","X1844","X1860","X1295","X1890","X388","X1408","X1481",
#                                       "X2410","X1991","X2477","X2100","X2347","X1458","X1850","X1965","X1223","X2453","X1462","X2033","X1255","X2151","X2215","X2010","X1996",
#                                       "X1058","X1163","X1936","X2425","X1053","X2422","X1269","X2442","X2354","X1581","X1673","X393","X2420","X1689","X491","X1145","X1140",
#                                       "X2373","X2369","X364","X2345","X2498","X2350","X1127","X1252","X2480","X387","X1090","X1633","X489","X1052","X1194","X2131","X2495",
#                                       "X1843","X2431","X2120","X1254","X2156","X1480","X379","X1095","X2365","X1655","X1685","X1398","X1205","X2438","X2267","X2238","X2492",
#                                       "X774","X2461","X484","X404","X1284","X2125","X1264","X1121","X2473","X2361","X483","X2250","X1658","X1477","X480","X1285","X373",
#                                       "X2253","X1863","X2430","X361","X2263","X391","X1066","X2401","X2343","X376","X2218","X2454","X1248","X397","X1265","X2456","X1967",
#                                       "X1668","X1598","X2372","X2084","X1212","X2203","X1217","X1648","X2087","X350")])
##### stepAIC used 299 covariates 2150 - 299 == 1851

#' ### 1.2.5 ###### using RFE to eliminate similar covariates
# ###### Recursive Feature Elimination RFE (it is using random Forest) ###### https://topepo.github.io/caret/recursive-feature-elimination.html # https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# library(caret)
# ctrl <- rfeControl(functions = lmFuncs,
#                    method = "repeatedcv",
#                    repeats = 8,
#                    verbose = T)
# 
# 
# library(randomForest)
# rfRFE <-  list(summary = defaultSummary,
#                fit = function(x, y, first, last, ...){
#                  library(randomForest)
#                  randomForest(x, y, importance = first, ...)
#                },
#                pred = function(object, x)  predict(object, x),
#                rank = function(object, x, y) {
#                  vimp <- varImp(object)
#                  vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
#                  vimp$var <- rownames(vimp)                  
#                  vimp
#                },
#                selectSize = pickSizeBest,
#                selectVar = pickVars)
# # rfRFE$summary
# # rfRFE$fit
# # rfRFE$pred
# # rfRFE$rank
# 
# ctrl$functions <- rfRFE
# ctrl$returnResamp <- "all"
# set.seed(10)
# # x: regressies
# df_sc_cal_sp_spc
# x <- df_sc_cal_sp_spc[ , 2:NCOL(df_sc_cal_sp_spc)]
# y <- df_sc_cal_sp_spc[[1]]
# # y: variable to be predicted
# rfProfile <- rfe(x, y, sizes = subsets, rfeControl = ctrl)
# rfProfile
# rfProfile$optVariables
# rfProfile$optsize
# 
# trellis.par.set(caretTheme())
# plot1 <- plot(rfProfile, type = c("g", "o"))
# plot2 <- plot(rfProfile, type = c("g", "o"), metric = "Rsquared")
# print(plot1, split=c(1,1,1,2), more=TRUE)
# print(plot2, split=c(1,2,1,2))
# 
# plot1 <- xyplot(rfProfile, 
#                 type = c("g", "p", "smooth"), 
#                 ylab = "RMSE CV Estimates")
# plot2 <- densityplot(rfProfile, 
#                      subset = Variables < 5, 
#                      adjust = 1.25, 
#                      as.table = TRUE, 
#                      xlab = "RMSE CV Estimates", 
#                      pch = "|")
# print(plot1, split=c(1,1,1,2), more=TRUE)
# print(plot2, split=c(1,2,1,2))
# ##### RFE end
# ###################################### pre treatment of spectrums -- end

# {
library(TeachingDemos) # dm = data_management
txtStart(paste0("../console/", paste(names(sp_spc[1]), "_", Sys.time(), "_console_ANN_dm.txt", sep = ""))) # save the output in console folder
date() 

#' ### 1.3 Random selection of sample to calibration and validation
### Randomly sort the data ###
sp_spc <-  sp_spc[sample(1:nrow(sp_spc) ,length(1:nrow(sp_spc))), 1:ncol(sp_spc)] 
dim(sp_spc)
sp_spc[1:12,1:4]

ind <- sample(1:nrow(sp_spc), (nrow(sp_spc)*0.80)) ### index of 80 %
cal_sp_spc <- sp_spc[ind,]
val_sp_spc <- sp_spc[-ind,]
dim(cal_sp_spc)
dim(val_sp_spc)

#############################################################################################################################
#' ## 2 -- I Artificial Neural Network - ANN 
library(MASS)
library(neuralnet)
# install.packages("DMwR")
library(DMwR) ### usacale the data ### Pacote arquivado: ### Instalar com: https://stackoverflow.com/questions/66923903/why-cant-i-install-the-dmwr-package https://cran.r-project.org/src/contrib/Archive/DMwR/ https://cran-archive.r-project.org/web/checks/2021/2021-03-16_check_results_DMwR.html
### DATA NORMALIZATION Scale data cal_sp_spc
maxValue <- apply(sp_spc, 2, max)
table(max(maxValue))
minValue <- apply(sp_spc, 2, min)
table(min(minValue))
# cal_sp_spc <- as.data.frame(scale(cal_sp_spc, center = minValue, scale = maxValue-minValue))
cal_sp_spc[1:5, 1:5]
# sc_cal_sp_spc <- scale(cal_sp_spc, center = minValue, scale = maxValue-minValue) ; df_sc_cal_sp_spc <- as.data.frame(sc_cal_sp_spc)
sc_cal_sp_spc <- scale(cal_sp_spc, center = minValue, scale = maxValue-minValue)
df_sc_cal_sp_spc <- as.data.frame(sc_cal_sp_spc)
# df_sc_cal_sp_spc[1:5, 1:5]

### DATA NORMALIZATION Scale data val_sp_spc
maxValue <- apply(sp_spc, 2, max)
table(max(maxValue))
minValue <- apply(sp_spc, 2, min)
table(min(minValue))
# val_sp_spc <- as.data.frame(scale(val_sp_spc, center = minValue, scale = maxValue-minValue))
sc_val_sp_spc <- scale(val_sp_spc, center = minValue, scale = maxValue-minValue)
df_sc_val_sp_spc <- as.data.frame(sc_val_sp_spc)
# df_sc_val_sp_spc[1:5, 1:5]

### configuration of neural network 13 4 2 1
allVars <- colnames(df_sc_cal_sp_spc)
dim(df_sc_cal_sp_spc)
# predictorVars <- allVars[!allVars%in%"C"]
predictorVars <- allVars[!allVars%in%names(df_sc_cal_sp_spc[1])]
predictorVars <- paste(predictorVars, collapse = "+")
# form=as.formula(paste("C~", predictorVars, collapse = "+")) # form: C ~ X350 + X351 + X352 + X353 ...X2500
form = as.formula(paste(names(df_sc_cal_sp_spc[1]), "~", predictorVars, collapse = "+")) # form: C ~ X350 + X351 + X352 + X353 ...X2500
# (form)
# {
ANN_model <- neuralnet(formula = form, data = df_sc_cal_sp_spc, hidden = c(13, 8, 5, 3, 1), linear.output = T)
# ANN_model
# str(ANN_model)
# plot(ANN_model, rep = "best")
# plot(ANN_model, rep = "best", x.entry = 0.15, x.out = 0.8,
# radius = 0.15, arrow.length = 0.12, intercept = TRUE,
# intercept.factor = 0.4, information = TRUE, information.pos = 0,
# col.entry.synapse = "black", col.entry = "black",
# col.hidden = "black", col.hidden.synapse = "black",
# col.out = "black", col.out.synapse = "black",
# col.intercept = "blue", fontsize = 8, dimension = 12,
# show.weights = TRUE, file = NULL)

### Predictions for validation data set
# preditions <- compute(ANN_model, teste)
preditions_cal <- predict(ANN_model, df_sc_cal_sp_spc[ ,2:NCOL(df_sc_cal_sp_spc)]) 
preditions_val <- predict(ANN_model, df_sc_val_sp_spc[ ,2:NCOL(df_sc_val_sp_spc)]) 
str(preditions_cal)
# preditions
# View(preditions$net.result)
# preditions_cal$net.result
# df_sc_val_sp_spc$C

# (preditions_cal)
### UNscale data df_sc_cal_sp_spc
usc_preditions_cal <- unscale(preditions_cal, sc_cal_sp_spc)
# usc_preditions_cal
### UNscale data df_sc_val_sp_spc
usc_preditions_val <- unscale(preditions_val, sc_val_sp_spc)
# usc_preditions_val

# library(devtools); install_bitbucket("brendo1001/ithir/pkg")
library(ithir)
# goof_ANN <- goof(DataFrame$C, ANN_model$result.matrix, type='spec')

# https://www.molecularecologist.com/2015/11/2d-posterior-density-plots-in-r/ # https://www.r-bloggers.com/5-ways-to-do-2d-histograms-in-r/ # https://www.biostars.org/p/291845/ # https://mathematica.stackexchange.com/questions/85530/how-2d-scatterplots-with-quantitative-density-dependent-coloring
# install.packages("hexbin")
# install.packages("RColorBrewer")
library(hexbin)
library(grid)
library(RColorBrewer)
# par(mar=c(3,4,2,2))
# display.brewer.all()
### Goodness of fit
### Model Calibration
goof_ANN_cal <- goof(cal_sp_spc[[1]], usc_preditions_cal, type='spec') # com predict
goof_ANN_cal
### External validation
goof_ANN_val <- goof(val_sp_spc[[1]], usc_preditions_val, type='spec') # com predict
goof_ANN_val

### color palet for hex plots
crp <- colorRampPalette(rev(brewer.pal(9,'YlOrBr')))
### save plot calibration
plt_cal <- as.data.frame(cbind(cal_sp_spc[[1]], usc_preditions_cal))
mypath <- file.path("../figure/", paste0(names(df_sc_cal_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_ANN_", "Obs_Pred_Calibration", "_", "R2_", round(goof_ANN_cal$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_cal$V1 ~ plt_cal$V2, data= plt_cal, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title= .9, # size name legend (Counts)
           cex.labels = .9, # size of names in the legend
           xbins = 30, # size of hex
           colorcut = seq(0, 1, length = 8), ### number of counts in the legend (8)
           aspect= "1", # control the ratio fixed if commented
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_ANN_cal$R2, 2), "\n", 
                          "MSE", round(goof_ANN_cal$MSE, 2), "\n",
                          "RMSE", round(goof_ANN_cal$RMSE, 2), "\n",
                          "RPD", round(goof_ANN_cal$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()
### save plot validation
plt_val <- as.data.frame(cbind(val_sp_spc[[1]], usc_preditions_val))
mypath <- file.path("../figure/", paste0(names(df_sc_val_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_ANN_", "Obs_Pred__Validation", "_", "R2_", round(goof_ANN_val$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_val$V1 ~ plt_val$V2, data= plt_val, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title= .9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect= "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_ANN_val$R2, 2), "\n", 
                          "MSE", round(goof_ANN_val$MSE, 2), "\n",
                          "RMSE", round(goof_ANN_val$RMSE, 2), "\n",
                          "RPD", round(goof_ANN_val$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()

            # 
            # 
            # 
            # 
            # plot(df_sc_cal_sp_spc[[1]], preditions_cal, col= "blue") 
            #      
            # goof_ANN_cal <- goof(cal_sp_spc[[1]], usc_preditions_cal, type='spec') # com predict
            # goof_ANN_cal
            # 
            # mypath <- file.path("../figure/", paste(names(df_sc_cal_sp_spc[1]), "_ANN_", "R2_", round(goof_ANN_cal$R2, 2), "Observed vs Predicted Calibration", "_", Sys.time(), ".png"))
            # png(file= mypath, height=12, width=12, units="cm", res=120)
            # plot(cal_sp_spc[[1]], usc_preditions_cal, col= "blue", pch=1, cex=1.2, type = "p", 
            #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))), 
            #      xlab = c(paste("Observed", names(df_sc_cal_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_cal_sp_spc[1])), unit))
            # abline(0,1,col="black")
            # mtext(c(paste("R²", round(goof_ANN_cal$R2, 2), "\n", 
            #               "MSE", round(goof_ANN_cal$MSE, 2), "\n",
            #               "RMSE", round(goof_ANN_cal$RMSE, 2), "\n",
            #               "RPD", round(goof_ANN_cal$RPD, 2))), 
            #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
            # dev.off()
            # 
            # goof_ANN_val <- goof(val_sp_spc[[1]], usc_preditions_val, type='spec') # com predict
            # goof_ANN_val
            # 
            # mypath <- file.path("../figure/", paste(names(df_sc_val_sp_spc[1]), "_ANN_", "R2_", round(goof_ANN_val$R2, 2), "Observed vs Predicted Validation", "_", Sys.time(), ".png"))
            # png(file= mypath, height=12, width=12, units="cm", res=120)
            # plot(val_sp_spc[[1]], usc_preditions_val, col= "blue", pch=1, cex=1.2, type = "p", 
            #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
            #      xlab = c(paste("Observed", names(df_sc_val_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_val_sp_spc[1])), unit))
            # abline(0,1,col="black")
            # mtext(c(paste("R²", round(goof_ANN_val$R2, 2), "\n", 
            #               "MSE", round(goof_ANN_val$MSE, 2), "\n",
            #               "RMSE", round(goof_ANN_val$RMSE, 2), "\n",
            #               "RPD", round(goof_ANN_val$RPD, 2))), 
            #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
            # dev.off()
            # 
            
# mtext("(c)",side=3,line=-1.5, at=par("usr")[1]+0.05*diff(par("usr")[1:2]), cex=1.2)
# preditions_val[which.max(preditions_val), ] # 287, 213

# } ### from model
# } ### from Selection of each propertie + random selection of samples
goof_ANN_cal; goof_ANN_val
# } ### from top

### Saving the image (model and all variables)
save.image(file = paste0("../model/", paste(names(sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_ANN_Model_Image_", "R2_", round(goof_ANN_val$R2, 2), ".RData", sep = ""))) # save the output in model folder

# load("Image_ANN_Model_R2_092.RData")
# load(file = "ANN_Model_R2_091288.rda")
# save(ANN_model, file = "ANN_Model.rda")

### remove all variables, but let the data cal e val
rm(list=setdiff(ls(), c("sp_spc", "ind", "cal_sp_spc", "val_sp_spc", "form", "unit", "goof_ANN_cal", "goof_ANN_val")))

txtStop() # finaliza a anotação

### sem rows e sem pre tratamento. Mesmas amostras do dataset antigo.
# goof_ANN_cal;goof_ANN_val
#        R2 concordance         MSE       RMSE          bias        MSEc      RMSEc     RPD     RPIQ
# 0.9589638   0.9749308 0.001349476 0.03673522 -2.786534e-05 0.001349476 0.03673521 4.94683 6.470305
#        R2 concordance         MSE       RMSE        bias        MSEc      RMSEc      RPD     RPIQ
# 0.9227121   0.9419057 0.004100121 0.06403219 -0.01159733 0.003965623 0.06297319 3.627386 4.951127

###############
# R2 concordance         MSE       RMSE          bias
# 1 0.8650547   0.9142473 0.004106522 0.06408215 -2.812117e-05
# MSEc      RMSEc      RPD     RPIQ
# 1 0.004106521 0.06408214 2.688671 3.765153

#  goof_ANN
# R2 concordance         MSE       RMSE
# 1 0.9086103   0.9324209 0.003774425 0.06143635
# bias        MSEc      RMSEc      RPD    RPIQ
# 1 -0.003122562 0.003764674 0.06135694 3.328473 4.22512

# R2 concordance         MSE       RMSE       bias
# 1 0.9085494   0.9030384 0.007074569 0.08411045 0.03205134
# MSEc      RMSEc      RPD     RPIQ
# 1 0.00604728 0.07776426 2.207557 2.626668

# goof_ANN <- goof(actualValues, preditions_2, type='spec')
# goof_ANN

# MSE <- sum((preditions - actualValues)^2)/nrow(val_sp_spc)
# MSE

# RMSE <- sqrt(sum((preditions - actualValues)^2)/nrow(val_sp_spc)) ##### ??????
# RMSE

# plot(val_sp_spc$C, preditions, col= "blue", main = "Actual vs Predicted", pch=1, cex=1.2, type = "p", xlab = "Actual", ylab = "Predicted")
# abline(0,1,col="black")

# dev.off()
# plot(actualValues, preditions_2, col= "blue", main = "Actual vs Predicted", pch=1, cex=1.2, type = "p", xlab = "Actual", ylab = "Predicted")
# abline(0,1,col="black")


# saving transpond data ###  row.names = TRUE, col.names = T
# write.table(preditions_2, file = "preditions.csv", sep = ";", dec = ",", row.names = TRUE, col.names = F, fileEncoding = "UTF-8")

# saving transpond data ###  row.names = TRUE, col.names = T
# write.table(actualValues, file = "actualValues.csv", sep = ";", dec = ",", row.names = TRUE, col.names = T, fileEncoding = "UTF-8")


# library(neuralnet)
# # Split data
# train_idx <- sample(nrow(iris), 2/3 * nrow(iris))
# iris_train <- iris[train_idx, ]
# iris_test <- iris[-train_idx, ]
# # Binary classification
# nn <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
# pred <- predict(nn, iris_test)
# table(iris_test$Species == "setosa", pred[, 1] > 0.5)
# # Multiclass classification
# nn <- neuralnet((Species == "setosa") + (Species == "versicolor") + (Species == "virginica") ~ Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
# pred <- predict(nn, iris_test)
# table(iris_test$Species, apply(pred, 1, which.max))



# {
library(TeachingDemos)
txtStart(paste0("../console/", paste(names(sp_spc[1]), "_", Sys.time(), "_console_RF.txt", sep = ""))) # save the output in console folder

#############################################################################################################################
#' ## 3 -- II  RandomForest - RF
df_sc_cal_sp_spc <- cal_sp_spc
df_sc_val_sp_spc <- val_sp_spc

library(ranger)
RF_model <- ranger(formula = form, data = df_sc_cal_sp_spc, quantreg=TRUE, keep.inbag=TRUE, num.trees=150)

# RF_model
# str(RF_model)
# plot(RF_model, rep = "best")

### Predictions for validation data set
# preditions <- compute(RF_model, teste)

preditions_cal <- predict(RF_model, df_sc_cal_sp_spc[ ,2:NCOL(df_sc_cal_sp_spc)]) 
preditions_val <- predict(RF_model, df_sc_val_sp_spc[ ,2:NCOL(df_sc_val_sp_spc)]) 
str(preditions_cal)

library(ithir)
### Goodness of fit
### Model Calibration
goof_RF_cal <- goof(cal_sp_spc[[1]], preditions_cal$predictions, type='spec')
goof_RF_cal
### External validation
goof_RF_val <- goof(val_sp_spc[[1]], preditions_val$predictions, type='spec')
goof_RF_val

### color palet for hex plots
crp <- colorRampPalette(rev(brewer.pal(9,'YlOrBr')))
### save plot calibration
plt_cal <- as.data.frame(cbind(cal_sp_spc[[1]], preditions_cal$predictions))
mypath <- file.path("../figure/", paste0(names(df_sc_cal_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_RF_", "Obs_Pred_Calibration", "_", "R2_", round(goof_RF_cal$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_cal$V1 ~ plt_cal$V2, data= plt_cal, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title= .9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect = "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_RF_cal$R2, 2), "\n", 
                          "MSE", round(goof_RF_cal$MSE, 2), "\n",
                          "RMSE", round(goof_RF_cal$RMSE, 2), "\n",
                          "RPD", round(goof_RF_cal$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()
### save plot validation
plt_val <- as.data.frame(cbind(val_sp_spc[[1]], preditions_val$predictions))
mypath <- file.path("../figure/", paste0(names(df_sc_val_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_RF_", "Obs_Pred__Validation", "_", "R2_", round(goof_RF_val$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_val$V1 ~ plt_val$V2, data= plt_val, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title=.9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect = "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_RF_val$R2, 2), "\n", 
                          "MSE", round(goof_RF_val$MSE, 2), "\n",
                          "RMSE", round(goof_RF_val$RMSE, 2), "\n",
                          "RPD", round(goof_RF_val$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()



                          # # goof_plsr <- goof(sample_data$C, test_plsr$fitted.values[,,30], type='spec)'
                          # # goof_ANN <- goof(DataFrame$C, RF_model$result.matrix, type='spec')
                          # 
                          # plot(df_sc_cal_sp_spc[[1]], preditions_cal$predictions, col= "blue") 
                          # 
                          # goof_RF_cal <- goof(cal_sp_spc[[1]], preditions_cal$predictions, type='spec') # com predict
                          # goof_RF_cal
                          # 
                          # mypath <- file.path("../figure/", paste(names(df_sc_cal_sp_spc[1]), "_RF_", "R2_", round(goof_RF_cal$R2, 2), "Observed vs Predicted Calibration", "_", Sys.time(), ".png"))
                          # png(file= mypath, height=12, width=12, units="cm", res=120)
                          # plot(cal_sp_spc[[1]], preditions_cal$predictions, col= "blue", pch=1, cex=1.2, type = "p", 
                          #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))), 
                          #      xlab = c(paste("Observed", names(df_sc_cal_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_cal_sp_spc[1])), unit))
                          # abline(0,1,col="black")
                          # mtext(c(paste("R²", round(goof_RF_cal$R2, 2), "\n", 
                          #               "MSE", round(goof_RF_cal$MSE, 2), "\n",
                          #               "RMSE", round(goof_RF_cal$RMSE, 2), "\n",
                          #               "RPD", round(goof_RF_cal$RPD, 2))), 
                          #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
                          # dev.off()
                          # 
                          # goof_RF_val <- goof(val_sp_spc[[1]], preditions_val$predictions, type='spec') # com predict
                          # goof_RF_val
                          # 
                          # mypath <- file.path("../figure/", paste(names(df_sc_val_sp_spc[1]), "_RF_", "R2_", round(goof_RF_val$R2, 2), "Observed vs Predicted Validation", "_", Sys.time(), ".png"))
                          # png(file= mypath, height=12, width=12, units="cm", res=120)
                          # plot(val_sp_spc[[1]], preditions_val$predictions, col= "blue", pch=1, cex=1.2, type = "p", 
                          #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
                          #      xlab = c(paste("Observed", names(df_sc_val_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_val_sp_spc[1])), unit))
                          # abline(0,1,col="black")
                          # mtext(c(paste("R²", round(goof_RF_val$R2, 2), "\n", 
                          #               "MSE", round(goof_RF_val$MSE, 2), "\n",
                          #               "RMSE", round(goof_RF_val$RMSE, 2), "\n",
                          #               "RPD", round(goof_RF_val$RPD, 2))), 
                          #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
                          # dev.off()


# } ### from model
# } ### from Selection of each propertie + random selection of samples
goof_RF_cal; goof_RF_val
# } ### from top

### Saving the image (model and all variables)
save.image(file = paste0("../model/", paste(names(sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_RF_Model_Image_", "R2_", round(goof_RF_val$R2, 2), ".RData", sep = ""))) # save the output in model folder

# load("Image_RF_Model_R2_092.RData")
# load(file = "RF_Model_R2_091288.rda")
# save(RF_model, file = "RF_Model.rda")

### remove all variables, but let the data cal e val
rm(list=setdiff(ls(), c("sp_spc", "ind", "cal_sp_spc", "val_sp_spc", "form", "unit", "goof_ANN_cal", "goof_ANN_val", "goof_RF_cal", "goof_RF_val")))

txtStop() # finaliza a anotação


# {
library(TeachingDemos)
txtStart(paste0("../console/", paste(names(sp_spc[1]), "_", Sys.time(), "_console_PLSR.txt", sep = ""))) # save the output in console folder

#############################################################################################################################
#' ## 4 -- III PLSR
df_sc_cal_sp_spc <- cal_sp_spc
df_sc_val_sp_spc <- val_sp_spc

library(pls)

PLSR_model <- plsr(formula = form, data = df_sc_cal_sp_spc, ncomp= 30, 
                   validation="LOO", method="oscorespls", na.action= na.exclude)
# PLSR_model
str(PLSR_model)
summary(PLSR_model)

# plot(PLSR_model, plottype='validation')
# plot(PLSR_model, plottype='prediction')
# plot(PLSR_model, plottype='correlation')
# plot(PLSR_model, plottype='biplot')
# plot(PLSR_model, col= "blue", main = "Observed vs Predicted", pch=1, cex=1.2, type = "p", xlab = "Observed", ylab = "Predicted")
# abline(0,1,col="black")

### Predictions for validation data set
# preditions <- compute(PLSR_model, teste)
preditions_cal <- predict(PLSR_model, df_sc_cal_sp_spc[ ,2:NCOL(df_sc_cal_sp_spc)]) 
preditions_val <- predict(PLSR_model, df_sc_val_sp_spc[ ,2:NCOL(df_sc_val_sp_spc)]) 
str(preditions_cal)

library(ithir)
### Goodness of fit
### Model Calibration
goof_PLSR_cal <- goof(cal_sp_spc[[1]], preditions_cal[,,30], type='spec')
goof_PLSR_cal
### External validation
goof_PLSR_val <- goof(val_sp_spc[[1]], preditions_val[,,30], type='spec')
goof_PLSR_val

### color palet for hex plots
crp <- colorRampPalette(rev(brewer.pal(9,'YlOrBr')))
### save plot calibration
plt_cal <- as.data.frame(cbind(cal_sp_spc[[1]], preditions_cal[,,30]))
mypath <- file.path("../figure/", paste0(names(df_sc_cal_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_PLSR_", "Obs_Pred_Calibration", "_", "R2_", round(goof_PLSR_cal$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_cal$V1 ~ plt_cal$V2, data= plt_cal, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title= .9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect = "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_PLSR_cal$R2, 2), "\n", 
                          "MSE", round(goof_PLSR_cal$MSE, 2), "\n",
                          "RMSE", round(goof_PLSR_cal$RMSE, 2), "\n",
                          "RPD", round(goof_PLSR_cal$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()
### save plot validation
plt_val <- as.data.frame(cbind(val_sp_spc[[1]], preditions_val[,,30]))
mypath <- file.path("../figure/", paste0(names(df_sc_val_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_PLSR_", "Obs_Pred__Validation", "_", "R2_", round(goof_PLSR_val$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_val$V1 ~ plt_val$V2, data= plt_val, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title=.9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect = "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_PLSR_val$R2, 2), "\n", 
                          "MSE", round(goof_PLSR_val$MSE, 2), "\n",
                          "RMSE", round(goof_PLSR_val$RMSE, 2), "\n",
                          "RPD", round(goof_PLSR_val$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()

                    # # library(devtools); install_bitbucket("brendo1001/ithir/pkg")
                    # library(ithir)
                    # # goof_plsr <- goof(sample_data$C, test_plsr$fitted.values[,,30], type='spec)'
                    # # goof_ANN <- goof(DataFrame$C, PLSR_model$result.matrix, type='spec')
                    # 
                    # plot(df_sc_cal_sp_spc[[1]], preditions_cal[,,30], col= "blue") 
                    # plot(df_sc_val_sp_spc[[1]], preditions_val[,,30], col= "blue") 
                    # 
                    # # goof_plsr <- goof(sample_data$C, test_plsr$fitted.values[,,30], type='spec)'
                    # goof_pred <- goof(sample_pred$C, model_pred[,1], type='spec') 
                    # 
                    # 
                    # goof_PLSR_cal <- goof(cal_sp_spc[[1]], preditions_cal[,,30], type='spec') # com predict
                    # goof_PLSR_cal
                    # 
                    # mypath <- file.path("../figure/", paste(names(df_sc_cal_sp_spc[1]), "_PLSR_", "R2_", round(goof_PLSR_cal$R2, 2), "Observed vs Predicted Calibration", "_", Sys.time(), ".png"))
                    # png(file= mypath, height=12, width=12, units="cm", res=120)
                    # plot(cal_sp_spc[[1]], preditions_cal[,,30], col= "blue", pch=1, cex=1.2, type = "p", 
                    #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))), 
                    #      xlab = c(paste("Observed", names(df_sc_cal_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_cal_sp_spc[1])), unit))
                    # abline(0,1,col="black")
                    # mtext(c(paste("R²", round(goof_PLSR_cal$R2, 2), "\n", 
                    #               "MSE", round(goof_PLSR_cal$MSE, 2), "\n",
                    #               "RMSE", round(goof_PLSR_cal$RMSE, 2), "\n",
                    #               "RPD", round(goof_PLSR_cal$RPD, 2))), 
                    #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
                    # dev.off()
                    # 
                    # goof_PLSR_val <- goof(val_sp_spc[[1]], preditions_val[,,30], type='spec') # com predict
                    # goof_PLSR_val
                    # 
                    # mypath <- file.path("../figure/", paste(names(df_sc_val_sp_spc[1]), "_PLSR_", "R2_", round(goof_PLSR_val$R2, 2), "Observed vs Predicted Validation", "_", Sys.time(), ".png"))
                    # png(file= mypath, height=12, width=12, units="cm", res=120)
                    # plot(val_sp_spc[[1]], preditions_val[,,30], col= "blue", pch=1, cex=1.2, type = "p", 
                    #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
                    #      xlab = c(paste("Observed", names(df_sc_val_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_val_sp_spc[1])), unit))
                    # abline(0,1,col="black")
                    # mtext(c(paste("R²", round(goof_PLSR_val$R2, 2), "\n", 
                    #               "MSE", round(goof_PLSR_val$MSE, 2), "\n",
                    #               "RMSE", round(goof_PLSR_val$RMSE, 2), "\n",
                    #               "RPD", round(goof_PLSR_val$RPD, 2))), 
                    #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
                    # dev.off()




# } ### from model
# } ### from Selection of each propertie + random selection of samples
goof_PLSR_cal; goof_PLSR_val
# } ### from top

### Saving the image (model and all variables)
save.image(file = paste0("../model/", paste(names(sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_PLSR_model_Image_", "R2_", round(goof_PLSR_val$R2, 2), ".RData", sep = ""))) # save the output in model folder

# load("Image_PLSR_model_R2_092.RData")
# load(file = "PLSR_model_R2_091288.rda")
# save(PLSR_model, file = "PLSR_Model.rda")

### remove all variables, but let the data cal e val
rm(list=setdiff(ls(), c("sp_spc", "ind", "cal_sp_spc", "val_sp_spc", "form", "unit", "goof_ANN_cal", "goof_ANN_val", "goof_RF_cal", "goof_RF_val", "goof_PLSR_cal", "goof_PLSR_val")))

txtStop() # finaliza a anotação


# {
library(TeachingDemos)
txtStart(paste0("../console/", paste(names(sp_spc[1]), "_", Sys.time(), "_console_CB.txt", sep = ""))) # save the output in console folder

#############################################################################################################################
#' ## 5 -- IV Cubist - CB
df_sc_cal_sp_spc <- cal_sp_spc
df_sc_val_sp_spc <- val_sp_spc

## Cubist model fitting
library(Cubist)
# set.seed(875)
# training <- sample(nrow(DSM_data), 0.70 * nrow(DSM_data))
# mDat <- DSM_data[training, ]

#fit the model
CB_model <- cubist(x = df_sc_cal_sp_spc[,2:NCOL(df_sc_cal_sp_spc)], 
                     y = df_sc_cal_sp_spc[[1]],
                     cubistControl(rules = 100, extrapolation = 15), committees = 1)
### Model summary
# summary(CB_model)
# CB_model
# str(CB_model)
# plot(CB_model, rep = "best")

### Predictions for validation data set
# preditions <- compute(CB_model, teste)
preditions_cal <- predict(CB_model, df_sc_cal_sp_spc[ ,2:NCOL(df_sc_cal_sp_spc)]) 
preditions_val <- predict(CB_model, df_sc_val_sp_spc[ ,2:NCOL(df_sc_val_sp_spc)]) 
str(preditions_cal)

library(ithir)
### Goodness of fit
### Model Calibration
goof_CB_cal <- goof(cal_sp_spc[[1]], preditions_cal, type='spec') # com predict
goof_CB_cal
### External validation
goof_CB_val <- goof(val_sp_spc[[1]], preditions_val, type='spec') # com predict
goof_CB_val

### color palet for hex plots
crp <- colorRampPalette(rev(brewer.pal(9,'YlOrBr')))
### save plot calibration
plt_cal <- as.data.frame(cbind(cal_sp_spc[[1]], preditions_cal))
mypath <- file.path("../figure/", paste0(names(df_sc_cal_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_CB_", "Obs_Pred_Calibration", "_", "R2_", round(goof_CB_cal$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_cal$V1 ~ plt_cal$preditions_cal, data= plt_cal, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title= .9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect = "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_cal_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_CB_cal$R2, 2), "\n", 
                          "MSE", round(goof_CB_cal$MSE, 2), "\n",
                          "RMSE", round(goof_CB_cal$RMSE, 2), "\n",
                          "RPD", round(goof_CB_cal$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()
### save plot validation
plt_val <- as.data.frame(cbind(val_sp_spc[[1]], preditions_val))
mypath <- file.path("../figure/", paste0(names(df_sc_val_sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_CB_", "Obs_Pred__Validation", "_", "R2_", round(goof_CB_val$R2, 2), ".png"))
png(file= mypath, height=12, width=12, units="cm", res=120)
hexbinplot(plt_val$V1 ~ plt_val$preditions_val, data= plt_val, colramp= crp, 
           type = c("g", "r"), style = "colorscale", border = T,
           cex.title=.9, cex.labels = .9, xbins = 30, colorcut = seq(0, 1, length = 8), aspect = "1",
           # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
           xlab= bquote(paste("Observed", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))),
           ylab= bquote(paste("Predicted", " ", .(names(df_sc_val_sp_spc[1])), " ", .(unit))))
grid.text(label = c(paste("R²", round(goof_CB_val$R2, 2), "\n", 
                          "MSE", round(goof_CB_val$MSE, 2), "\n",
                          "RMSE", round(goof_CB_val$RMSE, 2), "\n",
                          "RPD", round(goof_CB_val$RPD, 2))), 
          x = unit(0.28, "npc"), y = unit(0.7, "npc"))
dev.off()
                    # 
                    # # library(devtools); install_bitbucket("brendo1001/ithir/pkg")
                    # library(ithir)
                    # 
                    # plot(df_sc_cal_sp_spc[[1]], preditions_cal, col= "blue") 
                    # 
                    # goof_CB_cal <- goof(cal_sp_spc[[1]], preditions_cal, type='spec') # com predict
                    # goof_CB_cal
                    # 
                    # mypath <- file.path("../figure/", paste(names(df_sc_cal_sp_spc[1]), "_CB_", "R2_", round(goof_CB_cal$R2, 2), "Observed vs Predicted Calibration", "_", Sys.time(), ".png"))
                    # png(file= mypath, height=12, width=12, units="cm", res=120)
                    # plot(cal_sp_spc[[1]], preditions_cal, col= "blue", pch=1, cex=1.2, type = "p", 
                    #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_cal_sp_spc[1]))), 
                    #      xlab = c(paste("Observed", names(df_sc_cal_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_cal_sp_spc[1])), unit))
                    # abline(0,1,col="black")
                    # mtext(c(paste("R²", round(goof_CB_cal$R2, 2), "\n", 
                    #               "MSE", round(goof_CB_cal$MSE, 2), "\n",
                    #               "RMSE", round(goof_CB_cal$RMSE, 2), "\n",
                    #               "RPD", round(goof_CB_cal$RPD, 2))), 
                    #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
                    # dev.off()
                    # 
                    # goof_CB_val <- goof(val_sp_spc[[1]], preditions_val, type='spec') # com predict
                    # goof_CB_val
                    # 
                    # mypath <- file.path("../figure/", paste(names(df_sc_val_sp_spc[1]), "_CB_", "R2_", round(goof_CB_val$R2, 2), "Observed vs Predicted Validation", "_", Sys.time(), ".png"))
                    # png(file= mypath, height=12, width=12, units="cm", res=120)
                    # plot(val_sp_spc[[1]], preditions_val, col= "blue", pch=1, cex=1.2, type = "p", 
                    #      # main = c(paste("Observed vs Predicted Calibration", names(df_sc_val_sp_spc[1]))),
                    #      xlab = c(paste("Observed", names(df_sc_val_sp_spc[1])), unit), ylab = c(paste("Predicted", names(df_sc_val_sp_spc[1])), unit))
                    # abline(0,1,col="black")
                    # mtext(c(paste("R²", round(goof_CB_val$R2, 2), "\n", 
                    #               "MSE", round(goof_CB_val$MSE, 2), "\n",
                    #               "RMSE", round(goof_CB_val$RMSE, 2), "\n",
                    #               "RPD", round(goof_CB_val$RPD, 2))), 
                    #       las = 1, side=3, line=-5.5, at=par("usr")[1] + 0.2* diff(par("usr")[1:2]), cex=1.2)
                    # dev.off()

# } ### from model
# } ### from Selection of each propertie + random selection of samples
goof_CB_cal; goof_CB_val
# } ### from top

### Saving the image (model and all variables)
save.image(file = paste0("../model/", paste0(names(sp_spc[1]), "_", format(Sys.time(), "%Y-%m-%d_%X"), "_CB_Model_Image_", "R2_", round(goof_CB_val$R2, 2), ".RData", sep = ""))) # save the output in model folder

# load("Image_CB_Model_R2_092.RData")
# load(file = "CB_Model_R2_091288.rda")
# save(CB_model, file = "RF_Model.rda")

### remove all variables, but let the data cal e val
rm(list=setdiff(ls(), c("sp_spc", "ind", "cal_sp_spc", "val_sp_spc", "form", "unit", "goof_ANN_cal", "goof_ANN_val", "goof_RF_cal", "goof_RF_val", "goof_PLSR_cal", "goof_PLSR_val", "goof_CB_cal", "goof_CB_val")))

goof_ANN_cal; goof_ANN_val
goof_RF_cal; goof_RF_val
goof_PLSR_cal; goof_PLSR_val
goof_CB_cal; goof_CB_val
names(sp_spc[1])
dim(cal_sp_spc)
dim(val_sp_spc)

txtStop() # finaliza a anotação


#############################################################################################################################

#     _/_/_/_/  _/      _/  _/_/_/        _/_/_/                      _/                _/      
#    _/        _/_/    _/  _/    _/    _/          _/_/_/  _/  _/_/        _/_/_/    _/_/_/_/   
#   _/_/_/    _/  _/  _/  _/    _/      _/_/    _/        _/_/      _/   _/    _/    _/        
#  _/        _/    _/_/  _/    _/          _/  _/        _/        _/   _/    _/    _/         
# _/_/_/_/  _/      _/  _/_/_/      _/_/_/      _/_/_/  _/        _/   _/_/_/        _/_/      
#                                                                     _/                       
#                                                                    _/
# figlet -f lean "End Script"
# showfigfonts

######## Convert R to Rmd ###########         
# library(knitr)
# (s = "Prediction_PNI_ANN_RF_PLSR_CB_hex.R") # (s = "/Path_of_THIS_SCRIP/Name_of_THIS_SCRIPT.R")
# spin(s, FALSE, format = "Rmd") # alternatively: # spin(s, FALSE, format = "Rhtml")
sink() # close sink


### schedule tasks (scripts)
# library(cronR)
# cmd <- cron_rscript("/home/yuri/MEGAsync/PNI_CHRIS_2019/PNI_spectral_soil_prediction_ANN_RF_PLSR_CB/script/Prediction_PNI_ANN_RF_PLSR_CB.R")
# cron_add(cmd, frequency = '*/1 * * * *', id = 'job1', description = 'Every 1 min')
# cron_njobs()
# cron_ls()
# cron_clear(ask=FALSE)
# cron_ls()
