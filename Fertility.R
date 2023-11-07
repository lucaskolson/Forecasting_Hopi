#estimating age specific fertility rates for females using linear model
years <- seq(1940, 2010, 5)

ff_model_linear <- data.frame(matrix(ncol=2, nrow = 15))
ff_model_linear[,1] <- years

ff_model_linear[1:4,2] <- 0.4528689 #provided by article
ff_model_linear[15,2] <- 0.3576 #provided by article

diff_f_linear <- (ff_model_linear[15,2]-ff_model_linear[4,2])/11

for (i in 5:14) {
  ff_model_linear[i,2] <- ff_model_linear[i-1,2] + diff_f_linear
}

#estimating age specific fertility rates for males using linear model
fm_model_linear <- data.frame(matrix(ncol=2, nrow = 15))
fm_model_linear[,1] <- years

fm_model_linear[1:4,2] <- 0.4713115 #provided by article
fm_model_linear[15,2] <- 0.36 #provided by article

diff_m_linear <- (fm_model_linear[15,2] - fm_model_linear[4,2])/11

for (i in 5:14) {
  fm_model_linear[i,2] <- fm_model_linear[i-1,2] + diff_m_linear
}

#female fertility rates
#185+108+118+77 = 488
#221/488 = 0.4528689

Native.F.fertility.linear <- matrix(data = 0, nrow = 18, ncol = 16)

Native.F.fertility.linear[5:8,1:5] <- ff_model_linear$X2[1]
Native.F.fertility.linear[5:8,6] <- ff_model_linear$X2[5]
Native.F.fertility.linear[5:8,7] <- ff_model_linear$X2[6]
Native.F.fertility.linear[5:8,8] <- ff_model_linear$X2[7]
Native.F.fertility.linear[5:8,9] <- ff_model_linear$X2[8]
Native.F.fertility.linear[5:8,10] <- ff_model_linear$X2[9]
Native.F.fertility.linear[5:8,11] <- ff_model_linear$X2[10]
Native.F.fertility.linear[5:8,12] <- ff_model_linear$X2[11]
Native.F.fertility.linear[5:8,13] <- ff_model_linear$X2[12]
Native.F.fertility.linear[5:8,14] <- ff_model_linear$X2[13]
Native.F.fertility.linear[5:8,15] <- ff_model_linear$X2[14]
Native.F.fertility.linear[5:8,16] <- ff_model_linear$X2[15]

#male fertility rates
#230/488 = 0.4713115

Native.M.fertility.linear <- matrix(data = 0, nrow = 18, ncol = 16)

Native.M.fertility.linear[5:8,1:5] <- fm_model_linear$X2[1]
Native.M.fertility.linear[5:8,6] <- fm_model_linear$X2[5]
Native.M.fertility.linear[5:8,7] <- fm_model_linear$X2[6]
Native.M.fertility.linear[5:8,8] <- fm_model_linear$X2[7]
Native.M.fertility.linear[5:8,9] <- fm_model_linear$X2[8]
Native.M.fertility.linear[5:8,10] <- fm_model_linear$X2[9]
Native.M.fertility.linear[5:8,11] <- fm_model_linear$X2[10]
Native.M.fertility.linear[5:8,12] <- fm_model_linear$X2[11]
Native.M.fertility.linear[5:8,13] <- fm_model_linear$X2[12]
Native.M.fertility.linear[5:8,14] <- fm_model_linear$X2[13]
Native.M.fertility.linear[5:8,15] <- fm_model_linear$X2[14]
Native.M.fertility.linear[5:8,16] <- fm_model_linear$X2[15]

#estimating age specific fertility rates for females using exponential model
ff_model_exp <- data.frame(matrix(ncol=2, nrow = 15))
rownames(ff_model_exp) <- years
ff_model_exp[,1] <- 1:15

ff_model_exp[1:4,2] <- 0.4528689 #provided by article
ff_model_exp[15,2] <- 0.3576 #provided by article

for (i in 5:14) {
  ff_model_exp[i,2] <- -exp(ff_model_exp[i,1]*0.0080226478) + 1.48548
}

#modify values to increase difference with linear model
ff_model_exp$X3 <- NA

for (i in 1:15) {
  if (i <= 4) {ff_model_exp[i,3] <- ff_model_exp[i,2]}
  else if (i==15) {ff_model_exp[i,3] <- ff_model_exp[i,2]}
  else {ff_model_exp[i,3] <- ff_model_exp[i,2] + 15*(ff_model_exp[i,2]-ff_model_linear[i,2])}
}

#estimating age specific fertility rates for males using exponential model
fm_model_exp <- data.frame(matrix(ncol=2, nrow = 15))
rownames(fm_model_exp) <- years
fm_model_exp[,1] <- 1:15

fm_model_exp[1:4,2] <- 0.4713115 #provided by article
fm_model_exp[15,2] <- 0.36 #provided by article

for (i in 5:14) {
  fm_model_exp[i,2] <- -exp(fm_model_exp[i,1]*0.00926281) + 1.509058 #values from graphical analysis
}

#modify values to increase difference with linear model
fm_model_exp$X3 <- NA

for (i in 1:15) {
  if (i <= 4) {fm_model_exp[i,3] <- fm_model_exp[i,2]}
  else if (i==15) {fm_model_exp[i,3] <- fm_model_exp[i,2]}
  else {fm_model_exp[i,3] <- fm_model_exp[i,2] + 15*(fm_model_exp[i,2]-fm_model_linear[i,2])}
}

#female fertility rates
#185+108+118+77 = 488
#221/488 = 0.4528689

Native.F.fertility.exp <- matrix(data = 0, nrow = 18, ncol = 16)

Native.F.fertility.exp[5:8,1:5] <- ff_model_exp$X3[1]
Native.F.fertility.exp[5:8,6] <- ff_model_exp$X3[5]
Native.F.fertility.exp[5:8,7] <- ff_model_exp$X3[6]
Native.F.fertility.exp[5:8,8] <- ff_model_exp$X3[7]
Native.F.fertility.exp[5:8,9] <- ff_model_exp$X3[8]
Native.F.fertility.exp[5:8,10] <- ff_model_exp$X3[9]
Native.F.fertility.exp[5:8,11] <- ff_model_exp$X3[10]
Native.F.fertility.exp[5:8,12] <- ff_model_exp$X3[11]
Native.F.fertility.exp[5:8,13] <- ff_model_exp$X3[12]
Native.F.fertility.exp[5:8,14] <- ff_model_exp$X3[13]
Native.F.fertility.exp[5:8,15] <- ff_model_exp$X3[14]
Native.F.fertility.exp[5:8,16] <- ff_model_exp$X3[15]

#male fertility rates
#230/488 = 0.4713115

Native.M.fertility.exp <- matrix(data = 0, nrow = 18, ncol = 16)

Native.M.fertility.exp[5:8,1:5] <- fm_model_exp$X3[1]
Native.M.fertility.exp[5:8,6] <- fm_model_exp$X3[5]
Native.M.fertility.exp[5:8,7] <- fm_model_exp$X3[6]
Native.M.fertility.exp[5:8,8] <- fm_model_exp$X3[7]
Native.M.fertility.exp[5:8,9] <- fm_model_exp$X3[8]
Native.M.fertility.exp[5:8,10] <- fm_model_exp$X3[9]
Native.M.fertility.exp[5:8,11] <- fm_model_exp$X3[10]
Native.M.fertility.exp[5:8,12] <- fm_model_exp$X3[11]
Native.M.fertility.exp[5:8,13] <- fm_model_exp$X3[12]
Native.M.fertility.exp[5:8,14] <- fm_model_exp$X3[13]
Native.M.fertility.exp[5:8,15] <- fm_model_exp$X3[14]
Native.M.fertility.exp[5:8,16] <- fm_model_exp$X3[15]

#estimating age specific fertility rates for females using log model
ff_model_log <- data.frame(matrix(ncol=2, nrow = 15))
rownames(ff_model_log) <- years
ff_model_log[,1] <- 1:15

ff_model_log[1:4,2] <- 0.4528689 #provided by article
ff_model_log[15,2] <- 0.3576 #provided by article

#exp((log(4)*0.3576/0.4528689 - log(15))/(1-0.3576/0.4528689)) = 0.0004669099
#-log(4*0.0004669099)/0.4528689 = 13.87395

for (i in 5:14) {
  ff_model_log[i,2] <- -log(0.0004669099*ff_model_log[i,1])/13.87395
}

#estimating age specific fertility rates for males using log model
fm_model_log <- data.frame(matrix(ncol=2, nrow = 15))
rownames(fm_model_log) <- years
fm_model_log[,1] <- 1:15

fm_model_log[1:4,2] <- 0.4713115 #provided by article
fm_model_log[15,2] <- 0.36 #provided by article

#exp((log(4)*0.36/0.4713115 - log(15))/(1-0.36/0.4713115)) = 0.0009276746
#-log(4*0.0009276746)/0.4713115 = 11.87439

for (i in 5:14) {
  fm_model_log[i,2] <- -log(0.0009276746*fm_model_log[i,1])/11.87439
}

#female fertility rates
#185+108+118+77 = 488
#221/488 = 0.4528689

Native.F.fertility.log <- matrix(data = 0, nrow = 18, ncol = 16)

Native.F.fertility.log[5:8,1:5] <- ff_model_log$X2[1]
Native.F.fertility.log[5:8,6] <- ff_model_log$X2[5]
Native.F.fertility.log[5:8,7] <- ff_model_log$X2[6]
Native.F.fertility.log[5:8,8] <- ff_model_log$X2[7]
Native.F.fertility.log[5:8,9] <- ff_model_log$X2[8]
Native.F.fertility.log[5:8,10] <- ff_model_log$X2[9]
Native.F.fertility.log[5:8,11] <- ff_model_log$X2[10]
Native.F.fertility.log[5:8,12] <- ff_model_log$X2[11]
Native.F.fertility.log[5:8,13] <- ff_model_log$X2[12]
Native.F.fertility.log[5:8,14] <- ff_model_log$X2[13]
Native.F.fertility.log[5:8,15] <- ff_model_log$X2[14]
Native.F.fertility.log[5:8,16] <- ff_model_log$X2[15]

#male fertility rates
#230/488 = 0.4713115

Native.M.fertility.log <- matrix(data = 0, nrow = 18, ncol = 16)

Native.M.fertility.log[5:8,1:5] <- fm_model_log$X2[1]
Native.M.fertility.log[5:8,6] <- fm_model_log$X2[5]
Native.M.fertility.log[5:8,7] <- fm_model_log$X2[6]
Native.M.fertility.log[5:8,8] <- fm_model_log$X2[7]
Native.M.fertility.log[5:8,9] <- fm_model_log$X2[8]
Native.M.fertility.log[5:8,10] <- fm_model_log$X2[9]
Native.M.fertility.log[5:8,11] <- fm_model_log$X2[10]
Native.M.fertility.log[5:8,12] <- fm_model_log$X2[11]
Native.M.fertility.log[5:8,13] <- fm_model_log$X2[12]
Native.M.fertility.log[5:8,14] <- fm_model_log$X2[13]
Native.M.fertility.log[5:8,15] <- fm_model_log$X2[14]
Native.M.fertility.log[5:8,16] <- fm_model_log$X2[15]