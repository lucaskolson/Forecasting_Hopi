#creating lifetable for Hopi Census 1937
HopiCensus1937 <- read_xlsx("Hopi Census 1937.xlsx")

#modifying age brackets
HopiCensus1937[18,2:4] <- HopiCensus1937[18,2:4] + HopiCensus1937[19,2:4]
HopiCensus1937 <- HopiCensus1937[1:18,]
HopiCensus1937[18,1] <- "85 and over"

HopiCensus1937M <- HopiCensus1937[,1:2]
HopiCensus1937F <- HopiCensus1937[,c(1,3)]

colnames(HopiCensus1937M) <- c("age", "lx")
colnames(HopiCensus1937F) <- c("age", "lx")

# create dfs sf1 through sf15, each containing 1000 simulations of brass model for females
for (i in 1:15) {
  foo <- LT_all[[2*i]]$L[1]/(5*LT_all[[2*i]]$lx[1]) #help file for make.Leslie.matrix: "The first row should be nL0/(n*l0)"
  
  foo <- data.frame(matrix(foo, nrow=1,ncol=1000)) #create data frame with 1 row and columns equal to the number of simulations
  
  fob <- simulate(eval(parse(text = paste0("logit.", years[i], ".f"))), nsim=1000, seed=1) #simulate values for brass relational model
  
  fob <- 1/(1+exp(-2*fob)) #convert logits back to survival probabilities
  
  colnames(foo) <- colnames(fob)
  
  assign(paste0("sf", i), rbind(foo, fob)) #combine "foo" for first row with simulations of brass models
  
}


# create dfs sm1 through sm15, each containing 1000 simulations of brass model for males
for (i in 1:15) {
  foo <- LT_all[[2*i-1]]$L[1]/(5*LT_all[[2*i-1]]$lx[1]) #help file for make.Leslie.matrix: "The first row should be nL0/(n*l0)"
  
  foo <- data.frame(matrix(foo, nrow=1,ncol=1000)) #create data frame with 1 row and columns equal to the number of simulations
  
  fob <- simulate(eval(parse(text = paste0("logit.", years[i], ".m"))), nsim=1000, seed=1) #simulate values for brass relational model
  
  fob <- 1/(1+exp(-2*fob)) #convert logits back to survival probabilities
  
  colnames(foo) <- colnames(fob)
  
  assign(paste0("sm", i), rbind(foo, fob)) #combine "foo" for first row with simulations of brass models
}


#create list of lists for males and females
simF <- list(sf1, sf2, sf3, sf4, sf5, sf6, sf7, sf8, sf9, sf10, sf11, sf12, sf13, sf14, sf15)
simM <- list(sm1, sm2, sm3, sm4, sm5, sm6, sm7, sm8, sm9, sm10, sm11, sm12, sm13, sm14, sm15)

#create empty df to store simulated leslie matrix, females
sim_leslie_F_linear <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(sim_leslie_F_linear) <- years
sim_leslie_F_linear$`1940` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1945` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1950` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1955` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1960` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1965` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1970` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1975` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1980` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1985` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1990` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`1995` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`2000` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`2005` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_linear$`2010` <- list(matrix(nrow=18, ncol=18))


#for (i in colnames(grrr)) {
#  grrr <- grrr %>% 
#    mutate(i = list(matrix(nrow=18, ncol=18)))
#}

#grrr <- grrr %>% 
#  mutate(`1940` = list(matrix(nrow=18, ncol=18)))

#sapply(grrr, mutate(list(matrix(nrow=18, ncol=18))) 

#for (i in colnames(grrr)) {
#  grrr$i <- list(matrix(nrow=18, ncol=18))
#}

#sim_leslie_F_linear <- tribble(
#  ~`1940`, ~`1945`, ~`1950`, ~`1955`, ~`1960`, ~`1965`, ~`1970`, ~`1975`, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
# matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
# matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18)
#)



for (j in 1:15) {#per period
  for (i in 1:1000) {#per simulation
    
    sim_leslie_F_linear[[i,j]] <- make.leslie.matrix(pop = LT_all[[2*j]]$lx,
                                                          surv = simF[[j]][,i],
                                                          fert = Native.F.fertility.linear[,j+1],
                                                          srb = LT_all[[2*j]]$lx[1]/LT_all[[2*j]]$lx[1],
                                                          age.int = 5)
    
    sim_leslie_F_linear[[i,j]][1,4:8] <- Native.F.fertility.linear[5:9,j+1] #readjusting fertility rates
    
  }}

#create empty df to store simulated leslie matrix, males
sim_leslie_M_linear <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(sim_leslie_M_linear) <- years
sim_leslie_M_linear$`1940` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1945` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1950` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1955` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1960` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1965` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1970` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1975` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1980` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1985` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1990` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`1995` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`2000` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`2005` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_linear$`2010` <- list(matrix(nrow=18, ncol=18))

#sim_leslie_M_linear <- tribble(
#  ~`1940`, ~`1945`, ~`1950`, ~`1955`, ~`1960`, ~`1965`, ~`1970`, ~`1975`, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
# matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18)
#)



for (j in 1:15) {#per period
  for (i in 1:1000) {#per simulation
    
    sim_leslie_M_linear[[i,j]] <- make.leslie.matrix(pop = LT_all[[2*j-1]]$lx,
                                                          surv = simM[[j]][,i],
                                                          fert = Native.M.fertility.linear[,j+1],
                                                          srb = LT_all[[2*j-1]]$lx[1]/LT_all[[2*j-1]]$lx[1],
                                                          age.int = 5)
    
    sim_leslie_M_linear[[i,j]][1,4:8] <- Native.M.fertility.linear[5:9,j+1] #readjusting fertility rates
    
  }}

projsF_linear <- data.frame(matrix(nrow = 1000, ncol = 5))
colnames(projsF_linear) <- years[11:15]
projsF_linear$`1990` <- list(matrix(nrow=18, ncol=1))
projsF_linear$`1995` <- list(matrix(nrow=18, ncol=1))
projsF_linear$`2000` <- list(matrix(nrow=18, ncol=1))
projsF_linear$`2005` <- list(matrix(nrow=18, ncol=1))
projsF_linear$`2010` <- list(matrix(nrow=18, ncol=1))

#projsF_linear <- tribble(
#  ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#  matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1)
#)

projsM_linear <- data.frame(matrix(nrow = 1000, ncol = 5))
colnames(projsM_linear) <- years[11:15]
projsM_linear$`1990` <- list(matrix(nrow=18, ncol=1))
projsM_linear$`1995` <- list(matrix(nrow=18, ncol=1))
projsM_linear$`2000` <- list(matrix(nrow=18, ncol=1))
projsM_linear$`2005` <- list(matrix(nrow=18, ncol=1))
projsM_linear$`2010` <- list(matrix(nrow=18, ncol=1))

# projsM_linear <- tribble(
#   ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1)
# )

for (i in 1:1000) {
  #female forecast to 1990
  projsF_linear[[i,1]] <- sim_leslie_F_linear[[i,11]]%*%sim_leslie_F_linear[[i,10]]%*%sim_leslie_F_linear[[i,9]]%*%sim_leslie_F_linear[[i,8]]%*%sim_leslie_F_linear[[i,7]]%*%sim_leslie_F_linear[[i,6]]%*%sim_leslie_F_linear[[i,5]]%*%sim_leslie_F_linear[[i,4]]%*%sim_leslie_F_linear[[i,3]]%*%sim_leslie_F_linear[[i,2]]%*%sim_leslie_F_linear[[i,1]]%*%HopiCensus1937F$lx
  
  #male forecast to 1990
  projsM_linear[[i,1]] <- sim_leslie_M_linear[[i,11]]%*%sim_leslie_M_linear[[i,10]]%*%sim_leslie_M_linear[[i,9]]%*%sim_leslie_M_linear[[i,8]]%*%sim_leslie_M_linear[[i,7]]%*%sim_leslie_M_linear[[i,6]]%*%sim_leslie_M_linear[[i,5]]%*%sim_leslie_M_linear[[i,4]]%*%sim_leslie_M_linear[[i,3]]%*%sim_leslie_M_linear[[i,2]]%*%sim_leslie_M_linear[[i,1]]%*%HopiCensus1937M$lx
}

for (i in 1:1000) {
  #female forecast to 1995
  projsF_linear[[i,2]] <- sim_leslie_F_linear[[i,12]]%*%projsF_linear[[i,1]]
  
  #male forecast to 1995
  projsM_linear[[i,2]] <- sim_leslie_M_linear[[i,12]]%*%projsM_linear[[i,1]]
}

for (i in 1:1000) {
  #female forecast to 2000
  projsF_linear[[i,3]] <- sim_leslie_F_linear[[i,13]]%*%projsF_linear[[i,2]]
  
  #male forecast to 2000
  projsM_linear[[i,3]] <- sim_leslie_M_linear[[i,13]]%*%projsM_linear[[i,2]]
}

for (i in 1:1000) {
  #female forecast to 2005
  projsF_linear[[i,4]] <- sim_leslie_F_linear[[i,14]]%*%projsF_linear[[i,3]]
  
  #male forecast to 2005
  projsM_linear[[i,4]] <- sim_leslie_M_linear[[i,14]]%*%projsM_linear[[i,3]]
}

for (i in 1:1000) {
  #female forecast to 2010
  projsF_linear[[i,5]] <- sim_leslie_F_linear[[i,15]]%*%projsF_linear[[i,4]]
  
  #male forecast to 2010
  projsM_linear[[i,5]] <- sim_leslie_M_linear[[i,15]]%*%projsM_linear[[i,4]]
}

#create a matrix for the sum of all the male and female projections
projs_totals_linear <- matrix(nrow = 1000, ncol=5)

for (i in 1:1000){
  for (j in 1:5) {
    projs_totals_linear[i,j] <- sum(projsF_linear[[i,j]]) + sum(projsF_linear[[i,j]])
  }}

colnames(projs_totals_linear) <- c(seq(1990,2010,5))

#---------------------------------------------------------------------------------------


#create empty df to store simulated leslie matrix, females
sim_leslie_F_exp <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(sim_leslie_F_exp) <- years
sim_leslie_F_exp$`1940` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1945` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1950` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1955` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1960` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1965` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1970` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1975` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1980` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1985` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1990` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`1995` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`2000` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`2005` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_exp$`2010` <- list(matrix(nrow=18, ncol=18))

#sim_leslie_F_exp <- tribble(
#  ~`1940`, ~`1945`, ~`1950`, ~`1955`, ~`1960`, ~`1965`, ~`1970`, ~`1975`, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18)
#)

for (j in 1:15) {#per period
  for (i in 1:1000) {#per simulation
    
    sim_leslie_F_exp[[i,j]] <- make.leslie.matrix(pop = LT_all[[2*j]]$lx,
                                                          surv = simF[[j]][,i],
                                                          fert = Native.F.fertility.exp[,j+1],
                                                          srb = LT_all[[2*j]]$lx[1]/LT_all[[2*j]]$lx[1],
                                                          age.int = 5)
    
    sim_leslie_F_exp[[i,j]][1,4:8] <- Native.F.fertility.exp[5:9,j+1] #readjusting fertility rates
    
  }}

#create empty df to store simulated leslie matrix, males
sim_leslie_M_exp <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(sim_leslie_M_exp) <- years
sim_leslie_M_exp$`1940` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1945` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1950` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1955` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1960` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1965` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1970` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1975` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1980` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1985` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1990` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`1995` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`2000` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`2005` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_exp$`2010` <- list(matrix(nrow=18, ncol=18))

#sim_leslie_M_exp <- tribble(
#  ~`1940`, ~`1945`, ~`1950`, ~`1955`, ~`1960`, ~`1965`, ~`1970`, ~`1975`, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#  matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18)
#)



for (j in 1:15) {#per period
  for (i in 1:1000) {#per simulation
    
    sim_leslie_M_exp[[i,j]] <- make.leslie.matrix(pop = LT_all[[2*j-1]]$lx,
                                                          surv = simM[[j]][,i],
                                                          fert = Native.M.fertility.exp[,j+1],
                                                          srb = LT_all[[2*j-1]]$lx[1]/LT_all[[2*j-1]]$lx[1],
                                                          age.int = 5)
    
    sim_leslie_M_exp[[i,j]][1,4:8] <- Native.M.fertility.exp[5:9,j+1] #readjusting fertility rates
    
  }}

projsF_exp <- data.frame(matrix(nrow = 1000, ncol = 5))
colnames(projsF_exp) <- years[11:15]
projsF_exp$`1990` <- list(matrix(nrow=18, ncol=1))
projsF_exp$`1995` <- list(matrix(nrow=18, ncol=1))
projsF_exp$`2000` <- list(matrix(nrow=18, ncol=1))
projsF_exp$`2005` <- list(matrix(nrow=18, ncol=1))
projsF_exp$`2010` <- list(matrix(nrow=18, ncol=1))

# projsF_exp <- tribble(
#   ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1)
# )

projsM_exp <- data.frame(matrix(nrow = 1000, ncol = 5))
colnames(projsM_exp) <- years[11:15]
projsM_exp$`1990` <- list(matrix(nrow=18, ncol=1))
projsM_exp$`1995` <- list(matrix(nrow=18, ncol=1))
projsM_exp$`2000` <- list(matrix(nrow=18, ncol=1))
projsM_exp$`2005` <- list(matrix(nrow=18, ncol=1))
projsM_exp$`2010` <- list(matrix(nrow=18, ncol=1))

# projsM_exp <- tribble(
#   ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1)
# )

for (i in 1:1000) {
  #female forecast to 1990
  projsF_exp[[i,1]] <- sim_leslie_F_exp[[i,11]]%*%sim_leslie_F_exp[[i,10]]%*%sim_leslie_F_exp[[i,9]]%*%sim_leslie_F_exp[[i,8]]%*%sim_leslie_F_exp[[i,7]]%*%sim_leslie_F_exp[[i,6]]%*%sim_leslie_F_exp[[i,5]]%*%sim_leslie_F_exp[[i,4]]%*%sim_leslie_F_exp[[i,3]]%*%sim_leslie_F_exp[[i,2]]%*%sim_leslie_F_exp[[i,1]]%*%HopiCensus1937F$lx
  
  #male forecast to 1990
  projsM_exp[[i,1]] <- sim_leslie_M_exp[[i,11]]%*%sim_leslie_M_exp[[i,10]]%*%sim_leslie_M_exp[[i,9]]%*%sim_leslie_M_exp[[i,8]]%*%sim_leslie_M_exp[[i,7]]%*%sim_leslie_M_exp[[i,6]]%*%sim_leslie_M_exp[[i,5]]%*%sim_leslie_M_exp[[i,4]]%*%sim_leslie_M_exp[[i,3]]%*%sim_leslie_M_exp[[i,2]]%*%sim_leslie_M_exp[[i,1]]%*%HopiCensus1937M$lx
}

for (i in 1:1000) {
  #female forecast to 1995
  projsF_exp[[i,2]] <- sim_leslie_F_exp[[i,12]]%*%projsF_exp[[i,1]]
  
  #male forecast to 1995
  projsM_exp[[i,2]] <- sim_leslie_M_exp[[i,12]]%*%projsM_exp[[i,1]]
}

for (i in 1:1000) {
  #female forecast to 2000
  projsF_exp[[i,3]] <- sim_leslie_F_exp[[i,13]]%*%projsF_exp[[i,2]]
  
  #male forecast to 2000
  projsM_exp[[i,3]] <- sim_leslie_M_exp[[i,13]]%*%projsM_exp[[i,2]]
}

for (i in 1:1000) {
  #female forecast to 2005
  projsF_exp[[i,4]] <- sim_leslie_F_exp[[i,14]]%*%projsF_exp[[i,3]]
  
  #male forecast to 2005
  projsM_exp[[i,4]] <- sim_leslie_M_exp[[i,14]]%*%projsM_exp[[i,3]]
}

for (i in 1:1000) {
  #female forecast to 2010
  projsF_exp[[i,5]] <- sim_leslie_F_exp[[i,15]]%*%projsF_exp[[i,4]]
  
  #male forecast to 2010
  projsM_exp[[i,5]] <- sim_leslie_M_exp[[i,15]]%*%projsM_exp[[i,4]]
}

#create a matrix for the sum of all the male and female projections
projs_totals_exp <- matrix(nrow = 1000, ncol=5)

for (i in 1:1000){
  for (j in 1:5) {
    projs_totals_exp[i,j] <- sum(projsF_exp[[i,j]]) + sum(projsF_exp[[i,j]])
  }}

colnames(projs_totals_exp) <- c(seq(1990,2010,5))

#---------------------------------------------------------------------------------------


#create empty tribble to store simulated leslie matrix, females
sim_leslie_F_log <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(sim_leslie_F_log) <- years
sim_leslie_F_log$`1940` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1945` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1950` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1955` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1960` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1965` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1970` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1975` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1980` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1985` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1990` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`1995` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`2000` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`2005` <- list(matrix(nrow=18, ncol=18))
sim_leslie_F_log$`2010` <- list(matrix(nrow=18, ncol=18))

# sim_leslie_F_log <- tribble(
#   ~`1940`, ~`1945`, ~`1950`, ~`1955`, ~`1960`, ~`1965`, ~`1970`, ~`1975`, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18)
# )



for (j in 1:15) {#per period
  for (i in 1:1000) {#per simulation
    
    sim_leslie_F_log[[i,j]] <- make.leslie.matrix(pop = LT_all[[2*j]]$lx,
                                                       surv = simF[[j]][,i],
                                                       fert = Native.F.fertility.log[,j+1],
                                                       srb = LT_all[[2*j]]$lx[1]/LT_all[[2*j]]$lx[1],
                                                       age.int = 5)
    
    sim_leslie_F_log[[i,j]][1,4:8] <- Native.F.fertility.log[5:9,j+1] #readjusting fertility rates
    
  }}

#create empty df to store simulated leslie matrix, males
sim_leslie_M_log <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(sim_leslie_M_log) <- years
sim_leslie_M_log$`1940` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1945` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1950` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1955` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1960` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1965` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1970` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1975` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1980` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1985` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1990` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`1995` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`2000` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`2005` <- list(matrix(nrow=18, ncol=18))
sim_leslie_M_log$`2010` <- list(matrix(nrow=18, ncol=18))

# sim_leslie_M_log <- tribble(
#   ~`1940`, ~`1945`, ~`1950`, ~`1955`, ~`1960`, ~`1965`, ~`1970`, ~`1975`, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),
#   matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18),matrix(nrow=18, ncol=18)
# )



for (j in 1:15) {#per period
  for (i in 1:1000) {#per simulation
    
    sim_leslie_M_log[[i,j]] <- make.leslie.matrix(pop = LT_all[[2*j-1]]$lx,
                                                       surv = simM[[j]][,i],
                                                       fert = Native.M.fertility.log[,j+1],
                                                       srb = LT_all[[2*j-1]]$lx[1]/LT_all[[2*j-1]]$lx[1],
                                                       age.int = 5)
    
    sim_leslie_M_log[[i,j]][1,4:8] <- Native.M.fertility.log[5:9,j+1] #readjusting fertility rates
    
  }}

projsF_log <- data.frame(matrix(nrow = 1000, ncol = 5))
colnames(projsF_log) <- years[11:15]
projsF_log$`1990` <- list(matrix(nrow=18, ncol=1))
projsF_log$`1995` <- list(matrix(nrow=18, ncol=1))
projsF_log$`2000` <- list(matrix(nrow=18, ncol=1))
projsF_log$`2005` <- list(matrix(nrow=18, ncol=1))
projsF_log$`2010` <- list(matrix(nrow=18, ncol=1))

# projsF_log <- tribble(
#   ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1)
# )

projsM_log <- data.frame(matrix(nrow = 1000, ncol = 5))
colnames(projsM_log) <- years[11:15]
projsM_log$`1990` <- list(matrix(nrow=18, ncol=1))
projsM_log$`1995` <- list(matrix(nrow=18, ncol=1))
projsM_log$`2000` <- list(matrix(nrow=18, ncol=1))
projsM_log$`2005` <- list(matrix(nrow=18, ncol=1))
projsM_log$`2010` <- list(matrix(nrow=18, ncol=1))
# 
# projsM_log <- tribble(
#   ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`,
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),
#   matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1),matrix(nrow=18, ncol=1)
# )

for (i in 1:1000) {
  #female forecast to 1990
  projsF_log[[i,1]] <- sim_leslie_F_log[[i,11]]%*%sim_leslie_F_log[[i,10]]%*%sim_leslie_F_log[[i,9]]%*%sim_leslie_F_log[[i,8]]%*%sim_leslie_F_log[[i,7]]%*%sim_leslie_F_log[[i,6]]%*%sim_leslie_F_log[[i,5]]%*%sim_leslie_F_log[[i,4]]%*%sim_leslie_F_log[[i,3]]%*%sim_leslie_F_log[[i,2]]%*%sim_leslie_F_log[[i,1]]%*%HopiCensus1937F$lx
  
  #male forecast to 1990
  projsM_log[[i,1]] <- sim_leslie_M_log[[i,11]]%*%sim_leslie_M_log[[i,10]]%*%sim_leslie_M_log[[i,9]]%*%sim_leslie_M_log[[i,8]]%*%sim_leslie_M_log[[i,7]]%*%sim_leslie_M_log[[i,6]]%*%sim_leslie_M_log[[i,5]]%*%sim_leslie_M_log[[i,4]]%*%sim_leslie_M_log[[i,3]]%*%sim_leslie_M_log[[i,2]]%*%sim_leslie_M_log[[i,1]]%*%HopiCensus1937M$lx
}

for (i in 1:1000) {
  #female forecast to 1995
  projsF_log[[i,2]] <- sim_leslie_F_log[[i,12]]%*%projsF_log[[i,1]]
  
  #male forecast to 1995
  projsM_log[[i,2]] <- sim_leslie_M_log[[i,12]]%*%projsM_log[[i,1]]
}

for (i in 1:1000) {
  #female forecast to 2000
  projsF_log[[i,3]] <- sim_leslie_F_log[[i,13]]%*%projsF_log[[i,2]]
  
  #male forecast to 2000
  projsM_log[[i,3]] <- sim_leslie_M_log[[i,13]]%*%projsM_log[[i,2]]
}

for (i in 1:1000) {
  #female forecast to 2005
  projsF_log[[i,4]] <- sim_leslie_F_log[[i,14]]%*%projsF_log[[i,3]]
  
  #male forecast to 2005
  projsM_log[[i,4]] <- sim_leslie_M_log[[i,14]]%*%projsM_log[[i,3]]
}

for (i in 1:1000) {
  #female forecast to 2010
  projsF_log[[i,5]] <- sim_leslie_F_log[[i,15]]%*%projsF_log[[i,4]]
  
  #male forecast to 2010
  projsM_log[[i,5]] <- sim_leslie_M_log[[i,15]]%*%projsM_log[[i,4]]
}

#create a matrix for the sum of all the male and female projections
projs_totals_log <- matrix(nrow = 1000, ncol=5)

for (i in 1:1000){
  for (j in 1:5) {
    projs_totals_log[i,j] <- sum(projsF_log[[i,j]]) + sum(projsF_log[[i,j]])
  }}

colnames(projs_totals_log) <- c(seq(1990,2010,5))