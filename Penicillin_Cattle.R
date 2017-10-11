# { 
#   Monte Carlo Analysis based on Penicillin PBPK model for Cattle (flow-limited model, linear metabolism equation, plasma protein binding)
#   The PBPK model code is based on the Oxytetracycline.mmd from Zhoumeng Lin
# }
# Loading the R package
library(deSolve) # R package for numerical treatment of systems of differential equations. 
library(truncnorm) # R package for Truncated normal distribution
library(EnvStats) # Package for Environmental Statistics, Including US EPA Guidance
library(ggplot2) # R package for ploting the data

N = 1000 # Number of iterations in the Monte Carlo simulation
starttime <- 0  # start time(h)
stoptime <- 24*15 # simulation stop time that is depending on the withdrawal time
dtout = 0.01
Time <- seq(starttime, stoptime, dtout)
#{Parameters for Various Exposure Scenarios}
doseX = 1
PDOSEsc = 0			# (mg/kg)
PDOSEim = 6.5 * doseX			# (mg/kg)

CL =  CLu =  CK =  CM =  CF = CV  = matrix(NA,(stoptime / dtout + 1), N)
#plot.new()
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
plot(0,type='n',xlab="Time (h)", ylab="Concentration-Liver", xlim=c(0, stoptime), ylim=c(0, 5))
for (i in 1:N) {
  set.seed(i) # set random seed so that the simulation result is reproducible, because randomly generated data is same if you set same random seed.
  ##{Physiological Parameters}
  # Blood Flow Rates
  #QCC = 5.970 			# L/h/kg, Cardiac Output (1960 Doyle)
  QCC = rtruncnorm(n = 1, a = qnorm(0.025, mean =5.97, sd = 1.99), 
                   b = qnorm(0.975, mean =5.97, sd = 1.99), mean = 5.97, sd = 1.99)  # Cardiac output index (L/h/kg)
  # Fracion of blood flow to organs (unitless) 
  #QLC =  0.405			# Fraction of blood flow to the liver (1996 Lescoat, 1960 Dlyle)
  QLC = rtruncnorm(n = 1, a = qnorm(0.025, mean =0.405, sd = 0.1942), 
                   b = qnorm(0.975, mean =0.405, sd = 0.1942), mean = 0.405, sd = 0.1942)
  #QKC = 0.090			# Fraction of blood flow to the kidneys (2016 Lin)
  QKC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.090, sd = 0.027), 
                   b = qnorm(0.975, mean = 0.090, sd = 0.027), mean = 0.090, sd = 0.027)
  #QMC = 0.180			# Fraction of blood flow to the muscle (2016 Lin)
  QMC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.180, sd = 0.054 ), 
                   b = qnorm(0.975, mean = 0.180, sd = 0.054 ), mean = 0.180, sd = 0.054 )
  #QFC = 0.080			# Fraction of blood flow to the fat (2016 Lin)
  QFC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.080, sd = 0.024), 
                   b = qnorm(0.975, mean = 0.080, sd = 0.024), mean = 0.080, sd = 0.024)
  QLuC = 1			# Fraction of blood flow to the lung considered to be 1
  #QrestC = 0.245			# Fraction of blood flow to the rest of body (total sum equals to 1)
  QrestC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.245, sd = 0.0736), 
                      b = qnorm(0.975, mean = 0.245, sd = 0.0736), mean = 0.245, sd = 0.0736)
  sum.Q <- QLC + QKC + QMC + QFC + QrestC # sum up cardiac output fraction
  QLC <- QLC/sum.Q # adjusted blood flow rate fraction to liver
  QKC <- QKC/sum.Q # adjusted blood flow rate fraction to kidney
  QMC <- QMC/sum.Q # adjusted blood flow rate fraction to muscle
  QFC <- QFC/sum.Q # adjusted blood flow rate fraction to fat
  QrestC <- QrestC/sum.Q # adjusted blood flow rate fraction to rest of body
  # Tissue Volumes 
  #BW = 299.96			# Body Weight (kg) (2011 Mirzaei)
  BW.CV <- 0.154
  BW.mean = 299.96	 # mean bodyweight 
  BW.sd = BW.mean * BW.CV # standard deviation of bodyweight
  BW = rtruncnorm(n = 1, a = qnorm(0.025, mean = BW.mean, sd = BW.sd), 
                  b = qnorm(0.975, mean = BW.mean, sd = BW.sd), mean = BW.mean, sd = BW.sd) 
  # Fractional organ tissue volumes (unitless)
  #VLC = 0.014			# Fractional liver tissue (1933 Swett)
  VLC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.014, sd = 0.00163),
                   b =qnorm(0.975, mean = 0.014, sd = 0.00163) , 
                   mean = 0.014, sd = 0.00163)
  #VKC = 0.002			# Fractional kidney tissue (1933 Swett)
  VKC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.002, sd = 4.321e-4),
                   b = qnorm(0.975, mean = 0.002, sd = 4.321e-4), mean = 0.002, sd = 4.321e-4)
  
  #VFC = 0.150			# Fractional fat tissue (2016 Lin, 2014 Leavens)
  VFC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.150, sd = 4.5e-2), 
                   b = qnorm(0.975, mean = 0.150, sd = 4.5e-2), mean = 0.150, sd = 4.5e-2)
  #VMC = 0.270			# Fractional muscle tissue (2016 Lin, 2014 Leavens)
  VMC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.270, sd = 8.1e-2 ), 
                   b = qnorm(0.975, mean = 0.270, sd = 8.1e-2 ), mean = 0.270, sd = 8.1e-2 )
  #VLuC = 0.008			# Fractional lung tissue (2016 Lin, 2014 Leavens)
  VLuC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.008, sd = 1.696e-3), 
                    b = qnorm(0.975, mean = 0.008, sd = 1.696e-3), mean = 0.008, sd = 1.696e-3)
  #VvenC = 0.030			# Venous blood volume, fraction of blood volume (2016 Lin# 2008 Leavens)
  VvenC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.030, sd = 8.88e-3), 
                     b = qnorm(0.975, mean = 0.030, sd = 8.88e-3), mean = 0.030, sd = 8.88e-3)
  #VartC = 0.010			# Arterial blood volume, fraction of blood volume (2016 Lin# 2008 Leavens)
  VartC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.010, sd = 3.12e-3), 
                     b = qnorm(0.975, mean = 0.010, sd = 3.12e-3), mean = 0.010, sd = 3.12e-3)
  #VrestC = 0.516			# Fractional rest of body (total sum equals to 1)
  VrestC = rtruncnorm(n = 1, a = qnorm(0.025, mean = 0.516, sd = 0.1548), 
                      b = qnorm(0.975, mean = 0.516, sd = 0.1548), mean = 0.516, sd = 0.1548)
  sum.V <- VLC + VKC + VMC + VFC + VLuC + VrestC + VartC + VrestC# sum up the tissue volumes
  VLC <- VLC/sum.V # adjusted fraction of tissue volume of liver
  VKC <- VKC/sum.V # adjusted fraction of tissue volume of kidney
  VMC <- VMC/sum.V # adjusted fraction of tissue volume of muscle
  VFC <- VFC/sum.V # adjusted fraction of tissue volume of fat
  VLuC <- VLuC/sum.V # adjusted fraction of tissue volume of lung
  VartC <- VartC/sum.V
  VvenC <- VvenC/sum.V
  VrestC <- VrestC/sum.V # adjusted fraction of tissue volume of rest of body
  #{Mass Transfer Parameters (Chemical-Specific Parameters)}
  # Partition Coefficients (PC, tissue:plasma)
  #PL = 3.000			# Liver:plasma PC (0.157, Tsuji et al., 1983, Table 4, in rats)
  PL.CV <- 0.2 # sd/mean
  PL.mean <- 3 # mean value
  PL.sd <- PL.CV * PL.mean # standard deviation
  sd.log.PL <- sqrt(log(1 + PL.sd^2/PL.mean^2)) # standard deviation of lognormal distribution
  m.log.PL <- log(PL.mean) - 0.5 * sd.log.PL^2 # mean of lognormal distribution
  PL = rlnormTrunc(1, meanlog = m.log.PL, sdlog = sd.log.PL, 
                   min = qlnorm(0.025, meanlog = m.log.PL, sdlog = sd.log.PL),
                   max = qlnorm(0.975, meanlog = m.log.PL, sdlog = sd.log.PL))
  PK.CV <- 0.2 # sd/mean
  PK.mean <- 2.500 # mean value
  PK.sd <- PK.CV * PK.mean # standard deviation
  sd.log.PK <- sqrt(log(1 + PK.sd^2/PK.mean^2)) # standard deviation of lognormal distribution
  m.log.PK <- log(PK.mean)-0.5 * sd.log.PK^2 # mean of lognormal distribution
  PK = rlnormTrunc(1, meanlog = m.log.PK, sdlog = sd.log.PK, 
                   min = qlnorm(0.025, meanlog = m.log.PK, sdlog = sd.log.PK),
                   max = qlnorm(0.975, meanlog = m.log.PK, sdlog = sd.log.PK))
  PM.CV <- 0.2 # sd/mean
  PM.mean <- 0.300 # mean value
  PM.sd <- PM.CV * PM.mean # standard deviation
  sd.log.PM <- sqrt(log(1 + PM.sd^2/PM.mean^2)) # standard deviation of lognormal distribution
  m.log.PM <- log(PM.mean)-0.5 * sd.log.PM^2 # mean of lognormal distribution
  PM = rlnormTrunc(1, meanlog = m.log.PM, sdlog = sd.log.PM, 
                   min = qlnorm(0.025, meanlog = m.log.PM, sdlog = sd.log.PM),
                   max = qlnorm(0.975, meanlog = m.log.PM, sdlog = sd.log.PM))
  PF.CV <- 0.2 # sd/mean
  PF.mean <- 0.040 # mean value
  PF.sd <- PF.CV * PF.mean # standard deviation
  sd.log.PF <- sqrt(log(1 + PF.sd^2/PF.mean^2)) # standard deviation of lognormal distribution
  m.log.PF <- log(PF.mean)-0.5 * sd.log.PF^2 # mean of lognormal distribution
  PF = rlnormTrunc(1, meanlog = m.log.PF, sdlog = sd.log.PF, 
                   min = qlnorm(0.025, meanlog = m.log.PF, sdlog = sd.log.PF),
                   max = qlnorm(0.975, meanlog = m.log.PF, sdlog = sd.log.PF))
  PLu.CV <- 0.2 # sd/mean
  PLu.mean <- 0.180 # mean value
  PLu.sd <- PLu.CV * PLu.mean # standard deviation
  sd.log.PLu <- sqrt(log(1 + PLu.sd^2/PLu.mean^2)) # standard deviation of lognormal distribution
  m.log.PLu <- log(PLu.mean)-0.5 * sd.log.PLu^2 # mean of lognormal distribution
  PLu = rlnormTrunc(1, meanlog = m.log.PLu, sdlog = sd.log.PLu, 
                    min = qlnorm(0.025, meanlog = m.log.PLu, sdlog = sd.log.PLu),
                    max = qlnorm(0.975, meanlog = m.log.PLu, sdlog = sd.log.PLu))
  Prest.CV <- 0.2 # sd/mean
  Prest.mean <- 0.479 # mean value
  Prest.sd <- Prest.CV * Prest.mean # standard deviation
  sd.log.Prest <- sqrt(log(1 + Prest.sd^2/Prest.mean^2)) # standard deviation of lognormal distribution
  m.log.Prest <- log(Prest.mean)-0.5 * sd.log.Prest^2 # mean of lognormal distribution
  Prest = rlnormTrunc(1, meanlog = m.log.Prest, sdlog = sd.log.Prest, 
                      min = qlnorm(0.025, meanlog = m.log.Prest, sdlog = sd.log.Prest),
                      max = qlnorm(0.975, meanlog = m.log.Prest, sdlog = sd.log.Prest))
  #{Kinetic Constants}
  # IM Absorption Rate Constants
  #Kim = 0.070			# /h, IM absorption rate constant 
  Kim.CV <- 0.3 # sd/mean
  Kim.mean <- 0.070 # mean value
  Kim.sd <- Kim.CV * Kim.mean # standard deviation
  sd.log.Kim <- sqrt(log(1 + Kim.sd^2/Kim.mean^2)) # standard deviation of lognormal distribution
  m.log.Kim <- log(Kim.mean)-0.5 * sd.log.Kim^2 # mean of lognormal distribution
  Kim = rlnormTrunc(1, meanlog = m.log.Kim, sdlog = sd.log.Kim, 
                    min = qlnorm(0.025, meanlog = m.log.Kim, sdlog = sd.log.Kim),
                    max = qlnorm(0.975, meanlog = m.log.Kim, sdlog = sd.log.Kim))
  #Frac = 0.600
  Frac.CV <- 0.1 # sd/mean
  Frac.mean <- 0.600 # mean value
  Frac.sd <- Frac.CV * Frac.mean # standard deviation
  sd.log.Frac <- sqrt(log(1 + Frac.sd^2/Frac.mean^2)) # standard deviation of lognormal distribution
  m.log.Frac <- log(Frac.mean)-0.5 * sd.log.Frac^2 # mean of lognormal distribution
  Frac = rlnormTrunc(1, meanlog = m.log.Frac, sdlog = sd.log.Frac, 
                     min = qlnorm(0.025, meanlog = m.log.Frac, sdlog = sd.log.Frac),
                     max = qlnorm(0.975, meanlog = m.log.Frac, sdlog = sd.log.Frac))
  #Kdiss = 1e-5 			# /h
  Kdiss.CV <- 0.3 # sd/mean
  Kdiss.mean <- 1e-5 # mean value
  Kdiss.sd <- Kdiss.CV * Kdiss.mean # standard deviation
  sd.log.Kdiss <- sqrt(log(1 + Kdiss.sd^2/Kdiss.mean^2)) # standard deviation of lognormal distribution
  m.log.Kdiss <- log(Kdiss.mean)-0.5 * sd.log.Kdiss^2 # mean of lognormal distribution
  Kdiss = rlnormTrunc(1, meanlog = m.log.Kdiss, sdlog = sd.log.Kdiss, 
                      min = qlnorm(0.025, meanlog = m.log.Kdiss, sdlog = sd.log.Kdiss),
                      max = qlnorm(0.975, meanlog = m.log.Kdiss, sdlog = sd.log.Kdiss))
  # SC Absorption Rate Constants
  #Ksc = 0.020 			# /h, SC absorption rate constant
  Ksc.CV <- 0.3 # sd/mean
  Ksc.mean <- 0.020 # mean value
  Ksc.sd <- Ksc.CV * Ksc.mean # standard deviation
  sd.log.Ksc <- sqrt(log(1 + Ksc.sd^2/Ksc.mean^2)) # standard deviation of lognormal distribution
  m.log.Ksc <- log(Ksc.mean)-0.5 * sd.log.Ksc^2 # mean of lognormal distribution
  Ksc = rlnormTrunc(1, meanlog = m.log.Ksc, sdlog = sd.log.Ksc, 
                    min = qlnorm(0.025, meanlog = m.log.Ksc, sdlog = sd.log.Ksc),
                    max = qlnorm(0.975, meanlog = m.log.Ksc, sdlog = sd.log.Ksc))
  #Fracsc = 0.700
  Fracsc.CV <- 0.1 # sd/mean
  Fracsc.mean <- 0.700 # mean value
  Fracsc.sd <- Fracsc.CV * Fracsc.mean # standard deviation
  sd.log.Fracsc <- sqrt(log(1 + Fracsc.sd^2/Fracsc.mean^2)) # standard deviation of lognormal distribution
  m.log.Fracsc <- log(Fracsc.mean)-0.5 * sd.log.Fracsc^2 # mean of lognormal distribution
  #Fracsc = rlnorm(n = 1, mean = m.log.Fracsc, sd = sd.log.Fracsc) # Liver:plasma PC
  Fracsc = rlnormTrunc(1, meanlog = m.log.Fracsc, sdlog = sd.log.Fracsc, 
                       min = qlnorm(0.025, meanlog = m.log.Fracsc, sdlog = sd.log.Fracsc),
                       max = qlnorm(0.975, meanlog = m.log.Fracsc, sdlog = sd.log.Fracsc))
  #Kdisssc = 1e-4
  Kdisssc.CV <- 0.3 # sd/mean
  Kdisssc.mean <- 1e-4 # mean value
  Kdisssc.sd <- Kdisssc.CV * Kdisssc.mean # standard deviation
  sd.log.Kdisssc <- sqrt(log(1 + Kdisssc.sd^2/Kdisssc.mean^2)) # standard deviation of lognormal distribution
  m.log.Kdisssc <- log(Kdisssc.mean)-0.5 * sd.log.Kdisssc^2 # mean of lognormal distribution
  Kdisssc = rlnormTrunc(1, meanlog = m.log.Kdisssc, sdlog = sd.log.Kdisssc, 
                        min = qlnorm(0.025, meanlog = m.log.Kdisssc, sdlog = sd.log.Kdisssc),
                        max = qlnorm(0.975, meanlog = m.log.Kdisssc, sdlog = sd.log.Kdisssc))
  # Percentage Plasma Protein Binding unitless
  #PB = 0.483			# Percentage of drug bound to plasma proteins (1965 Keen)
  PB.CV <- 0.3 # sd/mean
  PB.mean <- 0.483 # mean value
  PB.sd <- PB.CV * PB.mean # standard deviation
  sd.log.PB <- sqrt(log(1 + PB.sd^2/PB.mean^2)) # standard deviation of lognormal distribution
  m.log.PB <- log(PB.mean)-0.5 * sd.log.PB^2 # mean of lognormal distribution
  PB = rlnormTrunc(1, meanlog = m.log.PB, sdlog = sd.log.PB, 
                   min = qlnorm(0.025, meanlog = m.log.PB, sdlog = sd.log.PB),
                   max = qlnorm(0.975, meanlog = m.log.PB, sdlog = sd.log.PB))
  Free = 1-PB  			# Percentage of drug not bound to plasma protein
  
  #{Metabolic Rate Constant}
  #KmC = 0.0025			# metabolic rate constant
  KmC.CV <- 0.3 # sd/mean
  KmC.mean <- 0.0025 # mean value
  KmC.sd <- KmC.CV * KmC.mean # standard deviation
  sd.log.KmC <- sqrt(log(1 + KmC.sd^2/KmC.mean^2)) # standard deviation of lognormal distribution
  m.log.KmC <- log(KmC.mean)-0.5 * sd.log.KmC^2 # mean of lognormal distribution
  KmC = rlnormTrunc(1, meanlog = m.log.KmC, sdlog = sd.log.KmC, 
                    min = qlnorm(0.025, meanlog = m.log.KmC, sdlog = sd.log.KmC),
                    max = qlnorm(0.975, meanlog = m.log.KmC, sdlog = sd.log.KmC))
  # Urinary Elimination Rate Constants
  #KurineC = 0.450			# L/h/kg
  KurineC.CV <- 0.3 # sd/mean
  KurineC.mean <- 0.450 # mean value
  KurineC.sd <- KurineC.CV * KurineC.mean # standard deviation
  sd.log.KurineC <- sqrt(log(1 + KurineC.sd^2/KurineC.mean^2)) # standard deviation of lognormal distribution
  m.log.KurineC <- log(KurineC.mean)-0.5 * sd.log.KurineC^2 # mean of lognormal distribution
  KurineC = rlnormTrunc(1, meanlog = m.log.KurineC, sdlog = sd.log.KurineC, 
                        min = qlnorm(0.025, meanlog = m.log.KurineC, sdlog = sd.log.KurineC),
                        max = qlnorm(0.975, meanlog = m.log.KurineC, sdlog = sd.log.KurineC))
  
  
  #{Cardiac output and blood flow to tissues (L/h)}
  QC = QCC*BW 			# Cardiac output
  QL = QLC*QC 			# Liver
  QK = QKC*QC 			# Kidney
  QF = QFC*QC 			# Fat
  QM = QMC*QC 			# Muscle
  QLu = QLuC*QC 			# Lung
  QR = QrestC*QC 			# Rest of body
  
  #{Tissue volues (L)}
  VL = VLC*BW 			# Liver
  VK = VKC*BW 			# Kidney
  VF = VFC*BW 			# Fat
  VM = VMC*BW 			# Muscle
  VLu = VLuC*BW 			# Lung
  VR = VrestC*BW 			# Rest of body
  Vven = VvenC*BW 			# Venous Blood
  Vart = VartC*BW 			# Arterial Blood
  
  # Metabolism Rate Constants
  Kmet = KmC*BW
  
  # Urinary Elimination Rate Constants
  Kurine = KurineC*BW
  
  #{Dosing}
  # Dosing caculation based on BW
  DOSEsc = PDOSEsc*BW 	# (mg)
  DOSEim = PDOSEim*BW 	# (mg)
  tlen = dtout
  # Dosing, repeated doses
  tinterval = 24			# Varied dependent on the exposure paradigm (h)
  Tdose = 5 			# times for multiple oral gavage
  pbpkmodel <- function(Time, State, Parameters) {
    with(as.list(c(State, Paras)), {
      dosingperiod <- Time <= Tdose * tinterval-dtout
      Rinputim  <- DOSEim/tlen * (Time %% tinterval < tlen) * dosingperiod
      Rpenim = Rinputim*(1-Frac)#
      Rppgim =  Rinputim*Frac#
      Rim = Kim*Amtsiteim
      dAbsorbim = Rim
      dAmtsiteim = Rpenim- Rim + Kdiss* DOSEppgim
      dDOSEppgim = Rppgim-Kdiss* DOSEppgim
      # Dosing, SC, subcutaneous
      Rinputsc = DOSEsc*(Time %% tinterval < dtout)*dosingperiod
      Rpensc = Rinputsc*(1-Fracsc)#
      Rppgsc =  Rinputsc*Fracsc
      Rsc = Ksc*Amtsitesc
      dAbsorbsc = Rsc
      dAmtsitesc = Rpensc- Rsc + Kdisssc* DOSEppgsc
      dDOSEppgsc = Rppgsc-Kdisssc* DOSEppgsc
      
      ## Concentrations in the tissues and in the capillary blood of the tissues
      
      CV = AV/Vven			# CV drug concentration in the venous blood (mg/L)
      CA = AA/Vart			# CAfree concentration of unbound drug in the arterial blood (mg/L)
      
      CL = AL/VL      # Concentration of parent drug in the tissue of liver
      CVL = CL/PL     # Concentration of parent drug in the capillary blood of liver
      
      CLu = ALu/VLu   # Concentration of parent drug in the tissue of lung
      CVLu = CLu/PLu  # Concentration of parent drug in the capillary blood of lung
      
      CK = AK/VK      # Concentration of parent drug in the tissue of kidney
      CVK = CK/PK     # Concentration of parent drug in the capillary blood of kidney 
      
      CM = AM/VM      # Concentration of parent drug in the tissue of muscle
      CVM = CM/PM     # Concentration of parent drug in the capillary blood of muscle 
      
      CF = AF/VF      # Concentration of parent drug in the tissue of fat
      CVF = CF/PF     # Concentration of parent drug in the capillary blood of  fat
      
      CR = AR/VR			# Crest drug concentration in the rest of the body (mg/L)
      CVR = AR/(VR*Prest) # Concentration of parent drug in the capillary blood of the rest of body
      
      #{Penicillin distribution in each compartment}
      # Penicillin in venous blood compartment
      RV = (QL*CVL+QK*CVK+QF*CVF+QM*CVM+QR*CVR+Rsc+Rim)-QC*CV# RV the changing rate in the venous blood (mg/h)
      dAV = RV			# AV the amount of the drug in the venous blood (mg)
      CAfree = CA*Free
      RA = QC*(CVLu-CAfree)		# RA the changing rate in the arterial blood (mg/h)
      dAA = RA
      
      dAUCCV = CV		# AUCCV AUC of drug concentration in the venous blood (mg*h/L)
      ABlood = AA+AV
      # Penicillin in liver compartment, flow-limited model
      Rmet = Kmet*CL*VL		# Rmet the metabolic rate in liver (mg/h)
      dAmet = Rmet		# Amet the amount of drug metabolized in liver (mg)
      RL = QL*(CAfree-CVL)-Rmet 	# RL the changing rate of the amount of drug in liver (mg/h)
      dAL = RL 			# AL amount of drug in liver (mg) 
      dAUCCL = CL		# AUCCL area under the curve of drug concentration in liver (mg*h/L)
      # Metabolism of Penicillin in liver compartment
      
      # Penicillin in kidney compartment, flow-limited model
      Rurine = Kurine*CVK
      dAurine = Rurine
      RK = QK*(CAfree-CVK)-Rurine	# RK the changing rate of the amount of drug in kidney (mg/h)
      dAK = RK			# AK amount of drug in kidney (mg)
      dAUCCK = CK		# AUCCK AUC of drug concentration in kidney (mg*h/L)
      # Penicillin urinary excretion
      
      # Penicillin in muscle compartment, flow-limited model
      RM = QM*(CAfree-CVM) 	# RM the changing rate of the amount of drug in muscle (mg/h) 
      dAM = RM			# AM amount of the drug in muscle (mg)
      dAUCCM = CM
      # Penicillin in fat compartment, flow-limited model
      RF = QF*(CAfree-CVF) 		# RF the changing rate of the amount of drug in fat (mg/h)
      dAF = RF			# AF amount of the drug in fat (mg)
      dAUCCF = CF		# AUCCF AUC of drug concentration in fat (mg*h/L)
      # Penicillin in the compartment of rest of body, flow-limited model
      RR = QR*(CAfree-CVR) 		# Rrest the changing rate of the amount of drug in the rest of the body (mg/h)
      dAR = RR			# Arest amount of the drug in the rest of the body (mg)
      dAUCCR = CR		# AUCCrest AUC of drug concentration in the rest of the body (mg*h/L)
      # Penicillin in lung compartment, flow-limited model
      RLu = QLu*(CV-CVLu)		# RLu the changing rate of the amount of drug in the lung (mg/h)
      dALu = RLu			# ALu amount of the drug in the lung (mg)
      dAUCCLu = CLu		# AUCCLu AUC of drug concentration in the lung (mg*h/L)
      #{Mass balance equations}
      Qbal = QC-QM-QR-QF-QK-QL
      Tmass = ABlood+AM+ALu+AR+AF+AK+AL+Aurine+Amet
      Input = Absorbim+Absorbsc
      Bal = Input-Tmass
      list(c(dAbsorbim, dAmtsiteim, dDOSEppgim,  dAbsorbsc, dAmtsitesc, dDOSEppgsc,
             dAV, dAA, dAUCCV, dAL, dAUCCL, dAmet, dAK, dAUCCK, dAurine, dAM, dAUCCM, 
             dAF, dAUCCF, dAR, dAUCCR, dALu, dAUCCLu))
    })
  }
  
  State <- c(Absorbim = 0, Amtsiteim = 0, DOSEppgim = 0, Absorbsc = 0, Amtsitesc = 0, DOSEppgsc = 0,
             AV = 0, AA = 0, AUCCV = 0, AL = 0, AUCCL = 0, Amet = 0, AK = 0, AUCCK = 0, Aurine = 0, 
             AM = 0, AUCCM = 0, AF = 0, AUCCF = 0, AR = 0, AUCCR = 0, ALu = 0, AUCCLu = 0)
  
  Paras <- c(QC, QL, QK, QM, QF, QR)  # parameter constants
  # solution of ODE systems
  out <- ode(y = State,
             times = Time, 
             func = pbpkmodel, 
             parms = Paras,
             method = 'lsoda') 
  pbpkout <- as.data.frame(out)
  ## Check balance for parent drug
  Tmass = pbpkout$AA + pbpkout$AV + pbpkout$AL + pbpkout$AK + pbpkout$Aurine + 
    pbpkout$AM + pbpkout$AF + pbpkout$AR + pbpkout$ALu + pbpkout$Amet
  Bal = pbpkout$Absorbim + pbpkout$Absorbsc - Tmass
  # Check balance for the major metabolite ciprofloxacin
  
  
  #################################################################################
  ## calculate the concentration of the parent drug and the major metabolite in vein compartment
  CV[,i] = pbpkout$AV/Vven # concentratin of parent drug in vein
  ## Liver
  CL[,i] = pbpkout$AL/VL # concentratin of parent drug in the tissue of liver
  ## Kidney
  CK[,i] = pbpkout$AK/VK # concentratin of parent drug in the tissue of kidney
  ## Muslce
  CM[,i] = pbpkout$AM/VM # concentratin of parent drug in the tissue of muscle
  ## Fat
  CF[,i] = pbpkout$AF/VF # concentratin of parent drug in the tissue of fat
  lines(Time, CL[,i], type = 'l',col = i)
  print(i)
}
filename='Penicillin-Cattle'
timejump=seq(from = 1, to = length(Time), by = 0.1/dtout) 
CV = as.data.frame(CV[timejump,]) # concentration in vein of 0.1 hour time interval
CL = as.data.frame(CL[timejump,]) # concentration in liver of 0.1 hour time interval
CK = as.data.frame(CK[timejump,]) # concentration in kidney of 0.1 hour time interval
CM = as.data.frame(CM[timejump,]) # concentration in muscle of 0.1 hour time interval
CF = as.data.frame(CF[timejump,]) # concentration in fat of 0.1 hour time interval
Time1=Time[timejump]/24 - Tdose + 1 # change time to daily scale and start from last injection
# Kidney
CK.01 <- as.data.frame(apply(CK,1,function(x) quantile(x,prob = c(0.01))))
CK.m <- as.data.frame(apply(CK, 1, median))
CK.99 <- as.data.frame(apply(CK,1,function(x) quantile(x,prob = c(0.99))))
CK.data <- cbind(Time1, CK.01, CK.m, CK.99)
names(CK.data) = c('Time (Day)','1 Percentile', 'Median','99 Percentile' )
CK.data <- cbind(Time[timejump],CK,CK.data)
names(CK.data)[1]=c('Time (Hour)')
# Liver
CL.01 <- as.data.frame(apply(CL,1,function(x) quantile(x,prob = c(0.01))))
CL.m <- as.data.frame(apply(CL, 1, median))
CL.99 <- as.data.frame(apply(CL,1,function(x) quantile(x,prob = c(0.99))))
CL.data <- cbind(Time1, CL.01, CL.m, CL.99)
names(CL.data) = c('Time (Day)','1 Percentile', 'Median','99 Percentile' )
CL.data <- cbind(Time[timejump],CL,CL.data)
names(CL.data)[1]=c('Time (Hour)')
# Muscle
CM.01 <- as.data.frame(apply(CM,1,function(x) quantile(x,prob = c(0.01))))
CM.m <- as.data.frame(apply(CM, 1, median))
CM.99 <- as.data.frame(apply(CM,1,function(x) quantile(x,prob = c(0.99))))
CM.data <- cbind(Time1, CM.01, CM.m, CM.99)
names(CM.data) = c('Time (Day)','1 Percentile', 'Median','99 Percentile' )
CM.data <- cbind(Time[timejump],CM,CM.data)
names(CM.data)[1]=c('Time (Hour)')
# Vein
CV.01 <- as.data.frame(apply(CV,1,function(x) quantile(x,prob = c(0.01))))
CV.m <- as.data.frame(apply(CV, 1, median))
CV.99 <- as.data.frame(apply(CV,1,function(x) quantile(x,prob = c(0.99))))
CV.data <- cbind(Time1, CV.01, CV.m, CV.99)
names(CV.data) = c('Time (Day)','1 Percentile', 'Median','99 Percentile' )
CV.data <- cbind(Time[timejump],CV,CV.data)
names(CV.data)[1]=c('Time (Hour)')
# Fat
CF.01 <- as.data.frame(apply(CF,1,function(x) quantile(x,prob = c(0.01))))
CF.m <- as.data.frame(apply(CF, 1, median))
CF.99 <- as.data.frame(apply(CF,1,function(x) quantile(x,prob = c(0.99))))
CF.data <- cbind(Time1, CF.01, CF.m, CF.99)
names(CF.data) = c('Time (Day)','1 Percentile', 'Median','99 Percentile' )
CF.data <- cbind(Time[timejump],CF,CF.data)
names(CF.data)[1]=c('Time (Hour)')
TOL=0.05
## ggplot2; draw the concentration curve.
ggplot(CL.data, aes(Time1)) + 
  geom_ribbon(aes(ymin = CL.01,
                  ymax = CL.99
                  ,fill=''), 
              show.legend = F, 
              size = 0.1,
              alpha = 0.3) +  # alpha is transparency parameter. 
  geom_line(aes(y = CL.m,
                color = 'Median'), 
            size = 0.5, 
            show.legend = T) + # draw the mean value to the chart.
  geom_line(aes(y = CL.99,
                color = '99 Percentile'), 
            size = 0.5, 
            show.legend = T) +  
  geom_line(aes(y = CL.01,
                color = '1 Percentile'), 
            size = 0.5, 
            show.legend = T) + 
  xlab('Time (Day)') +  # add x axis label
  ylab(expression(paste('Concentration (',mu,'g/mL)'))) +
  geom_line(aes(y = TOL),color = 'black',size = 0.5, linetype = 'twodash', show.legend = F) + # add tolerance line
  scale_y_log10() + theme_bw() + theme(axis.text=element_text(size=16)) #logrithmize the y axis, set its limit, remove backgound

setwd('Z:/project 3/Lin')
save(list=c('CL.data','CK.data','CM.data','CF.data','CL.data'), file = paste0(filename,"-",doseX,'Xdose.RData'))
write.csv(CK.data, file = paste0(filename,"-CK-",doseX,"dose.CSV"))
write.csv(CL.data, file = paste0(filename,"-CL-",doseX,"dose.CSV"))
write.csv(CF.data, file = paste0(filename,"-CF-",doseX,"dose.CSV"))
write.csv(CM.data, file = paste0(filename,"-CM-",doseX,"dose.CSV"))
write.csv(CV.data, file = paste0(filename,"-CV-",doseX,"dose.CSV"))
