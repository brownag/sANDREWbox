#fine-earth fraction ranges
# logic derived directly from NASIS calculation "Textural class versus Particle Size Separates"
# intended for use with the ternary fine earth fractions (sand, silt, clay) commonly estimated in the field
# NOTE: does NOT check sand fractions in order to verify vfs/fs/cos subclasses
# 

#texcl - vector of numeric values (NASIS codes) corresponding to a set of texture groups
# returns a range for each of sand, silt and clay corresponding to the CLASS LIMITS imposed by the set of texture groups
getFineEarthLimitsFromTextureGroupIDs <- function(texcl) {
  buf <- matrix(nrow=3,ncol=2)
  rownames(buf) <- c("sand","silt","clay")
  colnames(buf) <- c("low", "high")
  buf[1,] <- c(getSandLow(texcl),getSandHigh(texcl))
  buf[2,] <- c(getSiltLow(texcl),getSiltHigh(texcl))
  buf[3,] <- c(getClayLow(texcl),getClayHigh(texcl))
  return(buf)
}

getFineEarthLimitsFromTextureClassName <- function(texclname) {
  texcl <- code(data.frame(texcl = tolower(texclname)))
  return(getFineEarthLimitsFromTextureGroupIDs(texcl))
}

getClayHigh <- function(texcl) {
  if(any(texcl == 21)) return(100) #clay
  else if(any(texcl == 20)) return(60)  #sic
  else if(any(texcl == 19)) return(55)  #sc
  else if(any(texcl == 18 | texcl == 17)) return(40)  #cl, sicl
  else if(any(texcl == 16)) return(35)  #scl
  else if(any(texcl == 14 | texcl == 13)) return(27)  #l, sil
  else if(any(texcl == 12 | texcl == 11 | texcl == 10 | texcl == 9)) return(20)  #sl, fsl, vfsl, cosl
  else if(any(texcl == 8 | texcl == 7 | texcl == 6 | texcl == 5)) return(15)  #ls, lvfs, lfs, lcos
  else if(any(texcl == 15)) return(12) #si
  else if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(10)
  return(NA)
}
getClayLow <- function(texcl) {
  if(any(texcl <=12 & texcl >= 1) | any(texcl == 14 | texcl == 15)) return(0)
  else if(any(texcl == 13)) return(7)
  else if(any(texcl == 16)) return(20)
  else if(any(texcl == 17 | texcl == 18)) return(27)
  else if(any(texcl == 19)) return(35)
  else if(any(texcl == 20 | texcl == 21)) return(40)
  return(NA)
}

getSiltHigh <- function(texcl) {
  if(any(texcl == 15)) return(100)
  else if(any(texcl == 14)) return(88)
  else if(any(texcl == 18)) return(73)
  else if(any(texcl == 20)) return(60)
  else if(any(texcl == 17)) return(53)
  else if(any(texcl <= 13 & texcl >= 9)) return(50)
  else if(any(texcl == 21)) return(40)
  else if(any(texcl <= 8 & texcl >=5)) return(30)
  else if(any(texcl == 16)) return(28)
  else if(any(texcl == 19)) return(20)
  else if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(15)
  return(NA)
}
getSiltLow <- function(texcl) {
  if(any(texcl <= 12 & texcl >= 1) | any(texcl == 16 | texcl == 19 | texcl == 21)) return(0)
  else if(any(texcl == 17)) return(15)
  else if(any(texcl == 13)) return(28)
  else if(any(texcl == 20 | texcl == 18)) return(40)
  else if(any(texcl == 14)) return(50)
  else if(any(texcl == 15)) return(80)
  return(NA)
}
getSandHigh <- function(texcl) {
  if(any(texcl == 4 | texcl == 3 | texcl == 2 | texcl == 1)) return(100)
  else if(any(texcl <= 8 & texcl >= 5)) return(90)
  else if(any(texcl <= 12 & texcl >= 9)) return(85)
  else if(any(texcl == 16)) return(80)
  else if(any(texcl == 19)) return(65)
  else if(any(texcl == 13)) return(52)
  else if(any(texcl == 14)) return(50)
  else if(any(texcl == 17 | texcl == 21)) return(45)
  else if(any(texcl == 15 | texcl == 18 | texcl == 20)) return(20)
  return(NA)
}

getSandLow <- function(texcl) {
  if(any(texcl == 21 | texcl == 20 | texcl == 18 | texcl == 14 | texcl == 15)) return(0)
  else if(any(texcl == 17)) return(20)
  else if(any(texcl == 13)) return(23)
  else if(any(texcl <= 12 & texcl >= 9)) return(43)
  else if(any(texcl == 16 | texcl == 19)) return(45)
  else if(any(texcl <= 8 & texcl >= 5)) return(70)
  else if(any(texcl <= 4 & texcl >= 1)) return(85)
  return(NA)
}