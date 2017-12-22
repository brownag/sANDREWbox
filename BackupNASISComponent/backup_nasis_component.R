#component backer upper
# fetches NASIS selected set component data and writes binary representation of SPC object to Rdata file. can reload with load()

library(soilDB)
f <- fetchNASIS_component_data()
save(f, file="S:/NRCS/Archive_Andrew_Brown/CA630/MehrtenMUs/302X/303X-404X_components_old.Rda")

#load(file="S:/NRCS/Archive_Andrew_Brown/CA630/MehrtenMUs/302X/302x_components_old.Rda")
