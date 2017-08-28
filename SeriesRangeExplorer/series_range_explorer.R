#series range explorer

library(soilDB)
x <- fetchNASIS()
x$taxonname = toupper(x$taxonname)

idx <- which(x$taxonname == 'FLANLY')

sx <- x[idx,]

groupedProfilePlot(sx, groups = 'taxonname', alt.label='ecositeid', alt.label.col='black', label='pedon_id', print.id = TRUE, id.style = 'side', cex.id=0.6, cex.names=0.6, y.offset=7, axis.line.offset=-2.5, group.name.cex=0.5, group.name.offset = -6, color='clay')

sdc <- getSoilDepthClass(sx)

plotSPC(sx, groups = 'taxonname', plot.order=order(sdc$depth), label='pedon_id', cex.id=0.7, cex.names=0.7, col.label=' ', width=0.15, y.offset=0, id.style='side', x.idx.offset=0.2, shrink=FALSE, shrink.cutoff=1)


plotSPC(sx, groups = 'taxonname', plot.order=order(sx$slope_field), label='pedon_id', cex.id=0.7, cex.names=0.7, col.label=' ', width=0.15, y.offset=0, id.style='side', x.idx.offset=0.2, shrink=FALSE, shrink.cutoff=1)



