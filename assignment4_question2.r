library('corrplot')
library("Cairo")
cairo_pdf("cor_matrix.pdf", width=10, height=10)
rData <- read.csv("data.csv")
rData <- na.omit(rData)

corrplot(cor(rData[4:dim(rData)[2]]), method="ellipse", type="upper", tl.cex=.7, tl.col="black", tl.srt=45)
dev.off()