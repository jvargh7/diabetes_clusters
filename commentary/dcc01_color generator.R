library(colorspace)

c1 = RGB(255/255, 37/255, 0/255) 
c2 = RGB(1/255, 160/255, 138/255) 
c3 = RGB(242/255, 173/255, 0/255)
c4 = RGB(90/255, 188/255, 214/255)
c5 = RGB(68/255, 100/255, 85/255) 
c6 = RGB(114/255, 148/255, 212/255)


# https://colorspace.r-forge.r-project.org/articles/manipulation_utilities.html
par(mar = rep(0,6))
swatchplot(c(hex(c1), hex(c2), hex(c3),
             hex(c4),hex(c5),hex(c6)))

mixcolor()