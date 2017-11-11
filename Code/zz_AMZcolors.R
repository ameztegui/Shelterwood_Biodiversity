# Color palette of 15 elements based on colors of graphic design

AMZ_blue <- "#104577"
AMZ_maroon <- "#ab2323"
AMZ_lightblue <- "#7cb5d2"
AMZ_violet <- "#bc94b7"
AMZ_green <- "#18a68c"
AMZ_pistachio <- "#bcd74a"
AMZ_mustard <- "#fde05e"
AMZ_lightpink <-"#f8e0c8"
AMZ_darkblue <-"#0f3e5a"
AMZ_brickred<- "#ec6645"
AMZ_orange<-"#f7a519"
AMZ_purple<- "#b54295"
AMZ_red<- "#ee1822"
AMZ_blue2<-"#106ab4"
AMZ_lightblue2<-"#31a5dd"



AMZcolors=function(nvar) {
        if(nvar==1)  {AMZ=c(AMZ_maroon)}
        else if (nvar==2) {AMZ=c(AMZ_maroon, AMZ_mustard)}
        else if (nvar==3) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green)}
        else if (nvar==4) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green, AMZ_orange)} 
        else if (nvar==5) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green, AMZ_orange,AMZ_purple)}
        else if (nvar==6) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green, AMZ_orange, AMZ_purple,AMZ_violet)}
        else if (nvar==7) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green, AMZ_orange, AMZ_purple,AMZ_violet, AMZ_blue)}
        else if (nvar==8) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green, AMZ_orange, AMZ_purple,AMZ_violet, AMZ_blue, )}
        else if (nvar==9) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green, AMZ_orange, AMZ_purple,AMZ_violet, AMZ_blue, AMZ_violet, AMZ_lightpink)}
        else if (nvar==10) {AMZ=c(AMZ_maroon, AMZ_mustard,AMZ_green,AMZ_orange, AMZ_purple,AMZ_violet, AMZ_blue, AMZ_violet, AMZ_lightpink, AMZ_red)}
return(AMZ)
}
AMZcolors(5)
