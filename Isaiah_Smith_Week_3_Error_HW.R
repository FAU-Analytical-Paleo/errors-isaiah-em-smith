#Isaiah Smith
#Computers in Geosciences
#Week 3 Homework
#24 November 2020

# Question 1 --------------------------------------------------------------

#Make function to convert DMS to radians:
distance_measure<-2550
distance_error<-25

convert_to_radians<-function(d,m,s){
  
  ((d+(m/60)+(s/3600))/180)*pi
  
}

#convert given DMS value to radians:
angle_measure<-convert_to_radians(1,21,0)
angle_error<-convert_to_radians(0,1,0)

height_calculation<-tan(angle_measure)*distance_measure

height_error<-height_calculation*((angle_error/angle_measure)+(distance_error/distance_measure))

#building height:
height_calculation
# 60.09408 m

#building height uncertainty:
height_error
# plus or minus 1.33106 m 


# Question 2 --------------------------------------------------------------

difference_function<-function(v1,v2){
  
  abs(v1-v2)
  
}

#duration of volcanic activity:
difference_function(25.53,29.66)
# 4.13 Ma

#duration uncertainty:
0.1+0.2
# plus or minus 0.3 Ma


# Question 3 --------------------------------------------------------------

earthquake<-read.csv("/Users/isaiahsmith/Desktop/ex3_eqscals.txt", 
                     header = FALSE, sep = "", 
                     col.names = c("X (km)", "r (m)", "Mo (Nm)"))

radii<-earthquake$r..m.
moments<-earthquake$Mo..Nm.
distance_from_station<-earthquake$X..km.


# Question 3a -------------------------------------------------------------

#find mean of r
r_mean<-mean(radii)

#find median of r
r_median<-median(radii)

#find standard deviation of r
r_sd<-sd(radii)

#find median absolute deviation of r
r_MAD<-mad(radii)

#find mean of Mo
Mo_mean<-mean(moments)

#find median of Mo
Mo_median<-median(moments)

#find standard deviation of Mo
Mo_sd<-sd(moments)

#find median absolute deviation of Mo
Mo_MAD<-mad(moments)

# Question 3b -------------------------------------------------------------

boxplot(radii, xlab="Radius", ylab="Meters")

scatter.smooth(distance_from_station, moments, 
               xlab = "Distance from Station (km)", 
               ylab = "Seismic Moment (Nm)", )

#no obvious outliers

# Question 3c -------------------------------------------------------------

trimmed_radii<-radii[radii<(3*r_MAD+r_median) &
                       radii>(r_median-3*r_MAD) ]

range(trimmed_radii)
range(radii)
all.equal(trimmed_radii, radii)

#It does not appear there are any outliers in the radius data

trimmed_moments<-moments[moments<(3*Mo_MAD+Mo_median) &
                           moments>(Mo_median-3*Mo_MAD) ]

range(trimmed_moments)
range(moments)
all.equal(trimmed_moments, moments)

#It does not appear there are any outliers in the moment data

#Because there are no outliers in either dataset, the trimmed
#datasets are identical, and thus so are the various calculated metrics.

#Best estimate for Mo is the median, and the best estimate 
#of uncertainty is the MAD, which is a median-derived 
#measure of dispersion:

# 7.586871e+15 Nm plus/minus 5.196378e+15 Nm


# Question 3d -------------------------------------------------------------

#calculate Mw using median value and MAD
#Mw = (log10(Mo))/1.5 - 6.0

Mw_function<-function(Mo){
  
  ((log10(Mo))/1.5) - 6.0
  
}


#Answer:
Mw_function(Mo_median)
# 4.586708 Nm


#Uncertainty:
Mw_function(Mo_MAD)
# plus or minus 4.477134 Nm




