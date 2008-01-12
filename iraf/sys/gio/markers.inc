# Data declarations for the standard markers.

real	mpx[86], mpy[86]	# marker polyline points
int	moff[9]	 		# offsets of the markers in mpx and mpy
int	mnpts[9]		# number of points in each polyline
int	k			# implied do-loop dummy index

# Box.
data	(mpx(k),k=01,05) /0.0, 1.0, 1.0, 0.0, 0.0/
data	(mpy(k),k=01,05) /0.0, 0.0, 1.0, 1.0, 0.0/
data	moff[1] /1/, mnpts[1] /5/

# Plus.
data	(mpx(k),k=06,10) /0.5, 0.5, 0.5, 0.0, 1.0/
data	(mpy(k),k=06,10) /0.0, 1.0, 0.5, 0.5, 0.5/
data	moff[2] /6/, mnpts[2] /5/

# Cross.
data	(mpx(k),k=11,15) /0.0, 1.0, 0.5, 0.0, 1.0/
data	(mpy(k),k=11,15) /0.0, 1.0, 0.5, 1.0, 0.0/
data	moff[3] /11/, mnpts[3] /5/

# Diamond.
data	(mpx(k),k=16,20) /0.5, 1.0, 0.5, 0.0, 0.5/
data	(mpy(k),k=16,20) /0.0, 0.5, 1.0, 0.5, 0.0/
data	moff[4] /16/, mnpts[4] /5/

# Horizonal line.
data	(mpx(k),k=21,22) /0.0, 1.0/
data	(mpy(k),k=21,22) /0.5, 0.5/
data	moff[5] /21/, mnpts[5] /2/

# Vertical line.
data	(mpx(k),k=23,24) /0.5, 0.5/
data	(mpy(k),k=23,24) /0.0, 1.0/
data	moff[6] /23/, mnpts[6] /2/

# Horizontal error bar.
data	(mpx(k),k=25,30) /0.0, 0.0, 0.0, 1.0, 1.0, 1.0/
data	(mpy(k),k=25,30) /0.0, 1.0, 0.5, 0.5, 1.0, 0.0/
data	moff[7] /25/, mnpts[7] /6/

# Vertical error bar.
data	(mpx(k),k=31,36) /0.0, 1.0, 0.5, 0.5, 1.0, 0.0/
data	(mpy(k),k=31,36) /0.0, 0.0, 0.0, 1.0, 1.0, 1.0/
data	moff[8] /31/, mnpts[8] /6/

# Circle.
data	(mpx(k),k=37,41) /1.000, 0.996, 0.984, 0.963, 0.936/	# X
data	(mpx(k),k=42,46) /0.901, 0.859, 0.812, 0.759, 0.702/
data	(mpx(k),k=47,51) /0.642, 0.580, 0.516, 0.452, 0.389/
data	(mpx(k),k=52,56) /0.327, 0.269, 0.214, 0.164, 0.119/
data	(mpx(k),k=57,61) /0.081, 0.050, 0.025, 0.009, 0.001/
data	(mpx(k),k=62,66) /0.001, 0.009, 0.025, 0.050, 0.081/
data	(mpx(k),k=67,71) /0.119, 0.164, 0.214, 0.269, 0.327/
data	(mpx(k),k=72,76) /0.389, 0.452, 0.516, 0.580, 0.642/
data	(mpx(k),k=77,81) /0.702, 0.759, 0.812, 0.859, 0.901/
data	(mpx(k),k=82,86) /0.936, 0.963, 0.984, 0.996, 1.000/

data	(mpy(k),k=37,41) /0.500, 0.564, 0.627, 0.688, 0.745/	# Y
data	(mpy(k),k=42,46) /0.799, 0.848, 0.891, 0.928, 0.957/
data	(mpy(k),k=47,51) /0.979, 0.994, 1.000, 0.998, 0.987/
data	(mpy(k),k=52,56) /0.969, 0.943, 0.910, 0.870, 0.824/
data	(mpy(k),k=57,61) /0.773, 0.717, 0.658, 0.596, 0.532/
data	(mpy(k),k=62,66) /0.468, 0.404, 0.342, 0.283, 0.227/
data	(mpy(k),k=67,71) /0.176, 0.130, 0.090, 0.057, 0.031/
data	(mpy(k),k=72,76) /0.013, 0.002, 0.000, 0.006, 0.021/
data	(mpy(k),k=77,81) /0.043, 0.072, 0.109, 0.152, 0.201/
data	(mpy(k),k=82,86) /0.255, 0.312, 0.373, 0.436, 0.500/

data	moff[9] /37/, mnpts[9] /50/
