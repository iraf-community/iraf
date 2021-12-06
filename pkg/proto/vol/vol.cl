#{ VOL -- Volume Images Package

# print(" ")
# print("This package contains tasks for viewing and manipulating 3d images.")
# print("It is a pre-release version, and does not reflect the ultimate")
# print("partitioning of n-dimensional image tasks within IRAF")
# print(" ")

# Load some needed packages now
# (None are needed yet)		# if images$tv not loaded, load for vidrecord.

package vol

task 	i2sun,
	im3dtran,
	imjoin,
	pvol		= "vol$src/x_vol.e"

#task	vidrecord	= "vol$src/vidrecord.cl"

clbye()

