# PVOL.H -- PVOL definitions.

define	COL		1			# image column index
define	LINE		2			# image line index
define	BAND		3			# image band index
define	DIS		(sqrt (($1)*($1) + ($2)*($2)))
define	DRADIAN		(57.295779513082320877D0)
define	DDEGTORAD	(double($1)/DRADIAN)
define	DRADTODEG	(double($1)*DRADIAN)
define	DPI		3.1415926535897932385D0
define	DTWOPI		6.2831853071795864769D0
define	DHALFPI		1.5707963267948966192D0
define	DTHREEPIOVER2	(1.5D0 * DPI)
define	DEF_IMIN	(0.0)
define	DEF_IMAX	(1.0)

define	ALG_INCREM	1			# incremental dda proj. algor.
define	ALG_BRESEN	2			# bresenham dda proj. algor.
define	ALG_MATRIX	3			# rotation matrix prol. algor.
define	P_ATTENUATE	1			# attenuate by voxval (opacity)
define	P_AVERAGE	2			# average projected intensities
define	P_SUM		3			# sum voxel intensities
define	P_INVDISPOW	4			# wt int. by inverse dis power
define	P_MODN		5			# use only f(ndecades) voxels
define	P_LASTONLY	6			# use only last voxval > cutoff

# Volume rotation descriptor
define	LEN_VP		30

# Projection geometry elements:
define	P_ALGORITHM	Memi[$1]		# Projection algorithm
define	DEGREES		Memr[P2R($1+1)]		# Degrees per rotation increment
define	NFRAMES		Memi[$1+2]		# Number of rotation increments
define	VECX		Memr[P2R($1+3)]		# Rotation axis X vector
define	VECY		Memr[P2R($1+4)]		# Rotation axis Y vector
define	VECZ		Memr[P2R($1+5)]		# Rotation axis Z vector
define	INIT_THETA	Memr[P2R($1+6)]		# Initial rotation angle
define	MAX_WS		Memi[$1+7]		# Maximum working set size
#			reserved space

# Light transmission elements:
define	OPACELEM	Memi[$1+10]		# Opacity element in 4th dimen
define	PTYPE		Memi[$1+11]		# Projection type, voxel val 1
define	OMIN		Memr[P2R($1+12)]	# Voxel opacity minimum
define	OMAX		Memr[P2R($1+13)]	# Voxel opacity maximum
define	OSCALE		Memr[P2R($1+14)]	# Opacity scale factor
define	AMIN		Memr[P2R($1+15)]	# Attenuation factor minimum
define	AMAX		Memr[P2R($1+16)]	# Attenuation factor maximum
define	VIMIN		Memr[P2R($1+17)]	# Voxel intensity minimum
define	VIMAX		Memr[P2R($1+18)]	# Voxel intensity maximum
define	IZERO		Memr[P2R($1+19)]	# Background illumination
define	DISPOWER	Memr[P2R($1+20)]	# Distance weighting power
define	MODN		Memi[$1+21]		# Use vox w/ (mod(val*100,modn))
define	IIMIN		Memr[P2R($1+22)]	# Input intensity minimum
define	IIMAX		Memr[P2R($1+23)]	# Input intensity maximum
define	VERBOSE		Memi[$1+24]		# Write verbose output?
define	DISCUTOFF	Memi[$1+25]		# Measure distance w/in cutoffs
#			reserved space
