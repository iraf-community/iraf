# Common for GKS emulator.

pointer	gp[NDEV]	# Graphics file descriptor for gio calls
int	gk_status[NDEV]	# Active bit = INACTIVE or ACTIVE
int	gk_std		# Index of gp array used for reference in set/get calls
int	gk_style	# Fill area type of fill - set by GSFAIS
int	gk_marker	# Marker type for use by GPM
int	gk_asf[NASF]	# Array for maintaining aspect source flags

common /gksemu/ gp, gk_status, gk_std, gk_style, gk_marker, gk_asf
