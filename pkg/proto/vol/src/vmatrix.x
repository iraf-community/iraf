include <imhdr.h>
include "pvol.h"


# VMATRIX -- Volume rotation, rotation matrix projection algorithm.
# Proceeds from origin at back of volume image toward front, writing
# output image lines in successive overlapping sheets.  See "Back to 
# Front Display of Voxel-Based Objects", G.Frieder, D.Gordon, R.Reynolds,
# IEEE Computer Graphics & Applications Jan. 85, p 52-60.

procedure vmatrix (im1, im2, vp)
pointer im1		# Input volume image
pointer	im2		# Output projection image
pointer	vp		# Volume projection descriptor

real	v, vx, vy, vz
real	dcosa, dcosb, dcosc
#real	t11,t21,t31, t12,t22,t32, t13,t23,t33

begin
	vx = VECX(vp)
	vy = VECY(vp)
	vz = VECZ(vp)
	v = sqrt (vx*vx + vy*vy + vz*vz)
	dcosa = vx / v
	dcosb = vy / v
	dcosc = vz / v

	# ???????
end

