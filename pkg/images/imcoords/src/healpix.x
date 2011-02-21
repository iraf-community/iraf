include	<math.h>

define	MTYPES	"|nest|ring|"
define	NEST	1
define	RING	2

define	NS_MAX		8192
define	TWOTHIRDS	0.66666666667


# ANG2PIX -- Compute the HEALPix map row from a spherical coordinate.
#
# It is up to the caller to know the coordinate type, map type, and
# resolution for the map.
#
# The returned row is 1 indexed.

procedure ang2row (row, lng, lat, mtype, nside)

int	row			#O Table row
double	lng			#I Longitude (deg)
double	lat			#I Latitude (deg)
int	mtype			#I HEALPix map type
int	nside			#I Resolution parameter

int	ipix
double	phi, theta
errchk	ang2pix_nest, ang2pix_ring

begin
	# Check parameters and call appropriate procedure.

	if (nside < 1 || nside > NS_MAX)
	    call error (1, "nside out of range")

	if (lat < -90D0 || lat > 90D0)
	    call error (2, "latitude out of range")

	phi = DEGTORAD (lng)
	theta = DEGTORAD (90D0 - lat)

	switch (mtype) {
	case NEST:
	    call ang2pix_nest (nside, theta, phi, ipix)
	case RING:
	    call ang2pix_ring (nside, theta, phi, ipix)
	default:
	    call error (3, "unknown HEALPix map type")
	}

	row = ipix + 1
end


# PIX2ANG -- Compute spherical coordinate from HEALPix map row.
#
# It is up to the caller to know the coordinate type, map type, and
# resolution for the map.

procedure row2ang (row, lng, lat, mtype, nside)

int	row			#I Table row (1 indexed)
double	lng			#O Longitude (deg)
double	lat			#O Latitude (deg)
int	mtype			#I HEALPix map type
int	nside			#I Resolution parameter

int	ipix
double	phi, theta
errchk	pix2ang_nest, pix2ang_ring

begin
	# Check input parameters and call appropriate procedure.

	if (nside < 1 || nside > NS_MAX)
	    call error (1, "nside out of range")

	if (row < 1 || row > 12*nside*nside)
	    call error (1, "row out of range")

	ipix = row - 1

	switch (mtype) {
	case NEST:
	    call pix2ang_nest (nside, ipix, theta, phi)
	case RING:
	    call pix2ang_ring (nside, ipix, theta, phi)
	default:
	    call error (3, "unknown HEALPix map type")
	}

	lng = RADTODEG (phi)
	lat = 90D0 - RADTODEG (theta)
end


# The following routines are SPP translations of the HEALPix software from
# the authors identified below.  If it matters, the C version was used
# though the translation is not necessarily exact.  Comments were
# largely removed.
#
# I'm not sure if the arguments to the floor function in the original
# can be negative.  Assuming not I just do an integer truncation.

# -----------------------------------------------------------------------------
#
#  Copyright (C) 1997-2008 Krzysztof M. Gorski, Eric Hivon,
#                          Benjamin D. Wandelt, Anthony J. Banday,
#                          Matthias Bartelmann,
#                          Reza Ansari & Kenneth M. Ganga
#
#
#  This file is part of HEALPix.
#
#  HEALPix is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published
#  by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  HEALPix is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with HEALPix; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
#  02110-1301  USA
#
#  For more information about HEALPix see http://healpix.jpl.nasa.gov
#
#----------------------------------------------------------------------------- 

	
# ANG2PIX_NEST -- Compute HEALPix index for a nested map.

procedure ang2pix_nest (nside, theta, phi, ipix)

int	nside			#I Resolution parameter
double	theta			#I Latitude (rad from pole)
double	phi			#I Longitude (rad)
int	ipix			#O HEALPix index

double	z, za, tt, tp, tmp
int	face_num, jp, jm
long	ifp, ifm
int	ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf, ntt
int	 x2pix[128], y2pix[128]
int	 setup_done

errchk	mk_xy2pix

data	setup_done/NO/

begin
	if (setup_done == NO) {
	    call mk_xy2pix (x2pix, y2pix)
	    setup_done = YES
	}

	z  = cos (theta)
	za = abs (z)
	if (phi >= TWOPI)
	    phi = phi -  TWOPI
	if (phi < 0.)
	    phi = phi +  TWOPI
	tt = phi / HALFPI
	
	if (za <= TWOTHIRDS) {
	    jp = int (NS_MAX * (0.5 + tt - z * 0.75))
	    jm = int (NS_MAX * (0.5 + tt + z * 0.75))
	    
	    ifp = jp / NS_MAX
	    ifm = jm / NS_MAX
	    
	    if (ifp==ifm)
	        face_num = mod (ifp, 4) + 4
	    else if (ifp<ifm)
	        face_num = mod (ifp, 4)
	    else
	        face_num = mod (ifm, 4) + 8
	    
	    ix = mod (jm, NS_MAX)
	    iy = NS_MAX - mod (jp, NS_MAX) - 1
	} else {
	    ntt = int (tt)
	    if (ntt >= 4)
	        ntt = 3
	    tp = tt - ntt
	    tmp = sqrt (3. * (1. - za))
	    
	    jp = int (NS_MAX * tp * tmp)
	    jm = int (NS_MAX * (1. - tp) * tmp)
	    jp = min (jp, NS_MAX-1)
	    jm = min (jm, NS_MAX-1)
	    
	    if (z >= 0) {
		face_num = ntt
		ix = NS_MAX - jm - 1
		iy = NS_MAX - jp - 1
	    } else {
		face_num = ntt + 8
		ix =  jp
		iy =  jm
	    }
	}
	
	ix_low = mod (ix, 128) + 1
	ix_hi  = ix / 128 + 1
	iy_low = mod (iy, 128) + 1
	iy_hi  = iy / 128 + 1

	ipf = (x2pix[ix_hi] + y2pix[iy_hi]) * (128 * 128) +
	    (x2pix[ix_low] + y2pix[iy_low])
	ipf = ipf / (NS_MAX/nside)**2
	ipix = ipf + face_num * nside**2
end

	
# ANG2PIX_RING -- Compute HEALPix index for a ring map.

procedure ang2pix_ring (nside, theta, phi, ipix)

int	nside			#I Resolution parameter
double	theta			#I Latitude (rad from pole)
double	phi			#I Longitude (rad)
int	ipix			#O HEALPix index

int	nl2, nl4, ncap, npix, jp, jm, ipix1
double	z, za, tt, tp, tmp
int	ir, ip, kshift
  
begin
	z = cos (theta)
	za = abs (z)
	if ( phi >= TWOPI)
	    phi = phi - TWOPI
	if (phi < 0.)
	    phi = phi + TWOPI
	tt = phi / HALFPI

	nl2 = 2 * nside
	nl4 = 4 * nside
	ncap  = nl2 * (nside - 1)
	npix  = 12 * nside * nside

	if (za <= TWOTHIRDS) {

	    jp = int (nside * (0.5 + tt - z * 0.75))
	    jm = int (nside * (0.5 + tt + z * 0.75))

	    ir = nside + 1 + jp - jm
	    kshift = 0
	    if (mod (ir,2) == 0)
	        kshift = 1

	    ip = int ((jp + jm - nside + kshift + 1) / 2) + 1
	    if (ip > nl4)
	        ip = ip - nl4

	    ipix1 = ncap + nl4 *  (ir - 1) + ip 
	} else {

	    tp = tt - int (tt)
	    tmp = sqrt (3. *  (1. - za))

	    jp = int (nside * tp * tmp)
	    jm = int (nside * (1. - tp) * tmp)

	    ir = jp + jm + 1
	    ip = int (tt * ir) + 1
	    if (ip > 4*ir)
	        ip = ip - 4 * ir

	    ipix1 = 2 * ir * (ir - 1) + ip
	    if (z<=0.) {
	      ipix1 = npix - 2 * ir * (ir + 1) + ip
	    }
	}
	ipix = ipix1 - 1
end


# PIX2ANG_NEST -- Translate HEALpix nested row to spherical coordinates.

procedure pix2ang_nest (nside, ipix, theta, phi)

int	nside			#I Resolution parameter
int	ipix			#I HEALPix index
double	theta			#O Latitude (rad from pole)
double	phi			#O Longitude (rad)

int	npface, face_num
int	ipf, ip_low, ip_trunc, ip_med, ip_hi
int	ix, iy, jrt, jr, nr, jpt, jp, kshift, nl4
double	z, fn, fact1, fact2

int	pix2x[1024], pix2y[1024]

int	jrll[12], jpll[12], setup_done
data	jrll/2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4/
data	jpll/1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7/
data	setup_done/NO/

begin
	if (setup_done == NO) {
	    call mk_pix2xy (pix2x,pix2y)
	    setup_done = YES
	}

	fn = 1. * nside
	fact1 = 1. / (3. * fn * fn)
	fact2 = 2. / (3. * fn)
	nl4   = 4 * nside

	npface = nside * nside

	face_num = ipix / npface + 1
	ipf = mod (ipix, npface)

	ip_low = mod (ipf, 1024) + 1
	ip_trunc = ipf / 1024 
	ip_med = mod (ip_trunc, 1024) + 1
	ip_hi  = ip_trunc / 1024 + 1

	ix = 1024*pix2x[ip_hi] + 32*pix2x[ip_med] + pix2x[ip_low]
	iy = 1024*pix2y[ip_hi] + 32*pix2y[ip_med] + pix2y[ip_low]

	jrt = ix + iy
	jpt = ix - iy

	jr =  jrll[face_num] * nside - jrt - 1
	nr = nside
	z  = (2 * nside - jr) * fact2
	kshift = mod (jr - nside, 2)
	if( jr < nside) {
	    nr = jr
	    z = 1. - nr * nr * fact1
	    kshift = 0
	} else if (jr > 3*nside) {
	    nr = nl4 - jr
	    z = - 1. + nr * nr * fact1
	    kshift = 0
	}

	jp = (jpll[face_num] * nr + jpt + 1 + kshift)/2
	if (jp > nl4)
	    jp = jp - nl4
	if (jp < 1)
	    jp = jp + nl4

	theta = acos(z)
	phi = (jp - (kshift+1)*0.5) * (HALFPI / nr)
end


# PIX2ANG_RING -- Convert HEALpix pixel to spherical coordinates.

procedure pix2ang_ring (nside, ipix, theta, phi)

int	nside			#I Resolution parameter
int	ipix			#I HEALPix index
double	theta			#O Latitude (rad from pole)
double	phi			#O Longitude (rad)

int	nl2, nl4, npix, ncap, iring, iphi, ip, ipix1
double	fact1, fact2, fodd, hip, fihip

begin
	npix = 12 * nside * nside
	ipix1 = ipix + 1
	nl2 = 2 * nside
	nl4 = 4 * nside
	ncap = 2 * nside * (nside - 1)
	fact1 = 1.5 * nside
	fact2 = 3.0 * nside * nside

	if (ipix1 <= ncap) {

	    hip = ipix1 / 2.
	    fihip = int (hip)
	    iring = int (sqrt (hip - sqrt (fihip))) + 1
	    iphi = ipix1 - 2 * iring * (iring - 1)

	    theta = acos (1. - iring * iring / fact2)
	    phi = (iphi - 0.5) * PI / (2. * iring)

	} else if (ipix1 <= nl2 * (5 * nside + 1)) {

	    ip    = ipix1 - ncap - 1
	    iring = (ip / nl4) + nside
	    iphi  = mod (ip, nl4) + 1

	    fodd  = 0.5 * (1 + mod (iring + nside, 2))
	    theta = acos ((nl2 - iring) / fact1)
	    phi   = (iphi - fodd) * PI / (2. * nside)

	} else {

	    ip    = npix - ipix1 + 1
	    hip   = ip/2.
	    
	    fihip = int (hip)
	    iring = int (sqrt (hip - sqrt (fihip))) + 1
	    iphi  = 4. * iring + 1 - (ip - 2. * iring * (iring-1))

	    theta = acos (-1. + iring * iring / fact2)
	    phi   = (iphi - 0.5) * PI / (2. * iring)

	}
end


# MK_XY2PIX
#
# Sets the array giving the number of the pixel lying in (x,y)
# x and y are in {1,128}
# the pixel number is in {0,128**2-1}
#
# if  i-1 = sum_p=0  b_p * 2^p
# then ix = sum_p=0  b_p * 4^p
# iy = 2*ix
# ix + iy in {0, 128**2 -1}

procedure mk_xy2pix (x2pix, y2pix)

int	x2pix[128], y2pix[128]

int	i, j, k, ip, id

begin
	do i = 1, 128
	    x2pix[i] = 0

	do i = 1, 128 {
	    j = i - 1
	    k = 0
	    ip = 1
	    while (j != 0) {
		id = mod (j, 2)
		j  = j / 2
		k  = ip * id + k
		ip = ip * 4
	    }
	    x2pix[i] = k
	    y2pix[i] = 2 * k
	}
end


# MK_PIX2XY
#
# Constructs the array giving x and y in the face from pixel number
# for the nested (quad-cube like) ordering of pixels.
# 
# The bits corresponding to x and y are interleaved in the pixel number.
# One breaks up the pixel number by even and odd bits.

procedure mk_pix2xy (pix2x, pix2y)

int	pix2x[1024], pix2y[1024]

int	kpix, jpix, ix, iy, ip, id

begin

	do kpix = 1, 1024
	    pix2x[kpix] = 0

	do kpix = 1, 1024 {
	    jpix = kpix - 1
	    ix = 0
	    iy = 0
	    ip = 1 
	    while (jpix != 0) {
		id = mod (jpix, 2)
		jpix = jpix / 2
		ix = id * ip + ix
		  
		id = mod (jpix, 2)
		jpix = jpix / 2
		iy = id * ip + iy
		  
		ip = 2 * ip
	    }

	    pix2x[kpix] = ix
	    pix2y[kpix] = iy
	}

end
