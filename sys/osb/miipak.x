# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mii.h>

.help mii
.nf ___________________________________________________________________________
MII -- Machine independent integer format conversion routines.  The MII integer
format provides for three machine independent integer datatypes:

    MII_BYTE	8 bit unsigned byte
    MII_SHORT	16 bit twos complement signed integer
    MII_LONG	32 bit twos complement signed integer

plus, more recently, two IEEE floating point formats:

    MII_REAL	32 bit IEEE floating point
    MII_DOUBLE	64 bit IEEE floating point

The MII datatypes are the same as are used in the FITS transportable image
format.  In the case of the short and long integers, the most significant
bytes of an integer are given first.

The routines in this package are provided for converting to and from the
MII format and the SPP format.  The latter format, of course, is potentially
quite machine dependent.  The implementation given here assumes that the
SPP datatypes include 16 bit and 32 bit twos complement integers; the ordering
of the bytes within these integer formats is described by the machine
constants BYTE_SWAP2 and BYTE_SWAP4.  Byte swapping for the IEEE floating
formats is defined by the machine constants IEEE_SWAP4 and IEEE_SWAP8.
.endhelp ______________________________________________________________________


# MIIPAK -- Pack a SPP array of type spp_type into a MII array of type
# mii_type.  The mii_types are defined in mii.h.

procedure miipak (spp, mii, nelems, spp_datatype, mii_datatype)

int	spp[ARB]		#I input array of SPP integers
int	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code
int	mii_datatype		#I MII datatype code

begin
	switch (mii_datatype) {
	case MII_BYTE:
	    call miipak8  (spp, mii, nelems, spp_datatype)
	case MII_SHORT:
	    call miipak16 (spp, mii, nelems, spp_datatype)
	case MII_LONG:
	    call miipak32 (spp, mii, nelems, spp_datatype)
	case MII_REAL:
	    call miipakr (spp, mii, nelems, spp_datatype)
	case MII_DOUBLE:
	    call miipakd (spp, mii, nelems, spp_datatype)
	}
end
