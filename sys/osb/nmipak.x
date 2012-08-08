# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<nmi.h>

.help nmi
.nf ___________________________________________________________________________
NMI -- Machine independent integer format conversion routines.  The NMI integer
format provides for three machine independent integer datatypes:

    NMI_BYTE	8 bit unsigned byte
    NMI_SHORT	16 bit twos complement signed integer
    NMI_LONG	32 bit twos complement signed integer

plus, more recently, two IEEE floating point formats:

    NMI_REAL	32 bit IEEE floating point
    NMI_DOUBLE	64 bit IEEE floating point

The NMI datatypes are the same as are used in the FITS transportable image
format.  In the case of the short and long integers, the most significant
bytes of an integer are given first.

The routines in this package are provided for converting to and from the
NMI format and the SPP format.  The latter format, of course, is potentially
quite machine dependent.  The implementation given here assumes that the
SPP datatypes include 16 bit and 32 bit twos complement integers; the ordering
of the bytes within these integer formats is described by the machine
constants BYTE_SWAP2 and BYTE_SWAP4.  Byte swapping for the IEEE floating
formats is defined by the machine constants IEEE_SWAP4 and IEEE_SWAP8.
.endhelp ______________________________________________________________________


# NMIPAK -- Pack a SPP array of type spp_type into a NMI array of type
# nmi_type.  The nmi_types are defined in nmi.h.

procedure nmipak (spp, nmi, nelems, spp_datatype, nmi_datatype)

int	spp[ARB]		#I input array of SPP integers
int	nmi[ARB]		#O output NMI format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code
int	nmi_datatype		#I NMI datatype code

begin
	switch (nmi_datatype) {
	case NMI_BYTE:
	    call nmipak8  (spp, nmi, nelems, spp_datatype)
	case NMI_SHORT:
	    call nmipak16 (spp, nmi, nelems, spp_datatype)
	case NMI_LONG:
	    call nmipak32 (spp, nmi, nelems, spp_datatype)
	case NMI_REAL:
	    call nmipakr (spp, nmi, nelems, spp_datatype)
	case NMI_DOUBLE:
	    call nmipakd (spp, nmi, nelems, spp_datatype)
	}
end
