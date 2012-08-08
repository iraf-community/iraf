# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMILEN -- Determine the number of SPP integers to store nelems of type
# nmi_type.  The nmi_type are defined in nmi.h.
#
# THIS PROCEDURE HAS BEEN OBSOLETED BY NMIPAKLEN.

int procedure nmilen (nelems, nmi_datatype)

int	nelems			#I number of NMI data elements
int	nmi_datatype		#I datatype of NMI data

begin
	return (((nelems * abs(nmi_datatype) / NBITS_BYTE + SZB_CHAR - 1) /
	    SZB_CHAR + SZ_INT - 1) / SZ_INT)
end
