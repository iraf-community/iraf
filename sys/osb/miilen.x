# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIILEN -- Determine the number of SPP integers to store nelems of type
# mii_type.  The mii_type are defined in mii.h.
#
# THIS PROCEDURE HAS BEEN OBSOLETED BY MIIPAKLEN.

int procedure miilen (nelems, mii_datatype)

int	nelems			#I number of MII data elements
int	mii_datatype		#I datatype of MII data

begin
	return (((nelems * abs(mii_datatype) / NBITS_BYTE + SZB_CHAR - 1) /
	    SZB_CHAR + SZ_INT32 - 1) / SZ_INT32)
end
