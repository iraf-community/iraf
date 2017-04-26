# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mii.h>


# MII.X -- This is a stand-alone port of the miiread/miiwrite routines from
# etc/osb, modified for IMFORT to use bfio.
#
#	status = i_miirdi (fp, spp, maxelem)
#	status = i_miirdl (fp, spp, maxelem)
#	status = i_miirdr (fp, spp, maxelem)
#
#	status = i_miiwri (fp, spp, nelem)
#	status = i_miiwrl (fp, spp, nelem)
#	status = i_miiwrr (fp, spp, nelem)
#
#	status = i_miirdc (fp, spp, maxchars)
#	status = i_miiwrc (fp, spp, nchars)


# MIIRDI -- Read a block of data stored externally in MII integer format.
# Data is returned in the format of the local host machine.

int procedure i_miirdi (fp, spp, maxelem)

pointer	fp			#I input file
int	spp[ARB]		#O receives data
int	maxelem			#I max number of data elements to be read

pointer	sp, bp
int	pksize, nchars, nelem
int	miipksize(), miinelem(), bfrseq()

begin
	pksize = miipksize (maxelem, MII_INT)
	nelem  = EOF

	if (pksize > maxelem * SZ_INT) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = bfrseq (fp, Memc[bp], pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_INT))
		call miiupki (Memc[bp], spp, nelem, TY_INT)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = bfrseq (fp, spp, pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_INT))
		call miiupki (spp, spp, nelem, TY_INT)
	    }
	}

	return (nelem)
end


# MIIRDL -- Read a block of data stored externally in MII long integer format.
# Data is returned in the format of the local host machine.

int procedure i_miirdl (fp, spp, maxelem)

pointer	fp			#I input file
long	spp[ARB]		#O receives data
int	maxelem			#I max number of data elements to be read

pointer	sp, bp
int	pksize, nchars, nelem
int	miipksize(), miinelem(), bfrseq()

begin
	pksize = miipksize (maxelem, MII_LONG)
	nelem  = EOF

	if (pksize > maxelem * SZ_LONG) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = bfrseq (fp, Memc[bp], pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_LONG))
		call miiupkl (Memc[bp], spp, nelem, TY_LONG)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = bfrseq (fp, spp, pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_LONG))
		call miiupkl (spp, spp, nelem, TY_LONG)
	    }
	}

	return (nelem)
end


# MIIRDR -- Read a block of data stored externally in MII real format.
# Data is returned in the format of the local host machine.

int procedure i_miirdr (fp, spp, maxelem)

pointer	fp			#I input file
real	spp[ARB]		#O receives data
int	maxelem			# max number of data elements to be read

pointer	sp, bp
int	pksize, nchars, nelem
int	miipksize(), miinelem(), bfrseq()

begin
	pksize = miipksize (maxelem, MII_REAL)
	nelem  = EOF

	if (pksize > maxelem * SZ_REAL) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, pksize, TY_CHAR)

	    nchars = bfrseq (fp, Memc[bp], pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_REAL))
		call miiupkr (Memc[bp], spp, nelem, TY_REAL)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = bfrseq (fp, spp, pksize)
	    if (nchars != EOF) {
		nelem = min (maxelem, miinelem (nchars, MII_REAL))
		call miiupkr (spp, spp, nelem, TY_REAL)
	    }
	}

	return (nelem)
end


# MIIWRI -- Write a block of data to a file in MII integer format.
# The input data is in the host system native binary format.

int procedure i_miiwri (fp, spp, nelem)

pointer	fp			#I output file
int	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

pointer	sp, bp
int	bufsize, status
int	miipksize(), bfwseq()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nelem, MII_INT)
	call salloc (bp, bufsize, TY_CHAR)

	call miipaki (spp, Memc[bp], nelem, TY_INT)
	if (bfwseq (fp, Memc[bp], bufsize) == ERR)
	    status = ERR

	call sfree (sp)
	return (status)
end


# MIIWRL -- Write a block of data to a file in MII long integer format.
# The input data is in the host system native binary format.

int procedure i_miiwrl (fp, spp, nelem)

pointer	fp			#I output file
long	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

pointer	sp, bp
int	bufsize, status
int	miipksize(), bfwseq()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nelem, MII_LONG)
	call salloc (bp, bufsize, TY_CHAR)

	call miipakl (spp, Memc[bp], nelem, TY_LONG)
	if (bfwseq (fp, Memc[bp], bufsize) == ERR)
	    status = ERR

	call sfree (sp)
	return (status)
end


# MIIWRR -- Write a block of data to a file in MII real format.
# The input data is in the host system native binary format.

int procedure i_miiwrr (fp, spp, nelem)

pointer	fp			#I output file
real	spp[ARB]		#I native format data to be written
int	nelem			#I number of data elements to be written

pointer	sp, bp
int	bufsize, status
int	miipksize(), bfwseq()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nelem, MII_REAL)
	call salloc (bp, bufsize, TY_CHAR)

	call miipakr (spp, Memc[bp], nelem, TY_REAL)
	if (bfwseq (fp, Memc[bp], bufsize) == ERR)
	    status = ERR

	call sfree (sp)
	return (status)
end


# MIIRDC -- Read a block of character data stored externally in MII format.
# Data is returned in the machine independent character format.

int procedure i_miirdc (fp, spp, maxchars)

pointer	fp			#I input file
char	spp[ARB]		#O receives data
int	maxchars		#I max number of chars to be read

pointer	sp, bp
int	pksize, nchars
int	miipksize(), miinelem(), bfrseq()

begin
	pksize = miipksize (maxchars, MII_BYTE)
	nchars = max (maxchars, pksize)

	if (nchars > maxchars) {
	    # Read data into local buffer and unpack into user buffer.

	    call smark (sp)
	    call salloc (bp, nchars, TY_CHAR)

	    nchars = bfrseq (fp, Memc[bp], pksize)
	    if (nchars != EOF) {
		nchars = min (maxchars, miinelem (nchars, MII_BYTE))
		call miiupk8 (Memc[bp], spp, nchars, TY_CHAR)
	    }

	    call sfree (sp)
	
	} else {
	    # Read data into user buffer and unpack in place.

	    nchars = bfrseq (fp, spp, pksize)
	    if (nchars != EOF) {
		nchars = min (maxchars, miinelem (nchars, MII_BYTE))
		call miiupk8 (spp, spp, nchars, TY_CHAR)
	    }
	}

	return (nchars)
end


# MIIWRC -- Write a block of character data to a file in MII format.
# The input data is assumed to be in a machine independent format. 

int procedure i_miiwrc (fp, spp, nchars)

pointer	fp			#I output file
char	spp[ARB]		#I data to be written
int	nchars			#I number of chars units to be written

pointer	sp, bp
int	bufsize, status
int	miipksize(), bfwseq()

begin
	status = OK
	call smark (sp)

	bufsize = miipksize (nchars, MII_BYTE)
	call salloc (bp, bufsize, TY_CHAR)

	call miipak8 (spp, Memc[bp], nchars, TY_CHAR)
	if (bfwseq (fp, Memc[bp], bufsize) == ERR)
	    status = ERR

	call sfree (sp)
	return (status)
end
