# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fio.h>
include <votParse_spp.h>


# VOTABLE.X -- Utility procedures for dealing with VOTables.


# VOTINIT -- Initialize the VOT struct, parse the document and save the
# summary.  We return the VOT struct pointer itself, the caller is
# responsible for accessing the VOT_ROOT to get the raw root handle

pointer procedure votinit (votable)

char	votable[ARB]				#i VOTable file name

pointer	vot, vot_handle

#  Declare the libVOTable functions we'll be using.
int	vx_openVOTABLE(), vx_getRESOURCE(), vx_getTABLE(), vx_getDATA()
int	vx_getTABLEDATA(), vx_getFIELD(), vx_getINFO(), vx_getPARAM()
int	vx_getNCols(), vx_getNRows(), vx_getLength()

begin
	# Allocate the structure.
	call calloc (vot, SZ_VOT_STRUCT, TY_STRUCT)

	# Open and parse the votable.
	vot_handle = vx_openVOTABLE (votable)
	if (vot_handle <= 0) {
	    call eprintf ("Cannot open file: '%s'\n")
		call pargstr (votable)
	    return (NULL)
	}
	VOT_ROOT(vot) = vot_handle

	# Now get various handles from the table.
	VOT_RES(vot)   = vx_getRESOURCE (vot_handle)
	VOT_TAB(vot)   = vx_getTABLE (VOT_RES(vot))
	VOT_DATA(vot)  = vx_getDATA (VOT_TAB(vot))
	VOT_TDATA(vot) = vx_getTABLEDATA (VOT_DATA(vot))

    	VOT_INFO(vot)  = vx_getINFO (VOT_RES(vot))
    	VOT_PARAM(vot) = vx_getPARAM (VOT_RES(vot))
    	VOT_FIELD(vot) = vx_getFIELD (VOT_TAB(vot))

	VOT_NRES(vot)  = vx_getLength (VOT_RES(vot))
	VOT_NCOLS(vot) = vx_getNCols (VOT_TDATA(vot))
	VOT_NROWS(vot) = vx_getNRows (VOT_TDATA(vot))

	return (vot) 				# return the struct pointer
end


# VOTCLOSE -- Close the VOT struct and free any resources.

procedure votclose (vot)

pointer	vot					#i VOT struct pointer

begin
	call vx_closeVOTABLE (VOT_ROOT(vot))
	call mfree (vot, TY_STRUCT)
end

# IS_VOTABLE --  Utility routine to determine if the named file is a VOTable
# XML document.

define	VOT_MAXLINES		10

bool procedure is_votable (fname)

char	fname[ARB]				#i local filename

int	i, fd, nchars
bool	stat
char	buf[SZ_LINE]

int	open (), access (), getline (), strsearch ()

begin
	stat = FALSE

	if (access (fname, 0, 0) == NO)
	    return (stat)

	iferr {
            fd = open (fname, READ_ONLY, TEXT_FILE)

	    # Look for a "<VOTABLE>" element in the first 10 lines of the file.
	    for (i=0; i < VOT_MAXLINES; i = i + 1) {
                call aclrc (buf, SZ_LINE)
                nchars = getline (fd, buf)
		if (nchars == EOF)
		    break

		call strupr (buf)
                if (strsearch (buf, "<VOTABLE") > 0)
                    stat = TRUE
            }
            call close (fd)
	} then
	    stat = FALSE

	return (stat)
end


# VOT_CONVERT -- Convert a VOTable to some other format.

define VOT_FMTS 	"|ascii|asv|bsv|csv|tsv|html|shtml|fits|xml|raw|votable"

define ASCII   1                       # ascii separated values
define ASV     2                       # ascii separated values
define BSV     3                       # bar separated values
define CSV     4                       # comma separated values
define TSV     5                       # tab separated values
define HTML    6                       # standalone HTML document
define SHTML   7                       # single HTML <table> element
define FITS    8                       # FITS binary table
define XML     9                       # VOTable alias
define RAW     10                      #    "      "
define VOTBL   11                      #    "      "

int procedure vot_convert (in, out, fmt)

char	in[ARB]					#i VOTable file name
char	out[ARB]				#i FITS bintable file name
char	fmt[ARB]				#i format name

pointer	sp, nodename, buf, vot_handle
char	osfn[SZ_PATHNAME], cnvname[SZ_PATHNAME], format[SZ_LINE]
int	vfd, status, ip, opt, delim, infile, outfile

int	vx_openVOTABLE()
int	vfnopen(), vfnmapu(), access(), ki_gnode(), strdic(), strncmp()
int	open(), getline()
bool	streq()

begin
	call smark (sp)
	call salloc (nodename, SZ_FNAME, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)


	# Map input VFN to OSFN.
	ip = 1
	if (strncmp (in, "http://", 7) == 0) {
	    call strcpy (in, osfn, SZ_PATHNAME)
	} else {
	    vfd = vfnopen (in, READ_ONLY)
	    status = vfnmapu (vfd, osfn, SZ_PATHNAME)
	    call vfnclose (vfd, VFN_NOUPDATE)

	    # If the file resides on the local node strip the node name,
	    # returning a legal host system filename as the result.
	    if (ki_gnode (osfn, Memc[nodename], delim) == 0)
	        ip = delim + 1
	}

        # Create a tempfile name for the converted output file.
        call mktemp ("/tmp/vo", cnvname, SZ_PATHNAME)
        call strcat (".", cnvname, SZ_PATHNAME)
        call strcat (fmt, cnvname, SZ_PATHNAME)


	# Validate the format.
	opt = strdic (fmt, format, SZ_LINE, VOT_FMTS)
        if (opt == 0) {
            call eprintf ("Invalid output format '%s'\n")
		call pargstr (fmt)
	    call sfree (sp)
            return (ERR)
        }
        vot_handle = vx_openVOTABLE (osfn[ip])
        if (vot_handle <= 0) {
            call eprintf ("Cannot open file: '%s'\n")
                call pargstr (osfn[ip])
            return (ERR)
        }

        if (opt == ASCII || opt == ASV)
            call vx_writeASV (vot_handle, cnvname)
        if (opt == BSV)
            call vx_writeBSV (vot_handle, cnvname)
        if (opt == CSV)
            call vx_writeCSV (vot_handle, cnvname)
        if (opt == TSV)
            call vx_writeTSV (vot_handle, cnvname)
        if (opt == HTML)
            call vx_writeHTML (vot_handle, osfn[ip], cnvname)#
        if (opt == SHTML)
            call vx_writeSHTML (vot_handle, osfn[ip], cnvname)#
        if (opt == FITS)
            call vx_writeFITS (vot_handle, cnvname)
        if (opt == VOTBL || opt == XML || opt == RAW)
            call vx_writeXML (vot_handle, cnvname)
        call vx_closeVOTABLE (vot_handle)

	if (access (cnvname,0,0) == NO) {
	    call eprintf ("Cannot convert %s to '%s'\n")
		call pargstr (osfn[ip])
		call pargstr (fmt)
	    return (ERR)
	}

        # Delete the downloaded XML file, copy the bintable into its
        # place and delete the converted output filename.
	if (streq (in, out))
            call delete (in)


	# Copy converted file to output file.  Works for STDOUT/STDERR as
	# well.
	infile = open (cnvname, READ_ONLY, TEXT_FILE)
	outfile = open (out, NEW_FILE, TEXT_FILE)

	while (getline (infile, Memc[buf]) != EOF)
	    call putline (outfile, Memc[buf])

	call close (infile)
	call close (outfile)

        call delete (cnvname) 		# delete the temporary converted file
	call sfree (sp)
	return (OK)
end


# VOT_TO_FITS -- Convert a VOTable to a FITS bintable.

int procedure vot_to_fits (in, out)

char	in[ARB]					#i VOTable file name
char	out[ARB]				#i FITS bintable file name

pointer	sp, nodename, vot_handle
char	osfn[SZ_PATHNAME], cnvname[SZ_PATHNAME]
int	vfd, status, ip, delim

int	vx_openVOTABLE()
int	vfnopen(), vfnmapu(), access(), ki_gnode()
bool	streq()

begin
	call smark (sp)
	call salloc (nodename, SZ_FNAME, TY_CHAR)

	# Map input VFN to OSFN.
	vfd = vfnopen (in, READ_ONLY)
	status = vfnmapu (vfd, osfn, SZ_PATHNAME)
	call vfnclose (vfd, VFN_NOUPDATE)

	# If the file resides on the local node strip the node name,
	# returning a legal host system filename as the result.
	if (ki_gnode (osfn, Memc[nodename], delim) == 0)
	    ip = delim + 1
	else
	    ip = 1


        # Create a tempfile name for the converted output file.
        call mktemp ("/tmp/vo", cnvname, SZ_PATHNAME)
        call strcat (".fits", cnvname, SZ_PATHNAME)

        # Convert the file from VOTable to FITS bintable.
        vot_handle = vx_openVOTABLE (osfn[ip])
        if (vot_handle <= 0) {
            call eprintf ("Cannot open file: '%s'\n")
                call pargstr (osfn[ip])
            return (ERR)
        }
        call vx_writeFITS (vot_handle, cnvname)
        call vx_closeVOTABLE (vot_handle)

	if (access (cnvname,0,0) == NO)
	    return (ERR)

        # Delete the downloaded XML file, copy the bintable into its
        # place and delete the converted output filename.
	if (streq (in, out))
            call delete (in)

        call fcopy (cnvname, out)   	# copy converted file to output file
        call delete (cnvname) 		# delete the temporary converted file

	call sfree (sp)

	return (OK)
end


# VOT_FROM_FITS -- Convert from a FITS bintable to a VOTable.

int procedure vot_from_fits (in, out)

char	in[ARB]					#i FITS bintable file name
char	out[ARB]				#i VOTable file name

pointer	vot_handle
char	osfn[SZ_PATHNAME], cnvname[SZ_PATHNAME]
int	vfd, status

int	vx_openVOTABLE()
int	vfnopen(), vfnmapu()
bool	streq()

begin
	# Map input VFN to OSFN.
	vfd = vfnopen (in, READ_ONLY)
	status = vfnmapu (vfd, osfn, SZ_PATHNAME)
	call vfnclose (vfd, VFN_NOUPDATE)

        # Create a tempfile name for the converted output file.
        call mktemp ("/tmp/vo", cnvname, SZ_PATHNAME)
        call strcat (".xml", cnvname, SZ_PATHNAME)

        # Convert the file from VOTable to FITS bintable.
        vot_handle = vx_openVOTABLE (osfn)
        if (vot_handle <= 0) {
            call eprintf ("Cannot open file: '%s'\n")
                call pargstr (osfn)
            return (ERR)
        }
        call vx_writeXML (vot_handle, cnvname)
        call vx_closeVOTABLE (vot_handle)

        # Delete the downloaded XML file, copy the bintable into its
        # place and delete the converted output filename.
	if (streq (in, out))
            call delete (in)

        call fcopy (cnvname, out)   	# copy converted file to output file
        call delete (cnvname) 		# delete the temporary converted file

	return (OK)
end
