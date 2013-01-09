/* This header file is included only by tbfxff.c.

There are three SPP FITSIO subroutines that have arrays of character strings
in their calling sequences.  The subroutines are fsibin, fsicol, and fsphbn,
and they are called by tbfdef and tbfnew.  The declared lengths of the
character strings must be the same in each of these subroutines, and the
macros for the declared lengths are defined in three separate header files.
These header files are:

	fitsio_spp.h              (this file, for the SPP/C interface)
	tblfits.h                 (used by tbfdef.x and tbfnew.x, and others)
	fitsio/fitssppb/fitsio.h  (used by the SPP FITSIO interface)

The extra char for end-of-string is added transparently for SPP code,
and it is added explicitly in tbfxff.c.
*/

# define  SZ_FTTYPE  70		/* length of column name string */
# define  SZ_FTFORM  70		/* len of col datatype and display fmt str */
# define  SZ_FTUNIT  70		/* length of column units string */
