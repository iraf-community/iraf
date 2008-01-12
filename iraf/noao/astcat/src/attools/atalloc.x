include "../../lib/astromdef.h"
include "../../lib/astrom.h"
include "../../lib/acatalog.h"
include "../../lib/aimparsdef.h"
include "../../lib/aimpars.h"


# AT_AINIT -- Initialize the main astrometry structure.

procedure at_ainit (at)

pointer	at			#I the pointer to the astrometry descriptor

begin
	call calloc (at, LEN_ASTROM, TY_STRUCT)
	AT_PIO(at) = NULL
	AT_PRCENTER(at) = NULL
	AT_PFILTER(at) = NULL
end


# AT_AFREE -- Free the main astrometry structure.

procedure at_afree (at)

pointer	at			#I the pointer to the astrometry descriptor

begin
	call mfree (at, TY_STRUCT)
end


# AT_RCINIT -- Initialize the field center structure.

procedure at_rcinit (at)

pointer	at			#I the pointer to the astrometry descriptor

pointer	fc

begin
	call calloc (fc, LEN_PRCENTER, TY_STRUCT)

	# Set the default field.
	AT_RCRA(fc) = 0.0d0
	AT_RCDEC(fc) = 0.0d0
	AT_RCRAWIDTH(fc) = 1.0d0
	AT_RCDECWIDTH(fc) = 1.0d0
	AT_RCRAUNITS(fc) = AT_HOURS
	AT_RCDECUNITS(fc) = AT_DEGREES
	call strcpy ("J2000", AT_RCSYSTEM(fc), SZ_FNAME)
	call strcpy ("", AT_RCSOURCE(fc), SZ_FNAME)

	# Initialize the internal data structures.
	#AT_RCCC(fc) = NULL
	AT_RCST(fc) = NULL

	AT_PRCENTER(at) = fc
end


# AT_RCFREE -- Free the field center structure.

procedure at_rcfree (at)

pointer	at			#I the pointer to the astrometry descriptor

pointer	fc

begin
	fc = AT_PRCENTER(at)

	# Close the field center symbol table.
	if (AT_RCST(fc) != NULL)
	    call stclose (AT_RCST(fc))
	AT_RCST(fc) = NULL

	# Close the coordinate structure.
	#if (AT_RCCC(fc) != NULL)
	    #call sk_close (AT_RCCC(fc))
	#AT_RCCC(fc) = NULL

	call mfree (AT_PRCENTER(at), TY_STRUCT)
	AT_PRCENTER(at) = NULL
end


# AT_IOINIT -- Initialize the i/o structure.

procedure at_ioinit (at)

pointer	at			#I the pointer to the i/o descriptor

pointer	io	

begin
	call calloc (io, LEN_PIO, TY_STRUCT)

	AT_CATALOGS(io) = EOS
	AT_SURVEYS(io) = EOS
	AT_IMAGES(io) = EOS
	AT_INPUT(io) = EOS
	AT_OUTPUT(io) = EOS
	AT_CATNAME(io) = EOS
	AT_SVNAME(io) = EOS
	AT_IMNAME(io) = EOS
	AT_INFNAME(io) = EOS
	AT_OUTFNAME(io) = EOS
	AT_CATDB(io) = EOS
	AT_IMDB(io) = EOS

	AT_PIO(at) = io
end


# AT_IOFREE -- Free the i/o structure.

procedure at_iofree (at)

pointer	at			#I the pointer to the i/o descriptor

pointer	io

begin
	io = AT_PIO(at)

	call mfree (io, TY_STRUCT)
	AT_PIO(at) = NULL
end


# AT_FSINIT -- Initialize the filtering / selection structure.

procedure at_fsinit (at)

pointer	at			#I the pointer to the astrometry descriptor

pointer	fs

begin
	call calloc (fs, LEN_PFILTER, TY_STRUCT)

	AT_FSORT(fs) = EOS
	AT_FREVERSE(fs) = NO
	AT_FREPLACE(fs) = YES

	call strcpy ("J2000", AT_FOSYSTEM(fs), SZ_LINE)
	call strcpy ("ra", AT_FIRA(fs), SZ_LINE)
	call strcpy ("dec", AT_FIDEC(fs), SZ_LINE)
	AT_FORAFORMAT(fs) = EOS
	AT_FODECFORMAT(fs) = EOS
	AT_FORAUNITS(fs) = AT_HOURS
	AT_FODECUNITS(fs) = AT_DEGREES

	call strcpy ("xp", AT_FIXP(fs), SZ_LINE)
	call strcpy ("yp", AT_FIYP(fs), SZ_LINE)
	call strcpy ("xc", AT_FIXC(fs), SZ_LINE)
	call strcpy ("yc", AT_FIYC(fs), SZ_LINE)
	AT_FOXFORMAT(fs) = EOS
	AT_FOYFORMAT(fs) = EOS

	call strcpy ("*", AT_FIELDS(fs), SZ_LINE)
	call strcpy ("yes", AT_FEXPR(fs), SZ_LINE)
	AT_FNAMES(fs) = EOS
	AT_FNTYPES(fs) = EOS
	AT_FNUNITS(fs) = EOS
	AT_FNFORMATS(fs) = EOS

	AT_PFILTER(at) = fs
end


# AT_FSFREE -- Free the filtering / selection structure.

procedure at_fsfree (at)

pointer	at			#I the pointer to the astrometry descriptor

pointer	fs

begin
	fs = AT_PFILTER(at)
	call mfree (fs, TY_STRUCT)
	AT_PFILTER(at) = NULL
end


# AT_WCINIT -- Initialize the default WCS structure

procedure at_wcinit (at)

pointer	at			#I the mail astrometry package descriptor

pointer	wc

begin
	call calloc (wc, LEN_PWCS, TY_STRUCT)
	AT_WXREF(wc) = INDEFD
	AT_WYREF(wc) = INDEFD
	AT_WXMAG(wc) = 1.0
	AT_WYMAG(wc) = 1.0
	AT_WXROT(wc) = 0.0
	AT_WYROT(wc) = 0.0
	AT_WRAREF(wc) = 0.0
	AT_WDECREF(wc) = 0.0
	AT_WRAUNITS(wc) = 0
	AT_WDECUNITS(wc) = 0
	AT_WPROJ(wc) = EOS
	AT_WSYSTEM(wc) = EOS

	AT_WCST(wc) = NULL

	AT_PWCS(at) = wc
end


# AT_WCFREE -- Free the default WCS structure

procedure at_wcfree (at)

pointer	at			#I the mail astrometry package descriptor

pointer	wc

begin
	wc = AT_PWCS(at)

	# Close the default wcspars parameters symbol table.
	if (AT_WCST(wc) != NULL)
	    call stclose(AT_WCST(wc))
	AT_WCST(wc) = NULL

	call mfree (wc, TY_STRUCT)
	AT_PWCS(at) = NULL
end


# AT_IMINIT -- Initialize the default mage data structure

procedure at_iminit (at)

pointer	at			#I the mail astrometry package descriptor

pointer	ip

begin
	call calloc (ip, LEN_PIMPARS, TY_STRUCT)
	AT_ESITELNG(ip) = INDEFD
	AT_ESITELAT(ip) = INDEFD
	AT_EMJDOBS(ip) = INDEFD

	AT_ESITEALT(ip) = INDEFR
	AT_ESITETZ(ip) = INDEFR
	AT_EDATAMIN(ip) = INDEFR
	AT_EDATAMAX(ip) = INDEFR
	AT_EGAIN(ip) = 1.0
	AT_ERDNOISE(ip) = 0.0
	AT_EWAVLEN(ip) = INDEFR
	AT_ETEMP(ip) = INDEFR
	AT_EPRESS(ip) = INDEFR

	AT_OBSERVAT(ip) = EOS

	AT_IMST(ip) = NULL

	AT_PIMPARS(at) = ip
end


# AT_IMFREE -- Free the default image data structure

procedure at_imfree (at)

pointer	at			#I the mail astrometry package descriptor

pointer	ip

begin
	ip = AT_PIMPARS(at)

	# Close the default wcspars parameters symbol table.
	if (AT_IMST(ip) != NULL)
	    call stclose(AT_IMST(ip))
	AT_IMST(ip) = NULL

	call mfree (ip, TY_STRUCT)
	AT_PIMPARS(at) = NULL
end
