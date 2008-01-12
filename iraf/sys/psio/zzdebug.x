# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <time.h>
include <psset.h>


task   pstest        = t_pstest

# PSTEST -- Test the PSIO package.  This test program pretty-prints a file
# with a header message and page number suitable for output to a printer.

procedure t_pstest()

pointer	ps
int     fd, ip, op
char    fname[SZ_FNAME], date[SZ_TIME], line[SZ_LINE], outline[SZ_LINE]

pointer ps_open()
int     open(), getline()
long    clktime()
errchk	open, close, getline, ps_setfont, ps_open

begin
        # Get the file to format and date string.
        call clgstr ("filename", fname, SZ_FNAME)
        call cnvtime (clktime(0), date, SZ_TIME)

        # Open the file.
        iferr (fd = open (fname, READ_ONLY, TEXT_FILE))
	    call error (1, "Error opening file.")

        # Initialize the PSIO interface.
        iferr (ps = ps_open (STDOUT, NO))
	    call error (1, "Error opening PSIO interface.")
        call ps_header (ps, fname, "NOAO/IRAF", date)
        call ps_footer (ps, "PSIO Test Page", "", "")
        call ps_write_prolog (ps)

        # Output the text in a fixed-width font.
        call ps_setfont (ps, F_TELETYPE)

        call ps_linebreak (ps, NO)
        while (getline (fd, line) != EOF) {
	
            if (line[1] == EOS) {
		# Simply break on a newline.
                call ps_linebreak (ps, NO)

            } else {
	        # Detab the line to preserve the spacing.
	        ip = 1
	        op = 1
                while (line[ip] != EOS && op <= SZ_LINE) {
                    if (line[ip] == '\t') {
                        repeat {
                            outline[op] = ' '
                            op = op + 1
                        } until (mod(op,8) == 1)
                        ip = ip + 1
                    } else {
                        outline[op] = line [ip]
                        ip = ip + 1
                        op = op + 1
                    }
                }
                outline[op] = EOS

                # Output the line and a newline break.
                call ps_output (ps, outline, NO)            
                call ps_newline (ps)
            }
        }
        call close (fd) 			# close the file

        # Close the PSIO interface, this writes the PS trailer.
        call ps_close (ps)
end
