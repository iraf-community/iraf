include <error.h>
include <mef.h>

# MEF_WRHDR -- Append the header from an input PHU or extension to output file.

procedure mef_wrhdr (mefi, mefo, in_phdu)

pointer	mefi		#I input mef descriptor
pointer	mefo		#I output mef descriptor
bool	in_phdu		#I true if input header is Primary Header Unit.

pointer hb, sp, ln
int	output_lines, out
int	i, index, naxis, mef_kctype(), strncmp()
bool    endk, new_outf
errchk  open, fcopyo

define	nextb_ 99

begin
	call smark (sp)
	call salloc (ln, LEN_CARDNL, TY_CHAR)

	# At this point the input first header has been read

	hb = MEF_HDRP(mefi)
	if (Memc[hb] == NULL)
	   call error(13,"mef_wrhdr: input header buffer is empty")

	out = MEF_FD(mefo)

	new_outf = false
	if (MEF_ACMODE(mefo) == NEW_IMAGE)
	   new_outf = true

	output_lines = 0
	endk = false

	# If we want to copy the header with no modification
	if (MEF_KEEPXT(mefo) == YES) {
	    for (i=1; i<37; i=i+1) {
	       switch (mef_kctype(Memc[hb], index)) {
	       case END:
		   call mef_pakwr (out, Memc[hb])
		   endk = true
		   output_lines = i
		   break
	       default:
		   call mef_pakwr (out, Memc[hb])
		   hb = hb + LEN_CARDNL
	       }
	    }
	    goto nextb_
	} 

	# Check for 1st card
	if (strncmp (Memc[hb], "SIMPLE  ", 8) == 0) {
	    # Append extension to existing file
	    if (!new_outf) {            
	        call mef_encodec ("XTENSION", "IMAGE", 5, Memc[ln],
		    "Image extension")
	        call mef_pakwr (out, Memc[ln])
	    } else
	        call mef_pakwr (out, Memc[hb])
	} else if (strncmp (Memc[hb], "XTENSION", 8) == 0 ) {
	    if (new_outf) {
	        # Create a PHU
	        # Must create a dummy header if input extension is not image
		if (strncmp (MEF_EXTTYPE(mefi), "IMAGE", 5) != 0) {
		    Memc[ln] = EOS
		    call mef_dummyhdr (out, Memc[ln])
		    new_outf = false
	            call mef_pakwr (out, Memc[hb])
	        } else {
	            call mef_encodeb ("SIMPLE", YES, Memc[ln],
		        "Standard FITS format")
	            call mef_pakwr (out, Memc[ln])
		}
	    } else
	        call mef_pakwr (out, Memc[hb])
	} else {
	    # Is the wrong kind of header
#	    call eprintf ("File %s is not FITS\n")
#		call erract (EA_FATAL)
	    call sprintf (Memc[ln],LEN_CARD, "File %s is not FITS")
	        call pargstr(MEF_FNAME(mefi))
            call error(13, Memc[ln])			
	}
	hb = hb + LEN_CARDNL

	for (i=2; i<37; i=i+1) {
	   switch (mef_kctype(Memc[hb], index)) {
	   case BITPIX:
	      # Get to calculate totpix value
	      call mef_gvali (Memc[hb], MEF_BITPIX(mefi))
	   case NAXIS:
	      naxis = index
	      MEF_NDIM(mefi) = index
	      if (in_phdu && !new_outf && naxis == 0) {
	         call mef_pakwr (out, Memc[hb])
		 call mef_wrpgcount (out)
	         output_lines = output_lines + 2
		 hb = hb + LEN_CARDNL
		 next
	      }
	   case NAXISN:
	      call mef_gvali (Memc[hb], MEF_NAXIS(mefi,index))
	      call mef_pakwr (out, Memc[hb])
	      if (index == naxis) {
	          if (in_phdu && !new_outf ) {
		      # We are writing from a phu to ehu.
		      # 2 new cards PCOUNT and GCOUNT

		      call mef_wrpgcount (out)
		      output_lines = output_lines + 2
	          }			
	          if (!in_phdu && new_outf) {
		      # We are writing from a ehu to a phu
		      call mef_encodeb ("EXTEND", YES, Memc[ln],
			  "There may be extensions")
		      call mef_pakwr (out, Memc[ln])
		      output_lines = output_lines + 1
		  }   
	      }
	      hb = hb + LEN_CARDNL
	      next
	   case EXTEND, FILENAME:
	      if (!new_outf) {
		  # Do not put these cards  when going to an ehu
	          output_lines = output_lines - 1
	          hb = hb + LEN_CARDNL
		  next
	      }
	   case INHERIT:
	      # Eliminate INHERIT keyword from an input IMAGE extension
	      # when creating a new output file. If file already exists
	      # then pass the card along.

	      if (new_outf) {
	          output_lines = output_lines - 1
	          hb = hb + LEN_CARDNL
		  next
	      } 
	   case PCOUNT,GCOUNT,EXTNAME,EXTVER:   
	      # Do not put these cards into PHU
	      if (new_outf) {
	         output_lines = output_lines - 1
	         hb = hb + LEN_CARDNL
		 next
	      } 
	   case END:
	      call mef_pakwr (out, Memc[hb])
	      endk = true
	      output_lines = i + output_lines
	      break
	   default:
	      ;
	   }
	   call mef_pakwr (out, Memc[hb])
	   hb = hb + LEN_CARDNL

	} # end for loop

nextb_
	# See if we need to keep reading header
	#
	if (!endk) 
	   repeat {
	      for (i=1; i<37; i=i+1) {
	        if (strncmp (Memc[hb], "END     ", 8) == 0) {
	           call mef_pakwr (out, Memc[hb])
		   endk = true
		   output_lines = i + output_lines
		   break
	        }
	        call mef_pakwr (out, Memc[hb])
		hb = hb + LEN_CARDNL
	      }
              if (endk) break
	      
	   } #end repeat
	call mef_wrblank (out, output_lines)

	call sfree(sp)
end
