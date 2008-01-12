# AT_MWSHOW -- Print a quick summary of the current wcs.

procedure at_mwshow (mwim, ltv, ltm, w, r, cd, ndim)

pointer mwim                    # pointer to the current wcs
double  ltv[ARB]                # the lterm offsets
double  ltm[ndim,ARB]           # the lterm rotation matrix
double  w[ARB]                  # the fits crval parameters
double  r[ARB]                  # the fits crpix parameters
double  cd[ndim,ARB]            # the fits rotation matrix
int     ndim                    # the dimension of the wcs

int     i,j
pointer sp, str
errchk  mw_gwattrs()

begin
        # Allocate working space.
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)

        # Print the axis banner.
        call printf ("   AXIS   ")
        do i = 1, ndim {
            call printf ("%8d  ")
                call pargi (i)
        }
        call printf ("\n")

        # Print the crval parameters.
        call printf ("   CRVAL  ")
        do i = 1, ndim {
            call printf ("%8g  ")
                call pargd (w[i])
        }
        call printf ("\n")

        # Print the crpix parameters.
        call printf ("   CRPIX  ")
        do i = 1, ndim {
            call printf ("%8g  ")
                call pargd (r[i])
        }
        call printf ("\n")

        # Print the cd matrix.
        do i = 1, ndim {
            call printf ("   CD %d   ")
                call pargi (i)
            do j = 1, ndim {
                call printf ("%8g  ")
                    call pargd (cd[j,i])
            }
            call printf ("\n")
        }

        # Print the ltv parameters.
        call printf ("   LTV    ")
        do i = 1, ndim {
            call printf ("%8g  ")
                call pargd (ltv[i])
        }
        call printf ("\n")

        # Print the ltm matrix.
        do i = 1, ndim {
            call printf ("   LTM %d  ")
                call pargi (i)
            do j = 1, ndim {
                call printf ("%8g  ")
                    call pargd (ltm[i,j])
            }
            call printf ("\n")
        }

        # Print the transformation type.
        call printf ("   WTYPE  ")
        do i = 1, ndim {
            iferr (call mw_gwattrs (mwim, i, "wtype", Memc[str], SZ_LINE))
                Memc[str] = EOS
            call printf ("%8s  ")
                call pargstr (Memc[str])
        }
        call printf ("\n")

        # Print the axis type.
        call printf ("   AXTYPE ")
        do i = 1, ndim {
            iferr (call mw_gwattrs (mwim, i, "axtype", Memc[str], SZ_LINE))
                Memc[str] = EOS
            call printf ("%8s  ")
                call pargstr (Memc[str])
        }
        call printf ("\n")

        # Print the units.
        call printf ("   UNITS  ")
        do i = 1, ndim {
            iferr (call mw_gwattrs (mwim, i, "units", Memc[str], SZ_LINE))
                Memc[str] = EOS
            call printf ("%8s  ")
                call pargstr (Memc[str])
        }
        call printf ("\n")

        # Print the label.
        call printf ("   LABEL  ")
        do i = 1, ndim {
            iferr (call mw_gwattrs (mwim, i, "label", Memc[str], SZ_LINE))
                Memc[str] = EOS
            call printf ("%8s  ")
                call pargstr (Memc[str])
        }
        call printf ("\n")

        # Print the format.
        call printf ("   FORMAT ")
        do i = 1, ndim {
            iferr (call mw_gwattrs (mwim, i, "format", Memc[str], SZ_LINE))
                Memc[str] = EOS
            call printf ("%8s  ")
                call pargstr (Memc[str])
        }
        call printf ("\n")

        call printf ("\n")

        call sfree (sp)
end
