#{  VOTPOS -- Extract the main RA/Dec columns from a VOTable.

procedure votpos (input)

string	input			{ prompt = "Input VOTable"		}
string	output   = "STDOUT"	{ prompt = "Output filename"		}

int	ra_col   = INDEF	{ prompt = "RA column number"		}
int	dec_col  = INDEF	{ prompt = "RA column number"		}
string	rows     = "-"		{ prompt = "Rows to print"		}

bool    header   = no           { prompt = "Print output header?"       }
bool    number   = no           { prompt = "Number output rows?"        }
bool    plot     = no           { prompt = "Plot coordinate points?"    }
bool    overplot = yes          { prompt = "Overplot an existing plot?" }
int	color    = 1		{ prompt = "Plot marker color"		}
string  title	 = ""		{ prompt = "Plot title"			}

bool    verbose  = yes          { prompt = "Verbose output?"            }
int	status   = 0		{ prompt = "Service status code"	}

begin
    int     racol, deccol, lcolor
    string  lin, lout, ra_name, dec_name, tpos, pfields, ltitle
    bool    do_number, do_hdr, do_plot, do_overplot


    lin         = input
    lout        = output
    do_plot     = plot
    do_number   = number
    do_hdr      = header
    ltitle      = title

    do_overplot = overplot
    if (do_overplot)
        lcolor = color + 1
    else
	lcolor = 1


    # Get the RA column using either the old or new UCD.
    if (ra_col > 0) {
	racol = ra_col
	keypar (lin, "TTYPE" // racol)
	ra_name = keypar.value
    } else {
        colbyucd (lin, "POS_EQ_RA_MAIN")
        if (colbyucd.column < 0) {
            colbyucd (lin, "pos.eq.ra;meta.main")
            if (colbyucd.column < 0) {
	        error (0, "No RA column found in table")
	    } else {
	        racol = colbyucd.column
	        ra_name = colbyucd.name
	    }
        } else {
	    racol = colbyucd.column
	    ra_name = colbyucd.name
        }
    }

    # Get the Dec column using either the old or new UCD.
    if (dec_col > 0) {
	deccol = dec_col
	keypar (lin, "TTYPE" // deccol)
	dec_name = keypar.value
    } else {
        colbyucd (lin, "POS_EQ_DEC_MAIN")
        if (colbyucd.column < 0) {
            colbyucd (lin, "pos.eq.dec;meta.main")
            if (colbyucd.column < 0) {
	        error (0, "No DEC column found in table")
	    } else {
	        deccol = colbyucd.column
	        dec_name = colbyucd.name
	    }
        } else {
	    deccol = colbyucd.column
	    dec_name = colbyucd.name
        }
    }

    # Extract the position columnms
    tpos = mktemp ("tmp$pos")
    tprint (lin, prparam-, prdata+, showrow=do_number, showhdr=do_hdr,
	column=ra_name//","//dec_name, rows=rows, option="plain", > tpos)


    # Print the selected columns.
    if (lout == "STDOUT") {
	type (tpos)
    } else if (lout != "") {
	copy (tpos, lout, verbose-, >& "dev$null")
    }

    # Plot the positions?
    if (do_plot) {
	pfields = "1,2"
        if (do_number)
	    pfields = "2,3"

	fields (tpos, pfields) | graph ("STDIN", point+, 
		xformat="%H", yformat="%h", xlabel="RA", ylabel="Dec",
		title=ltitle, marker="box", szmarker=0.01, colors=lcolor,
		vx1=0.15, vx2=0.95, vy1=0.1, vy2=0.95, round-,
		append=do_overplot)
    }

    # Update and save the plot color.
    if (overplot && lcolor < 8)
	color = lcolor + 1
    else 
	color = 1
    overplot = no

    delete (tpos, verify-, >& "dev$null")
end
