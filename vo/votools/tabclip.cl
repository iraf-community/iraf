# TABCLIP -- Clip a table to the specified boundaries.

procedure tabclip (intab, outtab, xcol, ycol, x1, y1, x2, y2)

string  intab                           { prompt="Input table"              }
string  outtab                          { prompt="Output table"             }
string  xcol                            { prompt="Name of X column"         }
string  ycol                            { prompt="Name of Y column"         }
real    x1                              { prompt="Left X clipping value"    }
real    y1                              { prompt="Bottom Y clipping value"  }
real    x2                              { prompt="Right X clipping value"   }
real    y2                              { prompt="Top Y clipping value"     }

begin
        string  expr, in, out, xc, yc, ttab
        real    xv1, yv1, xv2, yv2, temp

        # Get the parameters
        in  = intab	;	out = outtab
        xc = xcol 	; 	yc  = ycol
        xv1 = x1 	; 	yv1  = y1
        xv2 = x2 	; 	yv2  = y2

	# Swap if needed.
	if (xv1 > xv2) { temp = xv2 ; xv2 = xv1 ; xv1 = temp }
	if (yv1 > yv2) { temp = yv2 ; yv2 = yv1 ; yv1 = temp }


        if (access (in) == no) 
            error (0, "tabclip: Input table doesn't exist.")
        if (access (out) == no) 
            error (0, "tabclip: Cannot open output table.")

        # Form the clipping expression.
        expr  = xc // " >= " // xv1 // " && "
        expr += yc // " >= " // yv1 // " && "
        expr += xc // " <= " // xv2 // " && "
        expr += yc // " <= " // yv2

        # Do the selection.
        if (in != out) {
            tselect (in, out, expr)
        } else {
            ttab = mktemp ("/tmp/tt")
            tselect (in, ttab, expr)
            copy (ttab, out)
            delete (ttab, verify-, >& "dev$null")
        }
end
