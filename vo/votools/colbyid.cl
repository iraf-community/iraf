#{  COLBYID -- Find the column number (1-indexed) containing the specified
#   value of the ID attribute.

procedure colbyid (table, id)

string	table			{ prompt = "Table name"			}
string	id			{ prompt = "ID attribute"		}

int     column   = 0            { prompt = "Found column number (or -1)"}
string  ucd      = ""           { prompt = "Found column UCD"           }
string  name     = ""           { prompt = "Found column Name"          }
bool	print    = no		{ prompt = "Print the field number?"	}
bool	found    = no		{ prompt = "Was attribute found??"	}

begin
    string  ltab, lid
    int     nfound

    ltab   = table		# get parameters
    lid    = id
    column = -1
    nfound = 0

    tinfo (ltab, ttout-)	# get table info
    for (i=1; i <= tinfo.ncols; i=i+1) {
        keypar (ltab, "TID" // i,  >& "dev$null")
	found = keypar.found
        if (keypar.found == yes && keypar.value == lid) { 
	    if (nfound == 0) {
                column = i

                keypar (ltab, "TTYPE" // i,  >& "dev$null")
                name = ""
                if (keypar.found == yes)
                    name   = keypar.value

                keypar (ltab, "TUCD" // i,  >& "dev$null")
                ucd = ""
                if (keypar.found == yes)
                    ucd     = keypar.value
	    }
            nfound = nfound + 1
        } 
    }

    if (nfound > 1)
	printf ("Warning: %d columns found with ID='%s'\n", nfound, id)

    if (print)
	print (column)
end
