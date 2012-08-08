#{  COLBYUCD -- Find the column number (1-indexed) containing the specified
#   value of the UCD attribute.

procedure colbyucd (table, ucd)

string	table			{ prompt = "Table name"			}
string	ucd			{ prompt = "UCD attribute"		}

int	column   = 0		{ prompt = "Found column number (or -1)"}
string	name	 = ""		{ prompt = "found column name"		}
string	id	 = ""		{ prompt = "found column ID"		}
bool	print    = no		{ prompt = "Print the field number?"	}
bool    found    = no           { prompt = "Was attribute found??"      }

begin
    string  ltab, lucd
    int     nfound

    ltab   = table		# get parameters
    lucd   = ucd
    column = -1
    nfound = 0

    tinfo (ltab, ttout-)	# get table info
    for (i=1; i <= tinfo.ncols; i=i+1) {
        keypar (ltab, "TUCD" // i,  >& "dev$null")
        found = keypar.found
        if (keypar.found == yes && keypar.value == lucd) { 
            if (nfound == 0) {
                column = i

                keypar (ltab, "TTYPE" // i,  >& "dev$null")
	        name = ""
                if (keypar.found == yes)
	            name   = keypar.value

                keypar (ltab, "TID" // i,  >& "dev$null")
	        id = ""
                if (keypar.found == yes)
	            id     = keypar.value
            }
            nfound = nfound + 1
        } 
    }

    if (nfound > 1)
        printf ("Warning: %d columns found with UCD='%s'\n", nfound, ucd)

    if (print)
	print (column)
end
