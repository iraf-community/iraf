#{  COLBYNAME -- Find the column number (1-indexed) containing the specified
#   value of the NAME attribute.

procedure colbyname (table, name)

string	table			{ prompt = "Table name"			}
string	name			{ prompt = "NAME attribute"		}

int     column   = 0            { prompt = "Found column number (or -1)"}
string  ucd      = ""           { prompt = "Found column name"          }
string  id       = ""           { prompt = "Found column ID"            }
bool	print    = no		{ prompt = "Print the field number?"	}
bool    found    = no           { prompt = "Was attribute found??"      }

begin
    string  ltab, lname
    int     nfound


    ltab   = table		# get parameters
    lname  = name
    column = -1
    nfound = 0

    tinfo (ltab, ttout-)	# get table info
    for (i=1; i <= tinfo.ncols; i=i+1) {
        keypar (ltab, "TTYPE" // i,  >& "dev$null")
	found = keypar.found
        if (keypar.found == yes && keypar.value == lname) { 
            if (nfound == 0) {
                column = i

                keypar (ltab, "TUCD" // i,  >& "dev$null")
                ucd = ""
                if (keypar.found == yes)
                    ucd   = keypar.value

                keypar (ltab, "TID" // i,  >& "dev$null")
                id = ""
                if (keypar.found == yes)
                    id     = keypar.value
            }
        } 
    }

    if (nfound > 1)
        printf ("Warning: %d columns found with NAME='%s'\n", nfound, name)

    if (print)
	print (column)
end
