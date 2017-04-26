#{ BATCHRED -- Script file to generate another script file
#  which runs several ONEDSPEC tasks in an automated fashion.
#
#  Currently the following procedures are automated:
#    1. STANDARD
#    2. SENSFUNC
#    3. BSWITCH
#    4. CALIBRATE
#    5. ADDSETS
#

{
# Say hello to the guy on the other side of the screen and check batch file.
print ("\n----B A T C H   I I D S / I R S   F I L E   G E N E R A T O R----\n")

s2 = "process.cl"			# Batch file to be created.
if (access (s2)) {
    print ("A batch file already exists - ")
    if (query)
        delete (s2, verify=no)
}

# Initialize
rt = input				# Root name for spectra
ot = output				# Output root name
ttl = ">>&'" + ttylog + "')\n"		# Log file for tty output

out = rt
s1 = ""
st = ""
sns = ""
stat = ""
print ("i = ", start_rec, >>s2)

if (standard) {				# STANDARD?
    print ("\n#---STANDARD---\n")
    print ("\n#---STANDARD---\n", >>s2)

    st = std				# STD file
    if (access (st)) {
	print (st, " - already exists")
	if (query)
            delete (st, verify=no)
    }

    # Loop over all stars
    b1 = yes
    while (b1) {
        # Check that the last entry was different - otherwise end input
	records = ""
    	s3 = records
    	if (s3 == "")
    	    b1 = no
        else {
    	    print ("standard (input='",rt,"',output='",st,"',",>>s2)
    	    print ("\trecords='",s3,"',",>>s2)
    	    print ("\tstar_name='",star_name,"',beam_switch=yes,",>>s2)
	    print ("\tsamestar=yes,apertures='',bandwidth=INDEF,",>>s2)
	    print ("\tbandsep=INDEF,interact=no,",ttl,>>s2)
        }
    }

    print ("")
}


if (sensfunc) {				# SENSFUNC?
    print ("\n#---SENSFUNC---\n")
    print ("\n#---SENSFUNC---\n", >>s2)

    if (st == "")
	st = std			# STD file
    sns = sensitivity			# Sensitivity image
    stat = stats			# Statistics file

    print ("\nsensfunc (standards='",st,"',sensitivity='",sns,"',",>>s2)
    print ("\tlogfile='",stat,"',apertures='',ignoreaps=no,",>>s2)
    print ("\tfunction='",function,"',order=",order,",",>>s2)
    print ("\tinteract=no,",ttl,>>s2)

    print ("")
}


if (bswitch) {				# BSWITCH?
    print ("\n#---BSWITCH---\n")
    print ("\n#---BSWITCH---\n", >>s2)

    # Save starting output record number
    in = out
    out = "b" // ot
    wt = weight				# Weighting?
    if (stat == "")
	stat = stats			# Statistics file

    # Accumulate records
    print ("next_rec = i", >>s2)
    b1 = yes
    while (b1) {
	records = ""
        s3 = records
        if (s3 == "")
    	    b1 = no
        else {
	    print ("j = next_rec", >> s2)
    	    print ("bswitch (input='",in,"',output='",out,"',",>>s2)
    	    print ("\trecords='",s3,"',stats='",stat,"',",>>s2)
    	    print ("\tweighting=",wt,",subset=",subset,",",>>s2)
	    print ("\tstart_rec=j,",>>s2)
    	    print ("\twave1=",wave1,",wave2=",wave2,",",ttl,>>s2)
        }
    }

    # Output records
    print ("j = next_rec", >>s2)
    s1 = "str (i) // '-' // str(j-1)"
    print ("s1 = ", s1, >>s2)

    print ("")
}

if (calibrate) {			# CALIBRATE?
    print ("\n#---CALIBRATE---\n")
    print ("\n#---CALIBRATE---\n", >>s2)

    in = out
    out = "c" // ot
    if (sns == "")
        sns = sensitivity		# Sensivity file name

    if (s1 == "") {
	records = ""
	s1 = records
        print ("s1 = '", s1, "'", >>s2)
    }

    print ("calibrate (input='",in,"',output='",out,"',records=s1,",>>s2)
    print ("\tignoreaps=no,",>>s2)
    print ("\textinct=no,flux=yes,",>>s2)
    print ("\tsensitivity='",sns,"',fnu=",fnu,",",ttl,>>s2)

    print ("")
}

if (addsets) {				# ADDSETS?
    print ("\n#---ADDSETS---\n")
    print ("\n#---ADDSETS---\n", >>s2)

    in = out
    out = "a" // ot
    if (s1 == "") {
	records = ""
	s1 = records
        print ("s1 = '", s1, "'", >>s2)
    }

    print ("addsets (input='",in,"',output='",out,"',records=s1,",>>s2)
    print ("\tstart_rec=i,subset=2,",ttl,>>s2)
}

# All done with generator.  Ask whether to execute it.
print ("File generation complete - filename=",s2)
if (proceed == no)			# Execute batch file?
    bye
}

# Execute generated batch file
process &
