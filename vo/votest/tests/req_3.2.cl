#
#  Access every record in a Registry result record.

# Set the test description string.
votest.descr = "Access every record in a Registry result record"

print ("------------------------------------------------------------------")
print ("Req  3.3:  Users shall have access to all information in the query")
print ("	   return record.")
print ("------------------------------------------------------------------")


fcache init

# -------------------------------------------
# Execute the test commands.
# -------------------------------------------


# -------------------------------------------
# Using the REGISTRY task we can print all 
# fields in the record as follows:
# -------------------------------------------

registry ("noao", verbose+, record=2, interactive-)
printf ("\n\n")


# -------------------------------------------
# Programmatically this is done as follows:
# -------------------------------------------
int   ires, icount

ires = regSearch ("noao", 0)
icount = regResCount (ires)

printf ("\nFound %d records:\n\n", icount)

printf ("Num  %10.10s  %s\n", "ShortName", "Title")
printf ("---  %10.10s  %s\n", "---------", "-----")

for (i=0; i < icount; i=i+1) {
    printf ("%3d  %10.10s  %s\n", (i+1), 
	trim(regValue (ires, "ShortName", i)),
	trim(regValue (ires, "Title", i))
}
