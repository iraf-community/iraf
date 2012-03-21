#
#  Demonstrate column selection by id/name/ucd <FIELD> attribute.

# Set the test description string.
votest.descr = "Demonstrate column selection by id/name/ucd <FIELD> attribute"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"

print ("------------------------------------------------------------------")
print ("Req  1.2.1: Users shall be able to identify a column in a VOTable")
print ("            by the 'id', 'name' or 'ucd' attribute of a <FIELD> or")
print ("            by column number.")
print ("------------------------------------------------------------------")


fcache init

int	ra_col, dec_col

# Execute the test commands.

unlearn ("colbyid")	; colbyid.print   = yes
unlearn ("colbyname")	; colbyname.print = yes
unlearn ("colbyucd")	; colbyucd.print  = yes


########################################################################

print ("\n------  Identify RA,DEC col by 'id' string  ------\n")

print ("\nLogical Path:  data$usno-b.xml")		# logical path
  colbyid ("data$usno-b.xml", "RA") | scan (ra_col)
  colbyid ("data$usno-b.xml", "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nHTTP URI:  " // s1)				# remote http URI
  colbyid (s1, "RA") | scan (ra_col)
  colbyid (s1, "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nFile URI:  " // s2)				# file URI
  colbyid (s2, "RA") | scan (ra_col)
  colbyid (s2, "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nFile URI:  " // s3)				# file URI
  colbyid (s3, "RA") | scan (ra_col)
  colbyid (s3, "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

unlearn ("colbyid")


########################################################################

print ("\n------  Identify RA,DEC col by 'name' string  ------\n")

print ("\nLogical Path:  data$usno-b.xml")		# logical path
  colbyname ("data$usno-b.xml", "RA") | scan (ra_col)
  colbyname ("data$usno-b.xml", "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nHTTP URI:  " // s1)				# remote http URI
  colbyname (s1, "RA") | scan (ra_col)
  colbyname (s1, "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nFile URI:  " // s2)				# file URI
  colbyname (s2, "RA") | scan (ra_col)
  colbyname (s2, "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nFile URI:  " // s3)				# file URI
  colbyname (s3, "RA") | scan (ra_col)
  colbyname (s3, "DEC") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

unlearn ("colbyname")


########################################################################

print ("\n------  Identify RA,DEC col by 'ucd' string  ------\n")

print ("\nLogical Path:  data$usno-b.xml")		# logical path
  colbyucd ("data$usno-b.xml", "POS_EQ_RA_MAIN") | scan (ra_col)
  colbyucd ("data$usno-b.xml", "POS_EQ_DEC_MAIN") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nHTTP URI:  " // s1)				# remote http URI
  colbyucd (s1, "POS_EQ_RA_MAIN") | scan (ra_col)
  colbyucd (s1, "POS_EQ_DEC_MAIN") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nFile URI:  " // s2)				# file URI
  colbyucd (s2, "POS_EQ_RA_MAIN") | scan (ra_col)
  colbyucd (s2, "POS_EQ_DEC_MAIN") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

print ("\nFile URI:  " // s3)				# file URI
  colbyucd (s3, "POS_EQ_RA_MAIN") | scan (ra_col)
  colbyucd (s3, "POS_EQ_DEC_MAIN") | scan (dec_col)
  printf ("RA,DEC are cols: %d and %d\n", ra_col, dec_col)

unlearn ("colbyucd")
