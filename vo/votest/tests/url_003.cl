#
#  Rudimentary URL handling: votables

# Set the test description string.
votest.descr = "Rudimentary URL handling: votables"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"

fcache init

unlearn ("tprint")
tprint.rows="1-5"
tprint.columns="id,sRA,sDec"

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")              # logical path
tprint ("data$usno-b.xml", rows="1-5")

print ("\nHTTP URI:  " // s1)                           # remote http URI
tprint (s1, rows="1-5")

print ("\nFile URI:  " // s2)                           # file URI
tprint (s2, rows="1-5")

print ("\nFile URI:  " // s3)                           # file URI
tprint (s3, rows="1-5")

print ("\nHTTP Redirection")				# HTTP Redirection
tprint ("http://iraf.noao.edu/votest/redir_vot.php", rows="1-5")

print ("\nHTTP CGI query")				# HTTP CGI query
tprint http://iraf.noao.edu/scripts/voget?t=votable&f=usno-b.xml rows="1-5"
tprint ("http://iraf.noao.edu/scripts/voget?t=votable&f=usno-b.xml",rows="1-5")

s1 = "http://iraf.noao.edu/scripts/voget?t=votable&f=usno-b.xml"
tprint (s1, rows="1-5")

