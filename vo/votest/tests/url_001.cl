#
#  Rudimentary URL handling: files

# Set the test description string.
votest.descr = "Rudimentary URL handling: files"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"

fcache init

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")              # logical path
type ("data$usno-b.xml")

print ("\nHTTP URI:  " // s1)                           # remote http URI
type (s1)

print ("\nFile URI:  " // s2)                           # file URI
type (s2)

print ("\nFile URI:  " // s3)                           # file URI
type (s3)

print ("\nHTTP Redirection")				# HTTP Redirection
type ("http://iraf.noao.edu/votest/redir_file.php")

print ("\nHTTP CGI query")				# HTTP CGI query
type http://iraf.noao.edu/scripts/irafhelp?dir		# command mode

type ("http://iraf.noao.edu/scripts/irafhelp?dir")	# program mode

s1 = "http://iraf.noao.edu/scripts/irafhelp?dir"	# script vars
type (s1)


#  Exclude from auto-test scripts since output will change over time.
#
#print ("\nHTTP Server-side URL")			# Server-side URL
#type ("http://casa.nrao.edu/index.shtml")

