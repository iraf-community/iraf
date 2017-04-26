#
#  Rudimentary URL handling: images

# Set the test description string.
votest.descr = "Rudimentary URL handling: images"

# Convert the data$logical to a local path.
s1 = data_url // "/sif.fits"
s2 = "file://" // data_path // "/sif.fits"
s3 = "file:///localhost" // data_path // "/sif.fits"

# Execute the test commands.
print ("\nLogical Path:  data$sif.fits")                # logical path
imstat ("data$sif.fits", field="mean,min,max")

print ("\nHTTP URI:  " // s1)                           # remote http URI
imstat (s1, field="mean,min,max")

print ("\nFile URI:  " // s2)                           # file URI
imstat (s2, field="mean,min,max")

print ("\nFile URI:  " // s3)                           # file URI
imstat (s3, field="mean,min,max")

print ("\nHTTP Redirection")				# HTTP Redirection
imstat ("http://iraf.noao.edu/votest/redir_img.php", field="mean,min,max")

print ("\nHTTP CGI query")				# HTTP CGI query
imstat http://iraf.noao.edu/scripts/voget?t=fits&f=sif.fits field="mean,min,max"
imstat ("http://iraf.noao.edu/scripts/voget?t=fits&f=sif.fits", 
		field="mean,min,max")

s1 = "http://iraf.noao.edu/scripts/voget?t=fits&f=sif.fits"
imstat (s1, field="mean,min,max")

