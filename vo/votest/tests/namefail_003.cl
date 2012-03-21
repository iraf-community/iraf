#
# Test for filename errors

# Set the test description string.
votest.descr = "Test for invalid file URL"
votest.has_err = yes

set cache = "/tmp/cache/"
fcache init

iferr {
    type ("http://iraf.noao.edu/nosuchfile.html", >& "dev$null")
} then {
    print ("error in test")
}
fcache list
