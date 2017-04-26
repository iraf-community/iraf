#
# Test for filename errors

# Set the test description string.
votest.descr = "Test for invalid image URL"
votest.has_err = yes

set cache = "/tmp/cache/"
fcache init

iferr {
    imstat ("http://iraf.noao.edu/votest/dbix.fits", >& "dev$null")
} then {
    print ("error in test")
}
fcache list
