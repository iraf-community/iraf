#{ IMAGES package -- General image processing.

# Check that login.cl version matches IRAF version.  This has nothing to
# do with IMAGES, this is just a convenient place to test for an old
# login.cl, since IMAGES is virtually guaranteed to be loaded with IRAF.

if (cl.logver != cl.version && cl.logregen) {
    print ("WARNING: login.cl is outdated - rebuild with `mkiraf'")
    beep; sleep(1); beep; sleep(1); beep
}

set	imcoords 	= "images$imcoords/"
set	imfilter	= "images$imfilter/"
set	imfit		= "images$imfit/"
set	imgeom		= "images$imgeom/"
set	immatch		= "images$immatch/"
set	imutil		= "images$imutil/"
set	tv		= "images$tv/"

package	images

task	imcoords.pkg	= "imcoords$imcoords.cl"
task	imfilter.pkg	= "imfilter$imfilter.cl"
task	imfit.pkg	= "imfit$imfit.cl"
task	imgeom.pkg	= "imgeom$imgeom.cl"
task	immatch.pkg	= "immatch$immatch.cl"
task	imutil.pkg	= "imutil$imutil.cl"
task	tv.pkg		= "tv$tv.cl"

# Load images subpackages (tv is not autoloaded).
imcoords
imfilter
imfit
imgeom
immatch
imutil

clbye()
