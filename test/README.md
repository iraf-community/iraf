# Unit tests for IRAF

This subdirectory contains a simple test script, `run_tests`, and a
collection of non-interactive tests for various aspectes of IRAF. 

The following tests are available:

 * [Check the IRAF tree for generated files](files.md)
 * [Preliminary Test Procedure for IRAF](testproc.md): basic functionality,
   mainly from the test procedure document written by Jeanette Barnes and 
   her "Beginner's Guide to Using IRAF",
 * [Programming interface](programming.md) for basic CL and SPP functionality,
 * [OS specific subroutines](os.md),
 * Test from the examples in several packages:
    - [`lists`](lists.md),
    - [`images.imcoords`](images.imcoords.md)
    - [`images.imfilter`](images.imfilter.md)
    - [`images.imfit`](images.imfit.md)
    - [`images.imgeom`](images.imgeom.md)
    - [`images.immatch`](images.immatch.md)
    - [`utilities.nttools`](utilities.nttools.md)
    - [`noao.digiphot.photcal`](noao.digiphot.photcal.md)
    - [`noao.astutil`](noao.astutil.md)
	- [`sys.vops`](sys.vops.md)
 * [Regression test](numerical-recipes.md) for the replacement of Numerical
   Recipes code
 * [A self-test](test-syntax.md) for the basic unit test functionality,
