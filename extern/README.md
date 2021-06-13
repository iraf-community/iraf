# Dynamic External Package Loading

Dynamic package loading is a new feature in v2.15 that allows for
package directories created in the iraf$extern directory to be
automatically defined when the CL is started.  The means that external
package installation no longer *requires* that the hlib$extern.pkg
file be edited to define the package, although that remains an option
for packages which somehow cannot conform to this new scheme.

The option to load external packages from the IRAF repository is
removed, since the repository is not available anymore.
