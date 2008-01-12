#{ TABLES.CL -- The STScI tables package.

cl < "tables$config/zzsetenv.def"
package	tables, bin = tablesbin$

task	fitsio.pkg	= "fitsio$fitsio.cl"
task	tbplot.pkg	= "tbplot$tbplot.cl"
task	tobsolete.pkg	= "tobsolete$tobsolete.cl"
task	ttools.pkg	= "ttools$ttools.cl"


#Load some of the tables packages
cl < tables$load.cl
# Print the Welcome banner, but only if the user wants it
if (motd)
type "tables$tables_motd"
;

clbye ()

