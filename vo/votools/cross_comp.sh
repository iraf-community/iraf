#!/bin/sh

# Usage:  svc.sh <upload.tbl> <maxdist> <catalog>
#
#  <upload.tbl>		input table (req'd colnames: 'ra', 'dec')
#  <maxdist>		max separation for matching (arcsec)
#  <catalog>		catalog name to match (see below)
#
#
#   Matching catalogs names		Allowed output formats
#
#   TWOMASS_PSC				votable
#   SDSS_DR7				tab
#   USNO_B1				bar
#   IRAS_PSC				csv

base="http://vao-web.ipac.caltech.edu/cgi-bin/VAOPortal/"

# Upload the user table.
rpath=`curl -s -S -F "file=@$1" ${base}nph-fileupload`

# Do the comparison.
data="maxdist=$2&tableA=$rpath&tableB=$3&custom_cntr1=cntr&custom_ra1=ra&custom_dec1=dec" 
oxml=`curl -c c.txt -s -S -d ${data} ${base}nph-catalogCompare`

# Get the filenames.
m=`echo $oxml | sed -e 's/<[a-z]*>//g' -e 's/<\/[a-z]*>//g' | awk '{print $2}'`
u=`echo $oxml | sed -e 's/<[a-z]*>//g' -e 's/<\/[a-z]*>//g' | awk '{print $3}'`


# Download and convert the matched results table.
mxml=`curl -b c.txt -s -S -d "type=votable&file=$m" ${base}nph-tableConvert`
ftp -V -o ${1}_match.xml $mxml
stilts tcopy ${1}_match.xml ${1}_match.fits ofmt=fits-basic	#2>1 /dev/null

# Download and convert the un-matched results table.
uxml=`curl -b c.txt -s -S -d "type=votable&file=$u" ${base}nph-tableConvert`
ftp -V -o ${1}_unmatch.xml $uxml
stilts tcopy ${1}_unmatch.xml ${1}_unmatch.fits ofmt=fits-basic	#2>1 /dev/null

# Clean up.
/bin/rm -f c.txt ${1}_*match.xml
