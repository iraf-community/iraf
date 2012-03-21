# Copyright(c) 1986 Assocation of Universities for Research in Astronomy Inc.

include "geomap.h"


# GEO_SETI -- Set integer parameters.

procedure geo_seti (fit, param, ival)

pointer	fit		#I pointer to the fit structure
int	param		#I paramter ID
int	ival		#I value

begin
	switch (param) {
	case GMPROJECTION:
	    GM_PROJECTION(fit) = ival
	case GMFIT:
	    GM_FIT(fit) = ival
	case GMFUNCTION:
	    GM_FUNCTION(fit) = ival
	case GMXXORDER:
	    GM_XXORDER(fit) = ival
	case GMXYORDER:
	    GM_XYORDER(fit) = ival
	case GMYXORDER:
	    GM_YXORDER(fit) = ival
	case GMYYORDER:
	    GM_YYORDER(fit) = ival
	case GMXXTERMS:
	    GM_XXTERMS(fit) = ival
	case GMYXTERMS:
	    GM_YXTERMS(fit) = ival
	case GMMAXITER:
	    GM_MAXITER(fit) = ival
	}
end


# GEO_SETD -- Set double parameters.

procedure geo_setd (fit, param, dval)

pointer	fit		#I pointer to the fit structure
int	param		#I paramter ID
double	dval		#I value

begin
	switch (param) {
	case GMXO:
	    GM_XO(fit) = dval
	case GMYO:
	    GM_YO(fit) = dval
	case GMXOREF:
	    GM_XOREF(fit) = dval
	case GMYOREF:
	    GM_YOREF(fit) = dval
	case GMREJECT:
	    GM_REJECT(fit) = dval
	}
end
