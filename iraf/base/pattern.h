# PATTERN.H -- Pattern Matching Metacharacters (STRMATCH, PATMATCH).

define	CH_BOL		'^'		# beginning of line symbol
define	CH_NOT		'^'		# not, in character classes
define	CH_EOL		'$'		# end of line symbol
define	CH_ANY		'?'		# match any single character
define	CH_CLOSURE	'*'		# zero or more occurrences
define	CH_CCL		'['		# begin character class
define	CH_CCLEND	']'		# end character class
define	CH_RANGE	'-'		# as in [a-z]
define	CH_ESCAPE	'\\'		# escape character
define	CH_WHITESPACE	'#'		# match optional whitespace
define	CH_IGNORECASE	'{'		# begin ignoring case
define	CH_MATCHCASE	'}'		# begin checking case
