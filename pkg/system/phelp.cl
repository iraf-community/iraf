# PHELP -- Paged help.

procedure phelp (template)

string	template		{prompt="module name template"}

bool	file_template=no	{prompt="print help file"}
bool	all=yes			{prompt="find all modules matching template"}
string	parameter="all"		{prompt="parameter for which help is desired"}
string	section="all"		{prompt="section for which help is desired"}
string	option="help"		{prompt="type of help desired"}
int	lmargin=1		{min=1, prompt="left margin"}
int	rmargin=72		{min=2, prompt="right margin"}
string	helpdb="helpdb"		{prompt="help database to be used"}

begin
	file	helptext
	string	s_template

	# Get a temp file to hold help text.
	helptext = mktemp ("tmp$htx")
	s_template = template

	# Run HELP, redirecting the output to the temp file.
	help (s_template, > helptext, page=no,
	    all = all,
	    file_template = file_template,
	    parameter = parameter,
	    section = section,
	    option = option,
	    lmargin = lmargin,
	    rmargin = rmargin,
	    device = "terminal",
	    helpdb = helpdb)

	# Page saved text output.
	page (helptext, prompt = s_template)

	# Delete temp file.
	delete (helptext, verify-)
end
