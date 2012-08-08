# TUTOR -- Tutorial Help

procedure tutor (topic)

string	topic		{prompt="Tutorial topic"}
string	package=""	{prompt="Tutorial package"}
string	tutordb="helpdb"	{prompt="Tutorial database"}

begin
	if ($nargs == 0)
	    help (package//".Tutorial", section="topics", helpdb=tutordb)
	else
	    help (package//".Tutorial", section=topic, helpdb=tutordb)
end
