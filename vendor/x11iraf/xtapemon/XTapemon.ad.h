"XTapemon.port:				5138",
"XTapemon.alternate:			0",
"XTapemon.debug:				0",
"XTapemon.widgets: \
	toplevel			Paned		panel \
\
	panel				Form		statusForm \
	panel.statusForm		Label		statusLabel \
	panel.statusForm		AsciiText	statusText \
	panel.statusForm		Command		quitButton \
\
	panel				AsciiText	messages \
\
	panel				Form		stringForm \
	panel.stringForm		Label		devtypeLabel \
	panel.stringForm		AsciiText	devtypeText \
	panel.stringForm		Label		tapetypeLabel \
	panel.stringForm		AsciiText	tapetypeText \
	panel.stringForm		Label		tapesizeLabel \
	panel.stringForm		AsciiText	tapesizeText \
	panel.stringForm		Label		tapeusedLabel \
	panel.stringForm		AsciiText	tapeusedText \
	panel.stringForm		Label		acmodeLabel \
	panel.stringForm		AsciiText	acmodeText \
	panel.stringForm		Label		densityLabel \
	panel.stringForm		AsciiText	densityText \
	panel.stringForm		Label		blksizeLabel \
	panel.stringForm		AsciiText	blksizeText \
	panel.stringForm		Label		nfilesLabel \
	panel.stringForm		AsciiText	nfilesText \
	panel.stringForm		Label		fileLabel \
	panel.stringForm		AsciiText	fileText \
	panel.stringForm		Label		recordLabel \
	panel.stringForm		AsciiText	recordText \
	panel.stringForm		Label		recsizeLabel \
	panel.stringForm		AsciiText	recsizeText",
"*panel.orientation:			vertical",
"*showGrip:				false",
"*statusForm.background:			lightgray",
"*statusForm*statusLabel.label:          Status:",
"*statusForm*statusLabel.background:	lightgray",
"*statusForm*statusText.fromHoriz:       statusLabel",
"*statusForm*statusText*width:           370",
"*statusForm*statusText*editType:        read",
"*statusForm*statusText.borderWidth:     0",
"*statusForm*statusText*translations:	#override\\n",
"*statusForm*statusText.background:	lightgray",
"*statusForm*quitButton.label:		Quit",
"*statusForm*quitButton.fromHoriz:	statusText",
"*statusForm*quitButton.background:	lightgray",
"*messages.scrollVertical:		always",
"*messages.scrollHorizontal:		whenNeeded",
"*messages*editType:			append",
"*messages*Scrollbar.thickness:		10",
"*messages*displayCaret:			False",
"*messages*string:			idle\\n",
"*input:					True",
"*Grip*height:				5",
"*Grip*width:				5",
"*Label.borderWidth:			0",
"*Label.internalWidth:			0",
"*Text*borderWidth:			1",
"*Text*font:				fixed",
"*Text*Scrollbar.thickness:		5",
"*Text*editType:				read",
"*Text.scrollHorizontal:			whenNeeded",
"*Text*displayCaret:			False",
"*stringForm.min:			100",
"*stringForm.max:			100",
"*stringForm.background:			lightgray",
"*stringForm*Label.background:		lightgray",
"*stringForm*devtypeLabel.label:		Device Type:",
"*stringForm*devtypeText*width:		415",
"*stringForm*devtypeText.fromHoriz:	devtypeLabel",
"*stringForm*tapetypeLabel.label:	\\ \\ Tape Type:",
"*stringForm*tapetypeLabel.fromVert:	devtypeLabel",
"*stringForm*tapetypeText.fromVert:	devtypeLabel",
"*stringForm*tapetypeText.fromHoriz:	tapetypeLabel",
"*stringForm*tapetypeText.width:		105",
"*stringForm*tapesizeLabel.label:	Capacity:",
"*stringForm*tapesizeLabel.fromVert:	devtypeLabel",
"*stringForm*tapesizeLabel.fromHoriz:	tapetypeText",
"*stringForm*tapesizeText.fromVert:	devtypeLabel",
"*stringForm*tapesizeText.fromHoriz:	tapesizeLabel",
"*stringForm*tapesizeText.width:		75",
"*stringForm*tapeusedLabel.label:	Used:",
"*stringForm*tapeusedLabel.fromVert:	devtypeLabel",
"*stringForm*tapeusedLabel.fromHoriz:	tapesizeText",
"*stringForm*tapeusedText.fromVert:	devtypeLabel",
"*stringForm*tapeusedText.fromHoriz:	tapeusedLabel",
"*stringForm*tapeusedText.width:		117",
"*stringForm*acmodeLabel.label:		Access Mode:",
"*stringForm*acmodeLabel.fromVert:	tapeusedLabel",
"*stringForm*acmodeText.fromVert:	tapeusedLabel",
"*stringForm*acmodeText.fromHoriz:	acmodeLabel",
"*stringForm*acmodeText.width:		70",
"*stringForm*densityLabel.label:		Density:",
"*stringForm*densityLabel.fromVert:	tapeusedLabel",
"*stringForm*densityLabel.fromHoriz:	acmodeText",
"*stringForm*densityText.fromVert:	tapeusedLabel",
"*stringForm*densityText.fromHoriz:	densityLabel",
"*stringForm*densityText.width:		72",
"*stringForm*blksizeLabel.label:		Device Block Size:",
"*stringForm*blksizeLabel.fromVert:	tapeusedLabel",
"*stringForm*blksizeLabel.fromHoriz:	densityText",
"*stringForm*blksizeText.fromVert:	tapeusedLabel",
"*stringForm*blksizeText.fromHoriz:	blksizeLabel",
"*stringForm*blksizeText.width:		71",
"*stringForm*nfilesLabel.label:		Total Files:",
"*stringForm*nfilesLabel.fromVert:	acmodeLabel",
"*stringForm*nfilesText.fromVert:	acmodeLabel",
"*stringForm*nfilesText.fromHoriz:	nfilesLabel",
"*stringForm*nfilesText.width:		55",
"*stringForm*fileLabel.label:		File:",
"*stringForm*fileLabel.fromVert:		acmodeLabel",
"*stringForm*fileLabel.fromHoriz:	nfilesText",
"*stringForm*fileText.fromVert:		acmodeLabel",
"*stringForm*fileText.fromHoriz:		fileLabel",
"*stringForm*fileText.width:		62",
"*stringForm*recordLabel.label:		Record:",
"*stringForm*recordLabel.fromVert:	acmodeLabel",
"*stringForm*recordLabel.fromHoriz:	fileText",
"*stringForm*recordText.fromVert:	acmodeLabel",
"*stringForm*recordText.fromHoriz:	recordLabel",
"*stringForm*recordText.width:		50",
"*stringForm*recsizeLabel.label:		Record Size:",
"*stringForm*recsizeLabel.fromVert:	acmodeLabel",
"*stringForm*recsizeLabel.fromHoriz:	recordText",
"*stringForm*recsizeText.fromVert:	acmodeLabel",
"*stringForm*recsizeText.fromHoriz:	recsizeLabel",
"*stringForm*recsizeText.width:		50",
"*beNiceToColormap:			False",
