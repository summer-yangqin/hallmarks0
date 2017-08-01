all:
	(cd ReferenceData; make)
	(cd GeneLists; make)
	(cd Signatures; make)
	(cd Scores; make)

