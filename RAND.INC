FUNCTION FINDPATH$(PathNM$)
	PathNM$=RTRIM$(PathNM$,"\")
	IF INSTR(PathNM$,"\") THEN
		LenNM=LEN(PathNM$):L=0
		DO
			NMChar$=MID$(PathNM$,LenNM-L,1)
			INCR L
		LOOP UNTIL NMChar$="\"
		FINDPATH$=LEFT$(PathNM$,LenNM-L)
	ELSE
		FINDPATH$=LEFT$(PathNM$,2)
	END IF
END FUNCTION
:
FUNCTION PROG$(DV$)
	IF INSTR(DV$, ANY " \/-") THEN
		PROG$=LEFT$(DV$,INSTR(DV$, ANY " \/-")-1)
	ELSE
		PROG$=DV$
	END IF
END FUNCTION
:
FUNCTION OUTNM$(NM$,OutName$,OutPath$)
	SHARED M
	DO
		OutPath$="":OutName$=""
		X=51:CRS=27
		BF=0
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1)
		OutFile$=KEYA$(%SelLine,X,33,126,CRS)
		IF OutFile$=CHR$(%Esc) THEN
			OUTNM$="":EXIT FUNCTION
		END IF
		OutFile$=UCASE$(RTRIM$(OutFile$, ANY "\ "))
		OutFile$=LTRIM$(OutFile$, ANY " \")
		IF OutFile$="" THEN
			OutPath$=RTRIM$(CURDIR$,"\")
		ELSEIF INSTR(OutFile$,":")=0 THEN
			OutPath$=RTRIM$(CURDIR$,"\")
			OutName$=OutFile$
		ELSEIF INSTR(OutFile$,":") THEN
			IF INSTR(OutFile$,"\") THEN
				IF OKDIR(OutFile$)=%TRUE THEN
					OutPath$=OutFile$
				ELSE
					OutPath$=FINDPATH$(OutFile$)
				END IF
			ELSE
				OutPath$=LEFT$(OutFile$,2)
			END IF
			OutName$=LTRIM$(REMOVE$(OutFile$,OutPath$),"\")
		END IF
		IF OutName$="" THEN OutName$=NM$
		OutFile$=OutPath$+"\"+OutName$
		IF M=%F4 THEN
			IF OutFile$=CURDIR$+NM$ THEN BF=6
		END IF
		BF=CHECKNAME(OutName$)
		IF NOT OKDIR(OutPath$) THEN BF=2
		IF BF THEN IF ERRA(BF) THEN OUTNM$="":EXIT FUNCTION
	LOOP WHILE BF
	OUTNM$=OutFile$
END FUNCTION
:
SUB RandCheck(NM$)
	SHARED Ink,Paper,DR$(),KeyFun$()
	CLS
	DIM Numb(255),Charac(255),DIFF(255),SC(255),REP$(1:11)
	Y=1:X=1
	COLOR %Yellow
	FOR N=0 TO 255
		LOCATE Y,X
		PRINT CHR$(N);
		INCR Y
		IF Y=25 THEN
			INCR X,7
			Y=1
		END IF
	NEXT N
	COLOR Ink
	XTRA$="HLA+-%SC"
	COLOR %Cyan
	FOR N=0 TO LEN(XTRA$)-1
		LOCATE Y+N,X
		PRINT MID$(XTRA$,N+1,1);
	NEXT N
	COLOR %Black,Ink
	TITLE$=" Occurrences of each character in file '"+NM$+"'"
	LOCATE 25,1:PRINT TITLE$;SPACE$(80-LEN(TITLE$));
	COLOR Ink,Paper
	OPEN NM$ FOR BINARY AS #1
	Hi=0:Lo=32750:Total&=0:Repeat=%FALSE:Extra=0
	LenMsg&=LOF(1):LenBite=32750
	LongFile=CHKLEN(Repeat,Extra,LenMsg&)
	Y=1:X=2
	FOR N=0 TO 255
		Count=0
		FOR Bite=1 TO Repeat+1
			IF LongFile THEN
				IF Bite>Repeat THEN
					GET$ #1,Extra,MSG$
				ELSE
					GET$ #1,LenBite,MSG$
				END IF
			ELSE
				GET$ #1,LenMsg&,MSG$
			END IF
			T=TALLY(MSG$,CHR$(N))
			INCR Count,T
			MSG$=""
		NEXT Bite
		LOCATE Y,X
		PRINT Count;
		Numb(N)=Count
		Charac(N)=N
		INCR Total&,Count
		INCR Y
		IF Y=25 THEN
			INCR X,7
			Y=1
		END IF
		SEEK #1,0
	NEXT N
	CLOSE

	ARRAY SORT Numb(), TAGARRAY Charac()

	PosChars=POSNUM(Numb())
	Ave!=Total&/PosChars
	AD!=AVDEV!(Numb(),PosChars,Ave!)
	SD!=STDEV!(Numb(),PosChars,Ave!)
	CS!=CHISQ!(Numb(),PosChars,Ave!,Total&)
	Hi=Numb(255)
	Lo=Numb(256-PosChars)

	LOCATE Y,X
	PRINT Hi
	LOCATE Y+1,X
	PRINT Lo
	LOCATE Y+2,X
	PRINT CINT(Ave!)
	LOCATE Y+3,X
	PRINT Hi-CINT(Ave!)
	LOCATE Y+4,X
	PRINT CINT(Ave!)-Lo
	LOCATE Y+5,X
	PRINT CINT(AD!)
	LOCATE Y+6,X
	PRINT CINT(SD!)
	LOCATE Y+7,X
	PRINT CINT(CS!);
	PRINT CONT$;
	DO
		Report:
		PRINT BORDER$;
		COLOR %Yellow,%Cyan
		PRINT CENTRE$(2," REPORT OF FILE '"+UCASE$(NM$)+"' ")
		COLOR Ink,Paper
		REP$(1)="Total number of bytes in file        ="+STR$(Total&)
		REP$(2)="Number of characters used in file    ="+STR$(PosChars)
		REP$(3)="Average no per character             ="+STR$(CINT(Ave!))
		REP$(4)="Highest no for any one character     ="+STR$(Hi)
		REP$(5)="Lowest no for any one character      ="+STR$(Lo)
		REP$(6)="Highest difference               (+) ="+STR$(Hi-CINT(Ave!))
		REP$(7)="Lowest difference                (-) ="+STR$(CINT(Ave!)-Lo)
		REP$(8)="Average deviation                (%) ="+STR$(AD!)
		REP$(9)="Standard deviation                   ="+STR$(SD!)
		REP$(10)="Chi square                           ="+STR$(CS!)
		Y=4
		FOR N=1 TO 10
			LOCATE Y,18
			PRINT REP$(N);
			INCR Y
		NEXT N
		COLOR %Yellow,%Cyan
		PRINT CENTRE$(15," SELECT: ")
		COLOR Ink,Paper
		LOCATE 17,3:COLOR %LCyan:PRINT "A:";
		COLOR Ink:PRINT " Occurrences of each character"
		LOCATE 17,40:COLOR %LCyan:PRINT "B:";
		COLOR Ink:PRINT " Order of frequencies of characters"
		LOCATE 18,3:COLOR %LCyan:PRINT "C:";
		COLOR Ink:PRINT " Order of differences"
		LOCATE 18,40:COLOR %LCyan:PRINT "D:";
		COLOR Ink:PRINT " Print results"
		LOCATE 19,3:COLOR %LCyan:PRINT "E:";
		COLOR Ink:PRINT " Create plot file"
		PRINT ESCMAIN$;
		DO
			BF=%FALSE:DiffCount=0
			CNT$=UCASE$(INPUT$(1))
			SELECT CASE CNT$
				CASE "A":CALL Each(Charac(),Numb())
				CASE "B":CALL OrdCh(Charac(),Numb())
				CASE "C":CALL OrdDiff(PosChars,DiffCount,Ave!,Charac(),Numb(),DIFF(),SC())
				CASE "D":CALL PrnRes(NM$,Charac(),Numb(),REP$(),Total&)
				CASE "E"
					CALL GetDiffs(Numb(),DiffCount,PosChars,Ave!,DIFF(),SC())
					CALL Chart(DiffCount,PosChars,DIFF(),SC())
					BF=%TRUE
				CASE CHR$(%Esc):
					CALL ClearWindow(%TopLeft1,%BottomRight1)
					PRINT FUNKEY$;
					EXIT SUB
				CASE ELSE
					SOUND 88,8
					BF=%TRUE
			END SELECT
		LOOP WHILE BF
	LOOP
END SUB
:
SUB Each(Charac(),Numb())
	SHARED Ink,Paper
	CLS
	COLOR %Black,Ink
	TITLE$=" Occurrences of each character"
	LOCATE 25,1:PRINT TITLE$;SPACE$(80-LEN(TITLE$));
	COLOR %Yellow,Paper
	ARRAY SORT Charac(), TAGARRAY Numb()
	Y=1:X=1
	FOR N=0 TO 255
		LOCATE Y,X
		PRINT CHR$(Charac(N));
		COLOR Ink
		PRINT Numb(N);
		COLOR %Yellow
		INCR Y
		IF Y=25 THEN
			INCR X,7
			Y=1
		END IF
	NEXT N
	PRINT CONT$;
END SUB
:
SUB OrdCh(Charac(),Numb())
	SHARED Ink,Paper
	CLS
	COLOR %Black,Ink
	TITLE$=" Order of frequencies of characters"
	LOCATE 25,1:PRINT TITLE$;SPACE$(80-LEN(TITLE$));
	COLOR %Yellow,Paper
	ARRAY SORT Numb(), TAGARRAY Charac()
	Y=1:X=1
	FOR N=255 TO 0 STEP -1
		LOCATE Y,X
		PRINT CHR$(Charac(N));
		COLOR Ink
		PRINT Numb(N);
		COLOR %Yellow
		INCR Y
		IF Y=25 THEN
			INCR X,7
			Y=1
		END IF
	NEXT N
	PRINT CONT$;
END SUB
:
SUB OrdDiff(PosChars,DiffCount,Ave!,Charac(),Numb(),DIFF(),SC())
	SHARED Ink,Paper
	CLS
	COLOR %Black,Ink
	TITLE$=" Order of differences"
	LOCATE 25,1:PRINT TITLE$;SPACE$(80-LEN(TITLE$));
	COLOR %Yellow,Paper
	ARRAY SORT Numb(), TAGARRAY Charac()
	Y=1:X=1
	CALL GetDiffs(Numb(),DiffCount,PosChars,Ave!,DIFF(),SC())
	FOR N=0 TO DiffCount
		LOCATE Y,X
		PRINT DIFF(N);
		COLOR Ink
		LOCATE Y,X+4
		PRINT SC(N);
		COLOR %Yellow
		INCR Y
		IF Y=25 THEN
			INCR X,8
			Y=1
		END IF
	NEXT N
	PRINT CONT$;
END SUB
:
SUB PrnRes(NM$,Charac(),Numb(),REP$(),Total&)
	LOCAL SumTotal&&
	ARRAY SORT Charac(), TAGARRAY Numb()
	FOR N=0 TO 255
		INCR SumTotal&&,N*Numb(N)
	NEXT N
	LPRINT "REPORT OF FILE `"+UCASE$(NM$)+"'"
	LPRINT
	FOR N=1 TO 10
		LPRINT REP$(N)
	NEXT N
	LPRINT "Mean value of characters             ="+STR$(SumTotal&&/Total&)
	LPRINT CHR$(12)
END SUB
:
SUB Chart(DiffCount,PosChars,DIFF(),SC())
	SHARED Ink,Paper,DD1$
	COLOR Ink,Paper
	NM$="PLOT"
	CALL ChPath(DD1$)
	LOCATE %SelLine,40:COLOR %Yellow:PRINT "Filename:";
	COLOR Ink:PRINT " [";SPACE$(27);"]"
	COLOR %Yellow:LOCATE %ActLine,75:PRINT "Quit";:COLOR Ink
	PlotFile$=OUTNM$(NM$,OutName$,OutPath$)
	IF PlotFile$="" THEN SkipPlot
	OPEN PlotFile$ FOR OUTPUT AS #1
	FOR N=0 TO DiffCount
		PRINT #1,DIFF(N);TAB(8);ROUND(SC(N)/PosChars,5)
	NEXT N
	CALL Signal
	CLOSE
	SkipPlot:
	LOCATE %SelLine,40:PRINT SPACE$(39);
	COLOR %Yellow:LOCATE %ActLine,75:PRINT "Menu";:COLOR Ink
END SUB
:
FUNCTION POSNUM(Numb())
	LOCAL I
	FOR N=0 TO 255
		IF Numb(N)=0 THEN INCR I
	NEXT N
	POSNUM=256-I
END FUNCTION
:
FUNCTION AVDEV!(Numb(),PosChars,Ave!)
	SHARED AvDiff!
	LOCAL Diff!
	I=256-PosChars
	FOR N=I TO 255
		INCR Diff!,ABS(Ave!-Numb(N))
	NEXT N
	AvDiff!=Diff!/PosChars
	AVDEV!=(AvDiff!/Ave!)*100
END FUNCTION
:
FUNCTION STDEV!(Numb(),PosChars,Ave!)
	LOCAL Diff!
	I=256-PosChars
	FOR N=I TO 255
		INCR Diff!,((Numb(N)-Ave!)^2)
	NEXT N
	STDEV!=SQR(Diff!/(PosChars-1))
END FUNCTION
:
FUNCTION CHISQ!(Numb(),PosChars,Ave!,Total&)
	LOCAL Chi!
	I=256-PosChars
	FOR N=I TO 255
		INCR Chi!,((Numb(N)-Ave!)^2)
	NEXT N
	CHISQ!=(PosChars/Total&)*Chi!
END FUNCTION
:
SUB GetDiffs(Numb(),DiffCount,PosChars,Ave!,DIFF(),SC())
	PREV=-1:SameCount=1
	Start=256-PosChars
	FOR N=Start TO 256
		IF Numb(N)=PREV THEN
			INCR SameCount
		ELSE
			DIFF(DiffCount)=Numb(N)-Ave!
			INCR DiffCount:SameCount=1
		END IF
		SC(DiffCount-1)=SameCount
		PREV=Numb(N)
	NEXT N
	DECR DiffCount,2
END SUB
