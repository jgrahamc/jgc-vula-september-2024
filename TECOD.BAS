$ERROR 	PARAM+,		BOUNDS-, 	NUMERIC-,		OVERFLOW-, 		STACK-
$LIB 		LPT-, 		COM-, 		GRAPH-, 		FULLFLOAT-,  IPRINT-
$OPTION	CNTLBREAK-,					AUTODIM-
CDProg$="TECOD"
DEFINT A-Z
%TC=0:%TE=-1:%TK=0:%TK5=0

$INCLUDE "BIOS.INC"
$INCLUDE "CHKERR.INC"
$INCLUDE "DOS.INC"
$INCLUDE "KONST.INC"
$INCLUDE "CODSUBS.INC"

$STACK 2048

DIM Salphabet(256),Ralphabet(256)
DIM FileEntry$(140),FileLen&(140),KeyFun$(1:20),DR$(1:11)

ON ERROR GOTO ABORT
OPEN "A:\"+CDProg$+".CNF" FOR INPUT AS #1
FOR L=1 TO 11
	LINE INPUT #1,DR$(L)
	IF L=11 THEN DR$(L)=CODEKEY$(DR$(L))
NEXT L
CLOSE
CALL SetParams

PW=%TRUE:Jump=0:StartDir$=CURDIR$

%BadName=1	:%NoCopy=2	:%Corrupt=3		:%TooLong=4		:%NoData=5
%DecDone=6	:%Full=7		:%NoDir=8			:%Failed=9		:%NoFile=10
%Exists=11	:%NoWipe=12	:%BadFile=13	:%NoExt=14		:%BadSav=15

FOR K=1 TO 20
	READ KeyFun$(K)
NEXT K
DATA "Load ","Save ","Del  ","Enc  ","Dec  ","Type ","Read ",Renam,Comms,Quit
DATA Merge,WipeD,WipeF,Store,Extct,"Comp ",Decmp,Renam,Setup,"DOS "

CALL PASSWORD
PRINT BORDER$;

MENU:
	DO
		BF=0:M=0:LenBite=10000
		KeyOn=%TRUE:Wild=%TRUE:NoDraw=%FALSE
		Title$="":WhichFiles$="*.*"
		ON ERROR GOTO ERMESG
		IF Jump THEN
			Drive$=RTRIM$(CURDIR$,"\")
			GOTO SkipMenu
		ELSE
			Drive$=RAM$
		END IF
		HL=0:LH=0:XF=%FALSE:XFH=%FALSE:XHL=0
		PRINT TOPLINE$;
		CALL ClearWindow(%TopLeft1,%BottomRight1)
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		COLOR BWYellow,%Cyan:PRINT CENTRE$(2," C O D E R ")
		COLOR %Yellow,Paper :PRINT CENTRE$(4,"Level "+Level$)
		COLOR BWYellow,%Cyan:PRINT CENTRE$(6," MAIN MENU ")
		COLOR ,Paper
		Y=8:X=27
		RESTORE MENU
		FOR ROW=1 TO 13
			LOCATE Y,X
			COLOR %LCyan
			READ MenuItem$:PRINT MenuItem$
			COLOR Ink
			LOCATE Y,X+4
			READ MenuItem$:PRINT MenuItem$
			INCR Y
		NEXT ROW
		DATA F1,": Load files from disk"
		DATA F2,": Save files onto disk"
		DATA F3,": Delete files on disk"
		DATA " "," "
		DATA F4,": Encipher files"
		DATA F5,": Decipher files"
		DATA " "," "
		DATA F6,": Type a new document"
		DATA F7,": Edit/read documents"
		DATA F8,": Rename files"
		DATA F9,": Terminal"
		DATA " "," "
		DATA F10,": Quit"
		COLOR %Yellow
		LOCATE 23,3 :PRINT "ALT Menu: Alt F1 - F10";TAB(63);"Level: Alt 1 - 4";
		PRINT CENTRE$(25,"Version  7.09");
		COLOR Ink
		DO
		  DO
				CH$=INKEY$
				IF CH$=CHR$(%Esc) OR CH$=CHR$(%Ent) THEN QUITTA
			LOOP WHILE LEN(CH$)<2
			M=ASC(RIGHT$(CH$,1))
			IF M>119 AND M<124 THEN
				Level=M-120
				Level$=LTRIM$(STR$(Level+1))
				PRINT SHOWLEVEL$(4,43,Level$)
			END IF
			SkipMenu:
			IF Jump THEN M=Jump
			SELECT CASE M
				CASE %F1:GOSUB LOADA
				CASE %F2:GOSUB SAVA
				CASE %F3:GOSUB DELLA
				CASE %F4:GOSUB ENCA
				CASE %F5:GOSUB DECA
				CASE %F6:GOSUB NEWDOC
				CASE %F7:GOSUB OLDOC
				CASE %F8:GOSUB RENNA
				CASE %F9:GOSUB TERMINAL
				CASE %F10:GOSUB QUITTA
				CASE %AltF1:GOSUB MERJA
				CASE %AltF2:GOSUB WDISK
				CASE %AltF3:GOSUB WIPA
				CASE %AltF4:GOSUB SAFESAV
				CASE %AltF5:GOSUB SAFELOD
				CASE %AltF6:GOSUB COMPA
				CASE %AltF7:GOSUB DECOMPA
				CASE %AltF8:GOSUB RENNA
				CASE %AltF9:GOSUB CONFIG
				CASE %AltF10:GOSUB DOS
			END SELECT
		LOOP UNTIL M=>%F1 AND M<=%F10 OR M=>%AltF1 AND M<=%AltF10
	LOOP
:
LOADA:
	Heading$="LOAD FILES INTO MEMORY"
	IF Jump=0 THEN Drive$=DDPath$
	Wild=%FALSE
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			IF Drive$=RAM$ THEN
				IF ERRA(%NoCopy) THEN RETURN
			ELSE
				EXIT LOOP
			END IF
		LOOP
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Copying '";NM$;"'...";
		IF LEN(RAM$)=2 THEN OutFile$=RAM$+"\" ELSE OutFile$=RAM$
		SHELL "COPY "+NM$+" "+OutFile$+" > NUL"
		PRINT CLEARLINE$(%SelLine,77,0);
	LOOP
RETURN
:
SAVA:
	Heading$="SAVE FILES IN MEMORY ONTO "
	Drive$=RAM$:OutPath$=DDPath$:Wild=%FALSE
	IF Jump AND RTRIM$(CURDIR$,"\")=Drive$ THEN
		CALL ReScreen("","",WhichFiles$)
	ELSE
		Jump=0
		CALL DoScreen(Heading$,Title$,WhichFiles$)
	END IF
	PRINT FITHEAD$(Heading$,DDPath$);
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				CALL SELECTA(NM$)
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" THEN
					IF NOT OKDIR(RTRIM$(NM$,"\")) THEN
						BF=%NoDir:EXIT LOOP
					END IF
					CD=%TRUE
					OutPath$=NM$
					PRINT FITHEAD$(Heading$,OutPath$);
				END IF
			LOOP WHILE CD
			IF BF<>%NoDir THEN
				IF OutPath$=RTRIM$(CURDIR$,"\") THEN BF=%NoCopy
				BF=CHECKFILE(NM$)
				IF EXIST(OutPath$,"DATAPTR",0) THEN BF=%BadSav
			END IF
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		DO
			IF INSTR(NM$,"*")=0 AND EXIST(OutPath$,NM$,0) THEN
				Msg$="Overwrite "+OutPath$+"\"+NM$+" ? (Y/N) Yes"
				LOCATE %SelLine,3:PRINT Msg$;SPACE$(77-LEN(Msg$))
				LOCATE %SelLine,LEN(Msg$),1
				SOUND 800,5
				DO
					YN$=UCASE$(INPUT$(1))
				LOOP UNTIL INSTR(YN$, ANY "YN"+CHR$(%Ent)+CHR$(%Esc))
				IF YN$=CHR$(%Esc) THEN LOCATE ,,0:RETURN
				IF YN$="N" THEN
					PRINT CLEARLINE$(%SelLine,77,1);
					EXIT LOOP
				END IF
			ELSE
				IF FileLen&(XHL)>FREESPACE&(OutPath$) THEN
					IF ERRA(%Full) THEN RETURN
					EXIT LOOP
				END IF
			END IF
			PRINT CLEARLINE$(%SelLine,77,0);
			PRINT "Copying '";NM$;"' to '";OutPath$;"\";NM$;"'...";
			IF LEN(OutPath$)=2 THEN OutPath$=OutPath$+"\"
			SHELL "COPY "+NM$+" "+OutPath$+" > NUL"
			PRINT CLEARLINE$(%SelLine,77,1);
			IF INSTR(NM$,"*")=0 THEN
				IF NOT EXIST(OutPath$,NM$,0) THEN
					IF ERRA(%Failed) THEN RETURN
					EXIT LOOP
				END IF
			END IF
		LOOP WHILE BF
	LOOP
RETURN
:
DELLA:
	Heading$="DELETE FILES"
	IF Jump=0 THEN Drive$=DDPath$
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Are you sure? (Y/N) Yes"
		LOCATE %SelLine,23
		SOUND 800,5
		DO
			DO
				YN$=UCASE$(INPUT$(1))
			LOOP UNTIL INSTR(YN$, ANY "YN"+CHR$(%Ent)+CHR$(%Esc))
			IF YN$=CHR$(%Esc) THEN RETURN
			PRINT CLEARLINE$(%SelLine,77,1);
			IF YN$="N" THEN EXIT LOOP
			PRINT "Deleting '";NM$;"'...";
			CALL WipeFile(NM$)
			PRINT REFILES$(WhichFiles$);
		LOOP UNTIL INSTR(YN$, ANY "Y"+CHR$(%Ent))
	LOOP
RETURN
:
ENCA:
	Heading$="ENCIPHER FILES (L."+Level$+")"
	Wild=%FALSE
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		DO
			KeyOn=%TRUE
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			KeyOn=%FALSE
			PRINT CLEARLINE$(%ActLine,40,0)
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			IF INSTR(NM$,"*") THEN
				LOCATE %SelLine,39:PRINT "Name for Output file: [        ]"
				OutFile$=GETSHRINK$
				IF OutFile$=CHR$(%Esc) THEN RETURN
				IF OutFile$="" THEN LOCATE %SelLine,62:PRINT " ";
			ELSE
				IF INSTR(NM$,".") THEN
					OutFile$=LEFT$(NM$,(INSTR(NM$,".")-1))
				ELSE
					OutFile$=NM$
				END IF
			END IF
		LOOP WHILE OutFile$=""
		DO
			BF=0
			IF Level=0 THEN
				LL=0:PAGE=0
				SendData$=GETENCKEY$(PAGE,LL)
				IF SendData$=CHR$(%Esc) THEN
					RETURN
				ELSEIF SendData$="" THEN
					EXIT LOOP
				END IF
			END IF
			IF NOT DOSHRINK(NM$,OutFile$,ZipKey$) THEN RETURN
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			LenMsg&=LOF(1):CLOSE
			IF Level THEN
				PRINT CLEARLINE$(%SelLine,77,0);
				PRINT "Ready to encipher '";NM$;"'";
				IF WARNDISK(DDPath$,"DATAPTR","DATA") THEN
					KILL OutFile$+".ZIP"
					RETURN
				END IF
				SELECT CASE LEVEL
					CASE 1:LenData=36
					CASE 2:LenData=LenMsg&\4:MaxLength&=163835&
					CASE 3:LenData=LenMsg&  :MaxLength&=32750
				END SELECT
				IF Level>1 THEN
					IF LenMsg&>MaxLength& THEN
						BF=%TooLong
						IF BF THEN
							KILL OutFile$+".ZIP"
							IF ERRA(BF) THEN RETURN ELSE EXIT LOOP
						END IF
					END IF
				END IF
				SM&=0:SendData$=GETDATA$(SM&,LenData,"SNUM")
				IF SendData$="" THEN
					KILL OutFile$+".ZIP"
					IF ERRA(%NoData) THEN RETURN ELSE EXIT LOOP
				END IF
			END IF
			PRINT CLEARLINE$(%ActLine,77,0);
			PRINT CLEARLINE$(%SelLine,77,0);
			PRINT "Enciphering '";NM$;"' - Level "+Level$+"...";
			COLOR Ink+%Flash:PRINT CHR$(219):COLOR Ink
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			OPEN OutFile$+Ext$ FOR BINARY AS #2
			LOCATE %ActLine,3:PRINT "File length:";LenMsg&;"characters"
			SM$=STARTM$(Level,SM&)
			IF Level=0 THEN
				SM$=SM$+CHR$(PAGE)+CHR$(LL)
			END IF
			PUT$ #2,SM$
			LongFile=CHKLEN(Repeat,Extra,LenBite,LenMsg&)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,MSG$
					ELSE
						GET$ #1,LenBite,MSG$
					END IF
				ELSE
					GET$ #1,LenMsg&,MSG$
				END IF
				SELECT CASE Level
					CASE 0,1
						Encrypt$=ENCLEV2$(LN,MSG$,SendData$)
					CASE 2
						WipeCount=0
						Encrypt$=ENCLEV3$(LN,MSG$,SendData$,WipeCount)
						LenData=WipeCount
					CASE 3
						Encrypt$=ENCLEV4$(MSG$,SendData$)
				END SELECT
				PUT$ #2,Encrypt$
				MSG$="":Encrypt$=""
			NEXT LN
			CLOSE:SendData$=""
			IF Level THEN
				IF NOT EXIST(DDPath$,"SNUM",0) THEN
					IF WARNDISK(DDPath$,"SNUM","DATA") THEN
						KILL OutFile$+".ZIP"
						RETURN
					END IF
				END IF
				CALL DiskFlush(SM&,LenData,"SNUM")
			END IF
			KILL OutFile$+".ZIP"
			PRINT REFILES$(WhichFiles$);
		LOOP WHILE BF
		CALL TidyUp
	LOOP
RETURN
:
FUNCTION ENCLEV2$(LN,MSG$,SendData$)
	LOCAL KeyPos
	LENMSG=LEN(MSG$)
	PREV=INITRAND(2,SendData$)
	DEF SEG=STRSEG(MSG$)
	SP&=STRPTR(MSG$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=INT(RND*256)
		CD=((PEEK(SP&)+RL) AND &HFF) XOR PREV
		POKE SP&,CD
		PREV=RL
		IF RL MOD 6=0 THEN
			R=ASC(MID$(SendData$,(KeyPos MOD 36)+1,1))
			RANDOMIZE LN+ENC+R
			INCR KeyPos
		END IF
		INCR SP&
	NEXT ENC
	$EVENT ON
	ENCLEV2$=MSG$
	DEF SEG
END FUNCTION
:
FUNCTION ENCLEV3$(LN,MSG$,SendData$,WC)
	LENMSG=LEN(MSG$)
	PREV=INITRAND(3,SendData$)
	DEF SEG=STRSEG(MSG$)
	SP&=STRPTR(MSG$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=INT(RND*256)
		CD=((PEEK(SP&)+RL) AND &HFF) XOR PREV
		POKE SP&,CD
		IF RL MOD 6=0 THEN
			RL=ASC(MID$(SendData$,WC+1,1))
			RANDOMIZE RL+ENC+LN
			INCR WC
		END IF
		PREV=RL
		INCR SP&
	NEXT ENC
	$EVENT ON
	ENCLEV3$=MSG$
	DEF SEG
END FUNCTION
:
FUNCTION ENCLEV4$(MSG$,SendData$)
	LENMSG=LEN(MSG$)
	PREV=ASC(RIGHT$(SendData$,1))
	DEF SEG=STRSEG(MSG$)
	SP&=STRPTR(MSG$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=ASC(MID$(SendData$,ENC,1))
		CD=((PEEK(SP&)+RL) AND &HFF) XOR PREV
		POKE SP&,CD
		PREV=RL
		INCR SP&
	NEXT ENC
	$EVENT ON
	PUT$ #2,MSG$
	DEF SEG
END FUNCTION
:
FUNCTION GETENCKEY$(PAGE,LL)
	LOCATE %SelLine,3,0
	PRINT "Page No: [   ]   Line No: [  ]   ";
	PRINT "Key: [";SPACE$(36);"]"
	FOR N=1 TO 2
		DO
			BF=0
			IF N=1 THEN
				X=13:CRS=3
			ELSE
				X=30:CRS=2
			END IF
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1)
			PgLn$=KEYA$(%SelLine,X,48,57,CRS)
			IF PgLn$="" OR PgLn$=CHR$(%Esc) THEN
				GETENCKEY$=PgLn$:EXIT FUNCTION
			END IF
			IF N=1 THEN
				PAGE=VAL(PgLn$)
				IF PAGE<1 OR PAGE>255 THEN
					BF=%BadPage
					IF ERRA(BF) THEN GETENCKEY$="":EXIT FUNCTION
				END IF
			ELSE
				LNE=VAL(PgLn$)
				IF LNE<1 OR LNE>99 THEN
					BF=%BadLine
					IF ERRA(BF) THEN GETENCKEY$="":EXIT FUNCTION
				END IF
			END IF
		LOOP WHILE BF
	NEXT N
	DO
		X=42:CRS=36
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1)
		KeyWord$=KEYA$(%SelLine,X,32,126,CRS)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETENCKEY$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<>CRS THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	RANDOMIZE PAGE
	LL=(LNE XOR INT(RND*256))
	RANDOMIZE PAGE*LNE
	GETENCKEY$=KeyWord$
END FUNCTION
:
FUNCTION GETDATA$(SM&,LenKey,DataFile$)
	SHARED DDPath$,Ink
	NULL$=STRING$(5,0)
	IF DataFile$="SNUM" THEN
		OPEN DDPath$+"\DATAPTR" FOR INPUT AS #1
		INPUT #1,SM&:CLOSE
		IF SM&+LenKey>360000& THEN
			GETDATA$="":EXIT FUNCTION
		END IF
	END IF
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT "Reading data...";
	DO
		BadKey=%FALSE
		SEEK #1,SM&:GET$ #1,LenKey,KeyData$
		IF DataFile$="SNUM" THEN
			IF INSTR(KeyData$,NULL$) THEN
				BadKey=%TRUE
				INCR SM&,PosBad+5
			END IF
		ELSE
			IF LEFT$(KeyData$,5)=NULL$ THEN
				KeyData$=""
				EXIT LOOP
			END IF
		END IF
	LOOP WHILE BadKey
	CLOSE
	GETDATA$=KeyData$
END FUNCTION
:
FUNCTION STARTM$(Level,SM&)
	RANDOMIZE TIMER
	StartRand=INT(RND*256) AND &HFC
	IF Level=0 THEN STARTM$=CHR$(StartRand):EXIT FUNCTION
	RANDOMIZE StartRand
	SecondRand=INT(RND*256) AND &HF8
	GetPos$=MKL$(SM&)
	Char1$=CHR$(StartRand XOR Level)
	Char2$=CHR$(SecondRand XOR ASC(MID$(GetPos$,3,1)))
	Char3$=MID$(GetPos$,2,1)
	Char4$=LEFT$(GetPos$,1)
	STARTM$=CHAR1$+CHAR2$+CHAR3$+CHAR4$
END FUNCTION
:
FUNCTION INITRAND(Lev,KeyData$)
	LOCAL StartRand
	LenKey=LEN(KeyData$)
	SELECT CASE Lev
		CASE 2
			FOR N=1 TO LenKey
				INCR StartRand,ASC(MID$(KeyData$,N,1))
			NEXT N
		CASE 3
		StartRand=LenKey XOR ASCII(KeyData$)
	END SELECT
	RANDOMIZE StartRand
	INITRAND=INT(RND*256)
END FUNCTION
:
SUB DiskFlush(SM&,BytesToWipe,DataFile$)
	SHARED DDPath$
	DataLeft$=LTRIM$(STR$(CEIL(((SM&+BytesToWipe)/360000)*100)))
	LOCATE %ActLine,3,0:PRINT "Data disk ";DataLeft$;" per cent used                  "
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT "Wiping used data...";
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	WipeData$=STRING$(BytesToWipe,0)
	SEEK #1,SM&
	PUT$ #1,WipeData$
	CLOSE
	IF DataFile$="SNUM" THEN
		INCR SM&,BytesToWipe
		OPEN DDPath$+"\DATAPTR" FOR OUTPUT AS #1
		PRINT #1,SM&
		CLOSE
	END IF
END SUB
:
DECA:
	Heading$="DECIPHER FILES"
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		KeyOn=%TRUE
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			OPEN NM$ FOR BINARY AS #1
			LenCgm&=LOF(1):GET$ #1,4,Ver$:CLOSE
			Ver=ASC(LEFT$(Ver$,1)) AND 3
			IF Ver THEN Header=4 ELSE Header=3
			DECR LenCgm&,Header
			KeyOn=%FALSE
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			PRINT CLEARLINE$(%ActLine,40,0);
			IF Ver=0 THEN
				RecData$=GETDECKEY$(Ver$)
				IF RecData$=CHR$(%Esc) THEN
					RETURN
				ELSEIF RecData$="Corrupt" OR RecData$="" THEN
					EXIT LOOP
				END IF
			ELSE
				IF WARNDISK(DDPath$,"RNUM","DATA") THEN RETURN
				SM&=READSM&(Ver$)
				IF SM&=%TRUE THEN
					IF ERRA(%BadFile) THEN RETURN ELSE EXIT LOOP
				END IF
				SELECT CASE Ver
					CASE 1:LenData=36
					CASE 2:LenData=LenCgm&\4
					CASE 3:LenData=LenCgm&
				END SELECT
				RecData$=GETDATA$(SM&,LenData,"RNUM")
				IF RecData$="" THEN
					IF ERRA(%DecDone) THEN RETURN ELSE EXIT LOOP
				END IF
			END IF
			PRINT CLEARLINE$(%ActLine,77,0);
			PRINT CLEARLINE$(%SelLine,77,0);
			PRINT "Deciphering '";NM$;"' - Level "+CHR$(49+Ver)+"...";
			COLOR Ink+%Flash:PRINT CHR$(219):COLOR Ink
			LOCATE %ActLine,3:PRINT "File length:";LenCgm&;"characters";
			IF INSTR(NM$,".")THEN
				DNM$=LEFT$(NM$,(INSTR(NM$,".")-1))+".BAD"
			ELSE
				DNM$=NM$+".BAD"
			END IF
			OPEN NM$ FOR BINARY AS #1
			OPEN DNM$ FOR BINARY AS #2
			SEEK #1,Header
			LongFile=CHKLEN(Repeat,Extra,LenBite,LenCgm&)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,CGM$
					ELSE
						GET$ #1,LenBite,CGM$
					END IF
				ELSE
					GET$ #1,LenCgm&,CGM$
				END IF
				SELECT CASE Ver
					CASE=0,1
						Decrypt$=DECLEV2$(LN,CGM$,RecData$)
					CASE=2
						WipeCount=0
						Decrypt$=DECLEV3$(LN,CGM$,RecData$,WipeCount)
						LenData=WipeCount
					CASE=3
						Decrypt$=DECLEV4$(CGM$,RecData$)
				END SELECT
				PUT$ #2,Decrypt$
				CGM$="":Decrypt$=""
			NEXT LN
			CLOSE:RecData$=""
			IF NOT CHKCOMP(DNM$) THEN
				KILL DNM$
				IF ERRA(%Corrupt) THEN RETURN ELSE EXIT LOOP
			END IF
			IF Ver THEN
				IF NOT EXIST(DDPath$,"RNUM",0) THEN
					IF WARNDISK(DDPath$,"RNUM","DATA") THEN
						KILL DNM$
						RETURN
					END IF
				END IF
				CALL DiskFlush(SM&,LenData,"RNUM")
			END IF
			IF NOT EXPAND(DNM$,ZipKey$) THEN RETURN ELSE EXIT LOOP
		LOOP
		PRINT REFILES$(WhichFiles$);
		CALL TidyUp
	LOOP
RETURN
:
FUNCTION DECLEV2$(LN,CG$,RecData$)
	LOCAL KeyPos
	LENCG=LEN(CG$)
	PREV=INITRAND(2,RecData$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=INT(RND*256)
		PK=PEEK(SP&)
		PL=((PK XOR PREV)-RL) AND &HFF
		POKE SP&,PL
		PREV=RL
		IF RL MOD 6=0 THEN
			R=ASC(MID$(RecData$,(KeyPos MOD 36)+1,1))
			RANDOMIZE LN+DEC+R
			INCR KeyPos
		END IF
		INCR SP&
	NEXT DEC
	$EVENT ON
	DECLEV2$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION DECLEV3$(LN,CG$,RecData$,WC)
	LENCG=LEN(CG$)
	PREV=INITRAND(3,RecData$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=INT(RND*256)
		PK=PEEK(SP&)
		PL=((PK XOR PREV)-RL) AND &HFF
		POKE SP&,PL
		IF RL MOD 6=0 THEN
			RL=ASC(MID$(RecData$,WC+1,1))
			RANDOMIZE RL+DEC+LN
			INCR WC
		END IF
		PREV=RL
		INCR SP&
	NEXT DEC
	$EVENT ON
	DECLEV3$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION DECLEV4$(CG$,RecData$)
	LENCG=LEN(CG$)
	PREV=ASC(RIGHT$(RecData$,1))
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=ASC(MID$(RecData$,DEC,1))
		PK=PEEK(SP&)
		PL=((PK XOR PREV)-RL) AND &HFF
		POKE SP&,PL
		PREV=RL
		INCR SP&
	NEXT DEC
	$EVENT ON
	DECLEV4$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION GETDECKEY$(Ver$)
	SHARED Ink
	PAGE=ASC(MID$(Ver$,2,1))
	RANDOMIZE PAGE
	LNE=ASC(MID$(Ver$,3,1)) XOR INT(RND*256)
	RANDOMIZE PAGE*LNE
	IF PAGE<1 OR PAGE>255 OR LNE<1 OR LNE>99 THEN
		IF ERRA(%Corrupt) THEN	GETDECKEY$="" ELSE GETDECKEY$="Corrupt"
		EXIT FUNCTION
	END IF
	PRINT CLEARLINE$(%SelLine,77,0);
	PRINT "Page No:";
	COLOR %Yellow:PRINT PAGE;
	COLOR Ink:PRINT TAB(17);" Line No:";
	COLOR %Yellow:PRINT LNE;TAB(35);
	COLOR Ink
	PRINT "Key: [";SPACE$(36);"]";
	DO
		X=41
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(35)
		KeyWord$=KEYA$(%SelLine,X,32,126,36)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETDECKEY$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<36 THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	GETDECKEY$=KeyWord$
END FUNCTION
:
FUNCTION READSM&(Ver$)
	Rndz=ASC(LEFT$(Ver$,1)) AND &HFC
	RANDOMIZE Rndz
	RandChar=INT(RND*256) AND &HF8
	Segmt=ASC(MID$(Ver$,2,1)) AND &H07
	Check=ASC(MID$(Ver$,2,1)) XOR Segmt
	CHAR1$=RIGHT$(Ver$,1)
	CHAR2$=MID$(Ver$,3,1)
	CHAR3$=CHR$(Segmt)
	CHAR4$=CHR$(&H00)
	IF RandChar <> Check THEN
		READSM&=%TRUE:EXIT FUNCTION
	END IF
	READSM&=CVL(CHAR1$+CHAR2$+CHAR3$+CHAR4$)
END FUNCTION
:
NEWDOC:
	Heading$="TYPE A NEW DOCUMENT"
	Drive$=RAM$
	NoDraw=%TRUE:KeyOn=%FALSE:Jump=0
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	LOCATE %SelLine,3,0:PRINT "Document name: [            ]"
	DO
		BF=0:X=19:Y=%SelLine
		LOCATE Y,X:PRINT CHR$(219);SPACE$(11)
		NM$=KEYA$(Y,X,33,126,12)
		IF NM$=CHR$(%Esc) OR NM$="" THEN RETURN
		BF=CHECKNAME(NM$)
		IF ERRA(BF) THEN RETURN
	LOOP WHILE BF
	IF NOT EDIT(RAM$,NM$) THEN RETURN
RETURN
:
OLDOC:
	Heading$="EDIT/READ FILES"
	DO
		CALL DoScreen(Heading$,Title$,WhichFiles$)
		NM$=GETNM$(Drive$)
		IF NM$="" THEN EXIT LOOP
		Path$=RTRIM$(CURDIR$,"\")
		IF NOT EDIT(Path$,NM$) THEN EXIT LOOP
	LOOP
RETURN
:
RENNA:
	Heading$="RENAME FILES"
	Title$=SPACE$(40)+"New name: [            ]"
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			BF=0:X=54
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(11)
			NNM$=UCASE$(KEYA$(%SelLine,X,33,126,12))
			IF NNM$=CHR$(%Esc) THEN RETURN
			IF NNM$="" THEN EXIT LOOP
			BF=CHECKNAME(NNM$)
			IF INSTR(NNM$,"*") THEN BF=%BadName
			IF NNM$=NM$ THEN BF=%Exists
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		IF NNM$<>"" THEN
			NAME NM$ AS NNM$
			PRINT REFILES$(WhichFiles$);
		END IF
	LOOP
RETURN
:
TERMINAL:
	Jump=0
	LOCATE 23,2
	SHELL "REN "+RAM$+"\* *.Q$} > NUL"
	IF FD1 THEN
		IF INSTR(TXProg$, ANY " /\-") THEN
			ComProg$=LEFT$(TXProg$,INSTR(TXProg$, ANY " /\-")-1)
		ELSE
			ComProg$=TXProg$
		END IF
		IF WARNDISK(TXPath$,ComProg$,"PROGRAM") THEN RETURN
	END IF
	CALL ChPath(TXPath$)
	PRINT TOPLINE$;
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	LOCATE 6,3,0:PRINT "Loading communications program...";
	SHELL TXProg$
	LOCATE 23,2,0
	SHELL	"REN "+RAM$+"\*.Q$} * > NUL"
	PRINT BORDER$;
RETURN
:
QUITTA:
	Jump=0:KeyOn=%FALSE
	CALL ChPath(StartDir$)
	CLS:END
RETURN
:
MERJA:
	Heading$="MERGE FILES"
	Title$=SPACE$(38)+"Join to: ["+SPACE$(27)+"]"
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			LOCATE %SelLine,51:PRINT CHR$(219);SPACE$(26)
			MNM$=UCASE$(KEYA$(%SelLine,51,33,126,27))
			IF MNM$=CHR$(%Esc) THEN RETURN
			IF MNM$="" THEN EXIT LOOP
			BF=JOIN(MNM$)
			BF=CHECKNAME(NNM$)
			IF INSTR(MNM$,"*") THEN BF=%BadName
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		IF MNM$<>"" THEN
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Merging '";NM$;"' to '";MNM$;"'...";
			OPEN NM$ FOR BINARY AS #1
			OPEN MNM$ FOR BINARY AS #2
			Repeat=0:Extra=0:Length&=LOF(1)
			LongFile=CHKLEN(Repeat,Extra,LenBite,Length&)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,PutStr$
					ELSE
						GET$ #1,LenBite,PutStr$
					END IF
				ELSE
					GET$ #1,Length&,PutStr$
				END IF
				LenJoin&=LOF(2)
				SEEK #2,LenJoin&
				PUT$ #2,PutStr$
				PutStr$=""
			NEXT LN
			CLOSE
			PRINT REFILES$(WhichFiles$);
		END IF
	LOOP
RETURN
:
WDISK:
	Heading$="WIPE DISK FREE SPACE"
	Drive$=DDPath$
	NoDraw=%TRUE
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			FD=ASCII(CURDIR$)
			IF FD<>65 AND FD<>66 THEN
				IF ERRA(%NoWipe) THEN RETURN ELSE EXIT LOOP
			END IF
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Wiping free space...";
			CALL WipeDisk
			EXIT LOOP
		LOOP
	LOOP
RETURN
:
WIPA:
	Heading$="WIPE FILES"
	IF Jump=0 THEN Drive$=DDPath$
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Are you sure? (Y/N) Yes"
		LOCATE %SelLine,23
		SOUND 800,5
		DO
			DO
				YN$=UCASE$(INPUT$(1))
			LOOP UNTIL INSTR(YN$, ANY "YN"+CHR$(%Ent)+CHR$(%Esc))
			IF YN$=CHR$(%Esc) THEN RETURN
			PRINT CLEARLINE$(%SelLine,77,1);
			IF YN$="N" THEN EXIT LOOP
			PRINT "Wiping '";NM$;"'...";
			CALL WipeFile(NM$)
			PRINT REFILES$(WhichFiles$);
			EXIT LOOP
		LOOP
	LOOP
RETURN
:
SAFESAV:
	Heading$="STORE FILES"
	Title$=SPACE$(39)+"Add to: ["+SPACE$(27)+"]"
	Wild=%FALSE
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			DO
				BF=0
				LOCATE %SelLine,51:PRINT CHR$(219);SPACE$(26);
				MNM$=UCASE$(KEYA$(%SelLine,51,33,126,27))
				IF MNM$=CHR$(%Esc) THEN RETURN
				IF MNM$="" THEN EXIT LOOP
				IF RIGHT$(MNM$,4)<>".SAV" THEN
					BF=%NoExt
					IF ERRA(BF) THEN RETURN
				END IF
			LOOP WHILE BF
			BF=JOIN(MNM$)
			IF BF=%NoFile THEN
				Msg$="Create "+MNM$+" ? (Y/N) Yes"
				LOCATE %SelLine,3:PRINT Msg$;SPACE$(77-LEN(Msg$))
				LOCATE %SelLine,LEN(Msg$),1
				SOUND 800,5
				DO
					YN$=UCASE$(INPUT$(1))
				LOOP UNTIL INSTR(YN$, ANY "YN"+CHR$(%Ent)+CHR$(%Esc))
				IF YN$=CHR$(%Esc) THEN LOCATE ,,0:RETURN
				IF YN$="N" THEN MNM$="":EXIT LOOP
				BF=0
			END IF
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		IF MNM$<>"" THEN
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Adding '";NM$;"' to '";MNM$;"'...";
			SHELL UTPath$+"\PKZIP -a -s"+ZipKey$+" "+MNM$+" "+NM$+" > NUL"
			PRINT REFILES$(WhichFiles$);
		END IF
	LOOP
RETURN
:
SAFELOD:
	Heading$="EXTRACT STORED FILES (Ctrl F for contents)"
	IF Jump=0 THEN Drive$=DDPath$
	WhichFiles$="*.SAV":Wild=%FALSE
	Title$=SPACE$(40)+"Get file: ["+SPACE$(12)+"]"
	DO
		NM$=""
		CALL DoScreen(Heading$,Title$,WhichFiles$)
		DO
			BF=0
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN
				IF Jump<>%AltF5 THEN
					WhichFiles$="*.*"
					PRINT REFILES$(WhichFiles$);
				END IF
				RETURN
			END IF
			IF NM$=CHR$(6) THEN
				IF FF=0 THEN
					BF=%NoFile
					IF ERRA(BF) THEN RETURN ELSE EXIT LOOP
				END IF
				CALL SHOWZIP(FileEntry$(XHL))
				EXIT LOOP
			END IF
			DO
				BF=0
				LOCATE %SelLine,54:PRINT CHR$(219);SPACE$(11);
				XNM$=UCASE$(KEYA$(%SelLine,54,33,126,12))
				IF XNM$=CHR$(%Esc) THEN RETURN
				BF=CHECKNAME(XNM$)
				IF ERRA(BF) THEN RETURN
			LOOP WHILE BF
			IF XNM$<>"" THEN
				PRINT CLEARLINE$(%SelLine,77,1);
				PRINT "Extracting '"+XNM$+"'...";
				IF LEN(RAM$)=2 THEN OutFile$=RAM$+"\" ELSE OutFile$=RAM$
				SHELL UTPath$+"\PKUNZIP "+NM$+" "+XNM$+" -o -s"+ZipKey$+" "+OutFile$+" > NUL"
				PRINT CLEARLINE$(%SelLine,77,0);
			END IF
		LOOP
	LOOP WHILE NM$=CHR$(6)
RETURN
:
SUB SHOWZIP(ZipFile$)
	SHARED UTPath$,WhichFiles$
	IF WARNDISK(UTPath$,"PKUNZIP.EXE","PROGRAM") THEN EXIT SUB
	CLS
	SHELL UTPath$+"\PKUNZIP -v "+ZipFile$
	LOCATE 25,1:PRINT "Press a key to continue . . .";
	CNT$=INPUT$(1)
	PRINT BORDER$;
	PRINT REFILES$(WhichFiles$);
END SUB
:
COMPA:
	Heading$="COMPRESS FILES"
	Title$=SPACE$(32)+"Name for Compressed file: [        ]"
	Wild=%FALSE
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		DO
			OutFile$=""
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			OutFile$=GETSHRINK$
		LOOP WHILE OutFile$=""
		IF OutFile$=CHR$(%Esc) THEN RETURN
		IF NOT DOSHRINK(NM$,OutFile$,"") THEN RETURN
		PRINT REFILES$(WhichFiles$);
	LOOP
RETURN
:
DECOMPA:
	Heading$="DECOMPRESS FILES"
	WhichFiles$="*.ZIP":Wild=%FALSE
	CALL DoScreen(Heading$,Title$,WhichFiles$)
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN
			IF Jump<>%AltF2 THEN
				WhichFiles$="*.*"
				PRINT REFILES$(WhichFiles$);
			END IF
			RETURN
		END IF
		IF NOT EXPAND(NM$,"") THEN RETURN
		PRINT CLEARLINE$(%SelLine,77,0);
		PRINT SPACEFREE$(0);
	LOOP
RETURN
:
CONFIG:
	PRINT TOPLINE$;
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	KeyOn=%FALSE
	RESTORE CONFIG
	FOR N=1 TO 11
		READ ConfigList$
		COLOR %LCyan
		LOCATE N+1,3:PRINT CHR$(N+64);
		COLOR Ink
		PRINT TAB(7);ConfigList$;TAB(36);"-   ";DR$(N);
	NEXT N
	DATA "Colour, B&W or LCD",Default level (1 - 4)
	DATA Path for data/message disk,RAM disk,Path for editor program
	DATA Name of editor program,Path for comms program,Name of comms program
	DATA Path for utilities,Encrypted file extension,Secondary key
	PRINT HORLINE$;
	LOCATE %SelLine,3:PRINT "Press letter of option to change";TAB(52);
	COLOR %LCyan
	PRINT "S";
	COLOR Ink
	PRINT " - Save changes"
	LOCATE %HeadLine,3:COLOR BWYellow,%Cyan:PRINT "CHANGE CONFIGURATION":COLOR Ink,Paper
	PRINT ESCMAIN$;
	ReConfig:
	DO
		DO
			LOCATE %SelLine,36
			COLOR %Flash+Ink:PRINT CHR$(219):COLOR Ink
			Choice$=UCASE$(INPUT$(1))
			IF ASC(Choice$)=>65 AND ASC(Choice$)<=76 THEN
				LOCATE%SelLine,36:PRINT " "
				TempName$=""
				Opt=ASC(Choice$)-64
				Y=Opt+1:X=40
				LOCATE Y,X,1,0,7
				DO
					DO
						Char$=INKEY$
					LOOP WHILE Char$=""
					IF LEN(Char$)=2 THEN
						IF ASC(RIGHT$(Char$,1))=%LfAr THEN
							Char$=CHR$(%Bsp)
						ELSE
							Char$=" "
						END IF
					END IF
					SELECT CASE Char$
						CASE CHR$(%Ent)
							IF TempName$<>"" THEN DR$(Opt)=RTRIM$(TempName$,"\")
							CALL SetParams
							Level=VAL(Level$)-1
							EXIT LOOP
						CASE CHR$(%Esc)
							LOCATE Y,40:PRINT SPACE$(40)
							LOCATE Y,40:PRINT DR$(Opt)
							EXIT LOOP
						CASE CHR$(%Bsp)
							DECR X
							IF X<40 THEN X=40:EXIT SELECT
							LOCATE Y,X:PRINT CHR$(32):LOCATE Y,X
							TempName$=LEFT$(TempName$,LEN(TempName$)-1)
						CASE ELSE
							IF X<78 THEN
								PRINT UCASE$(Char$);SPACE$(39-LEN(TempName$));
								INCR X:LOCATE Y,X
								TempName$=TempName$+UCASE$(Char$)
							ELSE
								SOUND 88,8
							END IF
					END SELECT
				LOOP
				LOCATE ,,0
			END IF
			IF Choice$="S" THEN
				IF WARNDISK("A:",CDProg$+".CNF","CONFIG") THEN RETURN
				OPEN "A:\"+CDProg$+".CNF" FOR OUTPUT AS #1
				FOR N=1 TO 11
					IF N<11 THEN
						PRINT #1,DR$(N)
					ELSE
						PRINT #1,CODEKEY$(DR$(11))
					END IF
				NEXT N
				CLOSE
			END IF
		LOOP UNTIL Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent)
		IF Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent) THEN
			JUMP=0:EXIT LOOP
		END IF
	LOOP
RETURN
:
FUNCTION JOIN(MNM$)
	LOCAL BF
	IF LEFT$(MNM$,1)="\" THEN
		MNM$=LEFT$(CURDIR$,2)+MNM$
	END IF
	IF INSTR(MNM$,":") THEN
		Path$=FINDPATH$(MNM$)
		IF OKDIR(Path$) THEN
			IF TALLY(MNM$,"\") THEN
				Nom$=LTRIM$(REMOVE$(MNM$,Path$),"\")
				IF NOT EXIST(Path$,Nom$,0) THEN BF=%NoFile
			ELSE
				Nom$=RIGHT$(MNM$,LEN(MNM$)-2)
				IF NOT EXIST(LEFT$(MNM$,2),Nom$,0) THEN BF=%NoFile
			END IF
		ELSE
			BF=%NoDir
		END IF
	ELSE
		BF=CHECKFILE(MNM$)
	END IF
	JOIN=BF
END FUNCTION
:
FUNCTION CODEKEY$(ShrinKey$)
	LOCAL TempKey$
	LK=LEN(ShrinKey$)
	IF LK=1 THEN CODEKEY$="":EXIT FUNCTION
	RANDOMIZE LK
	FOR N=1 TO LK
		RK=INT(RND*256)
		CK=ASC(MID$(ShrinKey$,N,1))
		XK=RK XOR CK
		TempKey$=TempKey$+CHR$(XK)
	NEXT N
	CODEKEY$=TempKey$
END FUNCTION
:
DOS:
	Jump=0
	COLOR Ink,%Black
	CLS
	PRINT "Type EXIT to return to program..."
	SHELL
	PRINT BORDER$;
RETURN
:
FUNCTION ERRA(BF)
	ERRA=%FALSE
	IF BF<1 THEN EXIT FUNCTION
	SHARED KeyOn,LenMsg&,MaxLength&
	CLOSE:SOUND 88,8
	SELECT CASE BF
		CASE %BadName:ERRMSG$="Unacceptable characters in name"
		CASE %NoCopy :ERRMSG$="File cannot be copied onto itself"
		CASE %BadPage:ERRMSG$="Unacceptable page number. Acceptable pages: 1 - 255"
		CASE %BadLine:ERRMSG$="Unacceptable line number. Acceptable lines: 1 - 99"
		CASE %BadFile:ERRMSG$="Un-enciphered or corrupted file"
		CASE %Corrupt:ERRMSG$="Bad key or corrupted file"
		CASE %TooLong:ERRMSG$="Message too long by"+STR$(LenMsg&-MaxLength&)+" bytes"
		CASE %NoData :ERRMSG$="Insufficient data left. Start a new DATA disk"
		CASE %DecDone:ERRMSG$="This file has already been deciphered"
		CASE %Full	 :ERRMSG$="Disk full"
		CASE %NoDir	 :ERRMSG$="Invalid path"
		CASE %Failed :ERRMSG$="Copy failed"
		CASE %NoFile :ERRMSG$="File not found"
		CASE %Exists :ERRMSG$="File already exists. Choose another name"
		CASE %NoWipe :ERRMSG$="Only floppy disks may be wiped"
		CASE %NoExt  :ERRMSG$="'.SAV' extension missing"
		CASE %BadSav :ERRMSG$="You are attempting to save to your Data Disk!"
	END SELECT
	IF WARNERR(ERRMSG$) THEN
		ERRA=%TRUE:EXIT FUNCTION
	ELSE
		IF KeyOn THEN PRINT FUNKEY$;
	END IF
END FUNCTION
:
ERMESG:
	CLOSE:SOUND 88,8
	SELECT CASE ERR
		CASE 53:ERRMSG$="File not found"
		CASE 58:ERRMSG$="File already exists. Choose another name"
		CASE 61:ERRMSG$="Out of memory/disk full. Delete files to make space"
		CASE 64:ERRMSG$="Invalid name"
		CASE 70:ERRMSG$="Disk write-protected. Switch tab to continue"
		CASE 71:ERRMSG$="Disk not in drive or door open"
		CASE 72:ERRMSG$="Disk corrupted"
		CASE 75:ERRMSG$="Path/File access error"
		CASE 76:ERRMSG$="Path not found"
		CASE ELSE:ERRMSG$="Error number"+STR$(ERR)+" at address"+STR$(ERADR)
	END SELECT
	IF WARNERR(ERRMSG$) THEN RESUME MENU
RESUME MENU
:
SUB SetParams SHARED
	StartInk=ASCII(DR$(1))
	SELECT CASE StartInk
		CASE 66		'B&W
			StartInk=&H0000
			Ink=%HWhite:Paper=%Black:BWYellow=Paper
		CASE 67		'COLOR
			StartInk=&H1000
			Ink=%HWhite:Paper=%Blue:BWYellow=%Yellow
		CASE 76		'LCD
			StartInk=&H0000
			Ink=%White:Paper=%Black:BWYellow=Paper
	END SELECT
	Level$=DR$(2)		:DDPath$=DR$(3)	:RAM$=DR$(4)
	EDPath$=DR$(5)	:EDProg$=DR$(6)	:TXPath$=DR$(7)	:TXProg$=DR$(8)
	UTPath$=DR$(9)	:Ext$=DR$(10)		:ZipKey$=DR$(11)
	Level=VAL(Level$)-1
	IF ASCII(UCASE$(EDPath$))=65 THEN FD1=%TRUE ELSE FD1=%FALSE
END SUB
:
SUB PASSWORD SHARED
	Pass$="":PL=10
	DIM PW$(PL)
	PW$(9)=CHR$(66):PW$(4)=CHR$(66):PW$(1)=CHR$(84):PW$(5)=CHR$(79):PW$(2)=CHR$(73)
  PW$(3)=CHR$(77):PW$(6)=CHR$(66):PW$(8)=CHR$(77):PW$(10)=CHR$(79):PW$(7)=CHR$(73)
	FOR N=1 TO PL:Pass$=Pass$+PW$(N):NEXT N
	ERASE PW$
	COLOR Ink,Paper:CLS
	LOCATE 12,20,0:PRINT "ENTER PASSWORD: [               ]"
	DO
		Y=12:X=37
		LOCATE Y,X:PRINT CHR$(219);SPACE$(14)
		PSW$=KEYA$(Y,X,32,126,15)
		IF PSW$="" THEN CLS:END
		PSW$=UCASE$(PSW$)
		IF PSW$=Pass$ THEN EXIT LOOP
		SOUND 88,8
	LOOP
	PW=%FALSE
END SUB
:
ABORT:
	CLS
	PRINT "DATA NOT FOUND - ABORTING!"
	END
