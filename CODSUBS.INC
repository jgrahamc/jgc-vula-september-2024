SUB WipeFile(NM$)
	SHARED M
	FOR Wipe=1 TO 3
		SELECT CASE Wipe
			CASE 1:WipeVal=0
			CASE 2:WipeVal=255
			CASE 3:WipeVal=246
		END SELECT
		OPEN NM$ FOR BINARY AS #1
		LenDel&=LOF(1)
		CALL WipeIt(WipeVal,LenDel&)
		CLOSE
		IF M=%F3 THEN EXIT FOR
	NEXT Wipe
	DelNM$=STRING$(8,255)
	NAME NM$ AS DelNM$
	KILL DelNM$
END SUB
:
SUB WipeDisk
	LenDel&=FREESPACE&(CURDIR$)
	DelNM$=STRING$(8,255)
	OPEN DelNM$ FOR BINARY AS #1
	CALL WipeIt(246,LenDel&)
	CLOSE
	KILL DelNM$
END SUB
:
SUB WipeIt(WipeVal,LenDel&)
	LOCAL Repeat,Extra
	LenBite=10000
	LongFile=CHKLEN(Repeat,Extra,LenBite,LenDel&)
	FOR LN=1 TO Repeat+1
		IF LongFile THEN
			IF LN>Repeat THEN
				PUT$ #1,STRING$(Extra,WipeVal)
			ELSE
				PUT$ #1,STRING$(LenBite,WipeVal)
			END IF
		ELSE
			PUT$ #1,STRING$(LenDel&,WipeVal)
		END IF
	NEXT LN
END SUB
:
FUNCTION EDIT(Path$,NM$)
	SHARED FD1,EDPath$,EDProg$
	EDIT=%FALSE
	IF FD1 THEN
		IF WARNDISK(EDPath$,EDProg$,"PROGRAM") THEN EXIT FUNCTION
	END IF
	LOCATE %SelLine,3,0
	SHELL EDPath$+"\"+EDProg$+" "+Path$+"\"+NM$
	PRINT BORDER$;
	EDIT=%TRUE
END FUNCTION
:
FUNCTION GETSHRINK$
	DO
		BF=0:X=62
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(7)
		OutFile$=UCASE$(KEYA$(%SelLine,X,33,126,8))
		IF OutFile$="" OR OutFile$=CHR$(%Esc) THEN
			GETSHRINK$=OutFile$:EXIT FUNCTION
		END IF
		IF INSTR(OutFile$,"*") OR INSTR(OutFile$,".") THEN BF=%BadName
		BF=CHECKNAME(OutFile$)
		IF ERRA(BF) THEN
			GETSHRINK$=CHR$(%Esc):EXIT FUNCTION
		END IF
	LOOP WHILE BF
	GETSHRINK$=OutFile$
END FUNCTION
:
FUNCTION DOSHRINK(InNM$,OutNM$,ZipKey$)
	SHARED FD1,UTPath$
	DOSHRINK=%FALSE
	IF FD1 THEN
		IF WARNDISK(UTPath$,"PKZIP.EXE","PROGRAM") THEN EXIT FUNCTION
	END IF
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT "Compressing '";InNM$;"'...";
	IF ZipKey$="" THEN Switch$=" -a " ELSE Switch$=" -s"+ZipKey$+" "
	SHELL UTPath$+"\PKZIP.EXE"+Switch$+OutNM$+" "+InNM$+" > NUL"
	DOSHRINK=%TRUE
END FUNCTION
:
FUNCTION CHKCOMP(NM$)
	OPEN NM$ FOR BINARY AS #1
	SEEK #1,0:GET$ #1,2,Comp$:CLOSE
	$IF %TK
		IF LEFT$(Comp$,1)<>CHR$(26) THEN
			CHKCOMP=%FALSE
		ELSE
			CHKCOMP=1
			EXIT FUNCTION
		END IF
	$ENDIF
	IF Comp$<>"PK" THEN
		CHKCOMP=%FALSE
	ELSE
		CHKCOMP=%TRUE
	END IF
END FUNCTION
:
FUNCTION EXPAND(XNM$,ZipKey$)
	SHARED M,FD1,UTPath$
	EXPAND=%FALSE
	IF RIGHT$(XNM$,4)=".BAD" THEN
		CNM$=LEFT$(XNM$,(INSTR(XNM$,".")-1))+".ZIP"
		NAME XNM$ AS CNM$
	ELSE
		CNM$=XNM$
	END IF
	IF RIGHT$(CNM$,4)=".ARC" THEN
		ProgName$="PKUNPAK.EXE":Switch$=" -R "
	ELSE
		ProgName$="PKUNZIP.EXE"
		IF ZipKey$="" THEN
			Switch$=" -o "
		ELSE
			Switch$=" -o -s"+ZipKey$+" "
		END IF
	END IF
	IF FD1 THEN
		IF WARNDISK(UTPath$,ProgName$,"PROGRAM") THEN EXIT FUNCTION
	END IF
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT "Decompressing '";CNM$;"'...";
	SHELL UTPath$+"\"+ProgName$+Switch$+CNM$+" > NUL"
	IF M=%F5 THEN KILL CNM$
	EXPAND=%TRUE
END FUNCTION
:
FUNCTION BORDER$
	SHARED Ink,Paper
	CLS
	COLOR Ink,Paper
	BDR$=STRING$(78,205)
	LOCATE 1,1:PRINT CHR$(201);BDR$;CHR$(187);
	FOR V=2 TO 21
		LOCATE V,1:PRINT CHR$(186);TAB(80);CHR$(186);
	NEXT V
	LOCATE 22,1:PRINT CHR$(200);BDR$;CHR$(188);
END FUNCTION
:
FUNCTION KEYA$(Y,X,LOLIM,UPLIM,CRS)
	SHARED Ink,Paper,PW
	LOCAL L,STNG$
	COLOR Ink,Paper
	DO
		K=%FALSE
		DO
			I=ASC(INPUT$(1))
			SELECT CASE I
				CASE %Ent
					K=%TRUE
					IF L=0 AND PW=%FALSE THEN
						KEYA$=""
						EXIT LOOP
					END IF
					IF L<CRS THEN
						LOCATE Y,X:PRINT CHR$(32)
						EXIT LOOP
					ELSE
						LOCATE Y,X-1:PRINT RIGHT$(STNG$,1)
						EXIT LOOP
					END IF
				CASE %Esc
					IF PW=%FALSE THEN
						KEYA$=CHR$(%Esc)
						EXIT FUNCTION
					END IF
				CASE %Bsp
					IF L THEN
						STNG$=LEFT$(STNG$,LEN(STNG$)-1)
						LOCATE Y,X-1:PRINT CHR$(219);
						IF L<CRS-1 THEN PRINT " "
						IF L=CRS-1 THEN LOCATE Y,X:PRINT " "
						DECR L:DECR X:EXIT LOOP
					ELSE
						SOUND 88,5
						EXIT LOOP
					END IF
				CASE <LOLIM,>UPLIM
					SOUND 88,5:EXIT LOOP
			END SELECT
			LOCATE Y,X
			IF L<CRS-1 THEN
				IF PW THEN
					PRINT "*";CHR$(219)
				ELSE
					PRINT CHR$(I);CHR$(219)
				END IF
			ELSEIF L=CRS-1 THEN
				COLOR Paper,Ink
				IF PW THEN
					PRINT "*"
				ELSE
					PRINT CHR$(I)
				END IF
				COLOR Ink,Paper
			ELSE
				SOUND 88,5
				EXIT LOOP
			END IF
			STNG$=STNG$+CHR$(I):INCR L:INCR X
		LOOP
	LOOP UNTIL K
	KEYA$=STNG$
END FUNCTION
:
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
FUNCTION WARNDISK(Path$,Prog$,Disk$)
	SHARED Ink,Paper,FD1,Jump,M
	IF Disk$="DATA" THEN DD=%TRUE
	IF FD1 THEN IF NOT OKDIR(Path$) THEN DD=%TRUE
	DO
		IF DD OR NOT EXIST(Path$,Prog$,0) THEN
			SOUND 800,5:Jump=0:DD=%FALSE
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			WarnMsg$="Place "+Disk$+" disk in drive"
			COLOR %Red+%Flash:PRINT CENTRE$(23,"** "+SPACE$(LEN(WarnMsg$))+" **");
			COLOR %Red:PRINT CENTRE$(23,WarnMsg$);
			COLOR Ink:PRINT CENTRE$(25,"Press any key to continue. ESCape for Main Menu");
			CNT$=INPUT$(1)
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			IF CNT$=CHR$(%Esc) THEN
				WARNDISK=%TRUE:EXIT FUNCTION
			END IF
		ELSE
			EXIT LOOP
		END IF
	LOOP
	WARNDISK=%FALSE
END FUNCTION
:
FUNCTION FUNKEY$
	SHARED Ink,Paper,KeyFun$(),M
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	A=1:X=2
	IF M>103 THEN
		INCR A,10
		COLOR Paper,Ink
		LOCATE 23,3:PRINT "ALT Menu"
		COLOR Ink,Paper
	END IF
	FOR N=A TO A+9
		LOCATE 25,X
		COLOR %LCyan
		PRINT RIGHT$(STR$(N),1);
		COLOR Paper,Ink
		PRINT KeyFun$(N);
		COLOR ,Paper
		INCR X,8
	NEXT N
	COLOR Ink
END FUNCTION
:
SUB GetDirra(WhichFiles$)
	SHARED FF,SubD,DTA$,FileEntry$(),FileLen&()
	FF=0:SubD=%FALSE
	DTA$=SPACE$(43):NM$=""
	ERASE FileEntry$,FileLen&
	CALL SETDTA(DTA$)
	NM$=FINDFIRST$(WhichFiles$)
	IF NM$="" THEN
		EXIT SUB
	ELSE
		FileEntry$(0)=RIGHT$(NM$,LEN(NM$)-4)
		FileLen&(0)=CVL(LEFT$(NM$,4))
		IF SubD THEN FileEntry$(0)="<"+FileEntry$(0)+">"
		FOR FF=1 TO 139
			NM$=FINDNEXT$
			IF NM$="" THEN EXIT FOR
			FileEntry$(FF)=RIGHT$(NM$,LEN(NM$)-4)
			FileLen&(FF)=CVL(LEFT$(NM$,4))
			IF SubD THEN FileEntry$(FF)="<"+FileEntry$(FF)+">"
		NEXT FF
		ARRAY SORT FileEntry$() FOR FF, TAGARRAY FileLen&()
	END IF
END SUB
:
SUB PrintName(FileEntry$())
	SHARED Ink,Paper,FF,HL,XFH,FileLen&()
	LOCATE 16,77,0
	FirstFile=0
	IF FF>70 THEN
		COLOR Paper,%Lred
		IF XFH THEN
			FirstFile=70
			PRINT "PgUp"
		ELSE
			PRINT "PgDn"
		END IF
		COLOR Ink,Paper
	ELSE
		COLOR Ink,Paper
		PRINT STRING$(3,196)+CHR$(182);
	END IF
	LOCATE 2,3:PRINT FileEntry$(FirstFile)
	FOR N=1 TO FF-FirstFile
		LOCATE (N\5)+2,((N MOD 5)*16)+3
		PRINT FileEntry$(FirstFile+N)
		IF N=FirstFile+69 THEN EXIT FOR
	NEXT N
END SUB
:
FUNCTION NEWDIR$(Drive$,Path$)
	SHARED HL,LH,XFH,XHL
	BF=0:HL=0:LH=0:XFH=%FALSE:XHL=0
	OldPath$=Drive$
	Path$=UCASE$(RTRIM$(Path$,"\"))
	IF MID$(Path$,2,1)=":" THEN
		IF NOT OKDIR(Path$) THEN
			IF ERRA(%NoDir) THEN NEWDIR$=CHR$(%Esc) ELSE NEWDIR$=""
			EXIT FUNCTION
		END IF
		IF INSTR(Path$,"\")=0 THEN
			CHDRIVE Path$
			CHDIR "\"
			EXIT IF
		ELSEIF INSTR(Path$,"\") THEN
			CHDRIVE Path$
			CHDIR RIGHT$(Path$,LEN(Path$)-2)
			EXIT IF
		END IF
	ELSEIF INSTR(Path$,"\") THEN
		IF INSTR(Path$,"\.") THEN
			IF INSTR(Path$,"\..") THEN
				CHDIR ".."
			ELSE
				CHDIR "\"
			END IF
		ELSE
			CHDIR RIGHT$(OldPath$,LEN(OldPath$)-2)+Path$
		END IF
	END IF
	NEWDIR$=RTRIM$(CURDIR$,"\")
END FUNCTION
:
SUB SELECTA(Naam$) SHARED
	LOCAL Choice,TempName$
	Jump=0
	IF XFH=%FALSE THEN
		IF FF THEN
			IF FF>70 THEN
				FPOS=69
			ELSE
				FPOS=FF-1
				IF HL>=FPOS THEN HL=FPOS:XHL=HL
			END IF
		END IF
	ELSE
		FPOS=FF-71
		IF HL>=FPOS THEN HL=FPOS:XHL=HL+70
	END IF
	Row=(HL\5)+2:Col=((HL MOD 5)*16)+3
	COLOR Ink,Paper
	IF NoDraw=%FALSE THEN
		IF FF THEN
			IF XFH THEN HiLite=HL+70 ELSE HiLite=HL
			COLOR Paper,Ink
			LOCATE Row,Col:PRINT FileEntry$(HiLite)
			PRINT CLEARLINE$(%SelLine,32,0);
			PRINT FileEntry$(HiLite);TAB(18);FileLen&(HiLite);
		END IF
	ELSE
  	LOCATE %SelLine,3
		IF M=%AltF2 THEN
			PRINT "Press Enter to start";
		ELSE
			PRINT "Enter filename";
		END IF
		PRINT SPACE$(24)
	END IF
	Col=3:LOCATE %SelLine,3,1,0,7
	DO
		DO
			Choice$=INKEY$
		LOOP WHILE Choice$=""
		Choice=ASC(RIGHT$(Choice$,1))
		IF LEN(Choice$)=2 THEN
			IF FF AND NoDraw=%FALSE THEN
				Col=3:LH=HL:TempName$=""
				SELECT CASE Choice
					CASE %RtAr:INCR HL
					CASE %LfAr:DECR HL
					CASE %DnAr:INCR HL,5
					CASE %UpAr:DECR HL,5
					CASE %Home:HL=0
					CASE %End :HL=FPOS
					CASE %PgDn:
						IF FF>70 THEN
							XFH=%TRUE
							FPOS=FF-71
							LOCATE ,,0
							CALL ClearWindow(%TopLeft2,%BottomRight2)
							CALL PrintName(FileEntry$())
							HL=0:LH=HL
						END IF
					CASE %PgUp
						IF FF>70 THEN
							XFH=%FALSE
							FPOS=69
							LOCATE ,,0
							CALL ClearWindow(%TopLeft2,%BottomRight2)
							CALL PrintName(FileEntry$())
							HL=0:LH=HL
						END IF
					CASE %F1 TO %F10, %AltF1 TO %AltF10
						NoDraw=%TRUE
						Jump=Choice
						IF ABS(M-Choice)>9 THEN ChangeMenu=%TRUE ELSE ChangeMenu=%FALSE
						EXIT LOOP
					$IF %TE
						CASE 120 TO 123
							Level=Choice-120
							Level$=LTRIM$(STR$(Level+1))
							IF M=%F4 THEN
								PRINT SHOWLEVEL$(%HeadLine,21,Level$);
							END IF
					$ENDIF
					$IF %TK
						CASE 122,123
							Level=Choice-119
							IF M=%F5 THEN
								PRINT SHOWLEVEL$(%HeadLine,21,LTRIM$(STR$(Level)));
							END IF
					$ENDIF
					$IF %TC
						CASE 32,37
							IF Choice=32 THEN
								Ver=1:Level$=" Disk  Version "
							ELSE
								Ver=0:Level$="Keyword version"
							END IF
							IF M=%F4 THEN
								PRINT SHOWLEVEL$(%HeadLine,21,LEFT$(LTRIM$(Level$),1));
							END IF
					$ENDIF
				END SELECT
				IF HL<0 THEN HL=LH
				IF HL>FPOS THEN
					IF HL=>FPOS-(FPOS MOD 5)+5 THEN
						HL=LH
					ELSE
						HL=FPOS
					END IF
				END IF
				XHL=HL:XLH=LH
				IF XFH THEN INCR XHL,70:INCR XLH,70
				LOCATE (LH\5)+2,((LH MOD 5)*16)+3
				COLOR Ink,Paper:PRINT FileEntry$(XLH)
				LOCATE (HL\5)+2,((HL MOD 5)*16)+3
				COLOR Paper,Ink:PRINT FileEntry$(XHL)
				IF NoDraw=%FALSE THEN
					PRINT CLEARLINE$(%SelLine,32,0);
					PRINT FileEntry$(XHL);TAB(18);FileLen&(XHL);
				ELSE
					COLOR Ink,Paper
					PRINT "Enter filename";SPACE$(24);
				END IF
				LOCATE %SelLine,3,1
				LH=HL
			ELSEIF FF<2 OR NoDraw THEN
				IF Choice=>%F1 AND Choice<=%F10 OR Choice=>%AltF1 AND Choice<=%AltF10 THEN
					Jump=Choice
					IF ABS(M-Choice)>9 THEN ChangeMenu=%TRUE ELSE ChangeMenu=%FALSE
					EXIT LOOP
				END IF
			END IF
		ELSE
			SELECT CASE Choice
				CASE %Ent
					IF TempName$="" THEN
						Naam$=FileEntry$(XHL)
						IF ASCII(Naam$)=60 THEN
							Naam$="\"+REMOVE$(Naam$, ANY "<>")
						END IF
					ELSE
						Naam$=TempName$
					END IF
					EXIT LOOP
				CASE %Esc:EXIT IF
				CASE %Bsp
					DECR Col
					IF Col<3 THEN Col=3:EXIT SELECT
					LOCATE %SelLine,Col:PRINT CHR$(32):LOCATE %SelLine,Col
					TempName$=LEFT$(TempName$,LEN(TempName$)-1)
				CASE 6
					IF M=%AltF5 THEN Naam$=CHR$(Choice):EXIT LOOP
				CASE ELSE
					IF LEN(TempName$)<32 THEN
						PRINT UCASE$(CHR$(Choice));SPACE$(31-LEN(TempName$));
						INCR Col:LOCATE %SelLine,Col
						TempName$=TempName$+UCASE$(CHR$(Choice))
					ELSE
						SOUND 88,5
					END IF
			END SELECT
		END IF
	LOOP UNTIL (Choice$)=CHR$(%Esc)
	LOCATE 16,80:PRINT CHR$(186);
	IF Choice$=CHR$(%Esc) THEN
		Jump=0:Naam$=""
	END IF
	LOCATE ,,0
END SUB
:
SUB DoScreen(Heading$,Title$,WhichFiles$)
	SHARED Drive$,Esau$,FileEntry$(),FF,Jump,KeyOn,Ink,Paper
	IF Jump THEN
		CALL ReScreen(Heading$,Title$,WhichFiles$)
	ELSE
		CALL ChPath(Drive$)
		CALL GetDirra(WhichFiles$)
		CALL ScreenDraw(Heading$)
		IF FF THEN CALL PrintName(FileEntry$())
		$IF %TC
			IF ESAU$="R" THEN EXIT SUB
		$ENDIF
		IF KeyOn THEN PRINT FUNKEY$;
	END IF
	COLOR Ink,Paper
END SUB
:
SUB ScreenDraw(Heading$)
	SHARED FF,NoDraw
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	PRINT PATHEAD$;
	PRINT HORLINE$;
	PRINT HEAD$(Heading$);
	PRINT SPACEFREE$(0);
	IF FF=0 THEN
		PRINT NOFILES$;
	ELSE
		IF NoDraw=%FALSE THEN
			PRINT ARROWS$;
			PRINT TITEL$(Title$);
		END IF
	END IF
	PRINT ESCMAIN$;
END SUB
:
SUB ReScreen(Heading$,Title$,WhichFiles$)
	SHARED ChangeMenu,FF,Jump,KeyOn,Level,Level$,M,NoDraw
	DiskChanged=%FALSE
	PRINT CLEARLINE$(%HeadLine,55,0);
	PRINT PATHEAD$;
	PRINT HEAD$(Heading$);
	PRINT SPACEFREE$(DiskChanged);

	$IF %TE
		IF M=%F4 THEN PRINT SHOWLEVEL$(%HeadLine,21,Level$);
		IF Jump=0 OR DiskChanged OR M=%AltF7 OR M=%AltF5 THEN
			PRINT REFILES$(WhichFiles$)
		END IF
	$ENDIF

	$IF %TK
		$IF NOT %TK5
			IF M=%F5 THEN PRINT SHOWLEVEL$(%HeadLine,21,LTRIM$(STR$(Level)));
		$ENDIF
		IF Jump=0 OR DiskChanged OR M=%AltF7 OR M=%AltF5 THEN
			PRINT REFILES$(WhichFiles$)
		END IF
	$ENDIF

	IF NoDraw=%FALSE THEN
		IF FF THEN
			PRINT ARROWS$;
			PRINT TITEL$(Title$);
		END IF
	ELSE
		IF FF=0 THEN PRINT NOFILES$;
		PRINT CLEARLINE$(%ActLine,62,0);
	END IF
	IF ChangeMenu THEN
		IF KeyOn THEN PRINT FUNKEY$;
	END IF
END SUB
:
FUNCTION REFILES$(WhichFiles$)
	SHARED FF,FileEntry$()
	CALL ClearWindow(%TopLeft2,%BottomRight2)
	CALL GetDirra(WhichFiles$)
	IF FF=0 OR NoDraw THEN
		PRINT NOFILES$;
	ELSE
		CALL PrintName(FileEntry$())
	END IF
	PRINT SPACEFREE$(0);
END FUNCTION
:
SUB ChPath(Path$)
	CHDRIVE Path$
	IF INSTR (Path$,"\") THEN
		CHDIR RIGHT$(Path$,LEN(Path$)-2)
	ELSE
		CHDIR "\"
	END IF
END SUB
:
FUNCTION SPACEFREE$(DC)
	STATIC OldFree&
	SHARED Paper
	FS&=FREESPACE&(CURDIR$)
	LOCATE %HeadLine,58
	COLOR %Yellow,Paper
	FS$=STR$(FS&)+" bytes free"
	PRINT FS$;SPACE$(79-(57+LEN(FS$)));
	IF FS&<>OldFree& THEN
		DC=%TRUE
	ELSE
		DC=%FALSE
	END IF
	OldFree&=FS&
END FUNCTION
:
SUB TidyUp
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT ARROWS$;
	PRINT ESCMAIN$;
	PRINT FUNKEY$;
END SUB
:
FUNCTION CHKLEN(Repeat,Extra,LenBite,Length&)
	IF Length&>LenBite THEN
		Repeat=Length&\LenBite
		Extra=Length& MOD LenBite
		CHKLEN=%TRUE
	ELSE
		CHKLEN=%FALSE
	END IF
END FUNCTION
:
FUNCTION CLEARLINE$(WhichLine,Blanks,Curs)
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE WhichLine,3
	PRINT SPACE$(Blanks);
	LOCATE WhichLine,3,Curs
END FUNCTION
:
FUNCTION PATHEAD$
	SHARED BWYellow,WhichFiles$
	PRINT TOPLINE$;
	COLOR BWYellow,%Cyan
	PRINT CENTRE$(1,RTRIM$(CURDIR$,"\")+"\"+RTRIM$(WhichFiles$,CHR$(0)));
END FUNCTION
:
FUNCTION FITHEAD$(Heading$,Path$)
	SHARED BWYellow,Ink,Paper
	Hed$=Heading$+RTRIM$(Path$,"\")
	IF LEN(Hed$)>55 THEN Hed$=LEFT$(Hed$,55)
	COLOR BWYellow,%Cyan
	LOCATE %HeadLine,3:PRINT Hed$;
	COLOR Ink,Paper
	PRINT SPACE$(55-LEN(Hed$));
END FUNCTION
:
FUNCTION TITEL$(Title$)
	SHARED FF,Ink,Paper
	IF FF THEN
		COLOR Ink,Paper
		LOCATE %SelLine,3
		PRINT Title$;
	END IF
END FUNCTION
:
FUNCTION ARROWS$
	SHARED Paper
	COLOR %LCyan,Paper
	LOCATE %ActLine,3
	PRINT CHR$(27)+CHR$(24)+CHR$(25)+CHR$(26);
	COLOR %Yellow
	PRINT " to highlight file, ENTER selects";
END FUNCTION
:
FUNCTION ESCMAIN$
	SHARED Paper
	COLOR %LCyan,Paper
	LOCATE %ActLine,65
	PRINT "ESC:";
	COLOR %Yellow
	PRINT " Main Menu";
END FUNCTION
:
FUNCTION HEAD$(Heading$)
	SHARED BWYellow
	COLOR BWYellow,%Cyan
	LOCATE %HeadLine,3
	PRINT Heading$;
END FUNCTION
:
FUNCTION HORLINE$
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE 16,1
	PRINT CHR$(199)+STRING$(78,196)+CHR$(182);
END FUNCTION
:
FUNCTION TOPLINE$
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE 1,2
	PRINT STRING$(78,205);
END FUNCTION
:
FUNCTION CENTRE$(Y,MSG$)
	X=40-(LEN(MSG$)\2)
	LOCATE Y,X,0
	PRINT MSG$;
END FUNCTION
:
FUNCTION NOFILES$
	SHARED Ink,Paper
	COLOR Ink+%Flash,Paper
	LOCATE 2,3:PRINT "No files!"
	COLOR Ink
	LOCATE %SelLine,3
	PRINT "Change drive?";SPACE$(64);
	PRINT CLEARLINE$(%ActLine,62,1);
END FUNCTION
:
FUNCTION SHOWLEVEL$(Y,X,Level$)
	SHARED BWYellow,Ink,Paper,M
	LOCATE Y,X
	IF M<120 THEN
		COLOR BWYellow,%Cyan
	ELSE
		COLOR %Yellow,Paper
	END IF
	PRINT Level$;
	COLOR Ink,Paper
END FUNCTION
:
FUNCTION WARNERR(ERRMSG$)
	SHARED Ink,Paper
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	COLOR Ink+%Flash,%Red:PRINT CENTRE$(23,"** "+SPACE$(LEN(ERRMSG$))+" **");
	COLOR Ink,%Red:PRINT CENTRE$(23,ERRMSG$);
	COLOR Ink,Paper:PRINT CENTRE$(25,"Press any key to continue. ESCape for Main Menu");
	CNT$=INPUT$(1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	IF CNT$=CHR$(%Esc) THEN
		WARNERR=%TRUE
	ELSE
		WARNERR=%FALSE
	END IF
END FUNCTION
:
FUNCTION GETNM$(Drive$) SHARED
	DO
		BF=0
		DO
			CD=%FALSE
			CALL SELECTA(NM$)
			IF NM$=CHR$(6) THEN GETNM$=NM$:EXIT FUNCTION
			IF NM$="" OR Jump THEN
				IF FF THEN PRINT CLEARLINE$(%SelLine,77,0);
				GETNM$="":EXIT FUNCTION
			END IF
			IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
				IF M=%AltF2 THEN
					IF ASCII(NM$)=92 THEN
						GETNM$=CHR$(%Ent):EXIT FUNCTION
					END IF
				END IF
				CD=%True
				Drive$=NEWDIR$(Drive$,NM$)
				IF Drive$=CHR$(%Esc) THEN GETNM$="":EXIT FUNCTION
				CALL ReScreen(Heading$,Title$,WhichFiles$)
			END IF
		LOOP WHILE CD
		$IF %TC
			IF M=%F4 THEN IF INSTR(NM$,".ENC") THEN BF=%NoCopy
			IF M=%F5 THEN IF INSTR(NM$,".DEC") THEN BF=%DecDone
		$ENDIF
		IF Wild THEN IF INSTR(NM$,"*") THEN BF=%BadName
		IF ERRA(BF) THEN EXIT LOOP
		IF FF=0 THEN PRINT NOFILES$;
	LOOP WHILE BF
	IF BF THEN
		GETNM$=""
	ELSE
		GETNM$=NM$
	END IF
END FUNCTION
