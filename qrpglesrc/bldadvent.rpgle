     H DFTACTGRP(*NO) BNDDIR('QC2LE')

     FQRPGSRC1  O    E             DISK    EXTDESC('EMPBJG/QRPGSRC')
     F                                     EXTFILE(*EXTDESC)
     F                                     EXTMBR('ADVENT1')
     F                                     RENAME(QRPGSRC:RPGSRC1)
     F                                     USROPN
     FQRPGSRC2  O    E             DISK    EXTDESC('EMPBJG/QRPGSRC')
     F                                     EXTFILE(*EXTDESC)
     F                                     EXTMBR('ADVENT2')
     F                                     RENAME(QRPGSRC:RPGSRC2)
     F                                     USROPN
     FQRPGSRC3  O    E             DISK    EXTDESC('EMPBJG/QRPGSRC')
     F                                     EXTFILE(*EXTDESC)
     F                                     EXTMBR('ADVENT3')
     F                                     RENAME(QRPGSRC:RPGSRC3)
     F                                     USROPN

      /INCLUDE "../qrpgleref/system_calls.rpgle"

     D SourceLine      DS                  QUALIFIED
     D  line                          5S 0 OVERLAY(SourceLine:1)
     D  spec                          1A   OVERLAY(SourceLine:6)
     D  comment                       1A   OVERLAY(SourceLine:7)
      * H-spec
     D  cntlEntry                    80A   OVERLAY(SourceLine:8)
      * P-spec
     D  functionName                 15A   OVERLAY(SourceLine:7)
     D  functionBeginEnd...
     D                                1A   OVERLAY(SourceLine:24)
      * D-spec
     D  varName                      15A   OVERLAY(SourceLine:7)
     D  varType                       2A   OVERLAY(SourceLine:24)
     D  varSize                       4S 0 OVERLAY(SourceLine:36)
     D  varDataType                   1A   OVERLAY(SourceLine:40)
     D  varDecPos                     2A   OVERLAY(SourceLine:41)
     D  varKeywords                  40A   OVERLAY(SourceLine:44)
      * C-spec
     D  factor1                      14A   OVERLAY(SourceLine:12)
     D  opcode                       10A   OVERLAY(SourceLine:26)
     D  factor2                      14A   OVERLAY(SourceLine:36)
     D  extFactor2                   45A   OVERLAY(SourceLine:36)
     D  result                       14A   OVERLAY(SourceLine:50)
     D  text                         80A   OVERLAY(SourceLine:9)
      * Table
     D  entry                        80A   OVERLAY(SourceLine:1)

     D A               S             10I 0
     D Adjusted        S               N
     D B               S             10I 0
     D Buffer          S          10000A
     D BufferLen       S             10I 0
     D C               S             10I 0
     D Count           S             10I 0
     D Dims            S             10A   DIM(50)
     D End             S             10I 0
     D EndforNeeded    S               N
     D EndforTag       S             14A
     D EndforTagFound  S               N
     D EndifNeeded     S               N
     D FileName        S            512A   VARYING
     D FunctionName    S             15A
     D I               S             10I 0
     D InFunc          S               N
     D Len             S             10I 0
     D Line#           S              5S 0
     D Options         S            512A   VARYING
     D Paren           S             10I 0
     D Pos             S             10I 0
     D ReadingDat      S               N
     D Func            S               N
     D SkipFunc        S               N
     D SrcSeq1         S                   LIKE(SrcSeq)
     D SrcSeq2         S                   LIKE(SrcSeq)
     D SrcSeq3         S                   LIKE(SrcSeq)
     D Start           S             10I 0
     D Statement       S            512A
     D Stream          S                   LIKE(pFile)
     D Success         S               *
     D Tab             S             10I 0
     D Tag             S             14A
     D Tags            S             14A   DIM(50)
     D Temp            S              4A   INZ
     D TestParm        S             40A
     D Token           S              4A   DIM(10) INZ
     D Type            S              1A
     D Var             S             14A
     D Vars            S             14A   DIM(50)
     D ZEROS           S              5A   INZ('00000')


       *INLR = *ON;

       system('CLRPFM EMPBJG/QRPGSRC ADVENT1');
       OPEN QRPGSRC1;
       system('CLRPFM EMPBJG/QRPGSRC ADVENT2');
       OPEN QRPGSRC2;
       system('CLRPFM EMPBJG/QRPGSRC ADVENT3');
       OPEN QRPGSRC3;

       CLEAR SourceLine;
       SourceLine.Spec = 'H';
       SourceLine.cntlEntry = 'DFTACTGRP(*NO) ACTGRP(*NEW)';
       WriteLine(SourceLine:2);

       FileName = '/home/brian/adventure/77-03-31_adventure.f';

       Options = 'r, crln=N';
       Stream = fopen(FileName:Options);

       DOW ReadRecord(Stream:Buffer:BufferLen:Line#);

           Adjusted = FALSE;

           DOU BufferLen = 0;

               SELECT;
                   // blank line
                   WHEN BufferLen = 0;
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       WriteLine(SourceLine);

                   // comment
                   WHEN %SUBST(Buffer:1:1) = 'C';
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = %SUBST(Buffer:2);
                       WriteLine(SourceLine);
                       Buffer = '';

                   // Unconvertable statement
                   // Contains =" or ." or (" or lline
                   WHEN %SCAN('="':Buffer) > 0
                       OR %SCAN('."':Buffer) > 0
                       OR %SCAN('("':Buffer) > 0
                       OR (%SUBST(Buffer:1:11) <> x'05' + 'DIMENSION '
                            AND %SCAN('LLINE(':Buffer) > 0);
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = '==== ' + Buffer;
                       WriteLine(SourceLine);
                       Buffer = '';

                   // Adjust buffer for LLINE
                   WHEN (%SUBST(Buffer:1:11) <> x'05' + 'DIMENSION '
                            AND %SCAN('LLINE(':Buffer) > 0)
                            AND NOT(Adjusted);
                       A = %SCAN('LLINE(':Buffer);
                       B = %SCAN(',':Buffer:A);
                       C = %SCAN(')':Buffer:B);
                       Buffer = %SUBST(Buffer:1:B-1)
                              + ').LINE('
                              + %SUBST(Buffer:B+1);
                       Adjusted = TRUE;

                   // number = convert to tag and split line
                   WHEN %SUBST(Buffer:1:1) >= '0'
                               AND %SUBST(Buffer:1:1) <= '9';
                       tab = %SCAN(x'05':Buffer);
                       IF tab = 0;
                           i = %CHECK(NUMBER:%SUBST(Buffer:1:5));
                           IF i = 0 AND %SUBST(Buffer:6:1) = ' ';
                               %SUBST(Buffer:6:1) = x'05';
                               tab = 6;
                           ENDIF;
                       ENDIF;
                       IF tab <> 0;
                           tag = 'TAG' + %SUBST(BUFFER:1:tab-1);
                           CLEAR SourceLine;
                           SourceLine.Line = line#;
                           SourceLine.spec = 'C';
                           SourceLine.factor1 = tag;
                           SourceLine.opcode = 'TAG';
                           WriteLine(SourceLine);
                           IF %SUBST(BUFFER:1:tab-1) = '1002';
                               // start reading .dat file
                               ReadingDat = TRUE;
                           ENDIF;
                           IF %SUBST(BUFFER:1:tab-1) = '1100';
                               // done reading .dat file
                               ReadingDat = FALSE;
                           ENDIF;
                       ELSE;
                           CLEAR SourceLine;
                           SourceLine.Line = line#;
                           SourceLine.Comment = '*';
                           SourceLine.Text = '???? ' + Buffer;
                           WriteLine(SourceLine);
                       ENDIF;
                       Buffer = %SUBST(Buffer:tab);
                       IF EndforNeeded AND tag = EndforTag;
                           EndforTagFound = *ON;
                       ENDIF;

                   // In the setup routine
                   // Just skip all these lines as they are handled in IFILE
                   WHEN ReadingDat;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = '==== ' + Buffer;
                       WriteLine(SourceLine);
                       Buffer = '';

                   // if
                   // IF(DIST)10,20,30
                   WHEN %SUBST(Buffer:1:4) = x'05' + 'IF(';
                       Count = 0;
                       Paren = 0;
                       FOR i = 4 TO BufferLen;
                           IF %SUBST(Buffer:i:1) = '(';
                               count = count + 1;
                           ELSEIF %SUBST(Buffer:i:1) = ')';
                               count = count - 1;
                           ENDIF;
                           IF Count = 0;
                               Paren = i;
                               LEAVE;
                           ENDIF;
                       ENDFOR;
                       IF Paren > 4;
                           IF %SUBST(Buffer:Paren+1:1) >= '0'
                               AND %SUBST(Buffer:Paren+1:1) <= '9';
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'SELECT';
                               WriteLine(SourceLine);

                               // < 0 option
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'WHEN';
                               SourceLine.factor2 = %SUBST(Buffer:4:Paren-3)
                                                  + '< 0';
                               WriteLine(SourceLine);
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'GOTO';
                               Start = Paren + 1;
                               End = %SCAN(',':Buffer:Start);
                               SourceLine.factor2 = 'TAG' + %SUBST(Buffer:Start:
                                                                     End-Start);
                               WriteLine(SourceLine);

                               // = 0 option
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'WHEN';
                               SourceLine.factor2 = %SUBST(Buffer:4:Paren-3)
                                                  + '= 0';
                               WriteLine(SourceLine);
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'GOTO';
                               Start = End + 1;
                               End = %SCAN(',':Buffer:Start);
                               SourceLine.factor2 = 'TAG' + %SUBST(Buffer:Start:
                                                                     End-Start);
                               Start = End + 1;
                               WriteLine(SourceLine);

                               // > 0 option
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'WHEN';
                               SourceLine.factor2 = %SUBST(Buffer:4:Paren-3)
                                                  + '> 0';
                               WriteLine(SourceLine);
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'GOTO';
                               Start = End + 1;
                               End = BufferLen + 1;
                               SourceLine.factor2 = 'TAG' + %SUBST(Buffer:Start:
                                                                     End-Start);
                               Start = End + 1;
                               WriteLine(SourceLine);

                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'ENDSL';
                               WriteLine(SourceLine);

                               CLEAR Buffer;

                           ELSE;
                               CLEAR SourceLine;
                               SourceLine.Line = line#;
                               SourceLine.spec = 'C';
                               SourceLine.opcode = 'IF';
                               Statement = %SUBST(Buffer:4:Paren-3);
                               FullReplace('.EQ.':' = ':3:Statement);
                               FullReplace('.NE.':' <> ':4:Statement);
                               FullReplace('.GT.':' > ':3:Statement);
                               FullReplace('.GE.':' >= ':4:Statement);
                               FullReplace('.LT.':' < ':3:Statement);
                               FullReplace('.LE.':' <= ':4:Statement);
                               FullReplace('.OR.':' OR ':4:Statement);
                               FullReplace('.AND.':' AND ':5:Statement);
                               FullReplace('.NOT.':' NOT ':5:Statement);
                               FullReplace('.TRUE.':' *ON ':5:Statement);
                               FullReplace('.FALSE.':' *OFF ':6:Statement);
                               CheckForFunctions(Statement);
                               SplitStatement(Statement);
                               Buffer = x'05' + %TRIM(%SUBST(Buffer:Paren+1));
                               EndifNeeded = *ON;
                           ENDIF;
                       ELSE;
                           CLEAR SourceLine;
                           SourceLine.Line = line#;
                           SourceLine.comment = '*';
                           SourceLine.text = 'if ' + Buffer;
                           WriteLine(SourceLine);
                           Buffer = '';
                       ENDIF;

                   // call
                   WHEN %SUBST(Buffer:1:6) = x'05' + 'CALL ';
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.spec = 'C';
                       // just comment out calls to GETIN for now
                       IF %SCAN('GETIN(':Buffer) > 0;
                           SourceLine.Comment = '*';
                           SourceLine.Text = '==== ' + Buffer;
                           WriteLine(SourceLine);
                       ELSE;
                           SourceLine.opcode = 'CALLP';
                           Statement = %SUBST(Buffer:7);
                           FullReplace(',':':':1:Statement);
                           SplitStatement(Statement);
                       ENDIF;
                       Buffer = '';
                       EXSR CheckForEnd;

                   // goto (
                   WHEN %SUBST(Buffer:1:6) = x'05' + 'GOTO('
                     OR %SUBST(Buffer:1:7) = x'05' + 'GOTO (';
                       Paren = 0;
                       FOR i = 6 TO BufferLen;
                           IF %SUBST(Buffer:i:1) = ')';
                               Paren = i;
                               LEAVE;
                           ENDIF;
                       ENDFOR;
                       IF paren <> 0;
                           count = 0;
                           IF %SUBST(Buffer:1:6) = x'05' + 'GOTO(';
                               Start = 7;
                           ELSE;
                               Start = 8;
                           ENDIF;
                           CLEAR Tags;
                           DOU End = Paren;
                               End = %SCAN(',':Buffer:Start);
                               IF End = 0;
                                   End = Paren;
                               ENDIF;
                               count = count + 1;
                               tags(count) = 'TAG' + %SUBST(Buffer:Start:
                                                                     End-Start);
                               Start = End + 1;
                           ENDDO;
                           TestParm = %SUBST(buffer:paren+1);
                       ENDIF;
                       IF count <> 0;
                           CLEAR SourceLine;
                           SourceLine.Line = line#;
                           SourceLine.spec = 'C';
                           SourceLine.opcode = 'SELECT';
                           WriteLine(SourceLine);
                           FOR i = 1 TO count;
                               SourceLine.opcode = 'WHEN';
                               SourceLine.ExtFactor2 = %TRIMR(testparm)
                                                             + ' = ' + %CHAR(i);
                               WriteLine(SourceLine);
                               SourceLine.opcode = 'GOTO';
                               SourceLine.ExtFactor2 = tags(i);
                               WriteLine(SourceLine);
                           ENDFOR;
                           SourceLine.opcode = 'ENDSL';
                           SourceLine.extfactor2 = '';
                           WriteLine(SourceLine);
                       ENDIF;
                       Buffer = '';
                       EXSR CheckForEnd;

                   // goto
                   WHEN %SUBST(Buffer:1:6) = x'05' + 'GOTO ';
                       tag = 'TAG' + %SUBST(BUFFER:7);
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.opcode = 'GOTO';
                       SourceLine.factor2 = tag;
                       WriteLine(SourceLine);
                       Buffer = '';
                       EXSR CheckForEnd;

                   // go to
                   WHEN %SUBST(Buffer:1:7) = x'05' + 'GO TO ';
                       tag = 'TAG' + %SUBST(BUFFER:8);
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.opcode = 'GOTO';
                       SourceLine.factor2 = tag;
                       WriteLine(SourceLine);
                       Buffer = '';
                       EXSR CheckForEnd;

                   // DO
                   WHEN %SUBST(Buffer:1:4) = x'05' + 'DO ';
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       IF SkipFunc;
                           SourceLine.Comment = '*';
                           SourceLine.Text = '==== ' + Buffer;
                           WriteLine(SourceLine);
                       ELSE;
                           Start = 5;
                           End = %SCAN(' ':Buffer:Start);
                           EndforNeeded = *ON;
                           EndforTag = 'TAG' + %SUBST(Buffer:Start:End-Start);
                           EndforTagFound = *OFF;
                           SourceLine.spec = 'C';
                           SourceLine.opcode = 'FOR';
                           Statement = %SUBST(Buffer:End+1);
                           FullReplace(',':' TO ':4:Statement);
                           SplitStatement(Statement);
                       ENDIF;
                       Buffer = '';

                   // CONTINUE
                   WHEN %SUBST(Buffer:1:9) = x'05' + 'CONTINUE';
                       IF EndforNeeded AND EndforTagFound;
                           CLEAR SourceLine;
                           SourceLine.Line = line#;
                           SourceLine.spec = 'C';
                           SourceLine.opcode = 'ENDFOR';
                           WriteLine(SourceLine);
                           EndforNeeded = *OFF;
                       ENDIF;
                       Buffer = '';

                   // RETURN
                   WHEN %SUBST(Buffer:1:8) = x'05' + 'RETURN ';
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.opcode = 'RETURN';
                       WriteLine(SourceLine);
                       Buffer = '';
                       EXSR CheckForEnd;

                   // SUBROUTINE
                   WHEN %SUBST(Buffer:1:12) = x'05' + 'SUBROUTINE ';
                       Paren = %SCAN('(':Buffer:13);
                       CLEAR Vars;
                       Count = 0;
                       IF Paren <> 0;
                           FunctionName = %SUBST(Buffer:13:Paren-13);
                           Start = Paren + 1;
                           DOW 1=1;
                               End = %SCAN(',':Buffer:Start);
                               IF End = 0;
                                   LEAVE;
                               ENDIF;
                               Count = Count + 1;
                               Vars(Count) = %SUBST(Buffer:Start:End-Start);
                               Start = End + 1;
                           ENDDO;
                           Count = Count + 1;
                           End = %SCAN(')':Buffer:Start);
                           Vars(Count) = %SUBST(Buffer:Start:End-Start);
                       ELSE;
                           FunctionName = %SUBST(Buffer:13);
                       ENDIF;
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'P';
                       SourceLine.functionName = ' ' + FunctionName;
                       SourceLine.functionBeginEnd = 'B';
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'D';
                       SourceLine.varName = ' ' + FunctionName;
                       SourceLine.varType = 'PI';
                       WriteLine(SourceLine);
                       FOR I = 1 TO Count;
                           CLEAR SourceLine;
                           SourceLine.Line = Line#;
                           SourceLine.spec = 'D';
                           SourceLine.varName = '  ' + Vars(i);
                           IF FunctionName = 'GETIN' AND I >= 2;
                               SourceLine.varSize = 5;
                               SourceLine.varDataType = 'A';
                               SourceLine.varDecPos = *BLANKS;
                           ELSE;
                               SourceLine.varSize = 10;
                               SourceLine.varDataType = 'I';
                               SourceLine.varDecPos = '00';
                           ENDIF;
                           SourceLine.varKeywords = 'VALUE';
                           WriteLine(SourceLine);
                       ENDFOR;
                       Buffer = '';
                       EXSR CheckForEnd;
                       InFunc = TRUE;
                       SkipFunc = (FunctionName = 'GETIN'
                                    OR FunctionName = 'SHIFT');


                   // LOGICAL FUNCTION
                   WHEN %SUBST(Buffer:1:17) = x'05' + 'LOGICAL FUNCTION';
                       Paren = %SCAN('(':Buffer:18);
                       IF Paren <> 0;
                           FunctionName = %SUBST(Buffer:18:Paren-18);
                       ELSE;
                           FunctionName = %SUBST(Buffer:18);
                       ENDIF;
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'P';
                       SourceLine.functionName = ' ' + FunctionName;
                       SourceLine.functionBeginEnd = 'B';
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'D';
                       SourceLine.varName = ' ' + FunctionName;
                       SourceLine.varType = 'PI';
                       SourceLine.varDataType = 'N';
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = '==== ' + Buffer;
                       WriteLine(SourceLine);
                       Buffer = '';
                       EXSR CheckForEnd;

                   // INTEGER FUNCTION
                   WHEN %SUBST(Buffer:1:17) = x'05' + 'INTEGER FUNCTION';
                       Paren = %SCAN('(':Buffer:18);
                       IF Paren <> 0;
                           FunctionName = %SUBST(Buffer:18:Paren-18);
                       ELSE;
                           FunctionName = %SUBST(Buffer:18);
                       ENDIF;
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'P';
                       SourceLine.functionName = ' ' + FunctionName;
                       SourceLine.functionBeginEnd = 'B';
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'D';
                       SourceLine.varName = ' ' + FunctionName;
                       SourceLine.varType = 'PI';
                       SourceLine.varSize = 10;
                       SourceLine.varDataType = 'I';
                       SourceLine.varDecPos = '00';
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = '==== ' + Buffer;
                       WriteLine(SourceLine);
                       Buffer = '';
                       EXSR CheckForEnd;

                   // END
                   WHEN %SUBST(Buffer:1:5) = x'05' + 'END ';
                       IF FunctionName = *BLANKS;
                           CLEAR SourceLine;
                           SourceLine.line = line#;
                           SourceLine.spec = 'C';
                           SourceLine.opcode = 'EVAL';
                           SourceLine.ExtFactor2 = '*INLR = *ON';
                           WriteLine(SourceLine);
                           CLEAR SourceLine;
                           SourceLine.line = line#;
                           SourceLine.spec = 'C';
                           SourceLine.opcode = 'RETURN';
                           WriteLine(SourceLine);
                       ELSE;
                           CLEAR SourceLine;
                           SourceLine.line = line#;
                           SourceLine.spec = 'P';
                           SourceLine.functionName = ' ' + FunctionName;
                           SourceLine.functionBeginEnd = 'E';
                           WriteLine(SourceLine);
                           InFunc = FALSE;
                       ENDIF;
                       Buffer = '';

                   // STOP
                   WHEN %SUBST(Buffer:1:6) = x'05' + 'STOP ';
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.opcode = 'EVAL';
                       SourceLine.ExtFactor2 = '*INLR = *ON';
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.opcode = 'RETURN';
                       WriteLine(SourceLine);
                       Buffer = '';

                   // PAUSE
                   WHEN %SUBST(Buffer:1:7) = x'05' + 'PAUSE ';
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.opcode = 'EVAL';
                       SourceLine.ExtFactor2 = 'MSG = ' + %SUBST(Buffer:8);
                       WriteLine(SourceLine);
                       CLEAR SourceLine;
                       SourceLine.line = line#;
                       SourceLine.spec = 'C';
                       SourceLine.factor1 = 'MSG';
                       SourceLine.opcode = 'DSPLY';
                       SourceLine.result = 'DUMMY';
                       WriteLine(SourceLine);
                       Buffer = '';

                   // DIMENSION
                   WHEN %SUBST(Buffer:1:11) = x'05' + 'DIMENSION ';
                       IF InFunc;
                           SourceLine.Line = line#;
                           SourceLine.Comment = '*';
                           SourceLine.Text = '==== ' + Buffer;
                           WriteLine(SourceLine);
                       ELSE;
                            CLEAR Vars;
                            CLEAR Dims;
                            A = 12;
                            Count = 0;
                            DOW 1=1;
                                B = %SCAN('(':Buffer:A);
                                C = %SCAN(')':Buffer:B+1);
                                IF B = 0;
                                    LEAVE;
                                ENDIF;
                                Count = Count + 1;
                                Vars(Count) = %SUBST(Buffer:A:B-A);
                                Dims(Count) = %SUBST(Buffer:B+1:C-B-1);
                                A = C + 2;
                            ENDDO;
                            IF Count > 0;
                                FOR I = 1 TO Count;
                                    CLEAR SourceLine;
                                    SourceLine.Line = Line#;
                                    SourceLine.spec = 'D';
                                    SourceLine.varName = ' ' + Vars(i);
                                    SourceLine.varType = 'S';
                                    SELECT;
                                    WHEN vars(i) = 'LLINE';
                                        SourceLine.varSize = 256;
                                        SourceLine.varDataType = 'A';
                                        A = %SCAN(',':Dims(i));
                                        SourceLine.varKeywords = 'DIM('
                                                  + %SUBST(%TRIM(Dims(i)):1:A-1)
                                                  + ')';
                                    WHEN vars(i) = 'ATAB';
                                        SourceLine.varSize = 5;
                                        SourceLine.varDataType = 'A';
                                        SourceLine.varKeywords = 'DIM('
                                                  + %TRIM(Dims(i)) + ')';
                                    OTHER;
                                        SourceLine.varSize = 10;
                                        SourceLine.varDataType = 'I';
                                        SourceLine.varDecPos = '00';
                                        SourceLine.varKeywords = 'DIM('
                                                  + %TRIM(Dims(i)) + ')';
                                    ENDSL;
                                    WriteLine(SourceLine);
                                ENDFOR;
                            ENDIF;
                       ENDIF;
                       Buffer = '';

                   // INTEGER
                   WHEN %SUBST(Buffer:1:9) = x'05' + 'INTEGER ';
                       CLEAR Vars;
                       Start = 10;
                       Count = 0;
                       DOW 1=1;
                           End = %SCAN(',':Buffer:Start);
                           IF End = 0;
                               LEAVE;
                           ENDIF;
                           Count = Count + 1;
                           Vars(Count) = %SUBST(Buffer:Start:End-Start);
                           Start = End + 1;
                       ENDDO;
                       Count = Count + 1;
                       Vars(Count) = %SUBST(Buffer:Start);
                       FOR I = 1 TO Count;
                           CLEAR SourceLine;
                           SourceLine.Line = Line#;
                           SourceLine.spec = 'D';
                           SourceLine.varName = ' ' + Vars(i);
                           SourceLine.varType = 'S';
                           SourceLine.varSize = 10;
                           SourceLine.varDataType = 'I';
                           SourceLine.varDecPos = '00';
                           WriteLine(SourceLine);
                       ENDFOR;
                       Buffer = '';

                   // REAL
                   // Skip REAL because it is only used for RAN which is a function?
                   WHEN  %SUBST(Buffer:1:6) = x'05' + 'REAL ';
                       //CLEAR Vars;
                       //Start = 7;
                       //Count = 0;
                       //DOW 1=1;
                       //    End = %SCAN(',':Buffer:Start);
                       //    IF End = 0;
                       //        LEAVE;
                       //    ENDIF;
                       //    Count = Count + 1;
                       //    Vars(Count) = %SUBST(Buffer:Start:End-Start);
                       //    Start = End + 1;
                       //ENDDO;
                       //Count = Count + 1;
                       //Vars(Count) = %SUBST(Buffer:Start);
                       //FOR I = 1 TO Count;
                       //    CLEAR SourceLine;
                       //    SourceLine.Line = Line#;
                       //    SourceLine.spec = 'D';
                       //    SourceLine.varName = ' ' + Vars(i);
                       //    SourceLine.varType = 'S';
                       //    SourceLine.varSize = 4;
                       //    SourceLine.varDataType = 'F';
                       //    WriteLine(SourceLine);
                       //ENDFOR;
                       Buffer = '';

                   // LOGICAL
                   WHEN  %SUBST(Buffer:1:9) = x'05' + 'LOGICAL ';
                       CLEAR Vars;
                       Start = 10;
                       Count = 0;
                       DOW 1=1;
                           End = %SCAN(',':Buffer:Start);
                           IF End = 0;
                               LEAVE;
                           ENDIF;
                           Count = Count + 1;
                           Vars(Count) = %SUBST(Buffer:Start:End-Start);
                           Start = End + 1;
                       ENDDO;
                       Count = Count + 1;
                       Vars(Count) = %SUBST(Buffer:Start);
                       FOR I = 1 TO Count;
                           CLEAR SourceLine;
                           SourceLine.Line = Line#;
                           SourceLine.spec = 'D';
                           SourceLine.varName = ' ' + Vars(i);
                           SourceLine.varType = 'S';
                           SourceLine.varDataType = 'N';
                           SourceLine.varKeywords = 'DIM(100)';
                           WriteLine(SourceLine);
                       ENDFOR;
                       Buffer = '';

                   // DATA(
                   WHEN  %SUBST(Buffer:1:6) = x'05' + 'DATA(';
                       A = %SCAN('(':Buffer:7);
                       Var = %SUBST(Buffer:7:A-7);
                       A = %SCAN('=':Buffer:A+1);
                       B = %SCAN(',':Buffer:A+1);
                       C = %SCAN(')':Buffer:B+1);
                       Start = %DEC(%SUBST(Buffer:a+1:b-a-1):5:0);
                       End = %DEC(%SUBST(Buffer:b+1:c-b-1):5:0);
                       A = %SCAN('/':Buffer:C+1);
                       FOR i = Start TO End;
                           B = %SCAN(',':Buffer:A+1);
                           IF B = 0;
                               B = %SCAN('/':Buffer:A+1);
                           ENDIF;
                           CLEAR SourceLine;
                           SourceLine.Line = Line#;
                           SourceLine.spec = 'C';
                           SourceLine.opcode = 'EVAL';
                           SourceLine.ExtFactor2 = %TRIM(Var) + '(' + %CHAR(i)
                                       + ') = ' + %SUBST(Buffer:A+1:B-A-1);
                           WriteLine(SourceLine);
                           A = B;
                       ENDFOR;
                       Buffer = '';

                   // just ignore these commands
                   WHEN %SUBST(Buffer:1:10) = x'05' + 'IMPLICIT ' OR
                        %SUBST(Buffer:1:8) = x'05' + 'COMMON ';
                       Buffer = '';

                   // catch untranslated codes so that just a tab will
                   // become an EVAL
                   WHEN %SUBST(Buffer:1:6) = x'05' + 'READ(' OR
                        %SUBST(Buffer:1:7) = x'05' + 'WRITE(' OR
                        %SUBST(Buffer:1:6) = x'05' + 'DATA ' OR
                        %SUBST(Buffer:1:8) = x'05' + 'FORMAT(' OR
                        %SUBST(Buffer:1:8) = x'05' + 'ACCEPT ' OR
                        %SUBST(Buffer:1:6) = x'05' + 'TYPE ';
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = '==== ' + Buffer;
                       WriteLine(SourceLine);
                       Buffer = '';

                   // tab - must be an EVAL
                   WHEN %SUBST(Buffer:1:1) = x'05';

                       CLEAR SourceLine;
                       SourceLine.Line = line#;

                       IF SkipFunc;
                           SourceLine.Comment = '*';
                           SourceLine.Text = '==== ' + Buffer;
                           WriteLine(SourceLine);
                       ELSE;
                           SourceLine.spec = 'C';
                           Statement = %SUBST(Buffer:2);
                           SourceLine.opcode = 'EVAL';
                           CheckForFunctions(Statement);
                           SplitStatement(Statement);
                       ENDIF;

                       Buffer = '';
                       EXSR CheckForEnd;

                   // unknown
                   OTHER;
                       CLEAR SourceLine;
                       SourceLine.Line = line#;
                       SourceLine.Comment = '*';
                       SourceLine.Text = '???? ' + Buffer;
                       WriteLine(SourceLine);
                       Buffer = '';

               ENDSL;

               BufferLen = %LEN(%TRIMR(Buffer));
           ENDDO;

       ENDDO;

       fClose(Stream);






       FileName = '/home/brian/adventure/77-03-31_adventure.dat';

       Options = 'r, crln=N';
       Stream = fopen(FileName:Options);

       CLEAR SourceLine;
       SourceLine.Entry = '**';
       WriteLine(SourceLine:3);

       Count = 0;
       Success = fgets(%ADDR(Buffer):%SIZE(Buffer):Stream);
       DOW Success <> *NULL;

           BufferLen = %SCAN(x'2500':Buffer) - 1;
           IF BufferLen <> 0;
               Buffer = %XLATE(x'05':' ':Buffer);
           ENDIF;

           IF BufferLen > 0;
               CLEAR SourceLine;
               SourceLine.Entry = %SUBST(Buffer:1:BufferLen);
               WriteLine(SourceLine:3);
               Count += 1;
           ENDIF;

           Success = fgets(%ADDR(Buffer):%SIZE(Buffer):Stream);
       ENDDO;

       CLEAR SourceLine;
       SourceLine.spec = 'D';
       SourceLine.varType = 'S';
       SourceLine.varName = ' DATA';
       SourceLine.varSize = 80;
       SourceLine.varDataType = 'A';
       SourceLine.varKeywords = 'CTDATA PERRCD(1) DIM('
                               + %CHAR(Count) + ')';
       WriteLine(SourceLine:2);

       fClose(Stream);



       CLOSE QRPGSRC1;
       CLOSE QRPGSRC2;
       CLOSE QRPGSRC3;

       system('CPYSRCF FROMFILE(EMPBJG/QRPGSRC) '
            + 'TOFILE(EMPBJG/QRPGSRC) '
            + 'FROMMBR(ADVENT2) TOMBR(ADVENT) MBROPT(*REPLACE)');
       system('CPYSRCF FROMFILE(EMPBJG/QRPGSRC) '
            + 'TOFILE(EMPBJG/QRPGSRC) '
            + 'FROMMBR(ADVENTA) TOMBR(ADVENT) MBROPT(*ADD)');
       system('CPYSRCF FROMFILE(EMPBJG/QRPGSRC) '
            + 'TOFILE(EMPBJG/QRPGSRC) '
            + 'FROMMBR(ADVENT1) TOMBR(ADVENT) MBROPT(*ADD)');
       system('CPYSRCF FROMFILE(EMPBJG/QRPGSRC) '
            + 'TOFILE(EMPBJG/QRPGSRC) '
            + 'FROMMBR(ADVENTB) TOMBR(ADVENT) MBROPT(*ADD)');
       system('CPYSRCF FROMFILE(EMPBJG/QRPGSRC) '
            + 'TOFILE(EMPBJG/QRPGSRC) '
            + 'FROMMBR(ADVENT3) TOMBR(ADVENT) MBROPT(*ADD)');



       RETURN;




       BEGSR CheckForEnd;

           IF EndifNeeded;
               CLEAR SourceLine;
               SourceLine.Line = line#;
               SourceLine.spec = 'C';
               SourceLine.opcode = 'ENDIF';
               WriteLine(SourceLine);
               EndifNeeded = *OFF;
           ENDIF;
           IF EndforNeeded AND EndforTagFound;
               CLEAR SourceLine;
               SourceLine.Line = line#;
               SourceLine.spec = 'C';
               SourceLine.opcode = 'ENDFOR';
               WriteLine(SourceLine);
               EndforNeeded = *OFF;
           ENDIF;

       ENDSR;



      //------------------------------------------------------------------------
     P ReadRecord      B
      //------------------------------------------------------------------------
     D ReadRecord      PI              N
     D  Stream                         *   CONST
     D  Record                    10000A
     D  RecordLen                    10I 0
     D  Record#                       5S 0

     D Buffer          S          10000A   STATIC
     D BufferLen       S             10I 0
     D BufferFilled    S               N   STATIC
     D LastSuccess     S               *
     D Success         S               *
     D Line#           S              5S 0 STATIC


       // Line# points to the line number of the input file
       // For continuation lines, only the first line# is returned

       CLEAR Record;
       CLEAR RecordLen;

       IF NOT(BufferFilled);
           CLEAR Buffer;
           Success = fgets(%ADDR(Buffer):%SIZE(Buffer):Stream);
           IF Success = *NULL;
               RETURN FALSE;
           ENDIF;
       ELSE;
           // Success is a pointer and we want it to be anything but *NULL
           Success = %ADDR(Line#);
       ENDIF;
       //BufferLen = %SCAN(x'0D2500':Buffer) - 1;
       BufferLen = %SCAN(x'2500':Buffer) - 1;
       IF BufferLen = -1;
           Record = %TRIMR(Record);
       ELSE;
           Record = %TRIMR(Record) + %SUBST(Buffer:1:BufferLen);
       ENDIF;

       Line# = Line# + 1;
       Record# = Line#;

       // Check for continuation records and append them to the main one
       DOW TRUE;
           LastSuccess = Success;
           Success = fgets(%ADDR(Buffer):%SIZE(Buffer):Stream);
           IF Success = *NULL;
               CLEAR Buffer;
               BufferFilled = FALSE;
               Success = LastSuccess;
               LEAVE;
           ENDIF;
           // adjust for some lines not starting with tab
           IF %SUBST(Buffer:1:6) = '      ' AND
              %SUBST(Buffer:7:1) >= '1' AND
              %SUBST(Buffer:7:1) <= '9' AND
              (%SUBST(Buffer:8:1) = x'05' OR
               %SUBST(Buffer:8:1) = ' ');
               Buffer = x'05' + %SUBST(Buffer:7);
           ENDIF;
           IF %SUBST(Buffer:1:1) = x'05' AND
              %SUBST(Buffer:2:1) >= '1' AND
              %SUBST(Buffer:2:1) <= '9' AND
              (%SUBST(Buffer:3:1) = x'05' OR
               %SUBST(Buffer:3:1) = ' ');
               //BufferLen = %SCAN(x'0D2500':Buffer) - 1;
               BufferLen = %SCAN(x'2500':Buffer) - 1;
               Record = %TRIMR(Record) + %SUBST(Buffer:4:BufferLen-3);
               Line# = Line# + 1;
               LastSuccess = Success;
           ELSE;
               BufferFilled = TRUE;
               Success = LastSuccess;
               LEAVE;
           ENDIF;
       ENDDO;

       RecordLen = %LEN(%TRIMR(Record));

       RETURN TRUE;

     P ReadRecord      E


      //------------------------------------------------------------------------
     P WriteLine       B
      //------------------------------------------------------------------------
     D WriteLine       PI
     D  SourceData                         LIKE(SrcDta) CONST
     D  pFile                        10I 0 CONST OPTIONS(*NOPASS)

     D File            S             10I 0


       IF %PARMS > 1;
           File = pFile;
       ELSE;
           File = 1;
       ENDIF;

       SrcDta = SourceData;
       SELECT;
           WHEN File = 1;
               SrcSeq1 += 1;
               SrcSeq = SrcSeq1;
               WRITE RPGSRC1;
           WHEN File = 2;
               SrcSeq2 += 1;
               SrcSeq = SrcSeq2;
               WRITE RPGSRC2;
           WHEN File = 3;
               SrcSeq3 += 1;
               SrcSeq = SrcSeq3;
               WRITE RPGSRC3;
       ENDSL;

       RETURN;

     P WriteLine       E


      //------------------------------------------------------------------------
     P FullReplace     B
      //------------------------------------------------------------------------
     D FullReplace     PI
     D  OldCode                       5A   CONST
     D  NewCode                       6A   CONST
     D  NewLen                       10I 0 CONST
     D  Statement                   512A

     D Len             S             10I 0
     D Start           S             10I 0


       Len = %LEN(%TRIMR(OldCode));
       Start = %SCAN(%TRIM(OldCode):Statement);
       DOW Start <> 0;
           Statement = %REPLACE(%SUBST(NewCode:1:NewLen):Statement:Start:Len);
           Start = %SCAN(%TRIM(OldCode):Statement);
       ENDDO;

       RETURN;

     P FullReplace     E

      //------------------------------------------------------------------------
     P CheckForFunctions...
     P                 B
      //------------------------------------------------------------------------
     D CheckForFunctions...
     D                 PI
     D  Statement                   512A

     D Comma           S             10I 0
     D Pos             S             10I 0


       Pos = %SCAN('MOD(':Statement) + 4;
       IF Pos > 0;
           Comma = %SCAN(',':Statement:Pos);
           DOW Comma <> 0;
               Statement = %REPLACE(':':Statement:Comma:1);
               Comma = %SCAN(',':Statement:Pos);
           ENDDO;
       ENDIF;

       RETURN;

     P CheckForFunctions...
     P                 E

      //------------------------------------------------------------------------
     P SplitStatement  B
      //------------------------------------------------------------------------
     D SplitStatement  PI
     D  Statement                   512A


       DOU Statement = '';
           IF %LEN(%TRIMR(Statement)) > %SIZE(SourceLine.ExtFactor2);
               FOR i = %SIZE(SourceLine.ExtFactor2) DOWNTO 1;
                   IF %SUBST(Statement:i:1) = *BLANK OR
                           %SUBST(Statement:i:1) = ':' OR
                           %SUBST(Statement:i:1) = '+' OR
                           %SUBST(Statement:i:1) = '-' OR
                           %SUBST(Statement:i:1) = '*' OR
                           %SUBST(Statement:i:1) = '/';
                       SourceLine.ExtFactor2 = %SUBST(Statement:1:i-1);
                       WriteLine(SourceLine);
                       SourceLine.opcode = '';
                       Statement = %SUBST(Statement:i);
                       LEAVE;
                   ENDIF;
               ENDFOR;
           ELSE;
               SourceLine.ExtFactor2 = Statement;
               WriteLine(SourceLine);
               Statement = '';
           ENDIF;
       ENDDO;

       RETURN;

     P SplitStatement  E
