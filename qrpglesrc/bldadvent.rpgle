**FREE
CTL-OPT DFTACTGRP(*NO) BNDDIR('QC2LE');

/INCLUDE "./qrpgleref/system_calls.rpgle"

DCL-DS SourceLine LEN(256) QUALIFIED;
    // Common to all
    line             ZONED(5:0) POS(1);
    spec             CHAR(1)    POS(6);
    // Comment
    comment          CHAR(1)    POS(7);
    // H-spec
    cntlEntry        CHAR(80)   POS(8);
    // P-spec
    functionName     CHAR(15)   POS(7);
    functionBeginEnd CHAR(1)    POS(24);
    // D-spec
    varName          CHAR(15)   POS(7);
    varType          CHAR(2)    POS(24);
    varSize          ZONED(4:0) POS(36);
    varDataType      CHAR(1)    POS(40);
    varDecPos        CHAR(2)    POS(41);
    varKeywords      CHAR(40)   POS(44);
    // C-spec
    factor1          CHAR(14)   POS(12);
    opcode           CHAR(10)   POS(26);
    factor2          CHAR(14)   POS(36);
    extFactor2       CHAR(45)   POS(36);
    result           CHAR(14)   POS(50);
    text             CHAR(80)   POS(9);
    // Table
    entry            CHAR(80)   POS(1);
END-DS;

DCL-S A              INT(10);
DCL-S Advent         INT(10);
DCL-S Advent_A       INT(10);
DCL-S Advent_B       INT(10);
DCL-S Advent_Dat     INT(10);
DCL-S Advent_F       INT(10);
DCL-S Adjusted       IND;
DCL-S B              INT(10);
DCL-S Buffer         CHAR(25000);
DCL-S BufferLen      INT(10);
DCL-S Buffer2        CHAR(25000) CCSID(819);
DCL-S C              INT(10);
DCL-S Count          INT(10);
DCL-S D              INT(10);
DCL-S Dims           CHAR(10) DIM(50);
DCL-S End            INT(10);
DCL-S EndforNeeded   IND;
DCL-S EndforTag      CHAR(14);
DCL-S EndforTagFound IND;
DCL-S EndifNeeded    IND;
DCL-S ErrNo          INT(10) BASED(ErrNoPtr);
DCL-S ErrText        CHAR(200);
DCL-S FileName       VARCHAR(512);
DCL-S FunctionName   CHAR(15);
DCL-S I              INT(10);
DCL-S InFunc         IND;
DCL-S Len            INT(10);
DCL-S Line#          ZONED(5:0);
DCL-S mode           UNS(10);
DCL-S Number         CHAR(10);
DCL-S oflag          INT(10);
DCL-S Paren          INT(10);
DCL-S path           CHAR(512);
DCL-S Pos            INT(10);
DCL-S SkipSection    IND;
DCL-S SourceData     CHAR(256);
DCL-S Start          INT(10);
DCL-S Statement      CHAR(512);
DCL-S Success        INT(10);
DCL-S Tab            INT(10);
DCL-S Tag            CHAR(14);
DCL-S Tags           CHAR(14) DIM(50);
DCL-S Temp           CHAR(4) INZ;
DCL-S TestParm       CHAR(40);
DCL-S Token          CHAR(4) DIM(10) INZ;
DCL-S Type           CHAR(1);
DCL-S TypeCode       CHAR(10);
DCL-S TypeFormat     CHAR(80);
DCL-S TypeVars       CHAR(80);
DCL-S Var            CHAR(14);
DCL-S Vars           CHAR(14) DIM(50);
DCL-S ZEROS          CHAR(5) INZ('00000');



*INLR = *ON;

// Make sure the old fortran code has the correct CCSID
system('CHGATR OBJ(''OriginalSource/77-03-31_adventure.f'') ATR(*CCSID) VALUE(819)');
system('CHGATR OBJ(''OriginalSource/77-03-31_adventure.dat'') ATR(*CCSID) VALUE(819)');

// Make sure the prewritten RPGLE code has the correct CCSID
system('CHGATR OBJ(''qrpglesrc/advent_a.rpgle'') ATR(*CCSID) VALUE(819)');
system('CHGATR OBJ(''qrpglesrc/advent_b.rpgle'') ATR(*CCSID) VALUE(819)');



// Open output file
path = 'qrpglesrc/advent.rpgle';
oflag = O_WRONLY + O_CREAT + O_CCSID + O_TEXTDATA + O_TEXT_CREAT;
mode = S_IRUSR + S_IWUSR + S_IXUSR;
Advent = open(%TRIMR(path):oflag:mode:ASCII:ASCII);
IF Advent = -1;
    ErrNoPtr = errorifs();
    ErrText = %STR(strerror(ErrNo));    
ENDIF;



// Write the header
path = 'qrpglesrc/advent_a.rpgle';
oflag = O_RDONLY + O_TEXTDATA;
Advent_A = open(%TRIMR(path):oflag);
IF Advent_A = -1;
    ErrNoPtr = errorifs();
    ErrText = %STR(strerror(ErrNo));    
ENDIF;

DOW TRUE;
    Success = read(Advent_A:%ADDR(Buffer):%SIZE(Buffer));
    SELECT;
        WHEN Success = -1;
            ErrNoPtr = errorifs();
            ErrText = %STR(strerror(ErrNo));    
        WHEN Success = 0;
            LEAVE;
        OTHER;
            Buffer2 = Buffer;
            CALLP write(Advent:%ADDR(Buffer2):Success);
    ENDSL;
ENDDO;

CALLP close(Advent_A);



// Convert the fortran source code to RPGLE and write it out
path = 'OriginalSource/77-03-31_adventure.f';
oflag = O_RDONLY + O_TEXTDATA;
Advent_F = open(%TRIMR(path):oflag);
IF Advent_F = -1;
    ErrNoPtr = errorifs();
    ErrText = %STR(strerror(ErrNo));    
ENDIF;

DOW ReadRecord(Advent_F:Buffer:BufferLen:Line#);

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

            // Adjust buffer for LLINE
            WHEN (%SUBST(Buffer:1:11) <> x'05' + 'DIMENSION '
                            AND %SCAN('LLINE(':Buffer) > 0)
                            AND NOT(Adjusted);
                A = %SCAN('LLINE(':Buffer);
                B = %SCAN(',':Buffer:A);
                C = %SCAN(')':Buffer:B);
                D = %SCAN('0':Buffer:C);
                IF D = 0;
                    Buffer = %SUBST(Buffer:1:B-1)
                           + %SUBST(Buffer:C);
                ELSE;
                    Buffer = %SUBST(Buffer:1:B-1)
                           + %SUBST(Buffer:C:D-C)
                           + '''''' 
                           + %SUBST(Buffer:D+1);
                ENDIF;           
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
                    Number = %SUBST(Buffer:1:tab-1);
                    IF SkipSection AND Number <> '1100';
                        SourceLine.Comment = '*';
                        SourceLine.Text = '==== ' + Buffer;
                        WriteLine(SourceLine);
                    ELSE;
                        tag = 'TAG' + Number;
                        CLEAR SourceLine;
                        SourceLine.Line = line#;
                        SourceLine.spec = 'C';
                        SourceLine.factor1 = tag;
                        SourceLine.opcode = 'TAG';
                        WriteLine(SourceLine);
                    ENDIF;    
                    IF Number = '1002';
                        // start reading .dat file
                        SkipSection = TRUE;
                    ENDIF;
                    IF Number = '1100';
                        // done reading .dat file
                        SkipSection = FALSE;
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

            // In the setup routine - skip all these lines as they are handled in IFILE
            // In a subroutine that we are skipping
            WHEN SkipSection;
                SourceLine.Line = line#;
                SourceLine.Comment = '*';
                SourceLine.Text = '==== ' + Buffer;
                WriteLine(SourceLine);
                Buffer = '';
                IF %SUBST(Buffer:1:5) = x'05' + 'END ';
                    SkipSection = FALSE;
                ENDIF;

            // IF 
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
                                                                     END-Start);
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
                                                                     END-Start);
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
                                                                     END-Start);
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

            // CALL
            WHEN %SUBST(Buffer:1:6) = x'05' + 'CALL ';
                CLEAR SourceLine;
                SourceLine.Line = line#;
                SourceLine.spec = 'C';
                SourceLine.opcode = 'CALLP';
                Statement = %SUBST(Buffer:7);
                FullReplace(',':':':1:Statement);
                SplitStatement(Statement);
                Buffer = '';
                EXSR CheckForEnd;

            // GOTO( or GOTO (
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
                                                                     END-Start);
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

            // GOTO
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

            // GO TO 
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
                IF SkipSection;
                    SourceLine.Comment = '*';
                    SourceLine.Text = '==== ' + Buffer;
                    WriteLine(SourceLine);
                ELSE;
                    Start = 5;
                    End = %SCAN(' ':Buffer:Start);
                    EndforNeeded = *ON;
                    EndforTag = 'TAG' + %SUBST(Buffer:Start:END-Start);
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
                        Vars(Count) = %SUBST(Buffer:Start:END-Start);
                        Start = End + 1;
                    ENDDO;
                    Count = Count + 1;
                    End = %SCAN(')':Buffer:Start);
                    Vars(Count) = %SUBST(Buffer:Start:END-Start);
                ELSE;
                    FunctionName = %SUBST(Buffer:13);
                ENDIF;
                // All subroutines are rewritten by hand
                IF (FunctionName = 'SPEAK'
                        OR FunctionName = 'GETIN'
                        OR FunctionName = 'YES'
                        OR FunctionName = 'SHIFT');
                    SkipSection = TRUE;
                    SourceLine.Line = line#;
                    SourceLine.Comment = '*';
                    SourceLine.Text = '==== ' + Buffer;
                    WriteLine(SourceLine);
                    Buffer = '';
                ELSE;
                    SkipSection = FALSE;
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
                ENDIF;    
                Buffer = '';
                EXSR CheckForEnd;
                InFunc = TRUE;


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
                    SkipSection = FALSE;
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
                SourceLine.opcode = 'CALLP';
                SourceLine.ExtFactor2 = 'TYPE(MSG)';
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
                    Vars(Count) = %SUBST(Buffer:Start:END-Start);
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
                //    Vars(Count) = %SUBST(Buffer:Start:END-Start);
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
                    Vars(Count) = %SUBST(Buffer:Start:END-Start);
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

            // TYPE
            WHEN %SUBST(Buffer:1:6) = x'05' + 'TYPE ';
                CLEAR SourceLine;
                SourceLine.Line = line#;
                SourceLine.Comment = '*';
                SourceLine.Text = '==== ' + Buffer;
                WriteLine(SourceLine);
                TypeVars = %SUBST(Buffer:7);
                TypeCode = '';
                Pos = %SCAN(',':TypeVars);
                IF Pos > 0;
                    TypeCode = %SUBST(TypeVars:1:Pos-1);
                    TypeVars = %SUBST(TypeVars:Pos+1);
                ENDIF;  
                Buffer = '';
                  
            // FORMAT       
            WHEN %SUBST(Buffer:1:8) = x'05' + 'FORMAT(';
                IF %TRIM(Number) = %TRIM(TypeCode);
                    CLEAR SourceLine;                
                    SourceLine.Comment = '*';
                    SourceLine.Text = '==== ' + Buffer;
                    WriteLine(SourceLine);
                    SourceLine.Comment = '*';
                    SourceLine.Text = '==== TypeCode = ' + TypeCode
                                    + ', TypeVars = ' + TypeVars;
                    WriteLine(SourceLine);
                    TypeFormat = %SUBST(Buffer:8);

                    // TODO:   actually write the code! 

                    IF %SUBST(TypeFormat:2:1) = '''';
                        // some kind of constant
                    ELSE;
                    ENDIF;

                    TypeFormat = *BLANKS;
                    TypeCode = *BLANKS;
                    TypeVars = *BLANKS;
                ENDIF;
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
                        %SUBST(Buffer:1:8) = x'05' + 'ACCEPT ';
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

                IF SkipSection;
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

CALLP close(Advent_F);



// Write the procedures
path = 'qrpglesrc/advent_b.rpgle';
oflag = O_RDONLY + O_TEXTDATA;
Advent_B = open(%TRIMR(path):oflag);
IF Advent_B = -1;
    ErrNoPtr = errorifs();
    ErrText = %STR(strerror(ErrNo));    
ENDIF;

DOW TRUE;
    Success = read(Advent_B:%ADDR(Buffer):%SIZE(Buffer));
    SELECT;
        WHEN Success = -1;
            ErrNoPtr = errorifs();
            ErrText = %STR(strerror(ErrNo));    
        WHEN Success = 0;
            LEAVE;
        OTHER;
            Buffer2 = Buffer;
            CALLP write(Advent:%ADDR(Buffer2):Success);
    ENDSL;
ENDDO;

CALLP close(Advent_B);



// Write the map data
path = 'OriginalSource/77-03-31_adventure.dat';
oflag = O_RDONLY + O_TEXTDATA;
Advent_Dat = open(%TRIMR(path):oflag);

Count = 0;
CLEAR SourceLine;
SourceLine.Entry = '**';
WriteLine(SourceLine);

DOW read(Advent_Dat:%ADDR(Buffer):%SIZE(Buffer)) > 0;

    Buffer = %XLATE(x'05':' ':Buffer);
    BufferLen = %SCAN(x'0D25':Buffer);

    DOW BufferLen > 0;
        CLEAR SourceLine;
        SourceLine.Entry = %SUBST(Buffer:1:BufferLen-1);
        IF SourceLine.Entry <> *BLANKS;
            WriteLine(SourceLine);
            Count += 1;
        ENDIF;
        Buffer = %SUBST(Buffer:BufferLen+2);
        BufferLen = %SCAN(x'0D25':Buffer);
    ENDDO;

ENDDO;
CALLP close(Advent_Dat);



// Close the created source file
CALLP close(Advent);



// Trigger a compile
system('CRTBNDRPG PGM(ADVENT) SRCSTMF(''qrpglesrc/advent.rpgle'') DBGVIEW(*ALL)');



RETURN;




//------------------------------------------------------------------------
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
DCL-PROC ReadRecord;
    DCL-PI ReadRecord IND;
        Stream    INT(10) CONST;
        Record    CHAR(10000);
        RecordLen INT(10);
        Record#   ZONED(5:0);
    END-PI;

    DCL-S Buffer       CHAR(20000) STATIC;
    DCL-S BufferLen    INT(10) STATIC;
    DCL-S LastSuccess  INT(10);
    DCL-S LineBreak    INT(10);
    DCL-S Success      INT(10);
    DCL-S Line#        ZONED(5:0) STATIC;


    // Line# points to the line number of the input file
    // For continuation lines, only the first line# is returned

    // Clear the return value
    CLEAR Record;
    CLEAR RecordLen;

    // If the buffer is empty, read a record
    IF BufferLen = 0;
        Success = read(Stream:%ADDR(Buffer):%SIZE(Buffer));
        IF Success = -1;
            ErrNoPtr = errorifs();
            ErrText = %STR(strerror(ErrNo));    
        ENDIF;    
        BufferLen = Success;
        LineBreak = %SCAN(x'0D25':Buffer); 
    ENDIF;

    // Make sure the buffer is fill
    IF BufferLen > 0 AND BufferLen < %SIZE(Buffer);
        LineBreak = %SCAN(x'0D25':Buffer); 
        // Skip blank lines
        DOW LineBreak = 1;
            Buffer = %SUBST(Buffer:3);
            BufferLen -= 2;
            LineBreak = %SCAN(x'0D25':Buffer); 
        ENDDO;
        DOW LineBreak = 0 AND Success > 0;
            Success = read(Stream:%ADDR(Buffer)+BufferLen:%SIZE(Buffer)-BufferLen);
            IF Success = -1;
                ErrNoPtr = errorifs();
                ErrText = %STR(strerror(ErrNo));    
            ELSEIF Success > 0;
                BufferLen += Success;
                LineBreak = %SCAN(x'0D25':Buffer); 
            ENDIF;    
        ENDDO;
    ENDIF;

    // Extract the first record from the buffer
    IF LineBreak < 1;
        Record = %TRIMR(Record);
    ELSE;
        Record = %TRIMR(Record) + %SUBST(Buffer:1:LineBreak-1);
    ENDIF;
    Line# = Line# + 1;
    Record# = Line#;
    // Adjust the buffer
    Buffer = %SUBST(Buffer:LineBreak+2);
    BufferLen -= (LineBreak + 1);

    // Check for continuation line and append them to the main one
    DOW TRUE;
        IF %SUBST(Buffer:1:6) = '      ' 
            AND %SUBST(Buffer:7:1) >= '1' 
            AND %SUBST(Buffer:7:1) <= '9' 
            AND (%SUBST(Buffer:8:1) = x'05' 
                 OR %SUBST(Buffer:8:1) = ' ');
            // If line starts with 6 spaces, conver that to a tab
            // to simplify the next test
            Buffer = x'05' + %SUBST(Buffer:7);
            BufferLen -= 5;
        ENDIF;
        IF %SUBST(Buffer:1:1) = x'05' 
            AND %SUBST(Buffer:2:1) >= '1' 
            AND %SUBST(Buffer:2:1) <= '9' 
            AND (%SUBST(Buffer:3:1) = x'05' 
                 OR %SUBST(Buffer:3:1) = ' ');
            // We have a continuation line
            LineBreak = %SCAN(x'0D25':Buffer);
            Record = %TRIMR(Record) + %SUBST(Buffer:4:LineBreak-4);
            Line# = Line# + 1;
            // Adjust the buffer
            Buffer = %SUBST(Buffer:LineBreak+2);
            BufferLen -= (LineBreak + 1);
            ITER;
        ELSE;
            LEAVE;    
        ENDIF;
    ENDDO;

    RecordLen = %LEN(%TRIMR(Record));

    RETURN (RecordLen > 0);

END-PROC;


//------------------------------------------------------------------------
DCL-PROC WriteLine;
    DCL-PI WriteLine;
        SourceData CHAR(256) VALUE;
    END-PI;

    DCL-S AsciiText CHAR(256) CCSID(819);

    SourceData = %TRIMR(SourceData) + CR + LF;
    AsciiText = SourceData;
    CALLP write(Advent:%ADDR(AsciiText):%LEN(%TRIMR(AsciiText)));

    RETURN;

END-PROC;


//------------------------------------------------------------------------
DCL-PROC FullReplace;
    DCL-PI FullReplace;
        OldCode   CHAR(5) CONST;
        NewCode   CHAR(6) CONST;
        NewLen    INT(10) CONST;
        Statement CHAR(512);
    END-PI;

    DCL-S Len   INT(10);
    DCL-S Start INT(10);


    Len = %LEN(%TRIMR(OldCode));
    Start = %SCAN(%TRIM(OldCode):Statement);
    DOW Start <> 0;
        Statement = %REPLACE(%SUBST(NewCode:1:NewLen):Statement:Start:Len);
        Start = %SCAN(%TRIM(OldCode):Statement);
    ENDDO;

    RETURN;

END-PROC;



//------------------------------------------------------------------------
DCL-PROC CheckForFunctions;
    DCL-PI CheckForFunctions;
        Statement CHAR(512);
    END-PI;

    DCL-S Comma INT(10);
    DCL-S Pos   INT(10);


    Pos = %SCAN('MOD(':Statement) + 4;
    IF Pos > 0;
        Comma = %SCAN(',':Statement:Pos);
        DOW Comma <> 0;
            Statement = %REPLACE(':':Statement:Comma:1);
            Comma = %SCAN(',':Statement:Pos);
        ENDDO;
    ENDIF;

    RETURN;

END-PROC;



//------------------------------------------------------------------------
DCL-PROC SplitStatement;
    DCL-PI SplitStatement;
        Statement CHAR(512);
    END-PI;


    DOU Statement = '';
        IF %LEN(%TRIMR(Statement)) > %SIZE(SourceLine.ExtFactor2);
            FOR i = %SIZE(SourceLine.ExtFactor2) DOWNTO 1;
                IF %SUBST(Statement:i:1) = *BLANK 
                    OR %SUBST(Statement:i:1) = ':'
                    OR %SUBST(Statement:i:1) = '+' 
                    OR %SUBST(Statement:i:1) = '-' 
                    OR %SUBST(Statement:i:1) = '*' 
                    OR %SUBST(Statement:i:1) = '/';
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

END-PROC;

