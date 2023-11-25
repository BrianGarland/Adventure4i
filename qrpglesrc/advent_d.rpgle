      //
      // The UI portion of a Colossal Cave Adventure
      //
      // Copyright 2023, Brian J Garland
      //

     H NOMAIN

     FADVENTFM  CF   E             WORKSTN  INFDS(DSPFDS) USROPN

     D #EXIT           C                   x'33'                                F3
     D #TOP            C                   x'B5'                                F17
     D #BOTTOM         C                   x'B6'                                F18
     D #PAGEUP         C                   X'F4'                                Page Up
     D #PAGEDOWN       C                   X'F5'                                Page Down

     D DSPFDS          DS
     D  FKey                          1A   OVERLAY(DSPFDS:369)

     D LineDS          DS
     D  Line01
     D  Line02
     D  Line03
     D  Line04
     D  Line05
     D  Line06
     D  Line07
     D  Line08
     D  Line09
     D  Line10
     D  Line11
     D  Line12
     D  Line13
     D  Line14
     D  Line15
     D  Line16
     D  Line17
     D  Line18
     D  Line19
     D  Line20
     D  Line21
     D  Line22
     D  Line23
     D  Line24
     D  Line                               LIKE(Line01) DIM(24)
     D                                     OVERLAY(LineDS:1)

     D CurrentLine     S              5I 0
     D History         S                   LIKE(Line01) DIM(10000)
     D HistoryLines    S              5U 0 INZ(0)



     P TYPE            B
     D TYPE            PI
     D  Message                    1024A   CONST

     D Buffer          S                   LIKE(LINE01)
     D BufferSize      S             10I 0 INZ(%SIZE(LINE01))
     D End             S             10I 0
     D WorkMsg         S           1024A   VARYING

       IF NOT(%OPEN(ADVENTFM));
           OPEN ADVENTFM;
       ENDIF;

       WorkMsg = %TRIM(Message);
       DOW WorkMsg <> *BLANKS;
           SELECT;
           WHEN %LEN(WorkMsg) <= BufferSize;
               Buffer = WorkMsg;
           WHEN %LEN(WorkMsg) > BufferSize
               AND (%SUBST(WorkMsg:BufferSize:1) = *BLANK
                    OR %SUBST(WorkMsg:BufferSize+1:1) = *BLANK);
               Buffer = WorkMsg;
               WorkMsg = %TRIM(%SUBST(WorkMsg:BufferSize+1));
           OTHER;
               End = BufferSize;
               DOW %SUBST(Buffer:End:1) <> *BLANK;
                   End -= 1;
               ENDDO;
               Buffer = %SUBST(WorkMsg:1:End);
               WorkMsg = %TRIM(%SUBST(WorkMsg:End+1));
           ENDSL;
           HistoryLines += 1;
           History(HistoryLines) = Buffer;
       ENDDO;

       RETURN;

     P TYPE            E



     P ACCEPT          B
     D ACCEPT          PI            80A

       IF NOT(%OPEN(ADVENTFM));
           OPEN ADVENTFM;
       ENDIF;

       Input = *BLANKS;

       DOW 1=1;

           WRITE S1;
           EXFMT S2;

           SELECT;
           WHEN FKey = #EXIT;
               HistoryLines += 1;
               History(HistoryLines) = '> QUIT';
               LEAVE;

           WHEN FKey = #TOP;
               CurrentLine = 1;

           WHEN FKey = #BOTTOM;
               FindBottom();

           WHEN FKey = #PAGEUP;
               IF CurrentLine > %ELEM(Line);
                   CurrentLine -= %ELEM(Line);
               ELSE;
                   CurrentLine = 1;
               ENDIF;

           WHEN FKey = #PAGEDOWN;
               IF (CurrentLine + %ELEM(Line)) <= HistoryLines;
                   CurrentLine += %ELEM(Line);
               ENDIF;

           OTHER;
               HistoryLines += 1;
               History(HistoryLines) = '> ' + Input;
               LEAVE;

           ENDSL;

       ENDDO;

       RETURN Input;

     P ACCEPT          E




      //------------------------------------------------------------------------
     P FindBottom      B
      //------------------------------------------------------------------------
     D FindBottom      PI

      /FREE

       IF HistoryLines > %ELEM(Line) - 2;
           CurrentLine = HistoryLines - (%ELEM(Line) - 2) + 1;
       ELSE;
           CurrentLine = 1;
       ENDIF;

       RETURN;

      /END-FREE

     P FindBottom      E