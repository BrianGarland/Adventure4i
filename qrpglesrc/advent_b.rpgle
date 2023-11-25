**FREE


       // MOD -- return the remainder of a division
       DCL-PROC MOD;
           DCL-PI *N INT(10);
               A INT(10) CONST;
               B INT(10) CONST;
           END-PI;
           RETURN %REM(A:B);
       END-PROC;



       // RAN -- return a random number
       DCL-PROC RAN;
           DCL-PI *N FLOAT(4);
               A FLOAT(4) CONST;
           END-PI;
           DCL-PR GetPRN EXTPROC('Qc3GenPRNs');
               Result    POINTER VALUE;
               ResultLen INT(10) CONST;
               Type      CHAR(1) CONST;
               Parity    CHAR(1) CONST;
               ErrorCode CHAR(32767) OPTIONS(*VARSIZE);
           END-PR;
           DCL-DS APIError;
               Provided INT(10) INZ(272);
               Avail    INT(10) INZ(0);
               MsgID    CHAR(7) INZ(*BLANKS);
               Reserved CHAR(1) INZ(*BLANKS);
               MsgDta   CHAR(256) INZ(*BLANKS);
           END-DS;
           DCL-S MaxInt  UNS(10) INZ(*HIVAL);
           DCL-S RandFlt FLOAT(8);
           DCL-S RandInt UNS(10);
           DCL-S Value   ZONED(20:0);
           GetPRN(%ADDR(RandInt):%SIZE(RandInt):'0':'0':APIError);
           RandFlt = RandInt / MaxInt;
           Value = %INT(RandFlt * 2);
           RETURN Value;
       END-PROC;



       // Routine to replace fortran IFILE
       // The parameters are ignored, just there for compatibility with fortran call
       // This routine replaces all the logic between 1002 and 1100
       DCL-PROC IFILE;
           DCL-PI *N;
               Handle INT(10) CONST;
               File   CHAR(80) CONST;
           END-PI;

           DCL-S Array CHAR(80) DIM(12);
           DCL-S EndSection IND INZ(*ON);
           DCL-S I INT(10);
           DCL-S iu INT(10);
           DCL-S Temp ZONED(4:0);
           DCL-S Text CHAR(80);
           DCL-S xikind INT(10);
           DCL-S xjkind INT(10);

           I = 1;

           FOR X = 1 TO %ELEM(Data);
               Array = Split(Data(X):Text);
               Temp = %DEC(Array(1):4:0);

               IF Temp = 0;
                   LEAVE;
               ENDIF;

               IF Temp = -1;
                   EndSection = *ON;
                   ITER;
               ENDIF;

               IF EndSection;
                   EndSection = *OFF;
                   ikind = Temp;
                   IF ikind = 3;
                       I = 1;
                   ENDIF;
                   ITER;
               ENDIF;

               IF ikind in %LIST(1:2:5:6);
                   lline(I) = %TRIM(Text);
               ENDIF;
               jKind = %DEC(Array(1):4:0);

               IF ikind in %LIST(1:6)
                   AND ikind = xikind
                   AND jKind = xjkind;
                   lline(i-1) = %TRIMR(lline(i-1)) + ' ' + %TRIM(Text);
                   xikind = ikind;
                   xjkind = jkind;
                   ITER;
               ENDIF;

               SELECT;
                   WHEN ikind = 1;
                       ltext(jkind) = I;
                       I += 1;
                   WHEN ikind = 2;
                       stext(jkind) = I;
                       I += 1;
                   WHEN ikind = 3;
                       lKind = %DEC(Array(2):4:0);
                       key(jkind) = I;
                       FOR J = 3 TO 12;
                           IF Array(j) = *BLANKS;
                               LEAVE;
                           ENDIF;
                           travel(i) = lkind*1024 + %DEC(Array(J):4:0);
                           I += 1;
                       ENDFOR;
                   WHEN ikind = 4;
                       iu += 1;
                       ktab(iu) = %DEC(Array(1):4:0);
                       atab(iu) = Text;
                   WHEN ikind = 5;
                       IF jkind > 200;
                           btext(jkind-100) = I;
                           btext(jkind-200) = I;
                       ELSE;
                           btext(jkind) = I;
                       ENDIF;
                       I += 1;
                   WHEN ikind = 6;
                       rtext(jkind) = I;
                       I += 1;
               ENDSL;

           ENDFOR;

           RETURN;

       END-PROC;



       // SPLIT -- Split a string on spaces
       DCL-PROC SPLIT;
           DCL-PI *N CHAR(80) DIM(12);
               Input CHAR(100) CONST;
               Text  CHAR(80);
           END-PI;

           DCL-S CurrChar CHAR(1);
           DCL-S End      INT(10);
           DCL-S I        INT(10);
           DCL-S J        INT(10);
           DCL-S LastChar CHAR(1);
           DCL-S Output   CHAR(20) DIM(50);
           DCL-S Start    INT(10) INZ(1);

           FOR I = 1 TO %SIZE(Input);
               CurrChar = %SUBST(Input:I:1);
               IF CurrChar = *BLANK AND LastChar <> *BLANK;
                   End = I - 1;
                   J += 1;
                   Output(J) = %SUBST(Input:Start:End-Start+1);
                   IF J = 1;
                       Text = %TRIM(%SUBST(Input:I));
                   ENDIF;
                   Start = End + 1;
               ENDIF;
               LastChar = CurrChar;
           ENDFOR;

           RETURN Output;

       END-PROC;



       // SPEAK -- Replace the SPEAK function
       DCL-PROC SPEAK;
           DCL-PI *N;
               IT INT(10) CONST;
           END-PI;

           KKT = RTEXT(IT);
           IF KKT = 0;
               RETURN;
           ENDIF;

           MSG =LLINE(KKT);
           DSPLY MSG DUMMY;

           RETURN;

       END-PROC;


