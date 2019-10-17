      ******************************************************************
      *  opencobol SAMPLE
      *
      *  Copyright 2019 Tokyo System House Co., Ltd.
      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 ZINKOMITSUDO2.
       AUTHOR.                     TSH.
       DATE-WRITTEN.               2019-10-10.
      ******************************************************************
       ENVIRONMENT                 DIVISION.
      ******************************************************************
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT ZINKO-FILE     ASSIGN TO "ZINKO-SEQ2"
                                   ORGANIZATION   IS   SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS.
           SELECT MENSEKI-FILE   ASSIGN TO "MENSEKI-SEQ2"
                                   ORGANIZATION   IS   SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS2.
           SELECT RESULT-FILE      ASSIGN TO "RESULT-SEQ"
                                   ORGANIZATION   IS   SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS3.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       FILE                        SECTION.
       FD  ZINKO-FILE.
       01  ZINKO-REC.
           03   Z-CODE           PIC  X(07).
           03   Z-NAME           PIC  N(04).
           03   Z-ZINKO          PIC  9(08).
       FD  MENSEKI-FILE.
       01  MENSEKI-REC.
           03   M-CODE           PIC  X(07).
           03   M-NAME           PIC  N(04).
           03   M-MENSEKI        PIC  9(05).
       FD  RESULT-FILE.
       01  RESULT-REC.
           03   R-CODE           PIC  X(07).
           03   R-NAME           PIC  N(04).
           03   R-ZINKO          PIC  9(08).
           03   R-MENSEKI        PIC  9(05).
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  F-STATUS                PIC  XX.
       01  F-STATUS2               PIC  XX.
       01  F-STATUS3               PIC  XX.
       01  S-STATUS                PIC  XX.
       01  ZIKOMITSUDO-DATA        PIC  9(08)V9(03).
       01  CNT                     PIC  9.
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           OPEN INPUT  ZINKO-FILE.
           OPEN OUTPUT RESULT-FILE.

           MOVE "00" TO F-STATUS.

           PERFORM UNTIL F-STATUS <> "00"
              READ ZINKO-FILE NEXT
              IF F-STATUS = "10" THEN
                 NEXT SENTENCE
              ELSE IF  F-STATUS <> "00" THEN
                 DISPLAY "READ ERROR:" F-STATUS
                 MOVE -1 TO RETURN-CODE
                 GOBACK
              END-IF

              OPEN INPUT  MENSEKI-FILE
              MOVE "00" TO F-STATUS2
              PERFORM UNTIL F-STATUS2 <> "00"
                 READ MENSEKI-FILE NEXT
                 IF F-STATUS2 = "10" THEN
                    DISPLAY "NOT FOUND:" Z-CODE
                    MOVE -1 TO RETURN-CODE
                    GOBACK
                 ELSE IF F-STATUS2 <> "00" THEN
                    DISPLAY "READ ERROR:" F-STATUS2
                    MOVE -1 TO RETURN-CODE
                    GOBACK
                 END-IF
                 END-IF
                 IF Z-CODE = M-CODE THEN
                    EXIT PERFORM
                 END-IF
              END-PERFORM
              CLOSE MENSEKI-FILE

              MOVE Z-CODE TO R-CODE
              MOVE Z-NAME TO R-NAME
              MOVE Z-ZINKO TO R-ZINKO
              MOVE M-MENSEKI TO R-MENSEKI

              WRITE RESULT-REC
           END-PERFORM.
           
           CLOSE ZINKO-FILE.
           CLOSE RESULT-FILE.
           
           OPEN INPUT RESULT-FILE.
           MOVE "00" TO F-STATUS3.

           PERFORM UNTIL F-STATUS3 <> "00"
              READ RESULT-FILE NEXT
              IF F-STATUS3 = "10" THEN
                 NEXT SENTENCE
              ELSE IF  F-STATUS3 <> "00" THEN
                 DISPLAY "READ ERROR:" F-STATUS3
                 MOVE -1 TO RETURN-CODE
                 GOBACK
              END-IF
              
              DISPLAY "県名：" R-NAME
              DISPLAY "人口：" R-ZINKO "人"
              DISPLAY "面積：" R-MENSEKI "平方キロメートル"
              
           END-PERFORM.
           CLOSE RESULT-FILE.
       MAIN-EXT.
           GOBACK.
