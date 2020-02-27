      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANDOM-GENERATOR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

           01 NUM-1        PIC 99.999999 VALUE ZEROES.
           01 NUM-2        PIC 9(08).
           01 IND          PIC 9(03).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND >10

               ACCEPT  NUM-2 FROM TIME

               DISPLAY NUM-2

               COMPUTE NUM-1 = FUNCTION RANDOM(NUM-2)

               DISPLAY NUM-1



               DISPLAY

           END-PERFORM.
       STOP RUN.
