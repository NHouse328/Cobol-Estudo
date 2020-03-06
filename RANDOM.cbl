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

           01 NUM-1        PIC 9(02)V9(11) VALUE ZEROES.
           01 NUM-2        PIC 9(08).
           01 NUM-3        PIC 9(11).
           01 IND          PIC 9(03).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           ACCEPT  NUM-2 FROM TIME

           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND >10

               COMPUTE NUM-2 = NUM-2 + 48266543255

               COMPUTE NUM-1 = FUNCTION RANDOM(NUM-2)

               COMPUTE NUM-3 = NUM-1 * 99999999999

               DISPLAY NUM-3

           END-PERFORM.

       STOP RUN.
