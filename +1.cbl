      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
           IDENTIFICATION DIVISION.
           PROGRAM-ID. mais-um.
           DATA DIVISION.
           FILE SECTION.
               WORKING-STORAGE SECTION.
                   01  NUM     PIC 9(10).
           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               PERFORM MAIN.

           STOP RUN.

           MAIN SECTION.

                PERFORM VARYING NUM FROM 1 BY 1 UNTIL 100
                   DISPLAY NUM.
