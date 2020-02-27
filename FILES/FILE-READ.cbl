      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
           IDENTIFICATION DIVISION.
               PROGRAM-ID. YOUR-PROGRAM-NAME.

           ENVIRONMENT DIVISION.

                       INPUT-OUTPUT SECTION.
                           FILE-CONTROL.
                               SELECT ARQUIVO ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-ENTRADA.

           DATA DIVISION.

               FILE SECTION.
                   FD ARQUIVO
                       LABEL RECORD STANDARD
                       VALUE OF FILE-ID IS
                   'C:\Users\re039833\Desktop\Cobol-Estudo\ARQUIVO.DAT'.

                       01 REG-ARQUIVO.
                           05 STRING-ARQUIVO   PIC X(04).
                           05 SLA              PIC X(01).

               WORKING-STORAGE SECTION.

                   01 FS-ENTRADA   PIC X(02).

                   01 X            PIC 9(04).

           PROCEDURE DIVISION.
               MAIN-PROCEDURE.
                   OPEN INPUT ARQUIVO.
                   DISPLAY 'FS ' FS-ENTRADA

                   PERFORM VARYING X FROM 1 BY 1 UNTIL X > 10
                   READ ARQUIVO
                   DISPLAY STRING-ARQUIVO
                   DISPLAY SLA
                   DISPLAY 'FS ' FS-ENTRADA

               END-PERFORM.

                   CLOSE ARQUIVO.
               STOP RUN.
           END PROGRAM YOUR-PROGRAM-NAME.
