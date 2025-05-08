IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLEADD.
       AUTHOR. AI Assistant.
       DATE-WRITTEN. 2025-05-07.
      * Simple COBOL program to add two numbers.
      * Location: Bengaluru, Karnataka, India.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1         PIC 9(2) VALUE 10.
       01 WS-NUM2         PIC 9(2) VALUE 15.
       01 WS-RESULT       PIC 9(3).
       01 WS-MESSAGE      PIC X(30).
       PROCEDURE DIVISION.
       A001-MAIN-LOGIC.
           ADD WS-NUM1, WS-NUM2 GIVING WS-RESULT.
           STRING "Result of " WS-NUM1 " + " WS-NUM2 " is: " WS-RESULT
               DELIMITED BY SIZE
               INTO WS-MESSAGE.
           DISPLAY WS-MESSAGE.
           STOP RUN.