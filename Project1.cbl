       *>****************************************************************
       *> Author: Prabjot Singh
       *> Date: 12-06-2024
       *> Purpose: Employee conversion to txt file
       *> Tectonics: cobc
       *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E-Record.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile ASSIGN TO 'EMPLOYEE.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

     *> The declaration or varibles and file structure
       DATA DIVISION.
       FILE SECTION.
       FD  EmployeeFile.
       01  EmployeeRecordFormat.
           05  Employee-ID    PIC 9(6).
           05  Dept           PIC 9(3).
           05  Last-Name      PIC A(20).
           05  First-Name     PIC A(20).
           05  ServiceYears   PIC 9(2).

       WORKING-STORAGE SECTION.
       01  WS-REPLY          PIC X(1).
       01  WS-EOF            PIC X(1) VALUE 'N'.
           88 EOF            VALUE 'Y'.

      *> List all the leveled processes for final output
      PROCEDURE DIVISION.
      000-INITIALIZATION.
           PERFORM 100-ENTER-RECORDS.
           PERFORM 200-DISPLAY-RECORDS.
           STOP RUN.

      *>Initialise the Enter-record and perform further processes
       100-ENTER-RECORDS.
           OPEN OUTPUT EmployeeFile.
           PERFORM WITH TEST BEFORE UNTIL WS-REPLY = 'N' OR WS-REPLY = 'n'
                DISPLAY 'Enter a new record? (Y/N) '
                ACCEPT WS-REPLY
                IF WS-REPLY = 'Y' OR WS-REPLY = 'y'
                    PERFORM 101-USER-INPUT
                    PERFORM 102-WRITE-RECORD
                END-IF
           END-PERFORM
           CLOSE EmployeeFile.

           *> Collects the user input and accept it in EmployeeRecordFormat
           101-USER-INPUT.
                   DISPLAY "Enter Employee ID (6)".
                   ACCEPT Employee-ID OF EmployeeRecordFormat.
                   DISPLAY "Enter Department Code (3)".
                   ACCEPT Dept OF EmployeeRecordFormat.
                   DISPLAY "Enter First Name (20)".
                   ACCEPT First-Name OF EmployeeRecordFormat.
                   DISPLAY "Enter Last Name (20)".
                   ACCEPT Last-Name OF EmployeeRecordFormat.
                   DISPLAY "Enter Years of Service (2)".
                   ACCEPT ServiceYears OF EmployeeRecordFormat.

           102-WRITE-RECORD.
                   WRITE EmployeeRecordFormat
                    BEFORE ADVANCING 1 LINE.
                   DISPLAY "Data saved.....".

           *> Display the formatted records in console output

       200-DISPLAY-RECORDS.
           OPEN INPUT EmployeeFile.
           DISPLAY "EmployeeID  DeptCode  FirstName            LastName           ServiceYears".
           DISPLAY "-------------------------------------------------------------------------".
           PERFORM 300-WRITE-INTO-FILE
           DISPLAY "-------------------------------------------------------------------------".
           CLOSE EmployeeFile.



       300-WRITE-INTO-FILE.
           READ EmployeeFile INTO EmployeeRecordFormat
           AT END
                SET EOF TO TRUE
           NOT AT END
            IF ServiceYears OF EmployeeRecordFormat >= 5
            DISPLAY
            Employee-ID OF EmployeeRecordFormat
            "     " Dept OF EmployeeRecordFormat
            "     " First-Name OF EmployeeRecordFormat
            " " Last-Name OF EmployeeRecordFormat
            " " ServiceYears OF EmployeeRecordFormat
           END-READ.

       END PROGRAM E-Record.
