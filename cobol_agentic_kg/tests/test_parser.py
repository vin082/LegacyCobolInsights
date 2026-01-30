"""
Unit tests for COBOL Parser Agent
Tests fix for file name extraction bugs
"""
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import unittest
from agents.parsing import parsing_agent
from utils.state import CobolProcessingState


class TestParserFileFixes(unittest.TestCase):
    """Test cases for parser file extraction fixes"""

    def test_cbact01c_file_extraction(self):
        """Test accurate file extraction from AWS CardDemo CBACT01C"""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBACT01C.
       AUTHOR.        AWS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTFILE-FILE ASSIGN TO ACCTFILE.
           SELECT OUT-FILE ASSIGN TO OUTFILE.
           SELECT ARRY-FILE ASSIGN TO ARRYFILE.
           SELECT VBRC-FILE ASSIGN TO VBRCFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCTFILE-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID PIC 9(11).
       FD OUT-FILE.
       01 OUT-ACCT-REC.
          05  OUT-ACCT-ID PIC 9(11).
       FD ARRY-FILE.
       01 ARR-ARRAY-REC.
          05  ARR-ACCT-ID PIC 9(11).
       FD VBRC-FILE.
       01 VBR-REC PIC X(80).

       PROCEDURE DIVISION.
           READ ACCTFILE-FILE.
           WRITE OUT-ACCT-REC.
           WRITE ARR-ARRAY-REC.
           WRITE VBR-REC.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "CBACT01C.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should extract correct file names, not fragments
        self.assertIn("ACCTFILE-FILE", parsed['files_read'])
        self.assertNotIn("THE", parsed['files_read'])  # Bug: Was extracting from comment

        self.assertIn("OUT-FILE", parsed['files_written'])
        self.assertIn("ARRY-FILE", parsed['files_written'])
        self.assertIn("VBRC-FILE", parsed['files_written'])

        # Should NOT extract these bugs
        self.assertNotIn("ARR", parsed['files_written'])
        self.assertNotIn("INTO", parsed['files_written'])
        self.assertNotIn("OUT", parsed['files_written'])
        self.assertNotIn("STATUS", parsed['files_written'])
        self.assertNotIn("VBR", parsed['files_written'])

    def test_comment_stripping(self):
        """Test that comments are stripped before parsing"""
        code = """
      *****************************************************************
      * FUNCTION: READ THE ACCOUNT FILE AND WRITE INTO FILES.        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.

       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO DATAFILE.

       DATA DIVISION.
       FILE SECTION.
       FD DATA-FILE.
       01 DATA-REC PIC X(100).

       PROCEDURE DIVISION.
      * This comment mentions THE file
           READ DATA-FILE.
           STOP RUN.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should only extract real file from READ statement
        self.assertEqual(["DATA-FILE"], parsed['files_read'])
        self.assertNotIn("THE", parsed['files_read'])  # From comment
        self.assertNotIn("ACCOUNT", parsed['files_read'])  # From comment

    def test_unisys_cobol_compatibility(self):
        """Test Unisys COBOL dialect compatibility"""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTRANS.
       AUTHOR. BANKING-OPERATIONS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. UNISYS-2200.
       OBJECT-COMPUTER. UNISYS-2200.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMAST".
           SELECT TRANS-FILE ASSIGN TO "DAILYTXN".
           SELECT POSTED-FILE ASSIGN TO "POSTED".

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 ACCT-NUMBER PIC 9(16).
       FD  TRANS-FILE.
       01  TRANSACTION-RECORD.
           05 TRANS-SEQ-NUM PIC 9(12).
       FD  POSTED-FILE.
       01  POSTED-RECORD.
           05 POSTED-ID PIC 9(12).

       PROCEDURE DIVISION.
           READ ACCOUNT-FILE.
           READ TRANS-FILE.
           WRITE POSTED-RECORD.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "POSTRANS.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        self.assertEqual(set(parsed['files_read']), {"ACCOUNT-FILE", "TRANS-FILE"})
        self.assertEqual(set(parsed['files_written']), {"POSTED-FILE"})
        self.assertEqual("BANKING-OPERATIONS", parsed['author'])

    def test_hyphenated_names(self):
        """Test proper handling of hyphenated COBOL names"""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG-01.

       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT CUSTOMER-MASTER-FILE ASSIGN TO CUSTMAST.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-MASTER-FILE.
       01 CUSTOMER-MASTER-REC.
          05 CUST-ID PIC 9(10).

       PROCEDURE DIVISION.
           READ CUSTOMER-MASTER-FILE.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        self.assertEqual("TEST-PROG-01", parsed['program_name'])
        self.assertIn("CUSTOMER-MASTER-FILE", parsed['files_read'])

    def test_write_record_to_file_mapping(self):
        """Test WRITE statements map record names to file names"""
        code = """
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO OUTFILE.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD-NAME.
          05 FIELD1 PIC X(10).

       PROCEDURE DIVISION.
           WRITE OUTPUT-RECORD-NAME.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should map OUTPUT-RECORD-NAME -> OUTPUT-FILE
        self.assertIn("OUTPUT-FILE", parsed['files_written'])
        # Should NOT have the record name
        self.assertNotIn("OUTPUT-RECORD-NAME", parsed['files_written'])

    def test_no_false_positives_from_working_storage(self):
        """Test that WORKING-STORAGE variables are not treated as files"""
        code = """
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO DATAFILE.

       DATA DIVISION.
       FILE SECTION.
       FD DATA-FILE.
       01 DATA-REC PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-STATUS PIC XX.
       01  WS-INTO-FIELD PIC X(10).
       01  WS-THE-COUNTER PIC 9(5).

       PROCEDURE DIVISION.
           READ DATA-FILE.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should only have the real file
        self.assertEqual(["DATA-FILE"], parsed['files_read'])
        self.assertEqual([], parsed['files_written'])

    def test_multiline_fd_declaration(self):
        """Test multi-line FD declarations (VBRC-FILE edge case from CBACT01C)"""
        code = """
       FILE-CONTROL.
           SELECT VBRC-FILE ASSIGN TO VBRCFILE.

       DATA DIVISION.
       FILE SECTION.
       FD VBRC-FILE
                  RECORDING MODE IS V
                  RECORD IS VARYING IN SIZE
                  FROM 10 TO 80 DEPENDING
                  ON WS-RECD-LEN.
       01 VBR-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-RECD-LEN PIC 9(4).

       PROCEDURE DIVISION.
           WRITE VBR-REC.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should map VBR-REC (record) -> VBRC-FILE (file)
        self.assertIn("VBRC-FILE", parsed['files_written'])
        # Should NOT have the record name
        self.assertNotIn("VBR-REC", parsed['files_written'])


class TestParserEdgeCases(unittest.TestCase):
    """Test edge cases and error handling"""

    def test_missing_file_section(self):
        """Test graceful handling when FILE SECTION is missing"""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOFILEREC.

       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X.

       PROCEDURE DIVISION.
           STOP RUN.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        self.assertEqual("completed", result['status'])

    def test_cobol_keywords_not_detected_as_files(self):
        """Test that COBOL keywords (DISPLAY, PERFORM, PIC) are not detected as files"""
        code = """
       FILE-CONTROL.
           SELECT PAYMENT-FILE ASSIGN TO PAYFILE.

       DATA DIVISION.
       FILE SECTION.
       FD PAYMENT-FILE.
       01 PAYMENT-REC PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-PAYMENTS-READ PIC 9(7).

       PROCEDURE DIVISION.
           PERFORM 2100-READ-PAYMENT
           DISPLAY 'Processing'
           STOP RUN.

       2100-READ-PAYMENT.
           READ PAYMENT-FILE.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should only detect actual file
        self.assertIn("PAYMENT-FILE", parsed['files_read'])

        # Should NOT detect COBOL keywords as files
        keywords = ["DISPLAY", "PERFORM", "PIC", "READ"]
        for keyword in keywords:
            self.assertNotIn(keyword, parsed['files_read'])
            self.assertNotIn(keyword, parsed['files_written'])

    def test_call_statement_quoted_programs_only(self):
        """Test that CALL only extracts quoted program names, not keywords"""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.

       PROCEDURE DIVISION.
           CALL 'SUBPROG1'
           CALL "SUBPROG2"

           IF STATUS = '00'
              CALL 'MQOPEN'
           END-IF.

           STOP RUN.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should extract quoted program names
        self.assertIn("SUBPROG1", parsed['calls'])
        self.assertIn("SUBPROG2", parsed['calls'])
        self.assertIn("MQOPEN", parsed['calls'])

        # Should NOT extract COBOL keywords
        keywords = ["IF", "STATUS", "END-IF"]
        for keyword in keywords:
            self.assertNotIn(keyword, parsed['calls'])

    def test_case_insensitivity(self):
        """Test that parser handles mixed case"""
        code = """
       file-control.
           select data-file assign to datafile.

       data division.
       file section.
       fd data-file.
       01 data-rec pic x(10).

       procedure division.
           read DATA-FILE.
        """

        state = {
            "file_content": code,
            "file_metadata": {"filename": "TEST.cbl"}
        }

        result = parsing_agent.process(state)
        parsed = result['parsed_data']

        # Should handle case-insensitive matches
        self.assertTrue(len(parsed['files_read']) > 0)


def run_tests():
    """Run all tests and print results"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    suite.addTests(loader.loadTestsFromTestCase(TestParserFileFixes))
    suite.addTests(loader.loadTestsFromTestCase(TestParserEdgeCases))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result.wasSuccessful()


if __name__ == '__main__':
    success = run_tests()
    sys.exit(0 if success else 1)
