import unittest
import Navigation.prod.Fix as Fix

class FixTest(unittest.TestCase):
    def setUp(self):
        self.className = 'Fix.'
    def tearDown(self):
        pass
    
#Acceptance Tests: 100
#    Analysis -    Constructor
#       inputs          
#            logfile
#       outputs
#             An instance of Fix
#       state change    
#            "Start of log" is written to logFile
#
#        Happy Path
#            Nominal Case: Fix()
#        Sad Path
#            logFile length < 1
#            logFile not a string
#            File cannot be created or appended
#
#    Happy Path
    def test100_010_ShouldCreateInstanceOfFix(self):
        self.assertIsInstance(Fix.Fix(), Fix.Fix)
    
    def test100_020_ShouldWriteStartOfLogToLogFile(self):
        fix = Fix.Fix()
        self.assertEqual(fix.readLogFile(), 'Start of log')
        
#    Sad Path
    def test100_910_ShouldRaiseExceptionWhenLogFileNotString(self):
        expectedDiag = self.className + '__init__:'
        with self.assertRaises(ValueError) as context:
            Fix.Fix(8)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test100_920_ShouldRaiseExceptionWhenLogFileLengthLTOne(self):
        expectedDiag = self.className + '__init__:'
        with self.assertRaises(ValueError) as context:
            Fix.Fix("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
    def test100_930_ShouldRaiseExecptionWhenLogFileCannotBeOpened(self):
        expectedDiag = self.className + '__init__:'
        with self.assertRaises(ValueError) as context:
            Fix.Fix('/')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
#Acceptance Tests: 200
#    Analysis -    setSightingFile
#       inputs          
#            sightingFile
#       outputs
#            A string with the same value as sightingFile
#            sightingFile has 'Start of sighting file f.xml' where f is the filename
#       state change    
#            Not sure
#
#        Happy Path
#            Nominal Case: returns a string with same value as sightingFile
#        Sad Path
#            sightingFile length < 1
#            sightingFile not a string
#            sightingFile cannot be created or appended
#            sightingFile doesn't have '.xml' as literal file extension
#
#    Happy Path
    def test200_010_ShouldReturnSightingFileString(self):
        sightingFile = 'sight.xml'
        fix = Fix.Fix()
        self.assertEquals(fix.setSightingFile(sightingFile), sightingFile)
        
    def test200_020_SightingFileShouldReadStartOfSightingFile(self):
        sightingFile = 'sight.xml'
        fix = Fix.Fix()
        fix.setSightingFile(sightingFile)
        self.assertEquals(fix.readSightingFile(), 'Start of sighting file ' + sightingFile)
        
#    Sad Path
    def test200_910_ShouldRaiseExceptionWhenSightingFileLengthLTOne(self):
        expectedDiag = self.className + 'setSightingFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setSightingFile('.xml')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
    def test200_920_ShouldRaiseExceptionWhenSightingFileNotAString(self):
        expectedDiag = self.className + 'setSightingFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setSightingFile(8)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
    def test200_930_ShouldRaiseExceptionWhenSightingFileCantBeCreatedOrAppended(self):
        expectedDiag = self.className + 'setSightingFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setSightingFile('/.xml')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    
    def test200_940_ShouldRaiseExceptionWhenSightingFileDoesntHaveXmlExtension(self):
        expectedDiag = self.className + 'setSightingFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setSightingFile('file')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test200_950_ShouldRaiseExceptionWhenNoSightingFileInput(self):
        expectedDiag = self.className + 'setSightingFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setSightingFile()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
#Acceptance Tests: 300
#    Analysis -    getSightings
#       inputs          
#            none
#       outputs
#            A tuple consisting of approximate position
#       state change    
#            The log is written to the log file
#
#        Happy Path
#            Nominal Case: returns a tuple consisting of the approximate location
#        Sad Path
#            Still to Do
#
#    Happy Path
    def test300_010_NominalCase(self):
        sightingFile = 'sightings.xml'
        fix = Fix.Fix()
        fix.setSightingFile(sightingFile)
        self.assertEquals(('0d0.0', '0d0.0'), fix.getSightings())
        
#    Sad Path