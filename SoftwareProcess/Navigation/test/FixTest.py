import unittest
import Navigation.prod.Fix as Fix
import os.path as path

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
        
    def test100_020_ShouldCreateInstanceOfFix(self):
        self.assertIsInstance(Fix.Fix('newlog.txt'), Fix.Fix)
    
#     def test100_020_ShouldWriteStartOfLogToLogFile(self):
#         fix = Fix.Fix()
#         self.assertEqual(fix.readLogFile(), 'Start of log')
        
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
            Fix.Fix('/.txt')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test100_940_ShouldRaiseExceptionWhenLogFileLengthLTOneExt(self):
        expectedDiag = self.className + '__init__:'
        with self.assertRaises(ValueError) as context:
            Fix.Fix('.txt')
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
        
#     def test200_020_SightingFileShouldReadStartOfSightingFile(self):
#         sightingFile = 'sight.xml'
#         fix = Fix.Fix()
#         fix.setSightingFile(sightingFile)
#         self.assertEquals(fix.readSightingFile(), 'Start of sighting file ' + sightingFile)
        
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

#Acceptance Tests: 400
#   Analysis -  setAriesFile
#       inputs
#           ariesFile
#       outputs
#           A string whose value is the absolute filepath of the file specified by the parameters
#       state change
#           self.logFile is appended with the path of the Aries file
#
#       Happy Path
#           Returns a string with the correct filepath
#       Sad Path
#           File Name is invalid (violates param specs or cant be opened)
#
#    Happy Path
    def test400_010_NominalCase(self):
        pathStr = str(path.abspath('aries.txt'))
        fix = Fix.Fix()
        self.assertEquals(pathStr, fix.setAriesFile('aries.txt'))
#    Sad Path
    def test400_910_ShouldRaiseExceptionWhenFileLengthLTOne(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setAriesFile('.txt')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
            
    def test400_920_ShouldRaiseExceptionWhenNoInput(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setAriesFile()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test400_930_ShouldRaiseExceptionWhenNotxtLiteral(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setAriesFile('aries')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test400_940_ShouldRaiseExceptionWhenFileCannotBeOpened(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setAriesFile('\.txt')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
# Acceptance Tests: 500
#    Analysis - setStarFile
#        inputs
#            starFile
#        outputs
#            A string whose value is the absolute filepath of the file specified by the parameter.
#        state change
#            Star file and starFile filepath written to self.logFile
#
#    Happy Path
#        Returns a string with correct filepath
#    Sad Path
#        File name invalid
#
#    Happy Path
    def test500_010_NominalCase(self):
        pathStr = str(path.abspath('stars.txt'))
        fix = Fix.Fix()
        self.assertEquals(pathStr, fix.setStarFile('stars.txt'))
#    Sad Path
    def test500_910_ShouldRaiseExceptionWhenFileLengthLTOne(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setStarFile('.txt')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
            
    def test500_920_ShouldRaiseExceptionWhenNoInput(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setStarFile()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test500_930_ShouldRaiseExceptionWhenNotxtLiteral(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setStarFile('aries')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
        
    def test500_940_ShouldRaiseExceptionWhenFileCannotBeOpened(self):
        expectedDiag = self.className + 'setAriesFile:'
        fix = Fix.Fix()
        with self.assertRaises(ValueError) as context:
            fix.setStarFile('\.txt')
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])

        