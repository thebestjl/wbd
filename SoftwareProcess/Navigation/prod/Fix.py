import xml.dom.minidom as minidom
import Angle as Angle
import math as Math

class Fix():
    def __init__(self, logFile = 'log.txt'):
        methodName = 'Fix.__init__:  '
        if (type(logFile) is not str):           
            raise ValueError(methodName + 'logFile must be a string')
        if (len(logFile) < 1):
            raise ValueError(methodName + 'length of logFile must be > 1')
        
        self.logFile = logFile
        
        try:
            log = open(self.logFile, 'w')
        except:
            raise ValueError(methodName + 'logFile could not be opened.')
        
        log.write('Start of log')
        log.close()
        
    def setSightingFile(self, sightingFile = None):
        methodName = 'Fix.setSightingFile:  '
        if (sightingFile == None):
            raise ValueError(methodName + 'sightingFile must be a string in the form file.xml')
        if (type(sightingFile) is not str):
            raise ValueError(methodName + 'sightingFile must be a string in the form file.xml')
        try:
            ind = sightingFile.index('.')
        except:
            raise ValueError(methodName + 'sightingFile must be a string in the form file.xml')
        
        if (len(sightingFile[0:ind]) < 1):
            raise ValueError(methodName + 'sightingFile must be a string in the form file.xml')
        if (sightingFile[ind:] != '.xml'):
            raise ValueError(methodName + 'sightingFile must be a string in the form file.xml')
        
        try:
            sight = open(sightingFile, 'a')
        except:
            raise ValueError(methodName + 'sightingFile could not be opened.')
        self.sightingFile = sightingFile
        
        #sight.write('Start of sighting file ' + self.sightingFile)
        sight.close()
        
        return self.sightingFile
    
    def getSightings(self):
        methodName = 'Fix.getSightings:  '
        if (self.sightingFile == None):
            raise ValueError(methodName + 'No sightingFile has been set.')
        
        try:
            logFile = open(self.logFile, 'a')
            sightingFile = open(self.sightingFile)
            dom = minidom.parse(sightingFile)
        except:
            raise ValueError(methodName + 'Error parsing sightingFile.')
        
        sightings = dom.getElementsByTagName('sighting')
        
        for sighting in sightings:
            sightingString = self.handleSighting(sighting)
            logFile.write(sightingString)
            
        approximateLatitude = '0d0.0'
        approximateLongitude = '0d0.0'
        return (approximateLatitude, approximateLongitude)
    
# My Methods        
    
    def handleSighting(self, sighting):
        methodName = 'Fix.handleSighting:  '
        try:
            body = sighting.getElementsByTagName('body')[0].childNodes
        except:
            raise ValueError(methodName + 'missing tag: body.')        
        
        bodyStr = self.handleNode(body).strip()
        
        try:    
            date = sighting.getElementsByTagName('date')[0].childNodes
        except:
            raise ValueError(methodName + 'missing tag: date.')
        
        dateStr = self.handleNode(date).strip()
        
        try:
            time = sighting.getElementsByTagName('time')[0].childNodes
        except:
            raise ValueError(methodName + 'missing tag: time.')

        timeStr = self.handleNode(time).strip()

        try:            
            observation = sighting.getElementsByTagName('observation')[0].childNodes
        except:
            raise ValueError(methodName + 'missing tag: observation.')
        
        observationStr = self.handleNode(observation).strip()
        
        try:
            height = sighting.getElementsByTagName('height')[0].childNodes
            heightStr = self.handleNode(height).strip()
        except:
            heightStr = '0'
        
        try:
            temperature = sighting.getElementsByTagName('temperature')[0].childNodes
            temperatureStr = self.handleNode(temperature).strip()
        except:
            temperatureStr = '72'
        
        try:
            pressure = sighting.getElementsByTagName('pressure')[0].childNodes
            pressureStr = self.handleNode(pressure).strip()
        except:
            pressureStr = '1010'

        try:
            horizon = sighting.getElementsByTagName('horizon')[0].childNodes
            horizonStr = self.handleNode(horizon).strip().lower()
        except:
            horizonStr = 'natural'
        
        adjAltStr = self.calculateAdjustedAltitude(observationStr, 
                    heightStr, temperatureStr, pressureStr, horizonStr)
        
        return bodyStr + '\t' + dateStr + '\t' + timeStr + '\t' + \
                observationStr + '\t' + adjAltStr
    
    def calculateAdjustedAltitude(self, observationStr, heightStr, 
                                  temperatureStr, pressureStr, horizonStr):
        methodName = 'Fix.calculateAdjustedAltitude:  '
        
        angle = Angle.Angle()
        angle.setDegreesAndMinutes(str(observationStr))
        if (angle.getDegrees() >= 90.0):
            raise ValueError(methodName + 'observation greater than 90 degrees.')
        
        height = float(heightStr)
        if (height < 0):
            raise ValueError(methodName + 'height less than 0.')
        
        temperature = float(temperatureStr)
        if (temperature < -20 or temperature > 120):
            raise ValueError(methodName + 'temperature must be .GE. -20 and .LE. 120 degrees.')
        
        pressure = float(pressureStr)
        if (pressure < 100 or pressure > 1100):
            raise ValueError(methodName + 'pressure must be .GE. 100 and .LE. 1100.')
        
        if (not(horizonStr == 'natural' or horizonStr == 'artificial')):
            raise ValueError(methodName + 'horizon must be either \'natural\' or \'artificial\'.')
        
        if (horizonStr == 'natural'):
            dip = (-0.97 * Math.sqrt(height)) / 60
        else:
            dip = 0
            
        refraction = (-0.00452 * pressure) / (273 + self.celsius(temperature)) \
                        / self.tangent(angle.getDegrees())
        
        dipAngle = Angle.Angle()
        dipAngle.setDegrees(dip)
        
        refractionAngle = Angle.Angle()
        refractionAngle.setDegrees(refraction)
        
        angle.add(dipAngle)
        angle.add(refractionAngle)
        
        return angle.getString()
    
    def tangent(self, degrees):
        rads = degrees * Math.pi / 180.0
        return Math.tan(rads)
    
    def celsius(self, temperature):
        temperature = temperature - 32
        temperature = temperature * float(5 / 9)
        return temperature
    
    def handleNode(self, nodes):
        rc = []
        for node in nodes:
            rc.append(node.data)
        return ''.join(rc)
        
    def readLogFile(self):
        methodName = 'Fix.readLogFile:  '
        
        try:
            log = open(self.logFile, 'r')
        except:
            raise ValueError(methodName + 'logFile could not be opened.')
        
        string = ''
        for line in log:
            string += line
        
        log.close()
        return string
    
    def readSightingFile(self):
        methodName = 'Fix.readSightingFIle:  '
        
        try:
            sight = open(self.sightingFile, 'r')
        except:
            raise ValueError(methodName + 'sightingFile could not be opened.')
        
        string = ''
        for line in sight:
            string += line
        
        sight.close()
        return string
            