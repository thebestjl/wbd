import xml.dom.minidom as minidom
import Angle as Angle
import math as Math
import datetime as datetime
import os.path as path

class Fix():
    def __init__(self, logFile = 'log.txt'):
        methodName = 'Fix.__init__:  '
        
        if (type(logFile) is not str):           
            raise ValueError(methodName + 'logFile must be a string')
        try:
            ind = logFile.index('.')
        except:
            raise ValueError(methodName + 'logFile must be a string in the form file.ext')
        
        if (len(logFile[0:ind]) < 1):
            raise ValueError(methodName + 'logFile must be a string in the form file.ext')
        if (len(logFile[ind + 1:]) < 1):
            raise ValueError(methodName + 'logFile must be a string in the form file.ext')
        
        self.logFile = logFile
        self.sightingFile = None
        self.ariesFile = None
        self.starFile = None
        self.error_count = 0
        
        try:
            log = open(self.logFile, 'w')
        except:
            raise ValueError(methodName + 'logFile could not be opened.')

        log.write('Log file:\t' + str(path.abspath(logFile)) + '\n')
        
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
        sight.close()
        
        logFile = open(self.logFile, 'a')
        logFile.write('Sighting file:\t' + str(path.abspath(self.sightingFile)) + '\n')
        logFile.close()
        return str(path.abspath(self.sightingFile))
    
    def setAriesFile(self, ariesFile = None):
        methodName = 'Fix.setAriesFile:  '
        if (ariesFile == None):
            raise ValueError(methodName + 'ariesFile must be a string in the form file.txt')
        if (type(ariesFile) is not str):
            raise ValueError(methodName + 'ariesFile must be a string in the form file.txt')
        try:
            ind = ariesFile.index('.')
        except:
            raise ValueError(methodName + 'ariesFile must be a string in the form file.txt')
        
        if (len(ariesFile[0:ind]) < 1):
            raise ValueError(methodName + 'ariesFile must be a string in the form file.txt')
        if (ariesFile[ind:] != '.txt'):
            raise ValueError(methodName + 'ariesFile must be a string in the form file.txt')
        
        try:
            aries = open(ariesFile, 'a')
        except:
            raise ValueError(methodName + 'ariesFile could not be opened.')
        self.ariesFile = ariesFile
        aries.close()
        
        logFile = open(self.logFile, 'a')
        logFile.write('Aries file:\t' + str(path.abspath(self.ariesFile)) + '\n')
        logFile.close()
        
        return str(path.abspath(self.ariesFile))
    
    def setStarFile(self, starFile = None):
        methodName = 'Fix.setStarFile:  '
        if (starFile == None):
            raise ValueError(methodName + 'starFile must be a string in the form file.txt')
        if (type(starFile) is not str):
            raise ValueError(methodName + 'starFile must be a string in the form file..txt')
        try:
            ind = starFile.index('.')
        except:
            raise ValueError(methodName + 'starFile must be a string in the form file.txt')
        
        if (len(starFile[0:ind]) < 1):
            raise ValueError(methodName + 'starFile must be a string in the form file.txt')
        if (starFile[ind:] != '.txt'):
            raise ValueError(methodName + 'starFile must be a string in the form file.txt')
        
        try:
            star = open(starFile, 'a')
        except:
            raise ValueError(methodName + 'starFile could not be opened.')
        self.starFile = starFile
        star.close()
        
        logFile = open(self.logFile, 'a')
        logFile.write('Star file:\t' + str(path.abspath(self.starFile)) + '\n')
        logFile.close
        
        return str(path.abspath(self.starFile))
    
    
    def getSightings(self):
        methodName = 'Fix.getSightings:  '
        if (self.sightingFile == None):
            raise ValueError(methodName + 'No sightingFile has been set.')
        if (self.ariesFile == None):
            raise ValueError(methodName + 'No ariesFile has been set.')
        if (self.starFile == None):
            raise ValueError(methodName + 'No starFile has been set.')       
        
        try:
            logFile = open(self.logFile, 'a')
            sightingFile = open(self.sightingFile)
            dom = minidom.parse(sightingFile)
        except:
            raise ValueError(methodName + 'Error parsing sightingFile.')
        
        sightings = dom.getElementsByTagName('sighting')
        
        all_sightings = []
        for sighting in sightings:
            (sightingDate, sightingTime, sightingString) = self.handleSighting(sighting)
            if (not(sightingDate == None or sightingTime == None or sightingString == None)):
                all_sightings.append((sightingDate, sightingTime, sightingString))
            
        self.sortSightings(all_sightings)
        
        for sighting in all_sightings:
            logFile.write('LOG:\t' + str(datetime.datetime.now()) \
                          + str(datetime.datetime.utcoffset(datetime.datetime.now())) \
                          + '\t' + sighting[2] + '\n')
            
        logFile.write('LOG:\t' + str(datetime.datetime.now()) \
                      + str(datetime.datetime.utcoffset(datetime.datetime.now())) \
                      + '\tSighting errors:\t' + str(self.error_count))
            
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
        
        retStr = bodyStr + '\t' + dateStr + '\t' + timeStr + '\t' + \
                adjAltStr
        
        (geoLong, SHA) = self.parseStarFile(retStr)
        GHA = self.parseAriesFile(retStr)
        if (geoLong == None or SHA == None or GHA == None):
            self.error_count += 1
            return (None, None, None)
        else:
            ang_1 = Angle.Angle()
            ang_2 = Angle.Angle()
            print GHA
            ang_1.setDegreesAndMinutes(GHA)
            ang_2.setDegreesAndMinutes(SHA)
            ang_1.add(ang_2)
            geoLat = ang_1.getString()
            retStr += '\t' + geoLat + '\t' + geoLong
        
            return (dateStr, timeStr, retStr)
    
    def calculateAdjustedAltitude(self, observationStr, heightStr, 
                                  temperatureStr, pressureStr, horizonStr):
        methodName = 'Fix.calculateAdjustedAltitude:  '
        
        angle = Angle.Angle()
        angle.setDegreesAndMinutes(str(observationStr))
        
        if (angle.getDegrees() >= 90.0):
            raise ValueError(methodName + 'observation greater than 90 degrees.')
        if (angle.getDegrees() < 0.1 / 60.0):
            raise ValueError(methodName + 'observation less than 0.1 arc-minutes.')
        
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
        
        dip = self.calculateDip(horizonStr, height)    
        refraction = self.calculateRefraction(pressure, temperature, angle)
        
        dipAngle = Angle.Angle()
        dipAngle.setDegrees(dip)
        
        refractionAngle = Angle.Angle()
        refractionAngle.setDegrees(refraction)
        
        angle.add(dipAngle)
        angle.add(refractionAngle)
        
        return angle.getString()
    
    def calculateRefraction(self, pressure, temperature, angle):
        numerator = -0.00452 * pressure
        denominator_1 = 273 + self.celsius(temperature)
        denominator_2 = self.tangent(angle.getDegrees())
        return numerator / denominator_1 / denominator_2
    
    def calculateDip(self, horizonStr, height):
        if (horizonStr == 'natural'):
            return (-0.97 * Math.sqrt(height)) / 60
        else:
            return 0
    
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
    
    def sortSightings(self, sightings):
        for i in range(1, len(sightings)):
            s = sightings[i]
            j = i - 1
            while j >= 0 and self.compare(sightings[j], s):
                sightings[j + 1] = sightings[j]
                j = j - 1
            sightings[j + 1] = s
            
    def compare(self, s1, s2):
        if (s1[0] > s2[0]):
            return True
        elif (s1[0] < s2[0]):
            return False
        elif (s1[1] > s2[1]):
            return True
        else:
            return False
    
    def parseAriesFile(self, starStr):
        lst = starStr.split('\t')
        date_lst = lst[1].split('-')
        date_year = date_lst[0][2:]
        date_month = date_lst[1]
        date_day = date_lst[2]

        ind = lst[2].index(':')
        hour = int(lst[2][0 : ind])
        lst2 = lst[2].split(':')
        seconds = float(lst2[2])
        aries = open(self.ariesFile, 'r')
        ariesAngle_1 = '0d0.0'
        ariesAngle_2 = '0d0.0'
        break_out = False
        for line in aries.readlines():
            lst = line.split('\t')
            date_lst_2 = lst[0].split('/')
            dy = date_lst_2[2]
            dm = date_lst_2[0]
            dd = date_lst_2[1]
            if break_out:
                ariesAngle_2 = lst[2].strip()
                break
            if dy == date_year and date_month == dm and date_day == dd:
                if int(lst[1]) == hour: 
                    ariesAngle_1 = lst[2].strip()
                    break_out = True

        ariesAngle = self.calculateGHA(ariesAngle_1, ariesAngle_2, seconds)
        
        aries.close()
        return ariesAngle
    
    def calculateGHA(self, angle_1, angle_2, seconds):
        ang_1 = Angle.Angle()
        ang_2 = Angle.Angle()
        ang_3 = Angle.Angle()
        
        ang_1.setDegreesAndMinutes(angle_1)
        ang_2.setDegreesAndMinutes(angle_2)
        
        ang_2.subtract(ang_1)
        temp = seconds / 3600 * ang_2.getDegrees()
        ang_3.setDegrees(temp)
        
        ang_1.add(ang_3)
        
        ariesAngle = ang_1.getString()
        return ariesAngle
    
    def parseStarFile(self, starStr):
        starList = starStr.strip().split('\t')
        star = open(self.starFile, 'r')
        starBody = starList[0].strip()
        star_lst = starList[1].split('-')
        starYear = star_lst[2][2:]
        starMonth = star_lst[0]
        starDay = star_lst[1]
        sy = '0'
        sm = '0'
        sd = '0'
        starLat = None
        starLong = None
        b = False
        for line in star.readlines():
            line = line.strip()
            lst = line.split('\t')
            date_lst_2 = lst[1].split('/')
            dy = date_lst_2[2]
            dm = date_lst_2[0]
            dd = date_lst_2[1]
            if lst[0].lower().strip() == starBody.lower():
                if not(b):
                    starLat = lst[2]
                    starLong = lst[3]
                    b = True
                if dy <= starYear and dm <= starMonth and dd <= starDay:
                    if dy > sy and dm > sm and dd > sd:
                        sy = dy
                        sm = dm
                        sd = dd
                        starLat = lst[2]
                        starLong = lst[3]

        star.close()    
        return (starLat, starLong)
        