class Angle():
    def __init__(self):
        self.degrees = 0.0
        self.minutes = 0.0
   
    def setDegrees(self, degrees = 0.0):
        methodName = 'Angle.setDegrees:  '
        if (not((type(degrees) is int) or (type(degrees) is float))):
            raise ValueError(methodName + 'degrees must be of type int or float.')
        self.degrees = degrees
        self.minutes = self.degrees - int(self.degrees)
        self.minutes *= 60.0
        self.degrees = int(self.degrees)
        self.cleanUpMinutes(self)
        self.cleanUpDegrees(self)
        return self.getDegreesNoRounding()
    
    def setDegreesAndMinutes(self, angleString = '0d0.0'):
        methodName = 'Angle.setDegreesAndMinutes:  '
        if (type(angleString) is not str):
            raise ValueError(methodName + 'angleString must be of type str.')
        try:
            ind = angleString.index('d')
            self.degrees = int(angleString[ : ind])
            self.minutes = float(angleString[ind + 1 : ])
            
            if (self.minutes < 0):
                self.degrees = 0
                self.minutes = 0
                raise ValueError(methodName + 'minutes cannot be negative on their own.')
            
            if (self.degrees < 0):
                self.minutes *= -1
                
            self.cleanUpMinutes(self)
            self.cleanUpDegrees(self)
            
            return self.getDegreesNoRounding()
        except:
            self.degrees = 0
            self.minutes = 0
            raise ValueError(methodName + 'angleString format must be #d#.#')
    
    def add(self, angle = None):
        methodName = 'Angle.add:  '
        if (angle == None):
            raise ValueError(methodName + 'angle must be instance of class \'Angle\'.')
        if (type(angle) is not type(self)):
            raise ValueError(methodName + 'angle must be instance of class \'Angle\'.')
        self.degrees += angle.degrees
        self.minutes += angle.minutes
        self.cleanUpMinutes(self)
        self.cleanUpDegrees(self)
        return float(self.getDegreesNoRounding())
    
    def subtract(self, angle = None):
        methodName = 'Angle.subtract:  '
        if (angle == None):
            raise ValueError(methodName + 'angle must be instance of class \'Angle\'.')
        if (type(angle) is not type(self)):
            raise ValueError(methodName + 'angle must be instance of class \'Angle\'.')
        
        self.degrees -= angle.degrees
        self.minutes -= angle.minutes
        self.cleanUpMinutes(self)
        self.cleanUpDegrees(self)
        return float(self.getDegreesNoRounding())
    
    def compare(self, angle = None):
        methodName = 'Angle.compare:  '
        if (angle == None):
            raise ValueError(methodName + 'angle must be instance of class \'Angle\'.')
        if (type(angle) is not type(self)):
            raise ValueError(methodName + 'angle must be instance of class \'Angle\'.')
        self.cleanUpMinutes(angle)
        self.cleanUpDegrees(angle)
        if self.degrees > angle.degrees:
            return 1
        elif self.degrees == angle.degrees:
            if self.minutes > angle.minutes:
                return 1
            elif self.minutes == angle.minutes:
                return 0
            else:
                return -1
        else:
            return -1
    
    def getString(self):
        return str(self.degrees) + 'd' + str('{:2.1f}'.format(self.minutes))
    
    def getDegrees(self):
        deg = float(self.degrees + (self.minutes / 60.0))
        return float('{:3.7f}'.format(deg))
    
    def getDegreesNoRounding(self):
        return float(self.degrees + (self.minutes / 60.0))

    def cleanUpDegrees(self, angle):
        angle.degrees = angle.degrees %360

    def cleanUpMinutes(self, angle):
        while angle.minutes >= 60.0:
            angle.minutes -= 60.0
            angle.degrees += 1
        while angle.minutes < 0:
            angle.minutes += 60.0
            angle.degrees -= 1
    


