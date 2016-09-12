class Angle():
    def __init__(self):
        self.degrees = 0.0
        self.minutes = 0.0
   
    def setDegrees(self, degrees):
        try:
            ind = degrees.index('=')
            self.degrees = float(degrees[ind + 1 : ])
            self.minutes = self.degrees - int(self.degrees)
            self.minutes *= 60.0
            self.degrees = int(self.degrees)
            self.cleanUpMinutes(self)
            self.cleanUpDegrees(self)
            return self.getDegrees()
        except:
            raise ValueError('Angle.setDegrees:  degrees formoat must be \'degrees=#.#\'')
    
    def setDegreesAndMinutes(self, angleString):
        try:
            ind = angleString.index('d')
            self.degrees = int(angleString[ : ind])
            self.minutes = float(angleString[ind + 1 : ])
            self.cleanUpMinutes(self)
            self.cleanUpDegrees(self)
            return self.getDegrees()
        except:
            raise ValueError('Angle.setDegreesAndMinutes:  angleString format must be #d#.#')
    
    def add(self, angle):
        try:
            self.cleanUpMinutes(angle)
            self.cleanUpDegrees(angle)
            self.degrees += angle.degrees
            self.minutes += angle.minutes
            self.cleanUpMinutes(self)
            self.cleanUpDegrees(self)
            return self.getDegrees()
        except:
            raise ValueError('Angle.add:  Error. Make sure input is correctly instantiated instance of class Angle.')
    
    def subtract(self, angle):
        try:
            self.cleanUpMinutes(angle)
            self.cleanUpDegrees(angle)
            self.degrees -= angle.degrees
            self.minutes -= angle.minutes
            self.cleanUpMinutes(self)
            self.cleanUpDegrees(self)
            return self.getDegrees()
        except:
            raise ValueError('Angle.subtract:  Error. Make sure input is correctly instantiated instance of class Angle.')
    
    def compare(self, angle):
        try:
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
        except:
            raise ValueError('Angle.compare: Error. Make sure input is correctly instantiated instance of class Angle.')
    
    def getString(self):
        return str(self.degrees) + 'd' + str('{:2.1f}'.format(self.minutes))
    
    def getDegrees(self):
        deg = self.degrees + (self.minutes / 60.0)
        return float('{:3.1f}'.format(deg))

    def cleanUpDegrees(self, angle):
        while angle.degrees >= 360.0:
            angle.degrees -= 360.0
        while angle.degrees < 0:
            angle.degrees += 360.0

    def cleanUpMinutes(self, angle):
        while angle.minutes >= 60.0:
            angle.minutes -= 60.0
            angle.degrees += 1
        while angle.minutes < -60.0:
            angle.minutes += 60.0
            angle.degrees += 1
        if angle.minutes < 0:
            angle.minutes += 60.0
            angle.degrees -= 1
