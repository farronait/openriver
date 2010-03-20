# -*- codinctangg: utf-8 -*-
import numpy as np

# section example
sec_rectangular=[(0, 0), (0, -2), (10, -2), (10, 0)]
sec_rectangular2=sec_rectangular[1:]
sec_rectangular2=sec_rectangular.insert(-1, sec_rectangular[-1])
print sec_rectangular
print sec_rectangular2
rect=np.array(sec_rectangular)
rect2=np.array(sec_rectangular2)
rect_y=np.array([i[0] for i in sec_rectangular])
rect_z=np.array([i[1] for i in sec_rectangular])
print rect,  rect2

import csv

class Segment:
    """
    It defines the segment of a river cross-section
    with its own roughness.
    """
    def __init__(self,
        yzcoordSegm=None,
        roughness=None):
        self.yzcoordSegm = yzcoordSegm
        self.roughness = roughness

class Section:
    """
    It defines attributes and methods for a river cross-section.
    It's possible to define sub-segments of the section,
    each one with a different roughness.
    Example of usage:

    coord = [[0,10],[0,0],[10,0],[20,0],[20,10]]
    sect = Section(0, coord)
    sect.addSegment(sect.yzcoord[0:2], 35)
    sect.addSegment(sect.yzcoord[2:], 40)
    """
    def __init__(self, xaxis=None,
        yzcoord=None, erodible=True,
        roughness=None, discontinuity=False):
        self.xaxis = xaxis
        self.coord = yzcoord
        self.erodible = erodible
        self.roughness = roughness
        self.discontinuity = discontinuity
        self.segment = []

    def addSegment(self, yzcoordSegm=None,
            roughness=None):
        segment = Segment(yzcoordSegm, roughness)
        self.segment.append(segment)

    def getRh(self):
        """Define Rh  for thee sectionon"""
        pass

    def getWetBorder(self):
        """Calculate web border from a given section"""
        pass

	
class Reach:
	"""
	It defines the geometric properties of a river reach.
	It is composed by sections and sections can be subdivided in
	segments.
	"""
	def __init__(self, lenght=None):
		self.lenght = lenght
		self.sections = []
		
	def addSection(self, section=None):
		self.sections.append(section)
		
	def importFile(self, filename):
		datalist = []
		geometryFile = open(filename, "r")
		readerpipe = csv.reader(geometryFile, delimiter = "\t")
		for row in readerpipe:
			datalist.append(row)
		xaxis = datalist[0][0]
		npoints = datalist[1][0]
		nsegments = datalist[1][1]
		print("ok")
		
		

