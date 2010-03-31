# -*- coding: utf-8 -*-
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
        roughness=None, discontinuity=False,
        subsection=False):
        self.xaxis = xaxis
        self.coord = yzcoord
        minimum = self.coord[1].argmin()
        self.min = minimum
        self.erodible = erodible
        self.roughness = roughness
        self.discontinuity = discontinuity
        self.subsection = subsection
        self.segment = []

    def addSegment(self, yzcoordSegm=None,
            roughness=None):
        self.segment.append(Section(yzcoord=yzcoordSegm, roughness=roughness, subsection=True))

    def firstPointAfter_h(self, points, h):
        """Return index of the first

        >>> points=np.array([ 742.73,  742.75,  742.77,  742.79,  747.27])

        >>> section.firstPointAfter_h(points, 745)
        4

        >>> section.firstPointAfter_h(points, 742)
        Traceback (innermost last):
          ...
        ValueError: h outside section
         h < min

        >>> section.firstPointAfter_h(points, 748)
        Traceback (innermost last):
          ...
        ValueError: h outside section
         h > max
        """
        if h > points.max():
            raise ValueError("h outside section\n h > max")
        elif h < points.min():
            raise ValueError("h outside section\n h < min")
        else:
            #print 'points:', points,  h
            for i, p in enumerate(points):
                #print i,  p
                if p > h:
                    return i

    def intersection(self, pn1, pn2, h):
        """Returnurn intersection between 2 points and height

            >>> section.intersection((0,5),(0,0),3)
            (0, 3)

        (h-y0)/(y1-y0) = (x-x0)/(x1-x0)
        x=(x1-x0)/(y1-y0)*(h-y0)+x0
        return x,h"""
        #print "intersectionection:", pn1, pn2, h
        #print "z: ", (pn2[0]-pn1[0])/(pn2[1]-pn1[1])*(h-pn1[1])+pn1[0]
        return (pn2[0]-pn1[0])/(pn2[1]-pn1[1])*(h-pn1[1])+pn1[0], h

    def getSect(self, h):
        """Return section only from left intersection to right intersection

            >>> section.getSect(745)
            array([[  4.71227679e-01,   7.45000000e+02],
                   [  9.30000000e-01,   7.42790000e+02],
                   [  7.19000000e+00,   7.42770000e+02],
                   [  1.25900000e+01,   7.42750000e+02],
                   [  1.80800000e+01,   7.42730000e+02],
                   [  1.89100000e+01,   7.42730000e+02],
                   [  1.94887253e+01,   7.45000000e+02]])

        """
        coordtrasp=self.coord.T
        lefttomin=self.coord[1][:self.min+1]
        # left point
        lpnt = self.firstPointAfter_h(lefttomin[::-1], h)
        # find index of left point
        l_pnt = self.min - lpnt
        # find left intersection
        l_intersect = self.intersection(coordtrasp[l_pnt], coordtrasp[l_pnt+1], h)
        # right point
        rpnt = self.firstPointAfter_h(self.coord[1][self.min:], h)
        # find index of right point
        r_pnt = self.min + rpnt
        # find right intersection
        r_intersect = self.intersection(coordtrasp[r_pnt], coordtrasp[r_pnt-1], h)
        # make new section geometries
        sez = coordtrasp[l_pnt+1:r_pnt]
        # Add left intersection on the top
        sez=np.insert(sez, [0,], l_intersect,axis=0)
        # Add rightht intersection on the bottom
        sez=np.append(sez,[r_intersect],axis=0)
        return sez

    def area(self, sez):
        """Return area given a section take from getSect

            >>> sez=section.getSect(745)
            >>> section.area(sez)
            41.448496601989973
        """
        # find area below water line
        area_h2o = (sez[-1][0]-sez[0][0])*sez[0][1]
        # find area bellow section
        area_sez = np.trapz(sez.T[1],x=sez.T[0])
        return area_h2o - area_sez

    def wetBorder(self,  sez):
        """Calculate web border from a given section

            >>> sez=section.getSect(745)
            >>> section.wetBorder(sez)
            23.557497596153816
        """
        # calculate with pitagora: sqrt(dx²+dy²)
        sez1=np.delete(sez, 0, axis=0)
        sez2=np.delete(sez, -1, axis=0)
        delta=sez1-sez2
        return np.sum(np.sqrt(delta * delta))

    def rh(self, h):
        """Return thee idraulic radius given height

            >>> section.rh(745)
            1.7594609288533762
        """
        sez=self.getSect(h)
        area=self.area(sez)
        wetborder = self.wetBorder(sez)
        return area/wetborder

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



if __name__ == "__main__":
    import doctest
    sezdata=np.array([[0.000, 0.930,  7.190, 12.590, 18.080, 18.910, 20.070],
[747.27000, 742.79000, 742.77000, 742.75000, 742.73000, 742.73000, 747.28000]])
    section=Section(yzcoord=sezdata)
    doctest.testmod()

