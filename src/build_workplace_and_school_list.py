#!/usr/bin/python
from random import random

size_breaks = [1,6,11,31,51,101,251,1001]
bin_labels = ['0', '6', '11', '31', '51', '101', '251']

def define_workplace_size_pmf():
    size_pmf = {k:[] for k in bin_labels}
    rate = 2.044227 # fitted powerlaw rate parameter to workplace size data

    for i in range(len(bin_labels)):
        label = bin_labels[i]
        lower, upper = size_breaks[i], size_breaks[i+1]
        for x in range(lower, upper):
            weight = x**(-rate)
            size_pmf[label].append(weight)
    for label in size_pmf.keys():
        total = sum(size_pmf[label])
        size_pmf[label] = [val/total for val in size_pmf[label]]
    return size_pmf

def rand_size(size_pmf, bin_label):
    bin_idx   = bin_labels.index(bin_label)
    bin_lower = size_breaks[bin_idx]
    probs     = size_pmf[bin_label]

    r = random()
    idx = 0
    while (r > probs[idx]):
        r -= probs[idx]
        idx += 1

    return bin_lower + idx

size_pmf = define_workplace_size_pmf()
'''
tjhladish@dragonfly:~/work/arbo_synth_pop/src$ head ../derived_data/non_school_denue_data.out
per_ocu,latitud,longitud
6 a 10 personas,21.25081711,-89.81653574
0 a 5 personas,20.86264811,-90.39676655
0 a 5 personas,21.29032725,-89.60912072
6 a 10 personas,21.27894027,-89.69406741
0 a 5 personas,20.85783136,-90.39405611
0 a 5 personas,21.27718767,-89.68678191
0 a 5 personas,20.59094565,-89.99671269
6 a 10 personas,20.85373688,-90.39860432
0 a 5 personas,21.39265141,-88.89570999
tjhladish@dragonfly:~/work/arbo_synth_pop/src$ head ../derived_data/school_coordinates_full_20181024_V1.csv
ID,ID_unique,MUNICIPIO,LOCALIDAD,long,lat,ntl
2,2,ABALA,MUCUYCHE,-89.6034684,20.6249107,1.2076376676559448
10,10,ABALA,SIHUNCHEN,-89.6813888,20.6919444,0.7426896691322327
13,13,ABALA,PEBA,-89.6855555,20.7224999,1.1732923984527588
14,14,ABALA,CACAO,-89.7463888,20.6927777,1.0477728843688965
15,15,ABALA,ABALA,-89.6805388,20.6499254,3.2445240020751953
19,19,ACANCEH,ACANCEH,-89.4495268,20.8126019,12.299120903015137
20,20,ACANCEH,ACANCEH,-89.4505802,20.8126938,12.299120903015137
21,21,ACANCEH,ACANCEH,-89.454401,20.8119197,14.699371337890625
24,24,ACANCEH,PETECTUNICH,-89.4772479,20.8418183,2.034075975418091
'''

#non_school_denue_data.out  school_coordinates_full_20181024_V1.csv

fo = open('../derived_data/workplaces_and_schools.out', 'w')
fo.write('type size x y\n')

header = True
for line in open('../derived_data/non_school_denue_data.out'):
    if header == True:
        header = False
        continue

    p = line.strip().split(',')
    bin_label = p[0].split()[0]
    fo.write('w ' + str(rand_size(size_pmf, bin_label)) + ' ' + p[2] + ' ' + p[1] + '\n')

header = True
for line in open('../derived_data/school_coordinates_full_20181024_V1.csv'):
    if header == True:
        header = False
        continue

    p = line.strip().split(',')
    fo.write('s 0 ' + p[4] + ' ' + p[5] + '\n')

fo.close()
