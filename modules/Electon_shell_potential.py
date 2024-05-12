import math
import scipy.interpolate
import scipy.integrate
import numpy as np
from io import StringIO

e2 = 1/137.0359895
m = 510999.06
aB = 1/(m*e2)
epsilon = m * e2 ** 2


def _E_trap(data, density, r):
    integrand = []
    for i in range(len(data[0][:, 0])):
        if data[0][i, 0] < r:
            integrand.append(density[i] * data[0][i, 0] ** 2)
    return math.sqrt(e2) / pow(aB, 2) * np.trapz(integrand, dx=0.05e-2) / r ** 2


def electron_shell_potential(isotope, path_to_work_dir, r):
    path_to_isotope_convn_file = path_to_work_dir + '/' + f'{isotope.name}.cno'

    with open(path_to_isotope_convn_file, 'r') as file:
        strings = file.read()
    strings = strings.split('\n LB=  ')
    strings.pop(0)
    data = []
    LB = []
    AJB = []
    for s in strings:
        data.append(np.genfromtxt(StringIO(s.split('\n Energy loop = 1')[0].strip()), skip_header=16,
                                  max_rows=500, usecols=[0, 1, 2]))
        header = s.split('LF')
        header = header[0].split('AJB=  ')
        LB.append(float(header[0]))
        AJB.append(float(header[1]))

    rows_length = len(data[0][:, 0])

    density = np.zeros(len(data[0][:, 0]))
    for shell_index in range(len(LB)):
        for row_index in range(rows_length):
            density[row_index] += data[shell_index][row_index, 1] ** 2 + data[shell_index][row_index, 2] ** 2

    cs = scipy.interpolate.make_interp_spline(data[0][:-1, 0], [_E_trap(data, density, r) for r in data[0][:-1, 0]], k=1)
    return -aB * np.asarray(scipy.integrate.quad(cs, r, max(data[0][:-1, 0]))[0])
