import os
import math
import numpy as np
from io import StringIO
from scipy.optimize import curve_fit

from modules.Electon_shell_potential import electron_shell_potential
from modules.Unscreened_nucleus_potential import unscreened_nucleus_potential

potentials_dir = 'potentials'
potentials_plot_dir = 'potentials_plot_data'


def _approximation_func(x, a, b, c):
    return a / x + b * pow(math.e, -c * x) / x


def _radius_from_hartree_fock_slater(isotope, path_to_work_dir):
    path_to_isotope_convn_file = path_to_work_dir + '/' + f'{isotope.name}.cno'

    with open(path_to_isotope_convn_file, 'r') as file:
        strings = file.read()
    strings = strings.split('\n LB=  ')
    strings.pop(0)
    data = []
    LB = []
    AJB = []
    for s in strings:
        test = s.split('N(x)')[1].split('\n Energy loop = 1')[0].strip()
        data.append(np.genfromtxt(StringIO(test), skip_header=2,
                                  max_rows=500, usecols=[0, 1, 2]))
        header = s.split('LF')
        header = header[0].split('AJB=  ')
        LB.append(float(header[0]))
        AJB.append(float(header[1]))

    return data[0][:, 0]


def _total_potential(isotope, path_to_work_dir, label):
    path_to_potentials_dir = path_to_work_dir + '/' + potentials_dir
    path_to_potentials_plot_dir = path_to_work_dir + '/' + potentials_plot_dir
    path_to_unscreened_potential_file = path_to_potentials_dir + '/' + 'unscreened_nucleus_potential_' + \
                                        isotope.name + '_' + label + '.txt'
    path_to_electron_potential_file = path_to_potentials_dir + '/' + 'electron_shell_potential_' + \
                                      isotope.name + '_' + label + '.txt'
    path_to_unscreened_potential_plot_file = path_to_potentials_plot_dir + '/' + 'plot_unscreened_nucleus_potential_' + \
                                             isotope.name + '_' + label + '.txt'
    path_to_electron_potential_plot_file = path_to_potentials_plot_dir + '/' + 'plot_electron_shell_potential_' + \
                                           isotope.name + '_' + label + '.txt'

    if not os.path.exists(path_to_potentials_dir):
        os.mkdir(path_to_potentials_dir)

    if not os.path.exists(path_to_potentials_plot_dir):
        os.mkdir(path_to_potentials_plot_dir)

    radius_from_hartree_fock_slater = _radius_from_hartree_fock_slater(isotope, path_to_work_dir)

    total_potential = []
    unscreened_potential = []
    electron_potential = []
    for r in radius_from_hartree_fock_slater:
        unscreened_potential_val = unscreened_nucleus_potential(isotope, r)
        electron_potential_val = electron_shell_potential(isotope, path_to_work_dir, r)
        total_potential_val = unscreened_potential_val + electron_potential_val

        unscreened_potential.append(unscreened_potential_val)
        electron_potential.append(electron_potential_val)
        total_potential.append(total_potential_val)

    np.savetxt(path_to_unscreened_potential_file, np.c_[radius_from_hartree_fock_slater, unscreened_potential],
               delimiter='\t', header='radius \tV_nucl, eV')
    np.savetxt(path_to_electron_potential_file, np.c_[radius_from_hartree_fock_slater, electron_potential],
               delimiter='\t', header='radius \tV_nucl, eV')

    plot_unscreened_potential = []
    plot_electron_potential = []
    radius = np.linspace(0.0001, 10, 100)
    for r in radius:
        plot_unscreened_potential.append(unscreened_nucleus_potential(isotope, r))
        plot_electron_potential.append(electron_shell_potential(isotope, path_to_work_dir, r))

    np.savetxt(path_to_unscreened_potential_plot_file, np.c_[radius, plot_unscreened_potential],
               delimiter='\t', header='radius \tV_nucl, eV')
    np.savetxt(path_to_electron_potential_plot_file, np.c_[radius_from_hartree_fock_slater, electron_potential],
               delimiter='\t', header='radius \tV_nucl, eV')

    return total_potential


def potential_approximation(isotope, path_to_work_dir, label):
    coeffs_lower_values = [-100, -100, 0]
    coeffs_upper_values = [0, 0, 100]

    radius_from_hartree_fock_slater = _radius_from_hartree_fock_slater(isotope, path_to_work_dir)
    total_potential = _total_potential(isotope, path_to_work_dir, label)

    results = curve_fit(_approximation_func, radius_from_hartree_fock_slater, total_potential,
                        bounds=(coeffs_lower_values, coeffs_upper_values))

    z = results[0][0]
    zs = results[0][1]
    alpha = results[0][2]

    return z, zs, alpha
