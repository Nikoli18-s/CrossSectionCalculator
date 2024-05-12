import os
import gc
import shutil
from multiprocessing import Process

import modules.CONVN as convn
import modules.RADIAL as radial
from modules.Total_potential import potential_approximation
from modules.Quantum_numbers import get_permitted_quantum_numbers
from modules.Zhang_eq import sigma_E_summand
from modules.Zhang_eq import sigma_M_summand

from modules.Total_potential import potentials_dir
from modules.Total_potential import potentials_plot_dir

direct_cross_sections_dir = 'direct'


def _calculate_direct_cross_section(isotope, electron_projectile_energy_values, excitation, n_points,  path_to_work_dir):
    convn.run(isotope, excitation, n_points,  path_to_work_dir)

    coeff_Z, coeff_ZS, coeff_A = potential_approximation(isotope,  path_to_work_dir, excitation)

    initial_state = isotope.initial_state
    final_state = isotope.excitations[excitation].spin_parity

    transition_probability = isotope.excitations[excitation].transition_probability
    transition_multipole = isotope.excitations[excitation].transition_multipole
    energy_level = isotope.excitations[excitation].energy_level

    permitted_quantum_numbers_set = get_permitted_quantum_numbers(initial_state, final_state, transition_multipole)

    cross_sections = {}
    for electron_projectile_energy in electron_projectile_energy_values:
        final_wave_functions = []
        initial_wave_functions = []
        cross_section_sum = 0

        E_initial = electron_projectile_energy
        E_final = electron_projectile_energy - energy_level

        if E_initial <= 0 or E_final <= 0:
            # print('Энергия состояния меньше нуля.')
            # print(f'Пропуск шага с энергией {E_initial}.')
            cross_sections[f'cross_section_{electron_projectile_energy}_{transition_multipole}'] = cross_section_sum
        else:
            for quantum_numbers in permitted_quantum_numbers_set:
                l_i = quantum_numbers[0]
                j_i = quantum_numbers[1]
                initial_wave_functions.append(radial.run(coeff_Z, coeff_ZS, coeff_A,
                                                         'initial', E_initial, l_i, j_i, path_to_work_dir))

                l_f = quantum_numbers[2]
                j_f = quantum_numbers[3]
                final_wave_functions.append(radial.run(coeff_Z, coeff_ZS, coeff_A,
                                                       'final', E_final, l_f, j_f, path_to_work_dir))

            for index in range(len(final_wave_functions)):
                lamb = int(transition_multipole[1])
                wave_function_i = initial_wave_functions[index]
                wave_function_f = final_wave_functions[index]
                summand = 0

                if isotope.excitations[excitation].transition_multipole[0] == 'M':
                    summand = sigma_M_summand(lamb, electron_projectile_energy, energy_level, wave_function_i,
                                              wave_function_f, transition_probability, wave_function_f.radius)
                elif isotope.excitations[excitation].transition_multipole[0] == 'E':
                    summand = sigma_E_summand(lamb, electron_projectile_energy, energy_level, wave_function_i,
                                              wave_function_f, transition_probability, wave_function_f.radius)
                else:
                    print(f'Недопустимое значение мультипольности: {isotope.excitations[excitation].transition_multipole[0]}.')
                    print('Допустимые значения: M1, M2, E1, E2.')
                    exit(14)

                cross_section_sum += summand

            if cross_section_sum <= 0:
                cross_section_sum = 0

            cross_sections[f'cross_section_{electron_projectile_energy}_{transition_multipole}'] = cross_section_sum

    output_filename = f'cross_section_{energy_level}_{transition_multipole}.txt'

    with open(path_to_work_dir + '/' + output_filename, 'w') as file:
        file.write("Electron_projectile_energy, keV\tcross_section, barn\n")
        for energy, cross_section in zip(electron_projectile_energy_values, list(cross_sections.values())):
            file.write(f"{energy}\t{cross_section}\n")

    print(f'Расчет прямого сечения для перехода с энергией {energy_level} и мультипольностью {transition_multipole} успешно завершен.')
    gc.collect()


def calculate_direct_cross_section_parallel(
        isotope, electron_projectile_energy_values, n_points, n_CPUs, path_to_calculation_dir):
    excitations_sets = []
    excitations_set = []

    if not os.path.exists(path_to_calculation_dir + '/' + direct_cross_sections_dir):
        os.mkdir(path_to_calculation_dir + '/' + direct_cross_sections_dir)

    if not os.path.exists(path_to_calculation_dir + '/' + potentials_dir):
        os.mkdir(path_to_calculation_dir + '/' + potentials_dir)

    if not os.path.exists(path_to_calculation_dir + '/' + potentials_plot_dir):
        os.mkdir(path_to_calculation_dir + '/' + potentials_plot_dir)

    count = 0
    for excitation in list(isotope.excitations.keys()):
        if count < n_CPUs-1:
            excitations_set.append(excitation)
            count += 1
        else:
            excitations_set.append(excitation)
            excitations_sets.append(excitations_set.copy())
            excitations_set.clear()
            count = 0

    if len(excitations_set) > 0:
        excitations_sets.append(excitations_set.copy())

    for ex_set in excitations_sets:
        n_processes = len(ex_set)
        processes = []
        processes_dirs = []

        for index in range(n_processes):
            path_to_work_dir = path_to_calculation_dir + '/' + 'processor' + str(index)

            processes_dirs.append(path_to_work_dir)
            if not os.path.exists(path_to_work_dir):
                os.mkdir(path_to_work_dir)

            excitation = ex_set[index]

            processes.append(Process(target=_calculate_direct_cross_section,
                                     args=(isotope, electron_projectile_energy_values, excitation, n_points,
                                           path_to_work_dir),
                                     daemon=True))
            processes[index].start()

        for p in processes:
            p.join()

        for index in range(n_processes):
            excitation = ex_set[index]
            energy_level = isotope.excitations[excitation].energy_level
            transition_multipole = isotope.excitations[excitation].transition_multipole

            result_filename = f'cross_section_{energy_level}_{transition_multipole}.txt'
            path_to_result_file = processes_dirs[index] + '/' + result_filename

            shutil.copy(path_to_result_file, path_to_calculation_dir + '/' + direct_cross_sections_dir + '/' + result_filename)

            for file in os.listdir(processes_dirs[index] + '/' + potentials_dir):
                path_to_potential_file = processes_dirs[index] + '/' + potentials_dir + '/' + file
                shutil.copy(path_to_potential_file, path_to_calculation_dir + '/' + potentials_dir + '/' + file)

            for file in os.listdir(processes_dirs[index] + '/' + potentials_plot_dir):
                path_to_potentials_plot_file = processes_dirs[index] + '/' + potentials_plot_dir + '/' + file
                shutil.copy(path_to_potentials_plot_file, path_to_calculation_dir + '/' + potentials_plot_dir + '/' + file)

            shutil.rmtree(processes_dirs[index])
