import os
import numpy as np

from modules.Isotope import path_to_isotopes_data
from modules.IndirectTransition import IndirectTransition
from modules.Direct_cross_section import direct_cross_sections_dir

absolute_conversion_filename = 'indirect_transitions_absolute_conversion_data.txt'
relative_conversion_filename = 'indirect_transition_relative_conversion_data.txt'
indirect_cross_sections_dir = 'indirect'
chains_dir = 'chains'


def _convert_absolute_conversion_to_relative(isotope):
    path_to_isotope_dir = path_to_isotopes_data + '/' + isotope.name
    path_to_absolute_conversion_filename = path_to_isotope_dir + '/' + absolute_conversion_filename
    path_to_relative_conversion_filename = path_to_isotope_dir + '/' + relative_conversion_filename

    if not os.path.exists(path_to_absolute_conversion_filename):
        print(f'Файл с данными о коэффициентах конверсии для изотопа {isotope.name}  не найден.')
        exit(15)

    if not os.path.exists(path_to_relative_conversion_filename):
        data = np.genfromtxt(path_to_absolute_conversion_filename, skip_header=1)

        seen_data_init_state = []
        duplicates_init_state = []

        for init_state in data[:, 0]:
            if init_state in seen_data_init_state:
                if init_state not in duplicates_init_state:
                    duplicates_init_state.append(init_state)
            else:
                seen_data_init_state.append(init_state)

        for init_state in duplicates_init_state:
            # find row indices of duplicates
            indexes = np.where(init_state == data[:, 0])[0]
            # find sum of third column values
            sum_value = 0
            for index in indexes:
                sum_value += data[index, 2]
            # set new value
            for index in indexes:
                data[index, 2] = data[index, 2] / sum_value

        for index in range(len(data[:, 2])):
            if data[index, 2] == 100.0:
                data[index, 2] = 1.0

        np.savetxt(path_to_relative_conversion_filename, data, delimiter='\t',
                   header='Initial state, keV\tFinal state, keV\tConversion i %\talpha')

        print(f'Значения коэффициентов конверсии для изотопа {isotope.name} успешно приведены к относительным.')
    else:
        print(f'Значения коэффициентов конверсии для изотопа {isotope.name} были приведены к относительным ранее.')


def _read_indirect_transition_data(isotope):
    path_to_isotope_dir = path_to_isotopes_data + '/' + isotope.name
    path_to_relative_conversion_filename = path_to_isotope_dir + '/' + relative_conversion_filename

    data = np.genfromtxt(path_to_relative_conversion_filename, skip_header=1, delimiter='\t')

    indirect_transitions = {}
    for index in range(len(data)):
        key = f'indirect_transition_{data[index, 0]}_{data[index, 1]}'
        indirect_transitions[key] = IndirectTransition(float(data[index, 0]), float(data[index, 1]),
                                                       float(data[index, 2]), float(data[index, 3]))

    return indirect_transitions


def _get_chains(indirect_transitions, energy_level):
    # create empty dict
    chains = {}
    test = {}

    # create function for looping through
    def expand_chain(_indirect_transitions, _energy_level, _previous):
        # allows to get dict from above
        nonlocal chains
        counts = 0
        # loop through energies
        for indirect_transition in _indirect_transitions.values():
            # if found loop again
            if indirect_transition.final_state == _energy_level:
                counts += 1
                test2 = _previous.copy()
                test2.append(indirect_transition.initial_state)
                expand_chain(_indirect_transitions, indirect_transition.initial_state, test2)
        # if end of chain put into dict
        if counts == 0:
            test[f'chain_{_energy_level}'] = _previous
            if f'chain_{_energy_level}' not in chains.keys():
                _previous.reverse()
                chains[f'chain_{_energy_level}'] = _previous
            else:
                loop = 0
                while f'chain_{_energy_level}_{loop}' in chains.keys():
                    loop += 1
                _previous.reverse()
                chains[f'chain_{_energy_level}_{loop}'] = _previous

    # call the function
    expand_chain(indirect_transitions, energy_level, [energy_level])
    chains_list = list(chains.values()).copy()

    for chain in chains_list:
        for index in range(len(chain) - 1):
            if chain[index:] not in chains.values():
                energy_level = chain[index]
                if f'chain_{energy_level}' not in chains.keys():
                    chains[f'chain_{energy_level}'] = chain[index:]
                else:
                    loop = 0
                    while f'chain_{energy_level}_{loop}' in chains.keys():
                        loop += 1
                    chains[f'chain_{energy_level}_{loop}'] = chain[index:]
    return chains


def _get_cross_section_files(chain, path_to_calculation_dir):
    for energy_level in chain:
        # Получаем текущую директорию

        # Ищем файлы, начинающиеся с "cross_section_" и содержащие указанный energy_level
        matching_files = [filename for filename in os.listdir(path_to_calculation_dir + '/' +direct_cross_sections_dir)
                          if filename.startswith(f'cross_section_{energy_level}')]

        # Выводим найденные файлы
        if matching_files:
            return matching_files

    print(f'Файлов для chain {chain} не найдено.')
    return None


def calculate_indirect_cross_section(isotope, energy_level, write_by_cains, path_to_calculation_dir):
    output_filename = f'indirect_cross_sections_{energy_level}.txt'
    path_to_output_file = path_to_calculation_dir + '/' + indirect_cross_sections_dir + '/' + output_filename

    if not os.path.exists(path_to_calculation_dir + '/' + indirect_cross_sections_dir):
        os.mkdir(path_to_calculation_dir + '/' + indirect_cross_sections_dir)

    _convert_absolute_conversion_to_relative(isotope)
    indirect_transitions = _read_indirect_transition_data(isotope)
    chains = _get_chains(indirect_transitions, energy_level)
    first_chain_in_chains = True

    for key, chain in chains.items():
        matching_files = _get_cross_section_files(chain, path_to_calculation_dir)
        # файлов начинающихся с cross_section_{energy_level} может быть несколько
        # нам нужны они все, мы их аккуратно сложим
        only_one_cross_section_file = True
        if matching_files is not None:
            for cross_section_filename in matching_files:
                path_to_cross_section_file = path_to_calculation_dir + '/' + direct_cross_sections_dir + '/' + cross_section_filename
                if only_one_cross_section_file:
                    Electron_projectile_energy = []
                    cross_section = []
                    # read file and form nice data array
                    data = np.genfromtxt(path_to_cross_section_file, skip_header=1, delimiter='\t')
                    # loop for all entries
                    for i in range(len(data)):
                        Electron_projectile_energy.append(float(data[i, 0]))
                        cross_section.append(float(data[i, 1]))
                    only_one_cross_section_file = False

                if not only_one_cross_section_file:
                    other_cross_section = []
                    # read file and form nice data array
                    data = np.genfromtxt(path_to_cross_section_file, skip_header=1, delimiter='\t')
                    # loop for all entries
                    for i in range(len(data)):
                        other_cross_section.append(float(data[i, 1]))

                    for index in range(len(other_cross_section)):
                        cross_section[index] += + other_cross_section[index]

            # теперь у нас есть актуальные данные для наиболее высокого уровня
            # Electron_projectile_energy = [...] и cross_section = [...]
            chain_conversion_ratio = 1
            for index in range(len(chain) - 1):
                # не забываем поделить на (1 + alpha) поскольку для каждого перехода
                # коэффициент перехода = i_relative / alpha = i_transition / ( (SUM(i_transitions)) * ( 1 + alpha_transition ) )
                # например для i(690->572) = = 17 / ( (17 + 0.5 + 100 + 30) * (1 + 0.28) )
                E_i = chain[index]
                E_f = chain[index + 1]
                conversion_ratio = indirect_transitions[f'indirect_transition_{E_i}_{E_f}'].conversion_ratio
                alpha_plus_one = indirect_transitions[f'indirect_transition_{E_i}_{E_f}'].alpha + 1
                conversion_ratio /= alpha_plus_one
                chain_conversion_ratio *= conversion_ratio

            # обновляем значения сечения согласно посчитанному chain_conversion_ratio
            for index in range(len(cross_section)):
                cross_section[index] *= chain_conversion_ratio

            if first_chain_in_chains:
                indirect_cross_section = cross_section.copy()
                first_chain_in_chains = False
            else:
                for index in range(len(cross_section)):
                    indirect_cross_section[index] += cross_section[index]

        if write_by_cains:
            path_to_chains_dir = path_to_calculation_dir + '/' + indirect_cross_sections_dir + '/' + chains_dir
            output_chain_filename = f'{energy_level}_indirect_cross_sections_{key}.txt'
            path_to_chain_output_file = path_to_chains_dir + '/' + output_chain_filename

            if not os.path.exists(path_to_chains_dir):
                os.mkdir(path_to_chains_dir)

            np.savetxt(path_to_chain_output_file, np.c_[Electron_projectile_energy, indirect_cross_section],
                       delimiter='\t',
                       header='Chain:\t' + str(chains[key]) + '\nElectron_projectile_energy, keV\tcross_section, cm^2')


    np.savetxt(path_to_output_file, np.c_[Electron_projectile_energy, indirect_cross_section], delimiter='\t',
               header='Electron_projectile_energy, keV\tcross_section, cm^2')
