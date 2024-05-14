import os
import time
import numpy as np

import modules.RHFS_TK as rhfs_tk
from modules.GraphsPlotter import GraphsPlotter
from modules.Indirect_cross_section import calculate_indirect_cross_section
from modules.Direct_cross_section import calculate_direct_cross_section_parallel

path_to_calculations_dir = 'calculations'
standard_calculation_name = 'calculation'


class Calculation:

    def __init__(self, isotope, name, direct=True, indirect=True):
        self.isotope = isotope

        if name != '':
            self.name = name
        else:
            self.name = standard_calculation_name

        self._check_existence()

        self.direct = direct
        self.CPUs_number = 0
        self.nucl_points_number = 0
        self.electron_projectile_energy_minimum = 0
        self.electron_projectile_energy_maximum = 0
        self.electron_projectile_energy_points_number = 0

        self.indirect = indirect
        self.write_by_chain = False
        self.energies_list = []

        self.plot_graphs = False
        self.graph_plotter = None

    def __str__(self):
        print_str = 'Общие парметры расчета\n'
        print_str += f'Название расчета: {self.isotope.name}_{self.name}\n'
        print_str += f'{str(self.isotope)}\n'

        if self.direct:
            print_str += '\nПараметры расчета прямого сечения\n'
            print_str += f'Число процессоров для расчета: {self.CPUs_number}\n'
            print_str += f'Число точек для расчета по расстоянию от ядра: {self.nucl_points_number}\n'
            print_str += f'Минимальная кинетическая энергия налетающего электрона: {self.electron_projectile_energy_minimum} кэВ\n'
            print_str += f'Максимальная кинетическая энергия налетающего электрона: {self.electron_projectile_energy_maximum} кэВ\n'
            print_str += f'Число точек энергии электрона: {self.electron_projectile_energy_points_number}\n'

        if self.indirect:
            print_str += '\nПараметры расчета непрямого сечения\n'
            if self.write_by_chain:
                print_str += 'Запись сечений по цепочкам включена\n'
            else:
                print_str += 'Запись сечений по цепочкам отключена\n'
            print_str += 'Список энергий для расчета:'
            for E in self.energies_list:
                print_str += f' {E} кэВ,'
            print_str = print_str[:-1]

        if self.plot_graphs:
            print_str += f'\n{str(self.graph_plotter)}'

        return print_str

    def _check_existence(self):
        calculation_full_name = f'{self.isotope.name}_{self.name}'
        path_to_calculation_dir = path_to_calculations_dir + '/' + calculation_full_name

        if os.path.exists(path_to_calculation_dir):
            self.is_exists = True
        else:
            self.is_exists = False

    def set_direct_parameters(self, CPUs_number, nucl_points_number, electron_projectile_energy_minimum,
                              electron_projectile_energy_maximum, electron_projectile_energy_points_number):
        self.CPUs_number = CPUs_number
        self.nucl_points_number = nucl_points_number
        self.electron_projectile_energy_minimum = electron_projectile_energy_minimum
        self.electron_projectile_energy_maximum = electron_projectile_energy_maximum
        self.electron_projectile_energy_points_number = electron_projectile_energy_points_number

    def set_indirect_parameters(self, write_by_chain, energies_list):
        self.write_by_chain = write_by_chain
        self.energies_list = energies_list.copy()

    def set_plot_graphs_parameters(self, plot_graphs, plot_potential, plot_direct_cross_sections,
                                   plot_indirect_cross_sections):
        calculation_full_name = f'{self.isotope.name}_{self.name}'
        path_to_calculation_dir = path_to_calculations_dir + '/' + calculation_full_name

        self.plot_graphs = plot_graphs

        if self.plot_graphs:
            self.graph_plotter = GraphsPlotter(path_to_calculation_dir, plot_potential, plot_direct_cross_sections,
                                               plot_indirect_cross_sections)

    def run(self):
        calculation_full_name = f'{self.isotope.name}_{self.name}'
        path_to_calculation_dir = path_to_calculations_dir + '/' + calculation_full_name

        if not os.path.exists(path_to_calculation_dir):
            os.mkdir(path_to_calculation_dir)

        start_time = time.time()

        print('')
        rhfs_tk.run(self.isotope)

        if self.direct:
            electron_projectile_energy_values = np.linspace(self.electron_projectile_energy_minimum,
                                                            self.electron_projectile_energy_maximum,
                                                            self.electron_projectile_energy_points_number)
            print('')
            calculate_direct_cross_section_parallel(self.isotope,
                                                    electron_projectile_energy_values,
                                                    self.nucl_points_number,
                                                    self.CPUs_number,
                                                    path_to_calculation_dir)

        if self.indirect:
            for E in self.energies_list:
                print('')
                calculate_indirect_cross_section(self.isotope, E, self.write_by_chain, path_to_calculation_dir)

        if self.plot_graphs:
            self.graph_plotter.run()

        end_time = time.time()
        elapsed_time = end_time - start_time
        elapsed_time_min = int(elapsed_time//60)
        elapsed_time_sec = elapsed_time % 60

        print('')
        print('Расчет завершен успешно.')
        print(f'Время выпонения расчета: {elapsed_time_min} мин, {elapsed_time_sec} c.')

