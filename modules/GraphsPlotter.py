import os
import math
import numpy as np
import matplotlib.pyplot as plt

from modules.Total_potential import potentials_plot_dir, potentials_dir
from modules.Direct_cross_section import direct_cross_sections_dir
from modules.Indirect_cross_section import indirect_cross_sections_dir

path_to_plots_dir = 'plots'
files_format = 'pdf'

plt.rcParams.update({"text.usetex": True})
plt.rcParams['figure.dpi'] = "300"
plt.style.use('physrev.mplstyle')
plt.rc('text.latex', preamble=r'\usepackage{amsmath}')


class GraphsPlotter:

    def __init__(self, path_to_calculation_dir, plot_potential, plot_direct_cross_sections,
                 plot_indirect_cross_sections):
        self.path_to_calculation_dir = path_to_calculation_dir

        self.plot_potential = plot_potential
        self.plot_direct_cross_sections = plot_direct_cross_sections
        self.plot_indirect_cross_sections = plot_indirect_cross_sections

    def __str__(self):
        print_str = ''

        if self.plot_potential or self.plot_direct_cross_sections or self.plot_indirect_cross_sections:
            print_str += 'Будут построены графики для '
            if self.plot_potential:
                print_str += 'потенциалов, '
            if self.plot_direct_cross_sections:
                print_str += 'прямого сечения рассеяния, '
            if self.plot_indirect_cross_sections:
                print_str += 'непрямого сечения рассеяния.'

        return print_str

    def _plot_potentials(self):
        full_path_to_plots_dir = self.path_to_calculation_dir + '/' + path_to_plots_dir
        path_to_potentials_plot_dir = self.path_to_calculation_dir + '/' + potentials_plot_dir

        if not os.path.exists(path_to_potentials_plot_dir):
            print('Дирректория с данными для построения графиков потенциалов не найдена.')
            return None

        filenames_pairs_list = []
        excitations_labels = []
        for file in os.listdir(path_to_potentials_plot_dir):
            filenames_pair = []
            if 'electron_shell_potential' in file:
                excitations_label = '_'.join(file[:-4].split('_')[-4:])
                excitations_labels.append(excitations_label)
                filenames_pair.append(file)

                for second_file in os.listdir(path_to_potentials_plot_dir):
                    if excitations_label in second_file and 'unscreened_nucleus_potential' in second_file:
                        filenames_pair.append(second_file)

                filenames_pairs_list.append(filenames_pair)

        for filenames_pair in filenames_pairs_list:
            electron_shell_potential_data = np.genfromtxt(path_to_potentials_plot_dir + '/' + filenames_pair[0],
                                                          skip_header=1, delimiter='\t')
            unscreened_nucleus_potential_data = np.genfromtxt(path_to_potentials_plot_dir + '/' + filenames_pair[1],
                                                              skip_header=1, delimiter='\t')

            r_electron_shell_potential = electron_shell_potential_data[:, 0]
            val_electron_shell_potential = electron_shell_potential_data[:, 1]
            r_unscreened_nucleus_potential = unscreened_nucleus_potential_data[:, 0]
            val_unscreened_nucleus_potential = unscreened_nucleus_potential_data[:, 1]

            energy_level = filenames_pair[0].split('_')[7]
            transition_multipole = filenames_pair[0].split('_')[8].split('.')[0]

            plot_filename = f'potentials_{energy_level}_{transition_multipole}'
            path_to_plot_file = full_path_to_plots_dir + '/' + plot_filename + '.' + files_format

            fig, ax = plt.subplots()

            ax.plot(r_electron_shell_potential, val_electron_shell_potential,
                    label=r'Electron shell potential', color='#377eb8')
            ax.plot(r_unscreened_nucleus_potential, val_unscreened_nucleus_potential,
                    label=r'Unscreened nucleus potential', color='#e41a1c')

            legend_title = f'{energy_level} keV, {transition_multipole}'
            ax.legend(title=legend_title, title_fontsize='6', fontsize='6', loc='upper left')

            ax.set(xlabel=r'Radius, $x=r/a_B$', ylabel='Potential (eV)', xlim=[0.0001, 10], ylim=[-1e6, 0])

            plt.yscale('symlog')
            plt.xscale('log')

            plt.savefig(path_to_plot_file)

    def _plot_direct_cross_sections(self):
        full_path_to_plots_dir = self.path_to_calculation_dir + '/' + path_to_plots_dir
        path_to_direct_cross_section_dir = self.path_to_calculation_dir + '/' + direct_cross_sections_dir

        if not os.path.exists(path_to_direct_cross_section_dir):
            print('Дирректория с данными для построения графиков прямых сечений рассеяния не найдена.')
            return None

        for file in os.listdir(path_to_direct_cross_section_dir):
            if 'cross_section' in file:
                energy_level = float(file[:-4].split('_')[-2])
                transition_multipole = file[:-4].split('_')[-1]
                plot_filename = file.replace('.txt', '.' + files_format)
                path_to_plot_file = full_path_to_plots_dir + '/' + plot_filename

                direct_cross_section_data = np.genfromtxt(path_to_direct_cross_section_dir + '/' + file,
                                                          skip_header=1, delimiter='\t')

                energies = direct_cross_section_data[:, 0]
                direct_cross_sections = direct_cross_section_data[:, 1]

                right_x_limit = max(energies)
                left_x_limit = 1

                max_direct_cross_section_value = max(direct_cross_sections)
                exponent_top = int(round(math.log10(abs(max_direct_cross_section_value))))
                last_direct_cross_section_value = direct_cross_sections[-1]
                exponent_bottom = int(round(math.log10(abs(last_direct_cross_section_value))))

                top_y_limit = float(f"{5.0}E{exponent_top + 1}")
                bottom_y_limit = float(f"{1.0}E{exponent_bottom}")

                fig, ax = plt.subplots()

                ax.plot(energies, direct_cross_sections,
                        label=f'Energy level {energy_level} keV, {transition_multipole}', color='black')

                ax.set(xlabel=r'Electron energy, keV', ylabel='Direct cross section (cm$^2$)',
                       xlim=[left_x_limit, right_x_limit], ylim=[bottom_y_limit, top_y_limit])

                ax.legend(title_fontsize='6', fontsize='6', loc='upper left')

                plt.yscale('log')
                plt.xscale('log')

                plt.savefig(path_to_plot_file)

    def _plot_indirect_cross_sections(self):
        full_path_to_plots_dir = self.path_to_calculation_dir + '/' + path_to_plots_dir
        path_to_indirect_cross_section_dir = self.path_to_calculation_dir + '/' + indirect_cross_sections_dir

        if not os.path.exists(path_to_indirect_cross_section_dir):
            print('Дирректория с данными для построения графиков непрямых сечений рассеяния не найдена.')
            return None

        for file in os.listdir(path_to_indirect_cross_section_dir):
            if 'indirect_cross_section' in file:
                energy_level = float(file[:-4].split('_')[-1])
                plot_filename = file.replace('.txt', '.' + files_format)
                path_to_plot_file = full_path_to_plots_dir + '/' + plot_filename

                indirect_cross_section_data = np.genfromtxt(path_to_indirect_cross_section_dir + '/' + file,
                                                            skip_header=1, delimiter='\t')

                energies = indirect_cross_section_data[:, 0]
                indirect_cross_sections = indirect_cross_section_data[:, 1]

                right_x_limit = max(energies)
                left_x_limit = 1

                max_direct_cross_section_value = max(indirect_cross_sections)
                exponent_top = int(round(math.log10(abs(max_direct_cross_section_value))))
                last_direct_cross_section_value = indirect_cross_sections[-1]
                exponent_bottom = int(round(math.log10(abs(last_direct_cross_section_value))))

                top_y_limit = float(f"{5.0}E{exponent_top + 1}")
                bottom_y_limit = float(f"{1.0}E{exponent_bottom}")

                fig, ax = plt.subplots()

                ax.plot(energies, indirect_cross_sections, label=f'Energy level {energy_level} keV', color='black')

                ax.set(xlabel=r'Electron energy, keV', ylabel='Indirect cross section (cm$^2$)',
                   xlim=[left_x_limit, right_x_limit], ylim=[bottom_y_limit, top_y_limit])

                ax.legend(title_fontsize='6', fontsize='6', loc='upper left')

                plt.yscale('log')
                plt.xscale('log')

                plt.savefig(path_to_plot_file)

    def run(self):
        full_path_to_plots_dir = self.path_to_calculation_dir + '/' + path_to_plots_dir

        if not os.path.exists(full_path_to_plots_dir):
            os.mkdir(full_path_to_plots_dir)

        if self.plot_potential:
            self._plot_potentials()

        if self.plot_direct_cross_sections:
            self._plot_direct_cross_sections()

        if self.plot_indirect_cross_sections:
            self._plot_indirect_cross_sections()

        print('Графики для расчета успешно построены.')
