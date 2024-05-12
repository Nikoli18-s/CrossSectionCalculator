import os
import re

from modules.S_to_z import symbols_to_z_numbers
from modules.Excitation import Excitation

path_to_isotopes_data = 'isotopes_data'
shells_data_filename = 'shells_data.txt'
excitations_data_filename = 'excitations_data.txt'
initial_state_data_filename = 'initial_state_data.txt'


class Isotope:

    def __init__(self, isotope_name):
        self.name = isotope_name
        self.chem_name = ''
        self.Z = 0
        self.A = 0
        self.shells = []
        self.excitations = {}
        self.initial_state = ''

        self._check_isotope_dir()
        self._set_Z_A()
        self._read_shells_data()
        self._read_excitations_data()
        self._read_initial_state_data()

    def __str__(self):
        print_str = ''
        print_str += f'Название изотопа: {self.name}\n'
        print_str += f'Химический элемент: {self.chem_name}\n'
        print_str += f'Зарядовое число (Z): {self.Z}\n'
        print_str += f'Массовое число (A): {self.A}\n'
        print_str += 'Элетронные оболочки:\n'
        for shell in self.shells:
            print_str += f'\t{shell}\n'
        print_str += 'Возбужденные состояния:\n'
        for key in self.excitations.keys():
            print_str += f'\t{key}: \t{self.excitations[key]}\n'
        print_str += f'Начальное состояние: {self.initial_state}'

        return print_str

    def _check_isotope_dir(self):
        if self.name in os.listdir(path_to_isotopes_data):
            pass
        else:
            print(f'Директория с данными для изотопа {self.name} не найдена.')
            print('Доступные изотопы:')
            for isotope_name in os.listdir(path_to_isotopes_data):
                print(f'\t{isotope_name}')

            exit(1)

    def _set_Z_A(self):
        symbol = str(re.findall(r'[a-zA-Z]+', self.name)[0])
        A = int(re.findall(r'\d+', self.name)[0])

        if symbol in symbols_to_z_numbers.keys():
            self.chem_name = symbol
            self.Z = symbols_to_z_numbers[symbol]
            self.A = A
        else:
            print(f'Не найдено зарядовое число Z для элемента {symbol}.')
            exit(2)

    def _read_shells_data(self):
        path_to_isotope_dir = path_to_isotopes_data + '/' + self.name

        if shells_data_filename in os.listdir(path_to_isotope_dir):
            path_to_shells_data_file = path_to_isotope_dir + '/' + shells_data_filename

            with open(path_to_shells_data_file, 'r') as file:
                shells_data = file.readlines()

            for shell_data in shells_data:
                self.shells.append(shell_data.strip())
        else:
            print(f'Файл с данными об электронных оболочках для изотопа {self.name} не найден.')
            exit(3)

    def _read_excitations_data(self):
        path_to_isotope_dir = path_to_isotopes_data + '/' + self.name

        if excitations_data_filename in os.listdir(path_to_isotope_dir):
            path_to_excitations_data_file = path_to_isotope_dir + '/' + excitations_data_filename

            with open(path_to_excitations_data_file, 'r') as file:
                excitations_str_data = file.readlines()[2:-1]

            for excitation_str_data in excitations_str_data:
                excitation = Excitation()
                excitation.set_from_str(excitation_str_data.strip())

                data_for_key = excitation_str_data.split('|')[0].split('>')
                key_0 = data_for_key[0].replace(' ', '').replace('\t', '').replace('-', '')
                key_1 = data_for_key[1].replace(' ', '').replace('\t', '')
                key_2 = excitation.transition_multipole
                key = f'excitation_{key_0}_{key_1}_{key_2}'

                self.excitations[key] = excitation
        else:
            print(f'Файл с данными о возбужденных состояниях для изотопа {self.name} не найден.')
            exit(4)

    def _read_initial_state_data(self):
        path_to_isotope_dir = path_to_isotopes_data + '/' + self.name

        if initial_state_data_filename in os.listdir(path_to_isotope_dir):
            path_to_initial_state_data_file = path_to_isotope_dir + '/' + initial_state_data_filename

            with open(path_to_initial_state_data_file, 'r') as file:
                initial_state_data = file.readline().strip()

            self.initial_state = initial_state_data
        else:
            print(f'Файл с данными о начальном состоянии для изотопа {self.name} не найден.')
            exit(5)
