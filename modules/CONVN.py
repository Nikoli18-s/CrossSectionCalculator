import os
import time
import shutil
import subprocess

from modules.Isotope import path_to_isotopes_data

convn_dir = 'CONVN'
name_convn_exe_file = 'convn.exe'
path_to_convn_exe = 'fortran/' + name_convn_exe_file


def _create_input_file(isotope, excitation, points_number, path_to_work_dir):
    rhfs_tk_output_filename = f'{isotope.name}.rho'
    input_filename = f'{isotope.name}.cni'
    path_to_input_file = path_to_work_dir + '/' + input_filename

    transition_multipole = isotope.excitations[excitation].transition_multipole
    multipole_sign = ''
    multipole_order = 0
    if transition_multipole.startswith('M'):
        multipole_sign = '-'
        multipole_order = transition_multipole[-1]
    elif transition_multipole.startswith('E'):
        multipole_sign = '+'
        multipole_order = transition_multipole[-1]
    else:
        print(f'Недопустимое значение мультипольности: {transition_multipole}.')
        print('Допустимые значения: M1, M2, E1, E2.')
        exit(7)

    energy_level = isotope.excitations[excitation].energy_level

    with open(path_to_input_file, 'w') as file:
        file.write(f'{rhfs_tk_output_filename}\n')
        file.write(f'{isotope.chem_name}\n')
        file.write('1.D-5\n')
        file.write(f'{multipole_sign}{multipole_order} 1\n')
        file.write(f'{len(isotope.shells)}\n')
        for shell in isotope.shells:
            file.write(f'{shell}\n')
        file.write('1\n')
        file.write(f'{energy_level}\n')
        file.write(f'{points_number}\n')

    print(f'Входной файл для программы {name_convn_exe_file} {path_to_input_file} успешно создан.')


def _init_calculation_dir(isotope, excitation, points_number, path_to_work_dir):
    rhfs_tk_output_filename = f'{isotope.name}.rho'
    path_to_convn_work_dir = path_to_work_dir + '/' + convn_dir
    path_to_isotope_rhfs_tk_file = path_to_isotopes_data + '/' + isotope.name + f'/{isotope.name}.rho'

    if not os.path.exists(path_to_convn_work_dir):
        os.mkdir(path_to_convn_work_dir)
        shutil.copyfile(path_to_isotope_rhfs_tk_file, path_to_convn_work_dir + '/' + rhfs_tk_output_filename)
        shutil.copyfile(path_to_convn_exe, path_to_convn_work_dir + '/' + name_convn_exe_file)

    _create_input_file(isotope, excitation, points_number, path_to_convn_work_dir)


def _run_calculation(isotope, excitation, path_to_work_dir):
    path_to_convn_work_dir = path_to_work_dir + '/' + convn_dir
    os.chdir(path_to_convn_work_dir)

    process = subprocess.run(name_convn_exe_file, stdout=subprocess.DEVNULL, timeout=10)

    err_code = process.returncode
    if err_code == 0:
        print(f'Расчет коэффициентов внутренней конверсии и волновой функции для изотопа {isotope.name} успешно завершен.')
        print(f'Энергетический уровень: {isotope.excitations[excitation].energy_level}')
        print(f'Мультипольность: {isotope.excitations[excitation].transition_multipole}')
    else:
        print(f'Расчет коэффициентов внутренней конверсии и волновой функции для изотопа {isotope.name} завершен с ошибкой.')
        print(f'Код ошибки: {err_code}.')
        exit(8)

    for part in path_to_convn_work_dir.split('/'):
        os.chdir('../')


def _clear_calculation_dir(isotope, path_to_work_dir):
    output_filename = f'{isotope.name}.cno'
    path_to_output_file = path_to_work_dir + '/' + convn_dir + '/' + output_filename
    path_to_isotope_output_file = path_to_work_dir + '/' + output_filename

    shutil.copyfile(path_to_output_file, path_to_isotope_output_file)

    # for file in os.listdir(path_to_work_dir + '/' + convn_dir):
    #     os.remove(path_to_work_dir + '/' + convn_dir + '/' + file)
    #     time.sleep(0.05)
    # os.rmdir(path_to_work_dir + '/' + convn_dir)


def run(isotope, excitation, points_number, path_to_work_dir):
    _init_calculation_dir(isotope, excitation, points_number, path_to_work_dir)
    time.sleep(0.01)
    _run_calculation(isotope, excitation, path_to_work_dir)
    time.sleep(0.01)
    _clear_calculation_dir(isotope, path_to_work_dir)
