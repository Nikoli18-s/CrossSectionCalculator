import os
import time
import shutil
import subprocess

from modules.WaveFunction import WaveFunction

radial_dir = 'RADIAL'
name_radial_exe_file = 'radial.exe'
path_to_radial_exe = 'fortran/' + name_radial_exe_file
eps = 1.0e-13


def _create_input_file(coeff_Z, coeff_ZS, coeff_A, E, l, j, path_to_work_dir):
    input_filename = 'radial_input.txt'
    path_to_input_file = path_to_work_dir + '/' + input_filename

    kappa_big = int((l - j) * (2 * j + 1))

    with open(path_to_input_file, 'w') as file:
        file.write('%.6f %.6f %.6f\n' % (coeff_Z, coeff_ZS, coeff_A))
        file.write('4\n')
        file.write(f'{E} {kappa_big} {eps}\n')

    # print(f'Входной файл для программы {name_radial_exe_file} {path_to_input_file} успешно создан.')


def _init_calculation_dir(coeff_Z, coeff_ZS, coeff_A, E, l, j, path_to_work_dir):
    path_to_radial_work_dir = path_to_work_dir + '/' + radial_dir

    if not os.path.exists(path_to_radial_work_dir):
        os.mkdir(path_to_radial_work_dir)
        shutil.copyfile(path_to_radial_exe, path_to_radial_work_dir + '/' + name_radial_exe_file)

    _create_input_file(coeff_Z, coeff_ZS, coeff_A, E, l, j, path_to_radial_work_dir)


def _run_calculation(E, l, j, path_to_work_dir):
    path_to_radial_work_dir = path_to_work_dir + '/' + radial_dir
    os.chdir(path_to_radial_work_dir)

    process = subprocess.run(name_radial_exe_file, stdout=subprocess.DEVNULL, timeout=10)

    err_code = process.returncode
    if err_code == 0:
        # print(f'Расчет волновой функции для электрона с энргией {E} кэВ успешно завершен.')
        # print(f'Орбитальный момент: {l}')
        # print(f'Полный спин: {j}')
        pass
    else:
        print(f'Расчет волновой функции для эенргии электрона с энргией {E} завершен с ошибкой.')
        print(f'Код ошибки: {err_code}.')
        exit(12)

    for part in path_to_radial_work_dir.split('/'):
        os.chdir('../')


def _clear_calculation_dir(type, l, j, path_to_work_dir):
    output_filename = 'WAVES.DAT'
    path_to_output_file = path_to_work_dir + '/' + radial_dir + '/' + output_filename

    wave_function = WaveFunction(type, l, j, path_to_output_file)

    # for file in os.listdir(path_to_work_dir + '/' + radial_dir):
    #     os.remove(path_to_work_dir + '/' + radial_dir + '/' + file)
    #     time.sleep(0.05)
    # os.rmdir(path_to_work_dir + '/' + radial_dir)

    return wave_function


def run(coeff_Z, coeff_ZS, coeff_A, type, E, l, j, path_to_work_dir):
    _init_calculation_dir(coeff_Z, coeff_ZS, coeff_A, E, l, j, path_to_work_dir)
    time.sleep(0.01)
    _run_calculation(E, l, j, path_to_work_dir)
    time.sleep(0.01)
    wave_function = _clear_calculation_dir(type, l, j, path_to_work_dir)

    return wave_function
