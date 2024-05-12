import os
import time
import shutil
import subprocess

from modules.Isotope import path_to_isotopes_data

rhfs_tk_dir = 'RHFS_TK'
name_rhfs_tk_exe_file = 'rhfs-tk.exe'
path_to_rhfs_tk_exe = 'fortran/' + name_rhfs_tk_exe_file


def _file_for_isotope_exists(isotope):
    output_filename = f'{isotope.name}.rho'
    path_to_isotope_dir = path_to_isotopes_data + '/' + isotope.name

    if output_filename in os.listdir(path_to_isotope_dir):
        return True
    else:
        return False


def _create_input_file(isotope):
    input_filename = f'{isotope.name}.rhi'
    output_filename = f'{isotope.name}.rho'
    path_to_input_file = rhfs_tk_dir + '/' + input_filename

    with open(path_to_input_file, 'w') as file:
        file.write(f'{output_filename}\n')
        file.write('0.8 0.8 1D-6 1\n')
        file.write(f'{isotope.chem_name}\n')
        file.write(f'{isotope.Z} {isotope.A}\n')
        file.write(f'{len(isotope.shells)}\n')
        for shell in isotope.shells:
            file.write(f'{shell}\n')

    print(f'Входной файл для программы {name_rhfs_tk_exe_file} {input_filename} успешно создан.')


def _init_calculation_dir(isotope):
    if not os.path.exists(rhfs_tk_dir):
        os.mkdir(rhfs_tk_dir)

    shutil.copyfile(path_to_rhfs_tk_exe, rhfs_tk_dir + '/' + name_rhfs_tk_exe_file)
    _create_input_file(isotope)


def _run_calculation(isotope):
    os.chdir(rhfs_tk_dir)

    process = subprocess.run(name_rhfs_tk_exe_file, stdout=subprocess.DEVNULL, timeout=10)

    err_code = process.returncode
    if err_code == 0:
        print(f'Расчет энергий связи электронов для изотопа {isotope.name} успешно завершен.')
    else:
        print(f'Расчет энергий связи электронов для изотопа {isotope.name} завершен с ошибкой.')
        print(f'Код ошибки: {err_code}.')
        exit(6)

    os.chdir('../')


def _clear_calculation_dir(isotope):
    output_filename = f'{isotope.name}.rho'
    path_to_isotope_dir = path_to_isotopes_data + '/' + isotope.name
    path_to_isotope_output_file = path_to_isotope_dir + '/' + output_filename
    path_to_output_file = rhfs_tk_dir + '/' + output_filename

    shutil.copyfile(path_to_output_file, path_to_isotope_output_file)

    for file in os.listdir(rhfs_tk_dir):
        os.remove(rhfs_tk_dir + '/' + file)
        time.sleep(0.1)
    os.rmdir(rhfs_tk_dir)


def run(isotope):
    if not _file_for_isotope_exists(isotope):
        _init_calculation_dir(isotope)
        time.sleep(0.1)
        _run_calculation(isotope)
        time.sleep(0.1)
        _clear_calculation_dir(isotope)
    else:
        print(f'Расчет энергий связи электронов для изотопа {isotope.name} был выполнен ранее.')
