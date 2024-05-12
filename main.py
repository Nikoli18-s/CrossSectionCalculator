import os

from modules.Isotope import Isotope
from modules.Calculation import Calculation
from modules.Calculation import standard_calculation_name


if __name__ == '__main__':
    # Получение входных данных для выбранного изотопа
    isotope_name = input('Введите название изтопа в формате {название элемента}{массовое число} (например Kr83): ')
    isotope = Isotope(isotope_name)

    name = input(f'Введите название расчета (название по умолчанию: {standard_calculation_name}): ')
    calculation = Calculation(isotope, name)

    if calculation.is_exists:
        print(f'Расчет с названием {calculation.name} для изотопа {isotope.name} уже был проведен ранее.')
        answer = input('Перезаписать данные? (Y/N) ')
        if answer == 'N':
            print('Работа программы успешно завершена.')
            exit(0)

        answer = input('Провести расчет прямого сечения? (Y/N) ')
        if answer == 'N':
            calculation.direct = False

    if calculation.direct:
        max_number_of_CPUs = os.cpu_count()
        number_of_CPUs = input('\nВведите число ядер для расчета прямого сечения.\n'
                                f'Достуное число ядер: {max_number_of_CPUs}\n'
                                'Значение по умолчанию: 2\n')
        if not number_of_CPUs:
            print('Выбрано значение по умолчанию.\n')
            number_of_CPUs = 2
        else:
            number_of_CPUs = int(number_of_CPUs)
        if not 0 < number_of_CPUs <= max_number_of_CPUs:
            print('Введено недопустимое значение параметра.')
            print(f'Значение должно быть больше 0 и меньше или равно {max_number_of_CPUs}.')
            exit(0)

        nuclear_points_number = input('Введите количество точек для расчета по расстоянию от ядра.\n'
                                      'Значение по умолчанию: 500\n')
        if not nuclear_points_number:
            print('Выбрано значение по умолчанию.\n')
            nuclear_points_number = 500
        else:
            nuclear_points_number = int(nuclear_points_number)
        if not 0 < nuclear_points_number:
            print('Введено недопустимое значение параметра.')
            print('Значение должно быть больше 0.')
            exit(0)

        electron_projectile_energy_minimum = input(
            'Введите минимальную кинетическую энергию налетающего электрона в кэВ.\n')
        if not electron_projectile_energy_minimum:
            print('Значение не введено')
            exit(0)
        else:
            electron_projectile_energy_minimum = float(electron_projectile_energy_minimum)
        if not 0 < electron_projectile_energy_minimum:
            print('Введено недопустимое значение параметра.')
            print('Значение должно быть больше 0.')
            exit(0)

        electron_projectile_energy_maximum = input(
            'Введите максимальную кинетическую энергию налетающего электрона в кэВ.\n')
        if not electron_projectile_energy_maximum:
            print('Значение не введено.')
            exit(0)
        else:
            electron_projectile_energy_maximum = float(electron_projectile_energy_maximum)
        if not 0 < electron_projectile_energy_maximum:
            print('Введено недопустимое значение параметра.')
            print('Значение должно быть больше 0.')
            exit(0)

        electron_projectile_energy_points_number = input('Введите число точек энергий электрона.\n')
        if not electron_projectile_energy_points_number:
            print('Значение не введено.')
            exit(0)
        else:
            electron_projectile_energy_points_number = int(electron_projectile_energy_points_number)
        if not 0 < electron_projectile_energy_points_number:
            print('Введено недопустимое значение параметра.')
            print('Значение должно быть больше 0.')
            exit(0)

        calculation.set_direct_parameters(number_of_CPUs,
                                          nuclear_points_number,
                                          electron_projectile_energy_minimum,
                                          electron_projectile_energy_maximum,
                                          electron_projectile_energy_points_number)

    answer = input('Провести расчет непрямого сечения? (Y/N) ')
    if answer == 'N':
        calculation.indirect = False
    else:
        write_by_chain = input('Включить запись сечений по цепочкам? (Y/N) ')
        if write_by_chain == 'N':
            write_by_chain = False
        else:
            write_by_chain = True

        print('Введите значения энергий для расчета в кэВ.\n')
        energy_list = []
        while True:
            energy = input('')
            if not energy:
                break
            else:
                energy = float(energy)
            if not 0 < energy:
                print('Введено недопустимое значение параметра.')
                print('Значение должно быть больше 0.')
                exit(0)
            else:
                energy_list.append(energy)
        if len(energy_list) == 0:
            print('Значений не введено.')
            exit(0)

        calculation.set_indirect_parameters(write_by_chain, energy_list)

    print(calculation)

    calculation.run()
