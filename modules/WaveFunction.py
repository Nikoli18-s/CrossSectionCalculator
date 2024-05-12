import os
import string


class WaveFunction:

    def __init__(self, type, l, j, path_to_input_file):
        self.type = type  # type = intial / final
        self.l = l  # orbital momentum
        self.j = j  # full spin = l+s
        self.g = []  # g = [ ... ] list of wave functions
        self.f = []  # f = [ ... ]
        self.radius = []  # radius = [ ... ]

        self._set_values(path_to_input_file)

    def __str__(self):
        print_str = ''
        print_str += f'Тип волновой функции: {self.type}\n'
        print_str += f'Орбитальный момент: {self.l}\n'
        print_str += f'Полный спин: {self.j}\n'
        print_str += f'Число точек: {len(self.radius)}'

        return print_str

    def _set_values(self, path_to_input_file):
        input_filename = path_to_input_file.split('/')[-1]
        path_to_dir = '/'.join(path_to_input_file.split('/')[:-1], )

        if input_filename in os.listdir(path_to_dir):
            with open(path_to_input_file, 'r') as file:
                data = file.readlines()[1:]

            for line in data:

                values = line.strip().split(' ')
                while " " in values:
                    values.remove(" ")
                while "" in values:
                    values.remove("")

                if len(values) >= 3:
                    # Первый столбец в radius
                    self.radius.append(float(values[1]))
                    # Второй столбец в f
                    self.f.append(float(values[2]))
                    # Третий столбец в g
                    self.g.append(float(values[3]))
                else:
                    print(f'Файл с данными для волновой функции {path_to_input_file} не корректен.')
                    exit(13)
        else:
            print(f'Файл с данными для волновой функции {input_filename} не найден в директории {path_to_dir}.')
            exit(14)


