

quantum_numbers_dir = 'quantum_numbers'
numbers_file_for_1 = 'first_order_numbers.txt'
numbers_file_for_2 = 'second_order_numbers.txt'


def _get_parity(state):
    if '+' in state:
        return '+'
    elif '-' in state:
        return '-'
    else:
        return None


def _cut_parity_states_collection(J, collection):
    # эта функция отбирает из коллекции финальных орбитальных моментов те, которые лежат выше J
    # потому что из s-состояния электрон переходит только в те наборы орбитальных значений,
    # которые (l - 0) >= J
    # тут 0 это орбитальный момент электрона
    # для collection = [0, 2, 4, 6] и J = 2, функция возвращает [2, 4, 6]
    collection.sort()

    index = 0
    for num in collection:
        if num >= J:
            break
        else:
            index += 1

    if index < len(collection):
        return collection[index:]
    else:
        return None


def get_spin(state):
    result = 0
    parity = _get_parity(state)
    # Удаляем знак четности из строки состояния
    state_without_parity = state.replace(parity, '')
    if '/' in state_without_parity:
        # Разделяем строку на числитель и знаменатель и выполняем деление
        numerator, denominator = state_without_parity.split('/')
        if float(denominator.replace(' ', '')) != 0:
            result = float(numerator.replace(' ', '')) / float(denominator.replace(' ', ''))
        else:
            print(f'Деление на ноль: {state_without_parity}.')
            exit(10)
    else:
        print(f'Недопустимый формат состояния: {state_without_parity}.')
        print('Требуемый формат: {int}/{int}.')
        exit(11)

    return result


def _get_numbers_for_transition_multipole(transition_multipole):
    electron_initial_orbital_number_set = []
    electron_initial_spin_number_set = []

    if transition_multipole in ['E1', 'M1']:
        path_to_quantum_numbers_file = quantum_numbers_dir + '/' + numbers_file_for_1

        with open(path_to_quantum_numbers_file, 'r') as file:
            quantum_numbers_data = file.readlines()[1:]

        for quantum_numbers_line in quantum_numbers_data:
            electron_initial_orbital_number_set.append(float(quantum_numbers_line.split('\t')[0]))
            electron_initial_spin_number_set.append(float(quantum_numbers_line.split('\t')[1]))
    elif transition_multipole in ['E2', 'M2']:
        path_to_quantum_numbers_file = quantum_numbers_dir + '/' + numbers_file_for_2

        with open(path_to_quantum_numbers_file, 'r') as file:
            quantum_numbers_data = file.readlines()[1:]

        for quantum_numbers_line in quantum_numbers_data:
            electron_initial_orbital_number_set.append(float(quantum_numbers_line.split('\t')[0]))
            electron_initial_spin_number_set.append(float(quantum_numbers_line.split('\t')[1]))
    else:
        print(f'Недопустимое значение мультипольности: {transition_multipole}.')
        print('Допустимые значения: M1, M2, E1, E2.')
        exit(9)

    return electron_initial_orbital_number_set, electron_initial_spin_number_set


def get_permitted_quantum_numbers(initial_state, final_state, transition_multipole):

    # задаем сет первоначальных состояний электрона (в зависимости от мультипольности transition_multipole)
    electron_initial_orbital_number_set, electron_initial_spin_number_set = _get_numbers_for_transition_multipole(transition_multipole)

    # мультипольность перехода
    J = int(transition_multipole[1])

    # четность перехода
    # определим с помощью правил отбора разрешенные по четности финальные состояния электрона
    even_parity_states_collection = [0, 2, 4, 6]  # зададим только ближайшие 4 перехода ["s", "d", "g", "i"]
    odd_parity_states_collection = [1, 3, 5, 7]  # то есть ["p", "f", "h", "j"]

    if _get_parity(initial_state) == _get_parity(final_state):
        parity_states_collection = _cut_parity_states_collection(J, even_parity_states_collection)
    else:
        parity_states_collection = _cut_parity_states_collection(J, odd_parity_states_collection)

    permitted_quantum_numbers_set = []
    for index in range(len(electron_initial_orbital_number_set)):
        l_i = electron_initial_orbital_number_set[index]
        j_i = electron_initial_spin_number_set[index]

        for orbital_number in parity_states_collection:
            parallel_and_antiparallel_states = [orbital_number + 1/2, orbital_number - 1/2]  # здесь +-1/2 это спин электрона
            for full_spin in parallel_and_antiparallel_states:
                if (full_spin - j_i) <= J <= (full_spin + j_i):
                    l_f = orbital_number
                    j_f = full_spin
                    set = [0, 0, 0, 0]
                    set[0] = int(l_i)  # начальный орбитальный момент электрона
                    set[1] = j_i  # начальный полный спин (l+s) электрона
                    set[2] = int(l_f)  # конечный орбитальный момент электрона
                    set[3] = j_f  # конечный полный спин (l+s) электрона
                    permitted_quantum_numbers_set.append(set)

    return permitted_quantum_numbers_set


