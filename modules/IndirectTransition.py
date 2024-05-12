class IndirectTransition:

    def __init__(self, initial_state, final_state, conversion_ratio, alpha):
        self.initial_state = initial_state
        self.final_state = final_state
        self.conversion_ratio = conversion_ratio
        self.alpha = alpha

    def __str__(self):
        print_str = ''
        print_str += f'Переход: {self.initial_state} кэВ --> {self.final_state} кэВ\n'
        print_str += f'Коэффициент конверсии: {self.conversion_ratio}\n'
        print_str += f'Коэффициент alpha: {self.alpha}'

        return print_str
