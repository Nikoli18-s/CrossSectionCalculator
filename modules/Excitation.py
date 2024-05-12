class Excitation:
    
    def __init__(self, energy_level=0, spin_parity='', transition_multipole='', transition_probability=0):
        self.energy_level = energy_level
        self.spin_parity = spin_parity
        self.transition_multipole = transition_multipole
        self.transition_probability = transition_probability

    def __str__(self):
        print_str = ''
        print_str += f'{self.energy_level}\t\t|'
        print_str += f'{self.spin_parity}\t\t|'
        print_str += f'{self.transition_multipole}\t\t|'
        print_str += f'{self.transition_probability}'

        return print_str

    def set_from_str(self, string_data):
        parameters = []
        for p in string_data.split('|')[1:]:
            parameters.append(p.replace(' ', '').replace('\t', ''))

        self.energy_level = float(parameters[0])
        self.spin_parity = parameters[1]
        self.transition_multipole = parameters[2]
        self.transition_probability = float(parameters[3])
