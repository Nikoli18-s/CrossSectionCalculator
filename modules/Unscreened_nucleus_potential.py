import math


def unscreened_nucleus_potential(isotope, r):
    aB = 5.29177210903 * pow(10, -11)  # Bohr radius, m
    R0 = 1.2 * pow(isotope.A, 1.0 / 3.0) * pow(10, -15)  # radius of the nucleus, fm -> m as  1 fm = 1.0E-15 m
    x_R0 = R0 / aB  # radius of the uniformly charged sphere, dimensionless

    # in Tkalya mathematica
    e2 = 1 / 137.0359895
    eps0 = math.sqrt(e2)

    if r >= 0:
        if r >= x_R0:
            return - eps0 * isotope.Z / r  # в атомных единицах получаем эВ
        else:
            return - eps0 * (isotope.Z / (2 * x_R0)) * (3 - (r / x_R0) ** 2)
    else:
        print(f'Введен отрицателный радиус r = {r}.')
        return None
