import numpy as np
import scipy.special as sc
import scipy.constants as con
import sympy.physics.wigner as wigner

from modules.WaveFunction import WaveFunction

c = 1  # ???????????????????????????? в каких единицах тут у китайцев
"""
Auxilary function to get p from the kinetic energy E
"""


def p_from_E(E):
    return (np.sqrt(((E + con.m_e * c ** 2) ** 2 - con.m_e ** 2 * c ** 4) /
                    c ** 2))


"""
Auxilary function to calculate the quantum number eta, see eq (29) in Zhang
"""


def eta(l, j):
    return (l - j) * (2 * j + 1)


"""
First equation of (33) from Zhang, your radius should be from 0 to inf
One might consider using the asymptotic form...
"""


def M_E(lamb, radius, kappa_little, wave_function_initial, wave_function_final):
    g_i = np.asarray(wave_function_initial.g)
    f_i = np.asarray(wave_function_initial.f)
    g_f = np.asarray(wave_function_final.g)
    f_f = np.asarray(wave_function_final.f)
    part1 = (sc.hankel1(lamb, kappa_little * radius) * (g_i * g_f + f_i * f_f) * 
             radius ** 2)
    part2 = (-kappa_little / lamb * sc.hankel1(lamb - 1, kappa_little * np.asarray(radius)) * 
             (g_i * g_f + f_i * f_f) * np.asarray(radius) ** 3)
    return np.trapz(part1[1:] + part2[1:], radius[1:])


"""
Second equation of (33) from Zhang, your radius should be from 0 to inf
One might consider using the asymptotic form...
"""


def M_M(lamb, radius, kappa_little, wave_function_initial, wave_function_final):
    l_i = wave_function_initial.l
    j_i = wave_function_initial.j
    l_f = wave_function_final.l
    j_f = wave_function_final.j
    g_i = np.asarray(wave_function_initial.g)
    f_i = np.asarray(wave_function_initial.f)
    g_f = np.asarray(wave_function_final.g)
    f_f = np.asarray(wave_function_final.f)
    eta_i = eta(l_i, j_i)
    eta_f = eta(l_f, j_f)
    integrand = (sc.hankel1(lamb, kappa_little * np.asarray(radius)) * (g_i * f_f + g_f * f_i) * 
                 np.asarray(radius) ** 2)
    integral = np.trapz(integrand[1:], radius[1:])
    return (eta_i + eta_f)/lamb * integral


"""
Equation (31) from Zhang, cross section of electronic transition
"""


def sigma_E_summand(lamb: int, electron_projectile_energy: float, energy_level: float,
                    wave_function_initial: WaveFunction, wave_function_final: WaveFunction, B_Weisskopf: float,
                    radius: list) -> float:
    l_i = wave_function_initial.l
    j_i = wave_function_initial.j
    l_f = wave_function_final.l
    j_f = wave_function_final.j

    kappa_little = energy_level / c
    E_i = electron_projectile_energy
    E_f = E_i - energy_level
    p_i = p_from_E(E_i)
    p_f = p_from_E(E_f)
    factor = (8 * con.pi ** 2 / con.c ** 4 * p_f / p_i *
              (E_f + con.m_e * con.c ** 2) / p_f ** 2 *
              (E_i + con.m_e * con.c ** 2) / p_i ** 2)

    factor1 = (kappa_little ** (2 * lamb + 2) / (sc.factorial2(2 * lamb - 1) ** 2) *
               B_Weisskopf)
    factor2 = (((2 * l_i + 1) * (2 * l_f + 1) * (2 * j_i + 1) *
                (2 * j_f + 1)) / ((2 * lamb + 1) ** 2))
    factor3 = (wigner.wigner_3j(l_f, l_i, lamb, 0, 0, 0) ** 2 *
               wigner.wigner_6j(l_i, lamb, l_f, j_f, 1 / 2, j_i) ** 2 *
               np.abs(M_E(lamb, np.asarray(radius), kappa_little, wave_function_initial, wave_function_final)) ** 2)
    result = factor1 * factor2 * factor3
    return result * factor


"""
Equation (32) from Zhang, cross section of magnetic transition, sum is implemented in the main_direct_transitions.py
"""


def sigma_M_summand(lamb: int, electron_projectile_energy: float, energy_level: float,
                    wave_function_initial: WaveFunction, wave_function_final: WaveFunction, B_Weisskopf: float,
                    radius: list) -> float:
    l_i = wave_function_initial.l
    j_i = wave_function_initial.j
    l_f = wave_function_final.l
    j_f = wave_function_final.j

    kappa_little = energy_level / c
    E_i = electron_projectile_energy
    E_f = E_i - energy_level
    p_i = p_from_E(E_i)
    p_f = p_from_E(E_f)
    factor = (8 * con.pi ** 2 / con.c ** 4 * p_f / p_i *
               (E_f + con.m_e * con.c ** 2) / p_f ** 2 *
               (E_i + con.m_e * con.c ** 2) / p_i ** 2)

    if eta(l_i, j_i) < 0:
        l_i = 2 * j_i - l_i
    factor1 = (kappa_little ** (2 * lamb + 2) / (sc.factorial2(2 * lamb - 1) ** 2) *
               B_Weisskopf)
    factor2 = (((2 * l_i + 1) * (2 * l_f + 1) * (2 * j_i + 1) *
                (2 * j_f + 1)) / ((2 * lamb + 1) ** 2))
    factor3 = (wigner.wigner_3j(l_f, l_i, lamb, 0, 0, 0) ** 2 *
               wigner.wigner_6j(l_i, lamb, l_f, j_f, 1 / 2, j_i) ** 2 *
               np.abs(M_M(lamb, np.asarray(radius), kappa_little, wave_function_initial, wave_function_final)) ** 2)
    result = factor1 * factor2 * factor3
    return result * factor






