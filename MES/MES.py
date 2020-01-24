import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import quad

# for this data we should expect exp(-1) function as a result
fa = lambda x: 2
fb = lambda x: -3
fc = lambda x: 1
f = lambda x: x**2
beta = 1
gamma = 2
uR = -1
N = 10


def l_v(v, start, end):
    return quad(lambda x: f(x) * v(x), start, end)[0] - gamma * v(0)


def b_u_v(u_der, v_der, u, v, start, end):
    return -beta * u(0) * v(0) - \
           quad(lambda x: fa(x) * u_der(x) * v_der(x), start, end)[0] + \
           quad(lambda x: fb(x) * u_der(x) * v(x), start, end)[0] + \
           quad(lambda x: fc(x) * u(x) * v(x), start, end)[0]


def e(i, n):
    return lambda x: 1.0 - abs(n * (x - i / n)) \
        if (1.0 - abs(n * (x - i / n)) >= 0) \
        else 0.0


def e_der(i, n):
    return lambda x: n if (i - 1) / n <= x < i / n \
        else -n if i / n <= x < (i + 1) / n \
        else 0.0


def main():
    zeros_matrix = np.zeros((N, N))

    for i in range(0, N):
        for j in range(0, N):
            if abs(i - j) > 1:
                zeros_matrix[i][j] = 0.0
                continue

            if abs(i - j) == 1:
                start = max(0.0, min(i, j) / N)
                end = min(1.0, max(i, j) / N)
            else:
                start = max(0.0, (i - 1) / N)
                end = min(1.0, (i + 1) / N)

            zeros_matrix[i][j] = b_u_v(e_der(j, N), e_der(i, N), e(j, N), e(i, N), start, end)

    right = np.zeros(N)

    for i in range(0, N):
        right[i] = l_v(e(i, N), max(0.0, (i - 1) / N), min(1.0, (i + 1) / N))

    shift = np.zeros(N)
    for i in range(0, N):
        shift[i] = b_u_v(lambda x: uR, e_der(i, N), lambda x: uR * x, e(i, N),
                         max(0.0, (i - 1) / N),
                         min(1.0, (i + 1) / N))

    right -= shift

    fu = np.linalg.solve(zeros_matrix, right)

    fux = np.arange(0.0, 1.0, 0.001)
    fuy = np.zeros(1000)
    for i in range(0, 1000):
        for j in range(0, N):
            fuy[i] += fu[j] * e(j, N)(fux[i])
        fuy[i] += fux[i] * uR

    print(fuy)
    plt.plot(fux, fuy)
    plt.show()


if __name__ == "__main__":
    main()
