from pylab import *
from scipy.integrate import quad

# for this data we should expect exp(-1) function as a result
fa = lambda x: 0
fb = lambda x: 1
fc = lambda x: 2
f = lambda x: np.exp(-x)
beta = 1
gamma = 1
u1 = np.exp(-1)
N = 1000


def l_v(v, start, end):
    return quad(lambda x: f(x) * v(x), start, end)[0] - gamma * v(0)


def b_u_v(u_der, v_der, u, v, start, end):
    return -beta * u(0) * v(0) - \
           quad(lambda x: fa(x) * u_der(x) * v_der(x), start, end)[0] + \
           quad(lambda x: fb(x) * u_der(x) * v(x), start, end)[0] + \
           quad(lambda x: fc(x) * u(x) * v(x), start, end)[0]


def e(i, n):
    return lambda x: 1.0 - abs(n * (x - double(i) / n)) \
        if (1.0 - abs(n * (x - double(i) / n)) >= 0) \
        else 0.0


def e_der(i, n):
    return lambda x: n if (i - 1) / n <= x < i / n \
        else -n if i / n <= x < (i + 1) / n \
        else 0.0


def main():
    zeros_matrix = zeros((N, N))

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

            zeros_matrix[i][j] = b_u_v(e_der(i, N), e_der(j, N), e(i, N), e(j, N), start, end)

    right = zeros(N)

    for i in range(0, N):
        right[i] = l_v(e(i, N), max(0.0, (i - 1) / N), min(1.0, (i + 1) / N))

    shift = zeros(N)
    for i in range(0, N):
        shift[i] = b_u_v(lambda x: u1, e_der(i, N), lambda x: u1 * x, e(i, N),
                         max(0.0, (i - 1) / N),
                         min(1.0, (i + 1) / N))

    right -= shift

    fu = linalg.solve(zeros_matrix, right)

    fux = arange(0.0, 1.0, 0.01)
    fuy = zeros(100)
    for i in range(0, 100):
        for j in range(0, N):
            fuy[i] += fu[j] * e(j, N)(fux[i])
        fuy[i] += fux[i] * u1

    print(fuy)
    plot(fux, fuy)
    show()


if __name__ == "__main__":
    main()
