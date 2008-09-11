#include <iostream>
#include <stdexcept>

int fib_rec(int n);
int fib_loop(int n);

int main (int argc, char const *argv[])
{
    int n;
    while (std::cin.good()) {
        std::cin >> n;
        if (std::cin.fail())
            return 0;
        std::cout << "loop: " << fib_loop(n) << std::endl;
        std::cout << "rec: " << fib_rec(n) << std::endl;
    }
    return 0;
}

int fib_rec(int n)
{
    if (n < 0) {
        throw std::runtime_error("no negative fibonacci numbers");
    }
    if (n < 2) {
        return 1;
    }
    return fib_rec(n - 1) + fib_rec(n - 2);
}

int fib_loop(int n)
{
    int tmp;
    int last = 1;
    int result = 1;
    if (n < 0) {
        throw std::runtime_error("no negative fibonacci numbers");
    }
    for (int i = 2; i <= n; i++) {
        tmp = result + last;
        last = result;
        result = tmp;
    }
    return result;
}