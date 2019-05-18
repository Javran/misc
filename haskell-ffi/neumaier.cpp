#include <cmath>

class NeumaierSum {
private:
    double sum, c;

public:
    NeumaierSum():sum(0),c(0) {
    }

    void add(double x) {
        double t = sum + x;
        if (abs(t) > abs(x)) {
            c += (sum - t) + x;
        } else {
            c += (x - t) + sum;
        }
        sum = t;
    }

    double done() {
        double ret = sum + c;
        sum = c = 0;
        return ret;
    }
};
