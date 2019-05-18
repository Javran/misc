#include <math.h>

typedef struct {
    double sum, c;
} neumaier_sum;

void neumaier_sum_init(neumaier_sum* obj) {
    obj->sum = obj->c = 0;
}

void neumaier_sum_add(neumaier_sum* obj, double x) {
    double t = obj->sum + x;
    if (fabs(t) > fabs(x)) {
        obj->c += (obj->sum - t) + x;
    } else {
        obj->c += (x - t) + obj->sum;
    }
    obj->sum = t;
}

double neumaier_sum_done(neumaier_sum* obj) {
    double ret = obj->sum + obj->c;
    neumaier_sum_init(obj);
    return ret;
}
