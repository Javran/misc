#include <math.h>

#ifndef NEUMAIER_H
#define NEUMAIER_H
typedef struct {
    double sum, c;
} neumaier_sum;

void neumaier_sum_init(neumaier_sum*);
void neumaier_sum_add(neumaier_sum*, double);
double neumaier_sum_done(neumaier_sum*);
#endif /* NEUMAIER_H */
