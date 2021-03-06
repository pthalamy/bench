# 1 "/seth/claycvs/svn/blinkyblocks/newcode/src/runtime/ensemble.bbh"
#ifndef _ENSEMBLE_H_
#define _ENSEMBLE_H_

#include "bb.h"
#include <stdint.h>

int16_t read_fcn_top (void);
void write_fcn_top (int16_t junk);

int16_t read_fcn_bottom (void);
void write_fcn_bottom (int16_t junk);

int16_t read_fcn_front (void);
void write_fcn_front (int16_t junk);

int16_t read_fcn_back (void);
void write_fcn_back (int16_t junk);

int16_t read_fcn_left (void);
void write_fcn_left (int16_t junk);

int16_t read_fcn_right (void);
void write_fcn_right (int16_t junk);

int16_t read_fcn_neighbors (void);
void write_fcn_neighbors (int16_t junk);

int16_t read_fcn_id (void);
void write_fcn_id (int16_t junk);

#endif /* _ENSEMBLE_H_ */
