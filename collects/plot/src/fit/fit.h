/* $Id: fit.h,v 1.1 2003/08/15 22:17:49 cozmic Exp $ */

/* GNUPLOT - fit.h */

/*  NOTICE: Change of Copyright Status
 *
 *  The author of this module, Carsten Grammes, has expressed in
 *  personal email that he has no more interest in this code, and
 *  doesn't claim any copyright. He has agreed to put this module
 *  into the public domain.
 *
 *  Lars Hecking  15-02-1999
 */

/*
 *	Header file: public functions in fit.c
 *
 *
 *	Copyright of this module:   Carsten Grammes, 1993
 *      Experimental Physics, University of Saarbruecken, Germany
 *
 *	Internet address: cagr@rz.uni-sb.de
 *
 *	Permission to use, copy, and distribute this software and its
 *	documentation for any purpose with or without fee is hereby granted,
 *	provided that the above copyright notice appear in all copies and
 *	that both that copyright notice and this permission notice appear
 *	in supporting documentation.
 *
 *      This software is provided "as is" without express or implied warranty.
 */

#include <scheme.h>

double * do_fit(Scheme_Object * function,
		int n_values,
		double * x_values,
		double * y_values,
		double * z_values,
		double * errors,
		int n_parameters,
		double * parameters);


double get_rms();
double get_varience();
double * get_asym_error();
double * get_asym_error_percent();
     

