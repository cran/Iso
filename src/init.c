#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(pava)(double *, double *, int *, int *);
extern void F77_NAME(smooth)(int *, int *, int *, double *,
                     double *, double *, double *, int *, int *,
                     double *, double *, double *, int *, double *,
                     double *, double *, double *, int *);
extern void F77_NAME(ufit)(double *xk, double *wk, double *xmode, double *x,
	       	double *w, double *mse, double *x1, double *w1, double *x2,
	       	double *w2, int *ind, int *kt, int *n, int *goof);

/*
   Note that the unimode routine does not feature in the foregoing
   since unimode is called only by ufit and never called directly
   by .Fortran().
*/

static const R_FortranMethodDef FortranEntries[] = {
    {"pava",   (DL_FUNC) &F77_NAME(pava),    4},
    {"smooth", (DL_FUNC) &F77_NAME(smooth), 18},
    {"ufit",   (DL_FUNC) &F77_NAME(ufit),   14},
    {NULL, NULL, 0}
};

void R_init_Iso(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
