typedef int I;
typedef I FI(int);
typedef I *PI;
typedef PI FPI(void);
typedef FI *PFI;
typedef PI *PPI;
typedef PPI FPPI(void);
typedef FPI *PFPI;
typedef PFI *PPFI;
FI fi;
PI pi;
FPI fpi;
PFI pfi;
FPPI fppi;
PFPI pfpi;
PPFI ppfi;

int ai[5];
int *api[5];
int (*pai)[5];
int **appi[5];
int *(*papi)[5];
int (**ppai)[5];

int FtFI(FI f);

typedef PFI FPFI(void);
FPFI fpfi;
#ifdef ERROR1
int (*ffpi2)(void)(void);
#endif

typedef PI (*PAPI)[5];
typedef PAPI FPAPI;
FPAPI fpapi;

const int ci;
const int *pci;
int *const cpi;
const int f(const int *volatile fci_vpci);
