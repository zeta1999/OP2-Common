/*
 * compare.c
 *
 * Simple text file (ASCI) value comparison programe for comparing results
 * printed
 * from the airfoil code
 *
 * written by: Gihan R. Mudalige, (Started 04-04-2011)
 */

#include <math.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool essentiallyEqual(float a, float b, float epsilon)
{
  return fabs(a - b) <= ( (fabs(a) > fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}


int main(int argc, char *argv[]) {

  /* read in file1 from disk*/
  FILE *fp1, *fp2;
  int lines1, elem_dim1, lines2, elem_dim2;

  /**indicate your tolerance**/
  double epsi = 1e-14;//0.000000000001;

  int differ = 0;

  if (argc < 3) {
    printf("Usage: ./compare file1 file2\n");
    exit(-1);
  }

  if ((fp1 = fopen(argv[1], "r")) == NULL) {
    printf("can't open file %s\n", argv[1]);
    exit(-1);
  }

  if (fscanf(fp1, "%d %d \n", &lines1, &elem_dim1) < 0) {
    printf("error reading from %s\n", argv[1]);
    exit(-1);
  }

  if ((fp2 = fopen(argv[2], "r")) == NULL) {
    printf("can't open file %s\n", argv[2]);
    exit(-1);
  }

  if (fscanf(fp2, "%d %d \n", &lines2, &elem_dim2) < 0) {
    printf("error reading from %s\n", argv[2]);
    exit(-1);
  }

  printf("File 1 %s lines %d, File2 %s lines %d\n", argv[1], lines1, argv[2], lines2);

  if (lines1 != lines2 || elem_dim1 != elem_dim2) {
    printf(
        "File mismatch: number of lines or element dimensions not matching\n");
    exit(-1);
  }

  double values1[elem_dim1];
  double values2[elem_dim2];

  for (int n = 0; n < lines1; n++) {

    for (int d = 0; d < elem_dim1; d++) {
      fscanf(fp1, "%lf ", &values1[d]);
      fscanf(fp2, "%lf ", &values2[d]);
      /*if(essentiallyEqual(values1[d],values2[d], epsi)){
        printf("File mismatch: at line %d element %d: % .15E, % .15E \n",
                n + 2, d + 1, values1[d], values2[d]);
        differ = 1;
      }*/

      /*if (fabs(values1[d] - values2[d]) > epsi) {
        printf("Absolute diff - % .15E ",fabs(values1[d] - values2[d]));
        printf("File mismatch: at line %d element %d: % .15E, % .15E \n",
                n + 2, d + 1, values1[d], values2[d]);
        differ = 1;
        continue;
      }*/

      //if (fabs(values1[d] - values2[d]) > fabs(DBL_EPSILON)) {
        //printf("Absolute diff less than DBL_EPSILON: % .15E\n", DBL_EPSILON );
        //continue;
      //}

      double largest = (values1[d] > values2[d]) ? values1[d] : values2[d];

      //if ( values1[d] == 0 || values2[d] == 0 ||
      //     fabs(values1[d] - values2[d]) < fabs(DBL_EPSILON)) {
      //     continue;
      //} else

      if ((fabs(values1[d] - values2[d])/largest) > epsi) {
        if (fabs(values1[d] - values2[d]) > fabs(DBL_EPSILON)) {
          printf("Absolute diff % .15E, Relative diff - % .15E ",
            fabs(values1[d] - values2[d]),
            fabs(values1[d] - values2[d])/values1[d]);
          printf("File mismatch: at line %d element %d: % .15E, % .15E \n",
                  n + 2, d + 1, values1[d], values2[d]);
          differ = 1;
        }
        else
          printf("Absolute diff % .15E less than DBL_EPSILON: % .15E\n",
                  fabs(values1[d] - values2[d]), DBL_EPSILON);
      }

    }
    fscanf(fp1, "\n");
    fscanf(fp2, "\n");
  }

  fclose(fp1);
  fclose(fp2);

  if (differ == 0)
    printf("Files Identical\n");
  else
    printf("Files Differ\n");
}
