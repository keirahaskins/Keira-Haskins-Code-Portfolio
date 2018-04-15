#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <sys/sysinfo.h>
#include <mm_malloc.h>
#include <omp.h>
#include <mpi.h>
#include "emmintrin.h"
#include "immintrin.h"

/**
* File:       Dense Matrix Matrix Multiplication - Parallel
* Author:     Keira E. Haskins
* Date:       February 25, 2017
*
* Synopsis:   Parallel implementation of various dense matrix matrix
*             multiplication algorithms. As of the writing of this
*             there are now three different algorithms that are being
*             implemented and tested. The base case being a very
*             typical/bruteforce algorithm with O(n^3) runtime.
*             The second algorithm transposes the second
*             matrix B such that it can be accessed row-wise, from
*             there it uses a temporary array/storage space in order
*             to access a specified number of elements sequentially,
*             (loop unrolling), in order to try and take advantage
*             of the cache. This is done by four elements at a time.
*
*             Lastly, and fastest overall by a fairly large
*             margin is an algorithm that as in the previous algorithm,
*             first transposes B using a parallel transpose in order
*             to take advantage of memory access/cache size, and then
*             uses memcpy() to copy entire rows of both A and B between
*             the i and j for loops, where the whole algorithm uses i, j,
*             and k indexed for loops. However in this case we load
*             entire rows into memory at each step and then process
*             them entirely before proceeding. This is also much more
*             efficient as we only load the next A row in each iteration
*             of the outer loop, thereby increasing the number of
*             cache hits.
*/

#define DEFAULT_DIMENSION 1000
#define MIN_DIMENSION 5
#define MAX_DIMENSION 15000
#define NUM_MULT_FUNC 2

/* Changed date type to unsigned int in order to ensure
   that algorithm maintains accuracy for much large matrices. */
unsigned int **A;
unsigned int **B;
unsigned int **C;
unsigned int **C_verify;

/**
 * Take the dimension given, either default or
 * specified and allocate three matrices of
 * the exact size needed for the analysis.
 *
 * parameters: unsigned int dimension -> square matrices as of now
 * returns:    unsigned short         -> whether or not memory
 *                                       allocation was successful.
 */
unsigned short allocate_matrices(unsigned int dimension)
{
    unsigned int i;
    if (A == NULL && B == NULL && C == NULL && C_verify == NULL)
    {
        unsigned int *contig_A;
        unsigned int *contig_B;
        unsigned int *contig_C;
        unsigned int *contig_C_verify;

        contig_A        = (unsigned int *) malloc(
                                           dimension * dimension * sizeof(unsigned int));
        contig_B        = (unsigned int *) malloc(
                                           dimension * dimension * sizeof(unsigned int));
        contig_C        = (unsigned int *) malloc(
                                           dimension * dimension * sizeof(unsigned int));
        contig_C_verify = (unsigned int *) malloc(
                                           dimension * dimension * sizeof(unsigned int));

        A               = (unsigned int **) malloc(dimension * sizeof(unsigned int *));
        B               = (unsigned int **) malloc(dimension * sizeof(unsigned int *));
        C               = (unsigned int **) malloc(dimension * sizeof(unsigned int *));
        C_verify        = (unsigned int **) malloc(dimension * sizeof(unsigned int *));

        A[0]            = contig_A;
        B[0]            = contig_B;
        C[0]            = contig_C;
        C_verify[0]     = contig_C_verify;

        for (i = 1; i < dimension; i++)
        {
            A[i]        =        A[i - 1] + dimension;
            B[i]        =        B[i - 1] + dimension;
            C[i]        =        C[i - 1] + dimension;
            C_verify[i] = C_verify[i - 1] + dimension;

        }
    }
    else
    {
        printf("Was unable to allocate memory\n");
        return 0;
    }
    return 1;
}

/**
 * Frees all memory from the heap after running algorithm
 * analysis.
 *
 *
 * parameters: unsigned int dimension
 * returns:    void
 *
 */
void deallocate_matrices(void)
{
    if (A != NULL || B != NULL || C != NULL || C_verify != NULL)
    {
        if (A != NULL)
        {
            free(A[0]);
            free(A);
            A = NULL;
        }
        if (B != NULL)
        {
            free(B[0]);
            free(B);
            B = NULL;
        }
        if (C != NULL)
        {
            free(C[0]);
            free(C);
            C = NULL;
        }
        if (C_verify != NULL)
        {
            free(C_verify[0]);
            free(C_verify);
            C = NULL;
        }
    }
}

/**
 * Generates two random dense matrices with
 * values between 4 and 24. Parallel version.
 *
 *
 * parameters: unsigned int dimension, unsigned short thread_count
 * returns:    void
 *
 *
 */
void generateMatrices(unsigned int dimension)
{
    unsigned int random;
    srand(time(NULL));

    for (unsigned int i = 0; i < dimension; i++)
    {
        for (unsigned int j = 0; j < dimension; j++)
        {
            random  = ((rand() % 20) + 5);
            A[i][j] = random;

            random  = ((rand() % 20) + 5);
            B[i][j] = random;
            C[i][j] = 0;
            C_verify[i][j] = 0;
        }
    }
}

/**
 * Procedure to transpose the matrix B, leading
 * to row-wise access instead of column-wise.
 *
 * parameters: unsigned int dimension
 * returns:    void
 *
 *
 */
void transpose(unsigned int dimension)
{
    unsigned int temp;
    for (unsigned int i = 0; i < dimension; i++)
    {
        for (unsigned int j = i + 1; j < dimension; j++)
        {
            temp    = B[i][j];
            B[i][j] = B[j][i];
            B[j][i] = temp;
        }
    }
}

/**
 * Procedure to transpose the matrix B, leading
 * to row-wise access instead of column-wise.
 * (Parallel simple version)
 *
 * parameters: unsigned int dimension, unsigned short thread_count
 * returns:    void
 *
 *
 */
void transposeParallel(unsigned int dimension,
    unsigned short thread_count)
{
    #pragma omp parallel num_threads(thread_count)
    {
        unsigned int temp;

        #pragma omp for
        for (unsigned int i = 0; i < dimension; i++)
        {
            for (unsigned int j = i + 1; j < dimension; j++)
            {
                temp    = B[i][j];
                B[i][j] = B[j][i];
                B[j][i] = temp;
            }
        }
    }
}

/**
 * Base-case parallel algorithm. Naive or brute force
 * method, intentionally kept as such in order to
 * test for performance improvement in other algorithms.
 *
 * parameters: unsigned int dimension, unsigned short thread_count
 * returns:    void
 *
 *
 */
void generalSquareMatrixMultiplySerial(unsigned int dimension)
{
    for (unsigned int i = 0; i < dimension; i++)
    {
        for (unsigned int j = 0; j < dimension; j++)
        {
            for (unsigned int k = 0; k < dimension; k++)
            {
                C_verify[i][j] += A[i][k] * B[k][j];
            }
        }
    }
}

/**
 * Serial implementation of row copy multiplication algorithm.
 * Used to test for matrix correctness. (Not included in runtime).
 * Used in place of general matrix multiply to ensure fastest
 * serial runtime for correctness verification.
 *
 * parameters: unsigned int dimension
 * returns:    void
 *
 *
 */
void rowCopyMatrixMatrixMultSerial(unsigned int dimension)
{
    unsigned int iRowA[dimension];
    unsigned int iRowB[dimension];
    unsigned int sum = 0;
    unsigned int i, j, k;

    transpose(dimension);

    for (i = 0; i < dimension; i++)
    {
        memcpy(iRowA, A[i], sizeof(iRowA));
        for (j = 0; j < dimension; j++)
        {
            memcpy(iRowB, B[j], sizeof(iRowB));
            for (k = 0; k < dimension; k++)
            {
                sum += iRowA[k] * iRowB[k];
            }
            C_verify[i][j] = sum;
            sum = 0;
        }
    }
    transpose(dimension);
}

/**
 * Base-case parallel algorithm. Naive or brute force
 * method, intentionally kept as such in order to
 * test for performance improvement in other algorithms.
 *
 * parameters: unsigned int dimension, unsigned short thread_count
 * returns:    void
 *
 *
 */
void generalSquareMatrixMultiply(unsigned int dimension,
    unsigned short thread_count)
{
    #pragma omp parallel num_threads (thread_count)
    {
        #pragma omp for
        for (unsigned int i = 0; i < dimension; i++)
        {
            for (unsigned int j = 0; j < dimension; j++)
            {
                for (unsigned int k = 0; k < dimension; k++)
                {
                    C[i][j] += A[i][k] * B[k][j];
                }
            }
        }
    }
}

/**
 * Fastest of the three loop unrolled algorithms, runs about
 * 16 times faster than the base/brute-force algorithm.
 * Uses a small, temporary array to store each calculation
 * in the inner for loop, as well as a transpose function
 * to change how B is accessed.
 *
 * parameters: unsigned int dimension, unsigned short thread_count
 * returns:    void
 *
 */
void sequentialMatrixMultiplicationFour(unsigned int dimension)
{
    //unsigned int i, j, k;
    transpose(dimension);

    // #pragma omp parallel num_threads(thread_count)
    {

        unsigned int c[4];
        unsigned int sum = 0;

        // #pragma omp for
        for (unsigned int i = 0; i < dimension; i++)
        {
            for (unsigned int j = 0; j < dimension; j++)
            {
                sum  = 0;
                c[0] = 0;
                c[1] = 0;
                c[2] = 0;
                c[3] = 0;
                for (unsigned int k = 0; k < dimension; k += 4)
                {
                    c[0] += A[i][k+0] * B[i][k+0];
                    c[1] += A[i][k+1] * B[i][k+1];
                    c[2] += A[i][k+2] * B[i][k+2];
                    c[3] += A[i][k+3] * B[i][k+3];
                }
                sum    += c[0];
                sum    += c[1];
                sum    += c[2];
                sum    += c[3];
                C[i][j] = sum;
            }
        }
    }
    transpose(dimension);
}

/**
 * Fastest parallel implementation algorithm, copies
 * entire rows from A and B before computing.
 * Uses parallel transpose function to make accessing of
 * B row-access.
 *
 * parameters: unsigned int dimension,
 *             unsigned short thread_count,
 *             unsigned int **recv_buff_A,
 *             unsigned int **recv_buff_C,
 *             int          size_rank
 * returns:    unsigned int **recv_buff_C
 */
unsigned int ** rowCopyMatrixMatrixMultParallel(unsigned int   dimension,
                                                unsigned short thread_count,
                                                unsigned int   **recv_buff_A,
                                                unsigned int   **recv_buff_C,
                                                int            rank,
                                                int            size_rank)
{
    if (rank == 0)
    {
        transposeParallel(dimension, thread_count);
    }

    #pragma omp parallel num_threads (thread_count)
    {
        unsigned int iRowA1[dimension];
        unsigned int iRowA2[dimension];
        unsigned int iRowA3[dimension];
        unsigned int iRowA4[dimension];

        unsigned int iRowB1[dimension];
        unsigned int iRowB2[dimension];
        unsigned int iRowB3[dimension];
        unsigned int iRowB4[dimension];

        unsigned int sum[16];

        #pragma omp for
        for (unsigned int i = 0; i < dimension; i+=4)
        {
            memcpy(iRowA1, recv_buff_A[i+0], sizeof(iRowA1));
            memcpy(iRowA2, recv_buff_A[i+1], sizeof(iRowA2));
            memcpy(iRowA3, recv_buff_A[i+2], sizeof(iRowA3));
            memcpy(iRowA4, recv_buff_A[i+3], sizeof(iRowA4));
            for (unsigned int j = 0; j < dimension / size_rank; j+=4)
            {
                sum[0]  = 0;
                sum[1]  = 0;
                sum[2]  = 0;
                sum[3]  = 0;

                sum[4]  = 0;
                sum[5]  = 0;
                sum[6]  = 0;
                sum[7]  = 0;

                sum[8]  = 0;
                sum[9]  = 0;
                sum[10] = 0;
                sum[11] = 0;

                sum[12] = 0;
                sum[13] = 0;
                sum[14] = 0;
                sum[15] = 0;

                memcpy(iRowB1, B[j+0], sizeof(iRowB1));
                memcpy(iRowB2, B[j+1], sizeof(iRowB2));
                memcpy(iRowB3, B[j+2], sizeof(iRowB3));
                memcpy(iRowB4, B[j+3], sizeof(iRowB4));
                for (unsigned int k = 0; k < dimension; k++)
                {
                    sum[0]  = sum[0]  + iRowA1[k] * iRowB1[k];
                    sum[1]  = sum[1]  + iRowA1[k] * iRowB2[k];
                    sum[2]  = sum[2]  + iRowA1[k] * iRowB3[k];
                    sum[3]  = sum[3]  + iRowA1[k] * iRowB4[k];

                    sum[4]  = sum[4]  + iRowA2[k] * iRowB1[k];
                    sum[5]  = sum[5]  + iRowA2[k] * iRowB2[k];
                    sum[6]  = sum[6]  + iRowA2[k] * iRowB3[k];
                    sum[7]  = sum[7]  + iRowA2[k] * iRowB4[k];

                    sum[8]  = sum[8]  + iRowA3[k] * iRowB1[k];
                    sum[9]  = sum[9]  + iRowA3[k] * iRowB2[k];
                    sum[10] = sum[10] + iRowA3[k] * iRowB3[k];
                    sum[11] = sum[11] + iRowA3[k] * iRowB4[k];

                    sum[12] = sum[12] + iRowA4[k] * iRowB1[k];
                    sum[13] = sum[13] + iRowA4[k] * iRowB2[k];
                    sum[14] = sum[14] + iRowA4[k] * iRowB3[k];
                    sum[15] = sum[15] + iRowA4[k] * iRowB4[k];
                }
                recv_buff_C[i+0][j+0] = sum[0];
                recv_buff_C[i+0][j+1] = sum[1];
                recv_buff_C[i+0][j+2] = sum[2];
                recv_buff_C[i+0][j+3] = sum[3];

                recv_buff_C[i+1][j+0] = sum[4];
                recv_buff_C[i+1][j+1] = sum[5];
                recv_buff_C[i+1][j+2] = sum[6];
                recv_buff_C[i+1][j+3] = sum[7];

                recv_buff_C[i+2][j+0] = sum[8];
                recv_buff_C[i+2][j+1] = sum[9];
                recv_buff_C[i+2][j+2] = sum[10];
                recv_buff_C[i+2][j+3] = sum[11];

                recv_buff_C[i+3][j+0] = sum[12];
                recv_buff_C[i+3][j+1] = sum[13];
                recv_buff_C[i+3][j+2] = sum[14];
                recv_buff_C[i+3][j+3] = sum[15];
            }
        }
        if (rank == 0)
        {
            transposeParallel(dimension, thread_count);
        }
    }
    return recv_buff_C;
}

/**
 * Fastest parallel implementation algorithm, copies
 * entire rows from A and B before computing. Only one
 * row at a time, *** does not implement vectorization or MPI. ***
 * Uses parallel transpose function to make accessing of
 * B row-access.
 *
 * parameters: unsigned int dimension, unsigned short thread_count
 * returns:    void
 */
void rowCopyMatrixMatrixMultParallelOpenMPBaseline(unsigned int dimension,
    unsigned short thread_count)
{
    transposeParallel(dimension, thread_count);

    #pragma omp parallel num_threads (thread_count)
    {
        unsigned int iRowA[dimension];
        unsigned int iRowB[dimension];
        unsigned int sum = 0;

        #pragma omp for
        for (unsigned int i = 0; i < dimension; i++)
        {
            memcpy(iRowA, A[i], sizeof(iRowA));
            for (unsigned int j = 0; j < dimension; j++)
            {
                sum = 0;
                memcpy(iRowB, B[j], sizeof(iRowB));

                for (unsigned int k = 0; k < dimension; k++)
                {
                    sum += iRowA[k] * iRowB[k];
                }
                C[i][j] = sum;
            }
        }
    }
    transposeParallel(dimension, thread_count);
}

/**
 * Verification algorithm, takes the serially generated
 * matrix which has already been verified to always be
 * correct and checks whether or not it equals the
 * resultant matrix from the parallel computation.
 * This code was adapted from the matrix mult code written
 * by Andree Jacobson on the class website.
 *
 */
void validResultantMatrix(unsigned int dimension,
    unsigned short print_flag)
{
    unsigned int i, j;
    /* Verify calculations are correct */
    unsigned short areSame = 1;

    printf ("Verifying correctness:"); fflush ( stdout );
    for (i = 0; i < dimension && areSame; i++)
    {
        if (i % 1000 == 0) {
            printf ("."); fflush ( stdout);
        }
        for (j = 0; j < dimension && areSame; j++)
        {
            printf("C_verify %u\n", C_verify[i][j]);
            if (C[i][j] != C_verify[i][j])
            {
                areSame = 0;
            }
        }
    }
    printf ("Matrices %s\n", areSame ? "are identical" : "differ");
    printf ("Problem size (square matrix dimensions) : %dx%d elements\n",
    dimension, dimension);

    if (print_flag)
    {
        printf("Printing Verification Matrix...\n");
        for (i = 0; i < dimension; i++)
        {
            printf("\n");
            for (j = 0; j < dimension; j++)
            {
                printf(" %u ", C_verify[i][j]);
            }
        }
        printf("\n");
        printf("\n");
    }
}

/**
 * Main allows for a fairly wide range of options
 * when running program:
 *
 *     ************** USAGE **************
 *
 * Takes anywhere from 0 to 3 additional arguments
 * from the basic executable, laid out as follows:
 *
 *   1. Nothing         -> Runs default
 *                         General Matrix Multiplication Algorithm
 *                         Default size = 1000
 *
 *   2. One argument    -> Either a matrix dimension between
 *                         MIN and MAX -> 10 up to 10000
 *                         (10,000 not recommended with General Multiply!)
 *                     OR
 *                         A number between 0 and 2 for each algorithm with
 *                         default dimension of 1000.
 *
 *   3. Two arguments   -> A dimension between MIN and MAX and
 *                         a number specifying algorithm.
 *
 *   4. Three arguments -> Same as with two arguments, but a value
 *                         of '1' must be given for this argument.
 *                         Prints out A, B and C matrices in full.
 *                         (Also not recommended for anything larger
 *                         than dimension = 20).
 *
 *   Multiplication Functions:
 *
 *      0 General Matrix Multiplication (Default)
 *
 *      1 Sequential Matrix Multiplication Four
 *
 *      2 Better Matrix Multiplication (Row-Copy, fastest)
 */
int main(int argc, char *argv[])
{
    double runtime_setup,
           runtime_test_matrix,
           runtime_baseline,
           runtime;

    int    size,
           rank,
           name_len,
           root                  = 0,
           count_per_rank        = DEFAULT_DIMENSION * DEFAULT_DIMENSION,
           elements_in_matrices  = DEFAULT_DIMENSION;

    unsigned int   i, j;
    unsigned int   dimension     = DEFAULT_DIMENSION;
    unsigned short success,
                   print_flag    = 0,
                   mult_function = 0;
    unsigned short thread_count  = get_nprocs();

    double begin             = 0,
           end               = 0,
           begin_setup       = 0,
           end_setup         = 0,
           begin_baseline    = 0,
           end_baseline      = 0,
           begin_test_matrix = 0,
           end_test_matrix   = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    char processor_name[MPI_MAX_PROCESSOR_NAME];

    MPI_Get_processor_name(processor_name, &name_len);

    //MPI Rank 0 parses arguments and sets up the matrices.
    if (rank == root)
    {
        if (argc < 2)
        {
            dimension = DEFAULT_DIMENSION;
        }
        else if (argc < 3 && (atoi(argv[1]) >= MIN_DIMENSION &&
            atoi(argv[1]) <= MAX_DIMENSION))
        {

            dimension = atoi(argv[1]);
        }
        else if (argc < 3 && (atoi(argv[1]) >= 0 && atoi(argv[1]) <= NUM_MULT_FUNC))
        {
            mult_function = atoi(argv[1]);
            dimension = DEFAULT_DIMENSION;
        }
        else if (argc < 4 && (atoi(argv[1]) >= MIN_DIMENSION &&
            atoi(argv[1]) <= MAX_DIMENSION) && (atoi(argv[2]) >= 0
            && atoi(argv[2]) <= NUM_MULT_FUNC))
        {

            dimension = atoi(argv[1]);
            mult_function = atoi(argv[2]);
        }
        else if (argc < 5 && (atoi(argv[1]) >= MIN_DIMENSION &&
            atoi(argv[1]) <= MAX_DIMENSION) && (atoi(argv[2]) >= 0
            && atoi(argv[2]) <= NUM_MULT_FUNC) && atoi(argv[3]) == 1)
        {
            dimension = atoi(argv[1]);
            mult_function = atoi(argv[2]);

            /* Print flag can only be specified whenever dimension and function */
            /* are specified.                                                   */
            print_flag = 1;
        }
        else {
            printf("You have chosen an invalid input size\n");
            printf("Please choose a dimension between %d and %d\n",
                MIN_DIMENSION, MAX_DIMENSION);
            return 1;
        }
        begin_setup = omp_get_wtime();
        if (mult_function == 2 && dimension % 4 != 0)
        {
            unsigned int dimension_new = dimension;
            while (1) {
                if (dimension_new % 4 == 0)
                {
                    dimension = dimension_new;
                    break;
                }
                // dimension_new = (unsigned int)pow((double)dimension_new,2.0);
                dimension_new++;
            }
        }
    }
    success = allocate_matrices(dimension);
    if (!success)
    {
        printf("Closing... \n");
        return 1;
    }

    /* Randomly create two matrices A and B of given or default dimension */
    if (rank == root)
    {
        generateMatrices(dimension);
        end_setup   = omp_get_wtime();

        /* Run optimized serial row copy matrix multiply to generate
           matrix to test for validity of parallel algorithm and to
           compare against runtime of parallel row copy algorithm.   */
        begin_test_matrix = omp_get_wtime();
        rowCopyMatrixMatrixMultSerial(dimension);
        //generalSquareMatrixMultiplySerial(dimension);
        end_test_matrix   = omp_get_wtime();

        // begin_baseline = omp_get_wtime();
        // rowCopyMatrixMatrixMultParallelOpenMPBaseline(dimension, thread_count);
        // end_baseline   = omp_get_wtime();
        count_per_rank       = (dimension * dimension) / size;
        elements_in_matrices = (dimension * dimension);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    unsigned int **recv_buff_A, *recv_contig_A;
    unsigned int **recv_buff_C, *recv_contig_C;

    recv_contig_A  = (unsigned int *) malloc(
                     (dimension/size) * (dimension) * sizeof(unsigned int));
    recv_buff_A    = (unsigned int **) malloc((dimension/size) * sizeof(unsigned int *));

    recv_contig_C  = (unsigned int *) malloc(
                     (dimension/size) * (dimension) * sizeof(unsigned int));
    recv_buff_C    = (unsigned int **) malloc((dimension/size) * sizeof(unsigned int *));

    recv_buff_A[0] = recv_contig_A;
    recv_buff_C[0] = recv_contig_C;

    for (j = 1; j < dimension/size; j++)
    {
        recv_buff_A[j] = recv_buff_A[j - 1] + (dimension);
        recv_buff_C[j] = recv_buff_C[j - 1] + (dimension);
    }

    for (i = 0; i < dimension; i++)
    {
        for (j = 0; j < dimension / size; j++)
        {
            recv_buff_C[i][j] = 0;
        }
    }

    // Scatter the data among the MPI Ranks before running primary algorithm.
    // printf("Rank ID %d\n", rank);
    if (rank == root)
    {
        transposeParallel(dimension, thread_count);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Scatter(A,
                count_per_rank,
                MPI_UNSIGNED,
                recv_buff_A,
                count_per_rank,
                MPI_UNSIGNED,
                root,
                MPI_COMM_WORLD);

    MPI_Barrier(MPI_COMM_WORLD);

    /* Broadcast a full copy of matrix B to each MPI rank. */
    MPI_Bcast(B,
              elements_in_matrices,
              MPI_UNSIGNED,
              root,
              MPI_COMM_WORLD);

    printf("Size = %d\n", size);
    for (i = 0; i < dimension; i++)
    {
        printf("\n");
        for (j = 0; j < dimension; j++)
        {
            printf(" %u ", B[i][j]);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    switch (mult_function)
    {
        case 0:
            begin = omp_get_wtime();
            generalSquareMatrixMultiply(dimension, thread_count);
            end = omp_get_wtime();
            break;
        case 1:
            begin = omp_get_wtime();
            sequentialMatrixMultiplicationFour(dimension);
            end = omp_get_wtime();
            break;
        case 2:
            begin = omp_get_wtime();
            recv_buff_C = rowCopyMatrixMatrixMultParallel(dimension, thread_count,
                                            recv_buff_A, recv_buff_C, rank, size);
            end = omp_get_wtime();
            break;
        default:
            printf("You have specified an invalid multiplication function\n");
            return 1;
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == root)
    {
        transposeParallel(dimension, thread_count);
        // transpose(dimension);
    }

    // MPI_Barrier(MPI_COMM_WORLD);
    printf("RANK = %d\n", rank);

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Gather(recv_buff_C,
               count_per_rank,
               MPI_UNSIGNED,
               C,
               count_per_rank,
               MPI_UNSIGNED,
               root,
               MPI_COMM_WORLD);

    printf("RANK = %d\n", rank);
    if (rank == root)
    {
        if (print_flag)
        {
            for (i = 0; i < dimension; i++)
            {
                printf("\n");
                for (j = 0; j < dimension; j++)
                {
                    printf(" %u ", A[i][j]);
                }
            }
            printf("\n");
            for (i = 0; i < dimension; i++)
            {
                printf("\n");
                for (j = 0; j < dimension; j++)
                {
                    printf(" %u ", B[i][j]);
                }
            }
            printf("\n");
        }
        runtime_setup       = (double)(end_setup       - begin_setup);
        runtime_test_matrix = (double)(end_test_matrix - begin_test_matrix);
        runtime_baseline    = (double)(end_baseline    - begin_baseline);
        runtime             = (double)(end             - begin);

        // switch (mult_function)
        // {
        //     case 0:
        //         printf("General Square Matrix Multiply\n");
        //         printf("\n");
        //         break;
        //     case 1:
        //         printf("Sequential Square Matrix Multiply by Four\n");
        //         printf("\n");
        //         break;
        //     case 2:
        //         printf("Unrolled Row Copy Matrix Multiply\n");
        //         printf("\n");
        //         break;
        // }

        // Formated output for python script.
        // Program setup runtime.
        printf("%f ", runtime_setup);

        // Program serial row copy runtime.
        printf("%f ", runtime_test_matrix);

        // Program openMP single row copy runtime.
        printf("%f ", runtime_baseline);

        // Program openMP four row copy optimized runtime.
        printf("%f ", runtime);

        // Not currently testing for validity, done through single run of the
        // basic C code.
        validResultantMatrix(dimension, print_flag);

        if (print_flag)
        {
            printf("Printing Resultant Matrix...\n");
            for (i = 0; i < dimension; i++)
            {
                printf("\n");
                for (j = 0; j < dimension; j++)
                {
                    printf(" %u ", C[i][j]);
                }
            }
        }
        deallocate_matrices();
    }
    MPI_Barrier(MPI_COMM_WORLD);

    if (recv_buff_A != NULL || recv_buff_C != NULL)
    {
        if (recv_buff_A != NULL)
        {
            free(recv_buff_A[0]);
            free(recv_buff_A);
            recv_buff_A = NULL;
        }
        if (recv_buff_C != NULL)
        {
            free(recv_buff_C[0]);
            free(recv_buff_C);
            recv_buff_C = NULL;
        }
    }
    MPI_Finalize();
    return 0;
}
