//--------------------------------------------------------------------------//
// linkedSort.c
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Fri Aug 31 22:45:49 EST 2007
//--------------------------------------------------------------------------//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_WORDS       200
#define MAX_WORD_LEN    256

//--------------------------------------------------------------------------//

typedef struct linkedNode {
    char* word;
    struct linkedNode* next;
} LinkedNode;

//--------------------------------------------------------------------------//

void usage();
LinkedNode* loadWords(char* inputFile);
void dumpWords(LinkedNode* words, char* outputFile);
void freeWords(LinkedNode* words);
void* safe_realloc(void* ptr, size_t size);
LinkedNode* linkedSort(LinkedNode* words);

//--------------------------------------------------------------------------//

int main(int argc, char *argv[])
{
    LinkedNode* words = NULL;

    if (argc != 3) {
        usage();
    }

    words = loadWords(argv[1]);
    words = linkedSort(words);
    dumpWords(words, argv[2]);

    return 0;
}

/**
 * Print the usage message and exit.
 */
void usage()
{
    fprintf(stderr, "Usage: linkedSort inputFile outputFile\n");
    exit(EXIT_FAILURE);
}

/**
 * Load the words stored in the file, one per line.
 */
LinkedNode* loadWords(char* inputFile)
{
    LinkedNode* node = NULL;
    LinkedNode* first = NULL;
    char word[MAX_WORD_LEN];
    int i = 0;
    FILE* iStream = NULL;

    iStream = fopen(inputFile, "r");
    if (iStream == NULL) {
        fprintf(stderr, "Error: couldn't open file ");
        fprintf(stderr, inputFile);
        fprintf(stderr, "\n");
        exit(EXIT_FAILURE);
    }

    first = node;
    i = 0;
    while (fgets(word, MAX_WORD_LEN, iStream) != NULL) {
        if (i > 0) {
            /* Second or later pass: use the node->next pointer. */
            node->next = (LinkedNode*) safe_realloc(NULL, sizeof(LinkedNode));
            node = node->next;
        } else {
            /* First pass: use the node pointer itself. */
            node = (LinkedNode*) safe_realloc(NULL, sizeof(LinkedNode));
            first = node;
        }
        node->word = (char*) safe_realloc(NULL, sizeof(char)*(strlen(word)+1));
        strcpy(node->word, word);
        i++;
    }
    fclose(iStream);
    node->next = NULL;
    printf("Read %d words\n", i);

    return first;
}

/**
 * Dump the words in the linked list to the given output file.
 */
void dumpWords(LinkedNode* node, char* outputFile)
{
    FILE* oStream = NULL;
    
    oStream = fopen(outputFile, "w");
    if (oStream == NULL)
    {
        fprintf(stderr, "Error: opening file ");
        fprintf(stderr, outputFile);
        fprintf(stderr, " for writing\n");
        exit(EXIT_FAILURE);
    }

    while (node != NULL)
    {
        fputs(node->word, oStream);
        node = node->next;
    }
    fclose(oStream);
    return;
}

/**
 * Free all the words.
 */
void freeWords(LinkedNode* node)
{
    LinkedNode* next = NULL;

    while (node != NULL) {
        next = node->next;
        free(node);
        node = next;
    }
    return;
}

/**
 * An error-checking version of realloc.
 */
void* safe_realloc(void* ptr, size_t size)
{
    void* newPtr = realloc(ptr, size);
    if (newPtr == NULL) {
        fprintf(stderr, "Error: out of memory\n");
        exit(EXIT_FAILURE);
    }
    return newPtr;
}

void* safe_malloc(size_t size)
{
    return safe_realloc(NULL, size);
}

/**
 * Sort the words using quicksort.
 */
LinkedNode* linkedSort(LinkedNode* node)
{
    LinkedNode* partition = node;
    LinkedNode* leftHead = NULL;
    LinkedNode* leftTail = NULL;
    LinkedNode* rightHead = NULL;
    LinkedNode* rightTail = NULL;

    /* Single nodes are already sorted. */
    if (node == NULL || node->next == NULL) {
        return node;
    }
    node = node->next;

    /* Partition around the first element. */
    while (node != NULL) {
        if (strcmp(node->word, partition->word) <= 0) {
            /* Append to the left tail. */ 
            if (leftTail == NULL) {
                leftHead = node;
                leftTail = node;
            } else {
                leftTail->next = node;
                leftTail = node;
            }
            node = node->next;
            leftTail->next = NULL;
        } else {
            /* Append to the right tail. */
            if (rightTail == NULL) {
                rightTail = node;
                rightHead = node;
            } else {
                rightTail->next = node;
                rightTail = node;
            }
            node = node->next;
            rightTail->next = NULL;
        }
    }

    /* Recursively sort. */
    leftHead = linkedSort(leftHead);
    rightHead = linkedSort(rightHead);

    /* Concatenated sorted results. */
    partition->next = rightHead;
    if (leftHead == NULL) {
        return partition;
    } else {
        node = leftHead;
        while (node->next != NULL) {
            node = node->next;
        }
        node->next = partition;
        return leftHead;
    }
}

