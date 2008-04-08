//--------------------------------------------------------------------------//
// mbs.cpp
// Lars Yencken <lars.yencken@gmail.com>
// vim: ts=4 sw=4 sts=4 expandtab:
// Tue Sep  4 16:49:00 EST 2007
//--------------------------------------------------------------------------//

#include <iostream>
#include <stdlib.h>
#include <locale.h>

const int BUFFER_SIZE = 150;

using namespace std;

wchar_t** loadData(const char* inputFile);
void dumpData(wchar_t** data, const char* outputFile);
void freeData(wchar_t** data);
void* safe_realloc(void* ptr, size_t size);

//--------------------------------------------------------------------------//
// main():
//
int main(int argc, char *argv[])
{
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " inputFile outputFile" << endl;
        return 1;
    } else if (!setlocale(LC_CTYPE, "")) {
        cerr << "Can't set the specified locale! Check LANG, LC_CTYPE, " 
            << "LC_ALL." << endl;
        return 1;
    }
    wchar_t** data = loadData(argv[1]);
    dumpData(data, argv[2]);
    freeData(data);
    return 0;
}

wchar_t** loadData(const char* inputFile)
{
    int n = 0, maxN = 1024;
    wchar_t** data = (wchar_t**) safe_realloc(NULL, sizeof(wchar_t*)*maxN);
    wchar_t wBuffer[BUFFER_SIZE];

    for (int i = 0; i < maxN; i++) {
        data[i] = NULL;
    }
    
    FILE* fp = fopen(inputFile, "r");
    if (fp == NULL) {
        cerr << "Error: couldn't open " << inputFile << " for reading" << endl;
        exit(EXIT_FAILURE);
    }

    while (fgetws(wBuffer, BUFFER_SIZE, fp) != NULL) {
        data[n] = (wchar_t*) safe_realloc(NULL, 
                sizeof(wchar_t)*(wcslen(wBuffer)+1));
        wcscpy(data[n], wBuffer);
        n++;

        if (n >= maxN) {
            maxN *= 2;
            data = (wchar_t**) safe_realloc(data, sizeof(wchar_t*)*maxN);
            for (int i = n; i < maxN; i++) {
                data[i] = NULL;
            }
        }
    }
    fclose(fp);
    return data;
}

void* safe_realloc(void* ptr, size_t size)
{
    void* result = realloc(ptr, size);
    if (result == NULL) {
        cerr << "Out of memory!" << endl;
        exit(EXIT_FAILURE);
    }
    return result;
}

void dumpData(wchar_t** data, const char* outputFile)
{
    FILE* fp = fopen(outputFile, "w");
    if (fp == NULL) {
        cerr << "Error: can't open file " << outputFile << " for writing"
            << endl;
        exit(EXIT_FAILURE);
    }

    for (int i = 0; data[i] != NULL; i++)
    {
        fprintf(fp, "%lc: %ls", data[i][0], data[i]);
        //fputws(data[i], fp);
    }
    return;
}

void freeData(wchar_t** data)
{
    for (int i = 0; data[i] != NULL; i++) {
        free(data[i]);
    }
    free(data);
    return;
}
