#include <iostream>
#include <fstream>
#include <vector>
#include <stdexcept>
#include <cstdlib>

void usage();
void read_data(const char* filename, std::vector<int>& data);

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        usage();
    }
    std::vector<int> data;
    read_data(argv[1], data);
    
    // Debugging print statement
    std::cout << "Read:";
    for (int i = 0; i < data.size(); i++) {
        std::cout << " " << data[i];
    }
    std::cout << std::endl;
    
    // Calculate sum.
    int sum = 0;
    for(size_t i = 0; i < data.size(); ++i) {
        sum += data[i];
    }
    std::cout << "Sum: " << sum << std::endl;
    return 0;
}

void usage()
{
    std::cerr << "Usage: io inputFile" << std::endl;
    exit(1);
}

void read_data(const char* filename, std::vector<int>& data) 
{
    std::ifstream input;
    int d;
    input.open(filename);
    if (!input.is_open()) {
        throw std::runtime_error("Couldn't open data file -- exitting");
    }
    while (true) {
        input >> d;
        if (input.eof()) {
            break;
        } else if (input.fail()) {
            throw std::runtime_error("Error whilst reading from file");
        }
        data.push_back(d);
    }
    input.close();
}
