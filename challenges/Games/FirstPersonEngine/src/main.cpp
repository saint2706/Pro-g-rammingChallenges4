#include "Application.h"

#include <filesystem>
#include <iostream>

int main(int argc, char **argv) {
    std::filesystem::path executablePath = (argc > 0) ? std::filesystem::absolute(std::filesystem::path(argv[0])).parent_path()
                                                      : std::filesystem::current_path();
    std::string assetRoot = executablePath.string();

    Application app;
    std::string log;
    if (!app.initialise(assetRoot, log)) {
        std::cerr << "Failed to initialise application: " << log << std::endl;
        return 1;
    }

    app.run();
    app.shutdown();
    return 0;
}

