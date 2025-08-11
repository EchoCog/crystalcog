#include "drawterm/styx_protocol.h"
#include <iostream>
#include <thread>
#include <chrono>

int main() {
    std::cout << "Styx Client/Server Demo" << std::endl;
    std::cout << "=======================" << std::endl;
    
    // Start server
    DrawTerm::Styx::StyxServer server("127.0.0.1", 9999);
    
    if (server.start()) {
        std::cout << "✅ Styx server started" << std::endl;
        
        // Give server time to start
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        
        // Create client
        DrawTerm::Styx::StyxConnection client("127.0.0.1", 9999);
        
        if (client.connect()) {
            std::cout << "✅ Client connected" << std::endl;
            
            if (client.authenticate("user", "password")) {
                std::cout << "✅ Authentication successful" << std::endl;
            }
            
            if (client.attach("/")) {
                std::cout << "✅ Filesystem attached" << std::endl;
            }
            
            std::cout << "✅ Styx protocol demo completed" << std::endl;
        } else {
            std::cout << "❌ Client connection failed" << std::endl;
        }
        
        server.stop();
    } else {
        std::cout << "❌ Failed to start server" << std::endl;
        return 1;
    }
    
    return 0;
}