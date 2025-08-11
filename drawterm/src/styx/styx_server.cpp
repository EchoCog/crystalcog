#include "drawterm/styx_protocol.h"
#include <iostream>
#include <functional>

namespace DrawTerm {
namespace Styx {

// StyxServer implementation
StyxServer::StyxServer(const std::string& bind_address, uint16_t port)
    : bind_address_(bind_address), port_(port) {
}

StyxServer::~StyxServer() {
    stop();
}

bool StyxServer::start() {
    if (running_) return true;
    
    try {
        acceptor_ = std::make_unique<boost::asio::ip::tcp::acceptor>(
            io_context_, 
            boost::asio::ip::tcp::endpoint(
                boost::asio::ip::address::from_string(bind_address_), 
                port_
            )
        );
        
        running_ = true;
        server_thread_ = std::thread(&StyxServer::run_server, this);
        
        std::cout << "Styx server started on " << bind_address_ << ":" << port_ << std::endl;
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Failed to start Styx server: " << e.what() << std::endl;
        running_ = false;
        return false;
    }
}

void StyxServer::stop() {
    if (!running_) return;
    
    running_ = false;
    
    if (acceptor_) {
        acceptor_->close();
    }
    
    io_context_.stop();
    
    if (server_thread_.joinable()) {
        server_thread_.join();
    }
    
    std::cout << "Styx server stopped" << std::endl;
}

void StyxServer::set_request_handler(RequestHandler handler) {
    request_handler_ = handler;
}

bool StyxServer::serve_directory(const std::string& root_path) {
    root_path_ = root_path;
    
    // Set default request handler for file serving
    set_request_handler([this](const Message& msg, StyxConnection& conn) {
        handle_message(msg, conn);
    });
    
    return true;
}

void StyxServer::add_user(const std::string& username, const std::string& password) {
    users_[username] = password;
}

void StyxServer::run_server() {
    while (running_) {
        try {
            auto socket = std::make_shared<boost::asio::ip::tcp::socket>(io_context_);
            acceptor_->accept(*socket);
            
            // Handle connection in a separate thread
            std::thread connection_thread(&StyxServer::handle_connection, this, socket);
            connection_thread.detach();
            
        } catch (const std::exception& e) {
            if (running_) {
                std::cerr << "Accept error: " << e.what() << std::endl;
            }
        }
    }
}

void StyxServer::handle_connection(std::shared_ptr<boost::asio::ip::tcp::socket> socket) {
    try {
        StyxConnection connection("", 0);  // Create connection wrapper
        
        while (running_ && socket->is_open()) {
            // Read message size
            std::array<uint8_t, 4> size_buffer;
            boost::asio::read(*socket, boost::asio::buffer(size_buffer));
            
            uint32_t msg_size = (static_cast<uint32_t>(size_buffer[0]) << 0) |
                                (static_cast<uint32_t>(size_buffer[1]) << 8) |
                                (static_cast<uint32_t>(size_buffer[2]) << 16) |
                                (static_cast<uint32_t>(size_buffer[3]) << 24);
            
            // Read rest of message
            std::vector<uint8_t> buffer(msg_size);
            std::copy(size_buffer.begin(), size_buffer.end(), buffer.begin());
            boost::asio::read(*socket, boost::asio::buffer(buffer.data() + 4, msg_size - 4));
            
            Message msg = Message::deserialize(buffer);
            
            // Handle message
            if (request_handler_) {
                request_handler_(msg, connection);
            } else {
                handle_message(msg, connection);
            }
        }
    } catch (const std::exception& e) {
        std::cerr << "Connection error: " << e.what() << std::endl;
    }
}

void StyxServer::handle_message(const Message& msg, StyxConnection& connection) {
    switch (msg.type) {
        case MessageType::Tversion:
            handle_version(msg, connection);
            break;
        case MessageType::Tauth:
            handle_auth(msg, connection);
            break;
        case MessageType::Tattach:
            handle_attach(msg, connection);
            break;
        case MessageType::Twalk:
            handle_walk(msg, connection);
            break;
        case MessageType::Topen:
            handle_open(msg, connection);
            break;
        case MessageType::Tcreate:
            handle_create(msg, connection);
            break;
        case MessageType::Tread:
            handle_read(msg, connection);
            break;
        case MessageType::Twrite:
            handle_write(msg, connection);
            break;
        case MessageType::Tclunk:
            handle_clunk(msg, connection);
            break;
        case MessageType::Tremove:
            handle_remove(msg, connection);
            break;
        case MessageType::Tstat:
            handle_stat(msg, connection);
            break;
        case MessageType::Twstat:
            handle_wstat(msg, connection);
            break;
        default:
            std::cerr << "Unknown message type: " << static_cast<int>(msg.type) << std::endl;
            break;
    }
}

// Default message handlers (simplified implementations)
void StyxServer::handle_version(const Message& msg, StyxConnection& connection) {
    // Send version response
    std::cout << "Handling version request" << std::endl;
}

void StyxServer::handle_auth(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling auth request" << std::endl;
}

void StyxServer::handle_attach(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling attach request" << std::endl;
}

void StyxServer::handle_walk(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling walk request" << std::endl;
}

void StyxServer::handle_open(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling open request" << std::endl;
}

void StyxServer::handle_create(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling create request" << std::endl;
}

void StyxServer::handle_read(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling read request" << std::endl;
}

void StyxServer::handle_write(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling write request" << std::endl;
}

void StyxServer::handle_clunk(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling clunk request" << std::endl;
}

void StyxServer::handle_remove(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling remove request" << std::endl;
}

void StyxServer::handle_stat(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling stat request" << std::endl;
}

void StyxServer::handle_wstat(const Message& msg, StyxConnection& connection) {
    std::cout << "Handling wstat request" << std::endl;
}

} // namespace Styx
} // namespace DrawTerm