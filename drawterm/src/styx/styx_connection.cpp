#include "drawterm/styx_protocol.h"
#include <iostream>
#include <stdexcept>
#include <thread>

namespace DrawTerm {
namespace Styx {

// Message implementation
std::vector<uint8_t> Message::serialize() const {
    std::vector<uint8_t> buffer;
    
    // Size (4 bytes)
    uint32_t total_size = 4 + 1 + 2 + data.size();
    buffer.push_back((total_size >> 0) & 0xFF);
    buffer.push_back((total_size >> 8) & 0xFF);
    buffer.push_back((total_size >> 16) & 0xFF);
    buffer.push_back((total_size >> 24) & 0xFF);
    
    // Type (1 byte)
    buffer.push_back(static_cast<uint8_t>(type));
    
    // Tag (2 bytes)
    buffer.push_back((tag >> 0) & 0xFF);
    buffer.push_back((tag >> 8) & 0xFF);
    
    // Data
    buffer.insert(buffer.end(), data.begin(), data.end());
    
    return buffer;
}

Message Message::deserialize(const std::vector<uint8_t>& buffer) {
    if (buffer.size() < 7) {
        throw std::runtime_error("Invalid message buffer size");
    }
    
    Message msg;
    
    // Size
    msg.size = (static_cast<uint32_t>(buffer[0]) << 0) |
               (static_cast<uint32_t>(buffer[1]) << 8) |
               (static_cast<uint32_t>(buffer[2]) << 16) |
               (static_cast<uint32_t>(buffer[3]) << 24);
    
    // Type
    msg.type = static_cast<MessageType>(buffer[4]);
    
    // Tag
    msg.tag = (static_cast<uint16_t>(buffer[5]) << 0) |
              (static_cast<uint16_t>(buffer[6]) << 8);
    
    // Data
    if (buffer.size() > 7) {
        msg.data.assign(buffer.begin() + 7, buffer.end());
    }
    
    return msg;
}

// AuthContext implementation
AuthContext::AuthContext(const std::string& username, const std::string& password)
    : username_(username), password_(password) {
}

bool AuthContext::authenticate() {
    // Simple authentication - in production this would be more sophisticated
    authenticated_ = !username_.empty() && !password_.empty();
    return authenticated_;
}

// StyxConnection implementation
StyxConnection::StyxConnection(const std::string& host, uint16_t port)
    : host_(host), port_(port) {
}

StyxConnection::~StyxConnection() {
    disconnect();
}

bool StyxConnection::connect() {
    try {
        socket_ = std::make_unique<boost::asio::ip::tcp::socket>(io_context_);
        boost::asio::ip::tcp::resolver resolver(io_context_);
        auto endpoints = resolver.resolve(host_, std::to_string(port_));
        boost::asio::connect(*socket_, endpoints);
        connected_ = true;
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Connection failed: " << e.what() << std::endl;
        connected_ = false;
        return false;
    }
}

void StyxConnection::disconnect() {
    if (socket_ && connected_) {
        socket_->close();
        connected_ = false;
    }
}

bool StyxConnection::authenticate(const std::string& username, const std::string& password) {
    auth_context_ = std::make_unique<AuthContext>(username, password);
    return auth_context_->authenticate();
}

bool StyxConnection::attach(const std::string& aname) {
    if (!connected_) return false;
    
    Message msg;
    msg.type = MessageType::Tattach;
    msg.tag = next_tag();
    
    // Add attach data (simplified)
    std::string data = aname;
    msg.data.assign(data.begin(), data.end());
    
    Message response = send_message(msg);
    return response.type == MessageType::Rattach;
}

std::vector<uint8_t> StyxConnection::read(uint32_t count, uint64_t offset) {
    if (!connected_) return {};
    
    Message msg;
    msg.type = MessageType::Tread;
    msg.tag = next_tag();
    
    // Simplified read implementation
    Message response = send_message(msg);
    return response.data;
}

Message StyxConnection::send_message(const Message& msg) {
    if (!send_raw_message(msg)) {
        throw std::runtime_error("Failed to send message");
    }
    return receive_message();
}

bool StyxConnection::send_raw_message(const Message& msg) {
    if (!connected_ || !socket_) return false;
    
    try {
        auto serialized = msg.serialize();
        boost::asio::write(*socket_, boost::asio::buffer(serialized));
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Send failed: " << e.what() << std::endl;
        return false;
    }
}

Message StyxConnection::receive_message() {
    if (!connected_ || !socket_) {
        throw std::runtime_error("Not connected");
    }
    
    // Read message size first
    std::array<uint8_t, 4> size_buffer;
    boost::asio::read(*socket_, boost::asio::buffer(size_buffer));
    
    uint32_t msg_size = (static_cast<uint32_t>(size_buffer[0]) << 0) |
                        (static_cast<uint32_t>(size_buffer[1]) << 8) |
                        (static_cast<uint32_t>(size_buffer[2]) << 16) |
                        (static_cast<uint32_t>(size_buffer[3]) << 24);
    
    // Read rest of message
    std::vector<uint8_t> buffer(msg_size);
    std::copy(size_buffer.begin(), size_buffer.end(), buffer.begin());
    boost::asio::read(*socket_, boost::asio::buffer(buffer.data() + 4, msg_size - 4));
    
    return Message::deserialize(buffer);
}

// Additional method implementations for completeness
bool StyxConnection::walk(const std::vector<std::string>& path) {
    // Simplified implementation
    return connected_;
}

bool StyxConnection::open(uint8_t mode) {
    return connected_;
}

bool StyxConnection::create(const std::string& name, uint32_t perm, uint8_t mode) {
    return connected_;
}

uint32_t StyxConnection::write(const std::vector<uint8_t>& data, uint64_t offset) {
    return connected_ ? data.size() : 0;
}

bool StyxConnection::close() {
    return connected_;
}

bool StyxConnection::remove() {
    return connected_;
}

Stat StyxConnection::stat() {
    Stat st;
    st.name = "example";
    return st;
}

bool StyxConnection::wstat(const Stat& stat) {
    return connected_;
}

std::vector<Stat> StyxConnection::list_directory() {
    return {};
}

bool StyxConnection::mount_remote(const std::string& remote_path, const std::string& local_path) {
    return connected_;
}

bool StyxConnection::serve_directory(const std::string& directory_path) {
    return connected_;
}

} // namespace Styx
} // namespace DrawTerm