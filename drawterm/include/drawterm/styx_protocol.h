#pragma once

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <functional>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <boost/asio.hpp>

namespace DrawTerm {
namespace Styx {

/**
 * @brief Plan 9's Styx protocol messages for network file system operations
 */
enum class MessageType : uint8_t {
    Tversion = 100, Rversion = 101,
    Tauth = 102, Rauth = 103,
    Tattach = 104, Rattach = 105,
    Terror = 106, Rerror = 107,
    Tflush = 108, Rflush = 109,
    Twalk = 110, Rwalk = 111,
    Topen = 112, Ropen = 113,
    Tcreate = 114, Rcreate = 115,
    Tread = 116, Rread = 117,
    Twrite = 118, Rwrite = 119,
    Tclunk = 120, Rclunk = 121,
    Tremove = 122, Rremove = 123,
    Tstat = 124, Rstat = 125,
    Twstat = 126, Rwstat = 127
};

/**
 * @brief File information structure (Styx Stat)
 */
struct Stat {
    uint16_t type;
    uint32_t dev;
    uint8_t qid_type;
    uint32_t qid_vers;
    uint64_t qid_path;
    uint32_t mode;
    uint32_t atime;
    uint32_t mtime;
    uint64_t length;
    std::string name;
    std::string uid;
    std::string gid;
    std::string muid;
};

/**
 * @brief Styx protocol message
 */
struct Message {
    uint32_t size;
    MessageType type;
    uint16_t tag;
    std::vector<uint8_t> data;
    
    // Serialization
    std::vector<uint8_t> serialize() const;
    static Message deserialize(const std::vector<uint8_t>& buffer);
};

/**
 * @brief Authentication context for Styx connections
 */
class AuthContext {
public:
    AuthContext(const std::string& username, const std::string& password);
    
    bool authenticate();
    bool is_authenticated() const { return authenticated_; }
    const std::string& get_username() const { return username_; }

private:
    std::string username_;
    std::string password_;
    bool authenticated_ = false;
};

/**
 * @brief Styx protocol client connection
 */
class StyxConnection {
public:
    StyxConnection(const std::string& host, uint16_t port);
    ~StyxConnection();
    
    // Connection management
    bool connect();
    void disconnect();
    bool is_connected() const { return connected_; }
    
    // Authentication
    bool authenticate(const std::string& username, const std::string& password);
    
    // File system operations
    bool attach(const std::string& aname = "");
    bool walk(const std::vector<std::string>& path);
    bool open(uint8_t mode);
    bool create(const std::string& name, uint32_t perm, uint8_t mode);
    std::vector<uint8_t> read(uint32_t count, uint64_t offset = 0);
    uint32_t write(const std::vector<uint8_t>& data, uint64_t offset = 0);
    bool close();
    bool remove();
    Stat stat();
    bool wstat(const Stat& stat);
    
    // Directory operations
    std::vector<Stat> list_directory();
    
    // Network transparency helpers
    bool mount_remote(const std::string& remote_path, const std::string& local_path);
    bool serve_directory(const std::string& directory_path);

private:
    std::string host_;
    uint16_t port_;
    bool connected_ = false;
    
    boost::asio::io_context io_context_;
    std::unique_ptr<boost::asio::ip::tcp::socket> socket_;
    
    std::unique_ptr<AuthContext> auth_context_;
    uint32_t fid_ = 1;  // File identifier
    uint16_t tag_ = 1;  // Message tag
    
    // Protocol helpers
    Message send_message(const Message& msg);
    bool send_raw_message(const Message& msg);
    Message receive_message();
    
    uint16_t next_tag() { return tag_++; }
    uint32_t next_fid() { return fid_++; }
};

/**
 * @brief Styx protocol server for network file system serving
 */
class StyxServer {
public:
    using RequestHandler = std::function<void(const Message&, StyxConnection&)>;
    
    StyxServer(const std::string& bind_address, uint16_t port);
    ~StyxServer();
    
    // Server lifecycle
    bool start();
    void stop();
    bool is_running() const { return running_; }
    
    // Request handling
    void set_request_handler(RequestHandler handler);
    
    // File system serving
    bool serve_directory(const std::string& root_path);
    
    // Security
    void set_authentication_required(bool required) { auth_required_ = required; }
    void add_user(const std::string& username, const std::string& password);

private:
    std::string bind_address_;
    uint16_t port_;
    bool running_ = false;
    bool auth_required_ = false;
    
    boost::asio::io_context io_context_;
    std::unique_ptr<boost::asio::ip::tcp::acceptor> acceptor_;
    std::thread server_thread_;
    
    RequestHandler request_handler_;
    std::string root_path_;
    std::map<std::string, std::string> users_;  // username -> password
    
    // Server implementation
    void run_server();
    void handle_connection(std::shared_ptr<boost::asio::ip::tcp::socket> socket);
    void handle_message(const Message& msg, StyxConnection& connection);
    
    // Default file system handlers
    void handle_version(const Message& msg, StyxConnection& connection);
    void handle_auth(const Message& msg, StyxConnection& connection);
    void handle_attach(const Message& msg, StyxConnection& connection);
    void handle_walk(const Message& msg, StyxConnection& connection);
    void handle_open(const Message& msg, StyxConnection& connection);
    void handle_create(const Message& msg, StyxConnection& connection);
    void handle_read(const Message& msg, StyxConnection& connection);
    void handle_write(const Message& msg, StyxConnection& connection);
    void handle_clunk(const Message& msg, StyxConnection& connection);
    void handle_remove(const Message& msg, StyxConnection& connection);
    void handle_stat(const Message& msg, StyxConnection& connection);
    void handle_wstat(const Message& msg, StyxConnection& connection);
};

} // namespace Styx
} // namespace DrawTerm