# CogServer main entry point
# This provides a standalone executable for running the CogServer

require "./cogserver"

module CogServer
  # Main entry point for command-line usage
  def self.main(args = ARGV)
    puts "CogServer #{VERSION} - OpenCog Network Server"
    
    # Parse command line arguments
    host = CogServer::DEFAULT_HOST
    port = CogServer::DEFAULT_PORT
    ws_port = CogServer::DEFAULT_WS_PORT
    
    i = 0
    while i < args.size
      case args[i]
      when "--host", "-h"
        host = args[i + 1]
        i += 2
      when "--port", "-p"
        port = args[i + 1].to_i
        i += 2
      when "--ws-port", "-w"
        ws_port = args[i + 1].to_i
        i += 2
      when "--help"
        print_usage
        return
      else
        puts "Unknown option: #{args[i]}"
        print_usage
        return
      end
    end
    
    # Initialize the system
    CogServer.initialize
    
    # Create and start the server
    server = CogServer::Server.new(host, port, ws_port)
    
    # Handle shutdown gracefully
    Signal::INT.trap do
      puts "\nShutting down CogServer..."
      server.stop
      exit(0)
    end
    
    Signal::TERM.trap do
      puts "\nShutting down CogServer..."
      server.stop
      exit(0)
    end
    
    # Start the server
    server.start
    
    puts "\nCogServer is running. Press Ctrl+C to stop."
    puts "Telnet interface: telnet #{host} #{port}"
    puts "HTTP API: http://#{host}:#{ws_port}"
    puts "Status: http://#{host}:#{ws_port}/status"
    
    # Keep the main thread alive
    while server.running?
      sleep 1
    end
  end
  
  private def self.print_usage
    puts <<-USAGE
    Usage: cogserver [options]
    
    Options:
      --host, -h HOST      Server host (default: #{DEFAULT_HOST})
      --port, -p PORT      Telnet port (default: #{DEFAULT_PORT})
      --ws-port, -w PORT   WebSocket/HTTP port (default: #{DEFAULT_WS_PORT})
      --help               Show this help message
    
    Examples:
      cogserver                           # Start with default settings
      cogserver --host 0.0.0.0 --port 17001
      cogserver -h localhost -p 17002 -w 18081
    USAGE
  end
end

# Run if this file is executed directly
if PROGRAM_NAME == __FILE__
  CogServer.main
end