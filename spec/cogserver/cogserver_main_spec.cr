require "spec"
require "../../src/cogserver/cogserver_main"

describe "CogServer Main" do
  describe "initialization" do
    it "initializes CogServer system" do
      CogServer.initialize
      # Should not crash
    end

    it "has correct version" do
      CogServer::VERSION.should eq("0.1.0")
    end

    it "creates server instance" do
      server = CogServer.create_server(17001)
      server.should be_a(CogServer::CogServer)
    end
  end

  describe "server configuration" do
    it "configures default port" do
      CogServer::DEFAULT_PORT.should eq(17001)
    end

    it "provides REST API endpoints" do
      CogServer::REST_API_VERSION.should eq("v1")
    end

    it "supports WebSocket connections" do
      CogServer::WEBSOCKET_SUPPORTED.should be_true
    end
  end

  describe "system integration" do
    it "integrates with AtomSpace" do
      CogUtil.initialize
      AtomSpace.initialize
      CogServer.initialize

      # Should be able to create server with atomspace
      atomspace = AtomSpace.create_atomspace
      server = CogServer.create_server(17002, atomspace)
      server.should_not be_nil
    end

    it "provides command interface" do
      server = CogServer.create_server(17003)

      # Should have command system
      server.respond_to?(:execute_command).should be_true
    end
  end
end
