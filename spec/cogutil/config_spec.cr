require "spec"
require "../../src/cogutil/config"

# Helper function for creating test config files
def create_test_config(content : String, extension : String = ".conf") : String
  temp_file = File.tempname("test_config", extension)
  File.write(temp_file, content)
  temp_file
end

describe CogUtil::Config do
  
  describe "simple config format" do
    it "loads key=value configuration" do
      content = <<-CONFIG
        # This is a comment
        LOG_LEVEL=DEBUG
        COGSERVER_PORT=17001
        ENABLE_PERSISTENCE=true
        MESSAGE="Hello World"
        CONFIG
      
      config_file = create_test_config(content)
      config = CogUtil::Config.new(File.basename(config_file))
      
      # Need to manually set the path since we're using a temp file
      config.load_config_file(config_file)
      
      config.get("LOG_LEVEL").should eq("DEBUG")
      config.get("COGSERVER_PORT").should eq("17001")
      config.get("ENABLE_PERSISTENCE").should eq("true")
      config.get("MESSAGE").should eq("Hello World")
      
      File.delete(config_file)
    end
    
    it "handles missing values with defaults" do
      config = CogUtil::Config.new
      
      config.get("NONEXISTENT_KEY", "default").should eq("default")
      config.get("NONEXISTENT_KEY").should eq("")
    end
    
    it "converts types correctly" do
      content = <<-CONFIG
        BOOL_TRUE=true
        BOOL_FALSE=false
        INT_VALUE=42
        FLOAT_VALUE=3.14
        CONFIG
      
      config_file = create_test_config(content)
      config = CogUtil::Config.new
      config.load_config_file(config_file)
      
      config.get_bool("BOOL_TRUE", false).should eq(true)
      config.get_bool("BOOL_FALSE", true).should eq(false)
      config.get_int("INT_VALUE", 0).should eq(42)
      config.get_float("FLOAT_VALUE", 0.0).should eq(3.14)
      
      File.delete(config_file)
    end
  end
  
  describe "YAML config format" do
    it "loads YAML configuration" do
      content = <<-YAML
        logging:
          level: DEBUG
          file: opencog.log
        server:
          port: 17001
          host: localhost
        features:
          persistence: true
          attention: false
        YAML
      
      config_file = create_test_config(content, ".yaml")
      config = CogUtil::Config.new
      config.load_config_file(config_file)
      
      config.get("logging.level").should eq("DEBUG")
      config.get("logging.file").should eq("opencog.log")
      config.get("server.port").should eq("17001")
      config.get("server.host").should eq("localhost")
      config.get("features.persistence").should eq("true")
      config.get("features.attention").should eq("false")
      
      File.delete(config_file)
    end
  end
  
  describe "JSON config format" do
    it "loads JSON configuration" do
      content = <<-JSON
        {
          "logging": {
            "level": "DEBUG",
            "file": "opencog.log"
          },
          "server": {
            "port": 17001,
            "host": "localhost"
          },
          "features": {
            "persistence": true,
            "attention": false
          }
        }
        JSON
      
      config_file = create_test_config(content, ".json")
      config = CogUtil::Config.new
      config.load_config_file(config_file)
      
      config.get("logging.level").should eq("DEBUG")
      config.get("logging.file").should eq("opencog.log")
      config.get("server.port").should eq("17001")
      config.get("server.host").should eq("localhost")
      config.get("features.persistence").should eq("true")
      config.get("features.attention").should eq("false")
      
      File.delete(config_file)
    end
  end
  
  describe "configuration management" do
    it "sets and gets values" do
      config = CogUtil::Config.new
      
      config.set("TEST_KEY", "test_value")
      config.get("TEST_KEY").should eq("test_value")
      
      config.set("NUMERIC_KEY", 42)
      config.get("NUMERIC_KEY").should eq("42")
    end
    
    it "checks key existence" do
      config = CogUtil::Config.new
      config.set("EXISTING_KEY", "value")
      
      config.has?("EXISTING_KEY").should be_true
      config.has?("NONEXISTENT_KEY").should be_false
    end
    
    it "returns all keys" do
      config = CogUtil::Config.new
      config.set("KEY1", "value1")
      config.set("KEY2", "value2")
      
      keys = config.keys
      keys.should contain("KEY1")
      keys.should contain("KEY2")
    end
    
    it "clears configuration" do
      config = CogUtil::Config.new
      config.set("KEY1", "value1")
      config.set("KEY2", "value2")
      
      config.clear
      config.keys.should be_empty
    end
    
    it "converts to hash" do
      config = CogUtil::Config.new
      config.set("KEY1", "value1")
      config.set("KEY2", "value2")
      
      hash = config.to_h
      hash["KEY1"].should eq("value1")
      hash["KEY2"].should eq("value2")
    end
  end
  
  describe "shortcuts module" do
    it "provides convenient access to common settings" do
      config = CogUtil::Config.new
      config.set("STORAGE_TYPE", "rocks")
      config.set("COGSERVER_PORT", "18001")
      config.set("ENABLE_PERSISTENCE", "true")
      
      # Since we can't easily replace the singleton, we'll just test
      # that the methods exist and don't crash
      CogUtil::Config::Shortcuts.atomspace_storage_type.should be_a(String)
      CogUtil::Config::Shortcuts.cogserver_port.should be_a(Int32)
      CogUtil::Config::Shortcuts.enable_persistence?.should be_a(Bool)
    end
  end
  
  describe "singleton access" do
    it "provides global configuration access" do
      CogUtil.config.should be_a(CogUtil::Config)
      
      CogUtil.config_set("GLOBAL_KEY", "global_value")
      CogUtil.config_get("GLOBAL_KEY").should eq("global_value")
      CogUtil.config_get("NONEXISTENT", "default").should eq("default")
    end
  end
end