# Crystal implementation of AtomSpace persistence interfaces
# Based on atomspace/opencog/persist/api/StorageNode.h
#
# This provides the base interface for persistent storage of AtomSpace contents.

require "./atom"
require "./truthvalue"
require "../cogutil/cogutil"
require "sqlite3"

module AtomSpace
  # Base interface for persistent storage
  abstract class StorageNode < Node
    def initialize(name : String)
      super(AtomType::STORAGE_NODE, name)
    end
    
    # Open connection to storage backend
    abstract def open : Bool
    
    # Close connection to storage backend  
    abstract def close : Bool
    
    # Check if connection is open
    abstract def connected? : Bool
    
    # Store a single atom
    abstract def store_atom(atom : Atom) : Bool
    
    # Fetch a single atom by handle
    abstract def fetch_atom(handle : Handle) : Atom?
    
    # Remove an atom from storage
    abstract def remove_atom(atom : Atom) : Bool
    
    # Store all atoms from AtomSpace
    abstract def store_atomspace(atomspace : AtomSpace) : Bool
    
    # Load all atoms into AtomSpace
    abstract def load_atomspace(atomspace : AtomSpace) : Bool
    
    # Get storage statistics
    abstract def get_stats : Hash(String, String | Int32 | Int64)
    
    # Bulk operations
    def store_atoms(atoms : Array(Atom)) : Bool
      success = true
      atoms.each do |atom|
        success = false unless store_atom(atom)
      end
      success
    end
    
    def fetch_atoms_by_type(type : AtomType) : Array(Atom)
      # Default implementation - subclasses should override for efficiency
      [] of Atom
    end
    
    # Utility methods
    protected def log_error(message : String)
      CogUtil::Logger.error("#{self.class.name}: #{message}")
    end
    
    protected def log_info(message : String)
      CogUtil::Logger.info("#{self.class.name}: #{message}")
    end
    
    protected def log_debug(message : String)
      CogUtil::Logger.debug("#{self.class.name}: #{message}")
    end
  end
  
  # File-based storage implementation
  class FileStorageNode < StorageNode
    @file_path : String
    @connected : Bool = false
    
    def initialize(name : String, @file_path : String)
      super(name)
      log_info("FileStorageNode created for: #{@file_path}")
    end
    
    def open : Bool
      return true if @connected
      
      begin
        # Ensure directory exists
        dir = File.dirname(@file_path)
        Dir.mkdir_p(dir) unless Dir.exists?(dir)
        
        # Test write access
        File.touch(@file_path) unless File.exists?(@file_path)
        
        @connected = true
        log_info("Opened file storage: #{@file_path}")
        true
      rescue ex
        log_error("Failed to open file storage: #{ex.message}")
        false
      end
    end
    
    def close : Bool
      @connected = false
      log_info("Closed file storage: #{@file_path}")
      true
    end
    
    def connected? : Bool
      @connected
    end
    
    def store_atom(atom : Atom) : Bool
      return false unless @connected
      
      begin
        File.open(@file_path, "a") do |file|
          file.puts(atom_to_scheme(atom))
        end
        log_debug("Stored atom: #{atom}")
        true
      rescue ex
        log_error("Failed to store atom: #{ex.message}")
        false
      end
    end
    
    def fetch_atom(handle : Handle) : Atom?
      return nil unless @connected
      
      # This is a simple implementation - in practice, we'd want indexing
      load_all_atoms.find { |atom| atom.handle == handle }
    end
    
    def remove_atom(atom : Atom) : Bool
      return false unless @connected
      
      begin
        # Read all atoms except the one to remove
        atoms = load_all_atoms.reject { |a| a.handle == atom.handle }
        
        # Rewrite file
        File.open(@file_path, "w") do |file|
          atoms.each { |a| file.puts(atom_to_scheme(a)) }
        end
        
        log_debug("Removed atom: #{atom}")
        true
      rescue ex
        log_error("Failed to remove atom: #{ex.message}")
        false
      end
    end
    
    def store_atomspace(atomspace : AtomSpace) : Bool
      return false unless @connected
      
      begin
        File.open(@file_path, "w") do |file|
          atomspace.get_all_atoms.each do |atom|
            file.puts(atom_to_scheme(atom))
          end
        end
        
        log_info("Stored AtomSpace (#{atomspace.size} atoms) to: #{@file_path}")
        true
      rescue ex
        log_error("Failed to store AtomSpace: #{ex.message}")
        false
      end
    end
    
    def load_atomspace(atomspace : AtomSpace) : Bool
      return false unless @connected
      
      begin
        return true unless File.exists?(@file_path)
        
        count = 0
        File.each_line(@file_path) do |line|
          line = line.strip
          next if line.empty? || line.starts_with?(';')
          
          atom = scheme_to_atom(line)
          if atom
            atomspace.add_atom(atom)
            count += 1
          end
        end
        
        log_info("Loaded #{count} atoms from: #{@file_path}")
        true
      rescue ex
        log_error("Failed to load AtomSpace: #{ex.message}")
        false
      end
    end
    
    def get_stats : Hash(String, String | Int32 | Int64)
      stats = Hash(String, String | Int32 | Int64).new
      stats["type"] = "FileStorage"
      stats["path"] = @file_path
      stats["connected"] = @connected ? "true" : "false"
      
      if File.exists?(@file_path)
        stats["file_size"] = File.size(@file_path)
        stats["file_exists"] = "true"
      else
        stats["file_exists"] = "false"
        stats["file_size"] = 0_i64
      end
      
      stats
    end
    
    # Convert atom to Scheme s-expression format
    private def atom_to_scheme(atom : Atom) : String
      case atom
      when Node
        tv_str = atom.truth_value == TruthValue::DEFAULT_TV ? "" : " #{atom.truth_value}"
        "(#{atom.type.to_s} \"#{atom.name}\"#{tv_str})"
      when Link
        outgoing_str = atom.outgoing.map { |a| atom_to_scheme(a) }.join(" ")
        tv_str = atom.truth_value == TruthValue::DEFAULT_TV ? "" : " #{atom.truth_value}"
        "(#{atom.type.to_s} #{outgoing_str}#{tv_str})"
      else
        atom.to_s
      end
    end
    
    # Convert Scheme s-expression to atom (simplified parser)
    private def scheme_to_atom(scheme : String) : Atom?
      # This is a simplified parser - in practice, we'd use a proper S-expression parser
      scheme = scheme.strip
      return nil unless scheme.starts_with?('(') && scheme.ends_with?(')')
      
      # Remove outer parentheses
      content = scheme[1..-2].strip
      
      # Split on first space to get type
      parts = content.split(' ', 2)
      return nil if parts.empty?
      
      begin
        type = AtomType.parse(parts[0])
        
        if type.node?
          # Parse node: (TYPE "name" [truth_value])
          if parts.size >= 2
            name_part = parts[1].strip
            if name_part.starts_with?('"') && name_part[1..].includes?('"')
              quote_end = name_part.index('"', 1)
              if quote_end
                name = name_part[1...quote_end]
                return Node.new(type, name)
              end
            end
          end
        else
          # Parse link: (TYPE atom1 atom2 ... [truth_value])
          # This would require recursive parsing - simplified for now
          return Link.new(type, [] of Atom)
        end
      rescue
        return nil
      end
      
      nil
    end
    
    # Load all atoms from file
    private def load_all_atoms : Array(Atom)
      atoms = [] of Atom
      return atoms unless File.exists?(@file_path)
      
      File.each_line(@file_path) do |line|
        line = line.strip
        next if line.empty? || line.starts_with?(';')
        
        atom = scheme_to_atom(line)
        atoms << atom if atom
      end
      
      atoms
    end
  end
  
  # SQLite-based storage implementation
  class SQLiteStorageNode < StorageNode
    @db_path : String
    @db : DB::Database?
    @connected : Bool = false
    
    def initialize(name : String, @db_path : String)
      super(name)
      log_info("SQLiteStorageNode created for: #{@db_path}")
    end
    
    def open : Bool
      return true if @connected
      
      begin
        # Ensure directory exists
        dir = File.dirname(@db_path)
        Dir.mkdir_p(dir) unless Dir.exists?(dir)
        
        @db = DB.open("sqlite3:#{@db_path}")
        create_tables
        
        @connected = true
        log_info("Opened SQLite storage: #{@db_path}")
        true
      rescue ex
        log_error("Failed to open SQLite storage: #{ex.message}")
        false
      end
    end
    
    def close : Bool
      if @db
        @db.try(&.close)
        @db = nil
      end
      @connected = false
      log_info("Closed SQLite storage: #{@db_path}")
      true
    end
    
    def connected? : Bool
      @connected
    end
    
    def store_atom(atom : Atom) : Bool
      return false unless @connected || !@db
      
      begin
        db = @db.not_nil!
        
        case atom
        when Node
          db.exec(
            "INSERT OR REPLACE INTO atoms (handle, type, name, truth_strength, truth_confidence) VALUES (?, ?, ?, ?, ?)",
            atom.handle.to_s, atom.type.to_s, atom.name, 
            atom.truth_value.strength, atom.truth_value.confidence
          )
        when Link
          # Store the link
          db.exec(
            "INSERT OR REPLACE INTO atoms (handle, type, name, truth_strength, truth_confidence) VALUES (?, ?, ?, ?, ?)",
            atom.handle.to_s, atom.type.to_s, "", 
            atom.truth_value.strength, atom.truth_value.confidence
          )
          
          # Store outgoing relationships
          db.exec("DELETE FROM outgoing WHERE link_handle = ?", atom.handle.to_s)
          atom.outgoing.each_with_index do |target, position|
            db.exec(
              "INSERT INTO outgoing (link_handle, target_handle, position) VALUES (?, ?, ?)",
              atom.handle.to_s, target.handle.to_s, position
            )
          end
        end
        
        log_debug("Stored atom in SQLite: #{atom}")
        true
      rescue ex
        log_error("Failed to store atom in SQLite: #{ex.message}")
        false
      end
    end
    
    def fetch_atom(handle : Handle) : Atom?
      return nil unless @connected || !@db
      
      begin
        db = @db.not_nil!
        
        # Get atom data
        db.query("SELECT type, name, truth_strength, truth_confidence FROM atoms WHERE handle = ?", handle.to_s) do |rs|
          if rs.move_next
            type = AtomType.parse(rs.read(String))
            name = rs.read(String)
            strength = rs.read(Float64)
            confidence = rs.read(Float64)
            tv = SimpleTruthValue.new(strength, confidence)
            
            if type.node?
              return Node.new(type, name, tv)
            else
              # Get outgoing atoms for links
              outgoing = [] of Atom
              db.query("SELECT target_handle FROM outgoing WHERE link_handle = ? ORDER BY position", handle.to_s) do |out_rs|
                while out_rs.move_next
                  target_handle = Handle.new(out_rs.read(String))
                  target_atom = fetch_atom(target_handle)
                  outgoing << target_atom if target_atom
                end
              end
              return Link.new(type, outgoing, tv)
            end
          end
        end
        
        nil
      rescue ex
        log_error("Failed to fetch atom from SQLite: #{ex.message}")
        nil
      end
    end
    
    def remove_atom(atom : Atom) : Bool
      return false unless @connected || !@db
      
      begin
        db = @db.not_nil!
        
        # Remove outgoing relationships if it's a link
        db.exec("DELETE FROM outgoing WHERE link_handle = ?", atom.handle.to_s)
        
        # Remove the atom
        db.exec("DELETE FROM atoms WHERE handle = ?", atom.handle.to_s)
        
        log_debug("Removed atom from SQLite: #{atom}")
        true
      rescue ex
        log_error("Failed to remove atom from SQLite: #{ex.message}")
        false
      end
    end
    
    def store_atomspace(atomspace : AtomSpace) : Bool
      return false unless @connected
      
      begin
        db = @db.not_nil!
        
        # Clear existing data
        db.exec("DELETE FROM outgoing")
        db.exec("DELETE FROM atoms")
        
        # Store all atoms
        atomspace.get_all_atoms.each do |atom|
          store_atom(atom)
        end
        
        log_info("Stored AtomSpace (#{atomspace.size} atoms) to SQLite: #{@db_path}")
        true
      rescue ex
        log_error("Failed to store AtomSpace to SQLite: #{ex.message}")
        false
      end
    end
    
    def load_atomspace(atomspace : AtomSpace) : Bool
      return false unless @connected || !@db
      
      begin
        db = @db.not_nil!
        count = 0
        
        # Load all atoms (nodes first, then links)
        db.query("SELECT handle FROM atoms ORDER BY CASE WHEN name = '' THEN 1 ELSE 0 END") do |rs|
          while rs.move_next
            handle = Handle.new(rs.read(String))
            atom = fetch_atom(handle)
            if atom
              atomspace.add_atom(atom)
              count += 1
            end
          end
        end
        
        log_info("Loaded #{count} atoms from SQLite: #{@db_path}")
        true
      rescue ex
        log_error("Failed to load AtomSpace from SQLite: #{ex.message}")
        false
      end
    end
    
    def get_stats : Hash(String, String | Int32 | Int64)
      stats = Hash(String, String | Int32 | Int64).new
      stats["type"] = "SQLiteStorage"
      stats["path"] = @db_path
      stats["connected"] = @connected ? "true" : "false"
      
      if @connected && @db
        begin
          db = @db.not_nil!
          db.query("SELECT COUNT(*) FROM atoms") do |rs|
            stats["atom_count"] = rs.move_next ? rs.read(Int64) : 0_i64
          end
          db.query("SELECT COUNT(*) FROM outgoing") do |rs|
            stats["link_count"] = rs.move_next ? rs.read(Int64) : 0_i64
          end
        rescue ex
          log_error("Failed to get SQLite stats: #{ex.message}")
        end
      end
      
      if File.exists?(@db_path)
        stats["file_size"] = File.size(@db_path)
      end
      
      stats
    end
    
    private def create_tables
      return unless @db
      
      db = @db.not_nil!
      
      # Create atoms table
      db.exec <<-SQL
        CREATE TABLE IF NOT EXISTS atoms (
          handle TEXT PRIMARY KEY,
          type TEXT NOT NULL,
          name TEXT,
          truth_strength REAL DEFAULT 1.0,
          truth_confidence REAL DEFAULT 1.0,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )
      SQL
      
      # Create outgoing relationships table
      db.exec <<-SQL
        CREATE TABLE IF NOT EXISTS outgoing (
          link_handle TEXT NOT NULL,
          target_handle TEXT NOT NULL,
          position INTEGER NOT NULL,
          PRIMARY KEY (link_handle, position),
          FOREIGN KEY (link_handle) REFERENCES atoms(handle),
          FOREIGN KEY (target_handle) REFERENCES atoms(handle)
        )
      SQL
      
      # Create indexes for performance
      db.exec "CREATE INDEX IF NOT EXISTS idx_atoms_type ON atoms(type)"
      db.exec "CREATE INDEX IF NOT EXISTS idx_atoms_name ON atoms(name)"
      db.exec "CREATE INDEX IF NOT EXISTS idx_outgoing_target ON outgoing(target_handle)"
      
      log_debug("Created SQLite tables and indexes")
    end
  end
  
  # Network storage implementation (for CogServer communication)
  class CogStorageNode < StorageNode
    @host : String
    @port : Int32
    @connected : Bool = false
    @base_url : String
    
    def initialize(name : String, @host : String, @port : Int32)
      super(name)
      @base_url = "http://#{@host}:#{@port}"
      log_info("CogStorageNode created for: #{@base_url}")
    end
    
    def open : Bool
      return true if @connected
      
      begin
        # Test connection with ping
        response = HTTP::Client.get("#{@base_url}/ping")
        
        if response.status_code == 200
          @connected = true
          log_info("Connected to CogServer: #{@base_url}")
          true
        else
          log_error("Failed to connect to CogServer: status #{response.status_code}")
          false
        end
      rescue ex
        log_error("Failed to connect to CogServer: #{ex.message}")
        false
      end
    end
    
    def close : Bool
      @connected = false
      log_info("Disconnected from CogServer: #{@base_url}")
      true
    end
    
    def connected? : Bool
      @connected
    end
    
    def store_atom(atom : Atom) : Bool
      return false unless @connected
      
      begin
        
        data = {
          "type" => atom.type.to_s,
          "name" => atom.responds_to?(:name) ? atom.name : nil,
          "outgoing" => atom.responds_to?(:outgoing) ? atom.outgoing.map(&.handle.to_s) : nil,
          "truth_value" => {
            "strength" => atom.truth_value.strength,
            "confidence" => atom.truth_value.confidence
          }
        }
        
        headers = HTTP::Headers{"Content-Type" => "application/json"}
        response = HTTP::Client.post("#{@base_url}/atoms", headers: headers, body: data.to_json)
        
        if response.status_code == 201
          log_debug("Stored atom via network: #{atom}")
          true
        else
          log_error("Failed to store atom via network: status #{response.status_code}")
          false
        end
      rescue ex
        log_error("Failed to store atom via network: #{ex.message}")
        false
      end
    end
    
    def fetch_atom(handle : Handle) : Atom?
      return nil unless @connected
      
      begin
        response = HTTP::Client.get("#{@base_url}/atoms/#{handle}")
        
        if response.status_code == 200
          # Parse JSON response and recreate atom
          # This would require proper JSON parsing and atom reconstruction
          log_debug("Fetched atom via network: #{handle}")
          nil # Simplified - would return actual atom
        else
          nil
        end
      rescue ex
        log_error("Failed to fetch atom via network: #{ex.message}")
        nil
      end
    end
    
    def remove_atom(atom : Atom) : Bool
      return false unless @connected
      
      begin
        response = HTTP::Client.delete("#{@base_url}/atoms/#{atom.handle}")
        
        response.status_code == 200
      rescue ex
        log_error("Failed to remove atom via network: #{ex.message}")
        false
      end
    end
    
    def store_atomspace(atomspace : AtomSpace) : Bool
      # Store atoms one by one
      atomspace.get_all_atoms.all? { |atom| store_atom(atom) }
    end
    
    def load_atomspace(atomspace : AtomSpace) : Bool
      return false unless @connected
      
      begin
        
        response = HTTP::Client.get("#{@base_url}/atoms")
        
        if response.status_code == 200
          data = JSON.parse(response.body)
          count = 0
          
          # This would require proper JSON parsing and atom reconstruction
          log_info("Loaded #{count} atoms from CogServer: #{@base_url}")
          true
        else
          false
        end
      rescue ex
        log_error("Failed to load AtomSpace from network: #{ex.message}")
        false
      end
    end
    
    def get_stats : Hash(String, String | Int32 | Int64)
      stats = Hash(String, String | Int32 | Int64).new
      stats["type"] = "CogStorage"
      stats["host"] = @host
      stats["port"] = @port
      stats["connected"] = @connected ? "true" : "false"
      stats["base_url"] = @base_url
      
      if @connected
        begin
          
          response = HTTP::Client.get("#{@base_url}/status")
          if response.status_code == 200
            server_stats = JSON.parse(response.body)
            stats["remote_atomspace_size"] = server_stats["atomspace_size"]?.try(&.as_i64) || 0_i64
          end
        rescue ex
          log_error("Failed to get network stats: #{ex.message}")
        end
      end
      
      stats
    end
  end
end