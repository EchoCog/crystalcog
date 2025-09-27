require "spec"
require "../../src/pattern_mining/pattern_mining_main"

describe "Pattern Mining Main" do
  describe "initialization" do
    it "initializes Pattern Mining system" do
      PatternMining.initialize
      # Should not crash
    end

    it "has correct version" do
      PatternMining::VERSION.should eq("0.1.0")
    end

    it "creates pattern miner" do
      atomspace = AtomSpace::AtomSpace.new
      miner = PatternMining.create_miner(atomspace)
      miner.should be_a(PatternMining::PatternMiner)
    end
  end

  describe "mining functionality" do
    it "provides mining utilities" do
      PatternMining.respond_to?(:mine_patterns).should be_true
    end

    it "provides frequency analysis" do
      PatternMining.respond_to?(:analyze_frequency).should be_true
    end
  end

  describe "system integration" do
    it "integrates with AtomSpace" do
      CogUtil.initialize
      AtomSpace.initialize
      PatternMining.initialize

      # Should work with atomspace
      atomspace = AtomSpace.create_atomspace
      miner = PatternMining.create_miner(atomspace)
      miner.should_not be_nil
    end
  end
end
