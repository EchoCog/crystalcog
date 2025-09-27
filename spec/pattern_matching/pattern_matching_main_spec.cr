require "spec"
require "../../src/pattern_matching/pattern_matching_main"

describe "Pattern Matching Main" do
  describe "initialization" do
    it "initializes Pattern Matching system" do
      PatternMatching.initialize
      # Should not crash
    end

    it "has correct version" do
      PatternMatching::VERSION.should eq("0.1.0")
    end

    it "creates pattern matcher" do
      atomspace = AtomSpace::AtomSpace.new
      matcher = PatternMatching.create_matcher(atomspace)
      matcher.should be_a(PatternMatching::PatternMatcher)
    end
  end

  describe "main functionality" do
    it "provides pattern creation utilities" do
      PatternMatching.respond_to?(:create_pattern).should be_true
    end

    it "provides matching utilities" do
      PatternMatching.respond_to?(:match_pattern).should be_true
    end
  end

  describe "system integration" do
    it "integrates with AtomSpace" do
      CogUtil.initialize
      AtomSpace.initialize
      PatternMatching.initialize

      # Should work with atomspace
      atomspace = AtomSpace.create_atomspace
      matcher = PatternMatching.create_matcher(atomspace)
      matcher.should_not be_nil
    end
  end
end
