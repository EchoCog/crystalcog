require "spec"
require "../../src/attention/attention_main"

describe "Attention Main" do
  describe "initialization" do
    it "initializes Attention system" do
      Attention.initialize
      # Should not crash
    end

    it "has correct version" do
      Attention::VERSION.should eq("0.1.0")
    end

    it "creates attention engine" do
      atomspace = AtomSpace::AtomSpace.new
      engine = Attention.create_engine(atomspace)
      engine.should be_a(Attention::AllocationEngine)
    end
  end

  describe "attention functionality" do
    it "provides attention allocation" do
      Attention.respond_to?(:allocate_attention).should be_true
    end

    it "provides rent collection" do
      Attention.respond_to?(:collect_rent).should be_true
    end

    it "provides diffusion" do
      Attention.respond_to?(:diffuse_attention).should be_true
    end
  end

  describe "system integration" do
    it "integrates with AtomSpace" do
      CogUtil.initialize
      AtomSpace.initialize
      Attention.initialize

      # Should work with atomspace
      atomspace = AtomSpace.create_atomspace
      engine = Attention.create_engine(atomspace)
      engine.should_not be_nil
    end
  end
end
