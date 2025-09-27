require "spec"
require "../../src/attention/allocation_engine"

describe Attention::AllocationEngine do
  describe "initialization" do
    it "creates allocation engine" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      engine = Attention::AllocationEngine.new(atomspace, bank)

      engine.should_not be_nil
    end

    it "has default parameters" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      engine = Attention::AllocationEngine.new(atomspace, bank)

      engine.atomspace.should eq(atomspace)
      engine.bank.should eq(bank)
    end
  end

  describe "allocation functionality" do
    it "performs attention allocation" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      engine = Attention::AllocationEngine.new(atomspace, bank)

      # Add some atoms
      concept = atomspace.add_concept_node("test")

      # Should be able to run allocation
      engine.run_allocation(1)
      # Should not crash
    end

    it "respects cycle limits" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      engine = Attention::AllocationEngine.new(atomspace, bank)

      # Should complete allocation cycles
      engine.run_allocation(3)
      # Should not crash or hang
    end
  end
end
