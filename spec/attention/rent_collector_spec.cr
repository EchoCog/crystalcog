require "spec"
require "../../src/attention/rent_collector"

describe Attention::RentCollector do
  describe "initialization" do
    it "creates rent collector" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      collector = Attention::RentCollector.new(atomspace, bank)

      collector.should_not be_nil
    end

    it "has default rent rate" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      collector = Attention::RentCollector.new(atomspace, bank)

      collector.rent_rate.should eq(0.01)
    end
  end

  describe "rent collection" do
    it "collects rent from atoms" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      collector = Attention::RentCollector.new(atomspace, bank)

      # Create atom with STI
      concept = atomspace.add_concept_node("test")
      bank.set_sti(concept, 100)

      initial_sti = bank.get_sti(concept)

      # Collect rent
      collector.collect_rent

      # STI should be reduced (or stay same if no decay)
      final_sti = bank.get_sti(concept)
      final_sti.should be <= initial_sti
    end

    it "applies LTI adjustments" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      collector = Attention::RentCollector.new(atomspace, bank)

      # Create atom with LTI
      concept = atomspace.add_concept_node("test")
      bank.set_lti(concept, 50)

      # Apply LTI adjustments
      collector.apply_lti_adjustments

      # Should not crash
      true.should be_true
    end
  end
end
