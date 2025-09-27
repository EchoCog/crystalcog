require "spec"
require "../../src/attention/attention_bank"

describe Attention::AttentionBank do
  describe "initialization" do
    it "creates attention bank" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)

      bank.should_not be_nil
    end

    it "has default STI and LTI funds" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)

      bank.sti_funds.should eq(10000)
      bank.lti_funds.should eq(10000)
    end

    it "allows custom funds" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace, 5000, 3000)

      bank.sti_funds.should eq(5000)
      bank.lti_funds.should eq(3000)
    end
  end

  describe "attention values" do
    it "sets STI values" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      concept = atomspace.add_concept_node("test")

      bank.set_sti(concept, 100)
      bank.get_sti(concept).should eq(100)
    end

    it "sets LTI values" do
      atomspace = AtomSpace::AtomSpace.new
      bank = Attention::AttentionBank.new(atomspace)
      concept = atomspace.add_concept_node("test")

      bank.set_lti(concept, 50)
      bank.get_lti(concept).should eq(50)
    end
  end
end
