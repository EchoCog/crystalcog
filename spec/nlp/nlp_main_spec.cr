require "spec"
require "../../src/nlp/nlp_main"

describe "NLP Main" do
  describe "initialization" do
    it "initializes NLP system" do
      NLP.initialize
      # Should not crash
    end
    
    it "has correct version" do
      NLP::VERSION.should eq("0.1.0")
    end
    
    it "creates NLP processor" do
      processor = NLP.create_processor
      processor.should be_a(NLP::TextProcessor)
    end
  end
  
  describe "NLP functionality" do
    it "provides tokenization" do
      NLP.respond_to?(:tokenize).should be_true
    end
    
    it "provides text processing" do
      NLP.respond_to?(:process_text).should be_true
    end
    
    it "provides linguistic atom creation" do
      NLP.respond_to?(:create_linguistic_atoms).should be_true
    end
  end
  
  describe "system integration" do
    it "integrates with AtomSpace" do
      CogUtil.initialize
      AtomSpace.initialize
      NLP.initialize
      
      # Should work with atomspace
      atomspace = AtomSpace.create_atomspace
      processor = NLP.create_processor(atomspace)
      processor.should_not be_nil
    end
  end
end