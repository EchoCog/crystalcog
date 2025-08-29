require "spec"

# Require all cogutil specs
require "./cogutil/logger_spec"
require "./cogutil/config_spec"
require "./cogutil/randgen_spec"

# Main spec runner for all CogUtil tests
describe "CogUtil Integration Tests" do
  it "initializes CogUtil system correctly" do
    # This would test the full initialization but we need to be careful
    # not to interfere with global state in other tests
    pending "Full integration test needs isolated environment"
  end
end