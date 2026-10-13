require "spec"
require "../../src/tools/cogshell"

describe CrystalCog::Shell do
  it "preserves knowledge across commands and exits cleanly" do
    input = IO::Memory.new("add blue whale\nadd blue whale\nlist\nstats\nquit\nadd ignored\n")
    output = IO::Memory.new
    shell = CrystalCog::Shell.new(input: input, output: output)
    shell.run
    shell.atomspace.size.should eq(1)
    shell.atomspace.get_nodes_by_name("blue whale").size.should eq(1)
    output.to_s.should contain("Atoms: 1, nodes: 1, links: 0")
    shell.atomspace.get_nodes_by_name("ignored").should be_empty
  end

  it "handles missing arguments, unknown commands, and EOF" do
    output = IO::Memory.new
    shell = CrystalCog::Shell.new(input: IO::Memory.new("add\nunknown\n"), output: output)
    shell.run
    output.to_s.should contain("Usage: add NAME")
    output.to_s.should contain("Unknown command: unknown")
    shell.atomspace.size.should eq(0)
  end

  it "accepts blank commands and exposes bounded reasoning" do
    output = IO::Memory.new
    shell = CrystalCog::Shell.new(output: output)
    shell.execute("").should be_true
    shell.execute("help").should be_true
    shell.execute("reason").should be_true
    output.to_s.should contain("Derived 0 inferences")
    shell.execute("exit").should be_false
  end
end
