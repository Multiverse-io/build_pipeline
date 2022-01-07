defmodule BuildPipeline.CommandLineArgumentsTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.CommandLineArguments

  @usage_instructions """
  usage ./build_pipline [--cwd ./path/to/directory/to/use] [--print-cmd-output]
  """

  describe "parse/1" do
    test "with no args returns the default setup" do
      assert {:ok, %{cwd: ".", print_cmd_output: false}} == CommandLineArguments.parse([])
    end

    test "with --cwd set, returns it" do
      path = "./some/path"
      assert {:ok, %{cwd: ^path}} = CommandLineArguments.parse(["--cwd", path])
    end

    test "with some unrecognised nonsense returns error with the usage instructions" do
      assert {:error, {:bad_cmd_args, "total nonsense", @usage_instructions}} ==
               CommandLineArguments.parse(["total", "nonsense"])
    end

    test "with --print-cmd-output set, returns it" do
      assert {:ok, %{print_cmd_output: true}} = CommandLineArguments.parse(["--print-cmd-output"])
    end

    test "without --print-cmd-output set, returns print_cmd_output false" do
      assert {:ok, %{print_cmd_output: false}} = CommandLineArguments.parse([])
    end
  end
end
