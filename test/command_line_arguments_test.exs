defmodule BuildPipeline.CommandLineArgumentsTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.CommandLineArguments

  @usage_instructions """
  usage ./build_pipline [--cwd ./path/to/directory/to/use] [--verbose or --debug]

  note: only --debug or --verbose can be set. It's one or the other
  """

  describe "parse/1" do
    test "with no args returns the default setup" do
      assert {:ok, %{cwd: ".", mode: :normal}} ==
               CommandLineArguments.parse([])
    end

    test "with --cwd set, returns it" do
      path = "./some/path"
      assert {:ok, %{cwd: ^path}} = CommandLineArguments.parse(["--cwd", path])
    end

    test "with some unrecognised nonsense returns error with the usage instructions" do
      assert {:error, {:bad_cmd_args, "total nonsense", @usage_instructions}} ==
               CommandLineArguments.parse(["total", "nonsense"])
    end

    test "with --verbose set, sets mode to verbose" do
      assert {:ok, %{mode: :verbose}} = CommandLineArguments.parse(["--verbose"])
    end

    test "with --debug set, sets mode to debug" do
      assert {:ok, %{mode: :debug}} = CommandLineArguments.parse(["--debug"])
    end

    test "with --debug and --verbose set, returns error & usage instructions" do
      assert {:error, {:bad_cmd_args, "--debug --verbose", @usage_instructions}} =
               CommandLineArguments.parse(["--debug", "--verbose"])

      assert {:error, {:bad_cmd_args, "--verbose --debug", @usage_instructions}} =
               CommandLineArguments.parse(["--verbose", "--debug"])

      assert {:error, {:bad_cmd_args, "--verbose --debug --cwd cool/path", @usage_instructions}} =
               CommandLineArguments.parse(["--verbose", "--debug", "--cwd", "cool/path"])
    end
  end
end
