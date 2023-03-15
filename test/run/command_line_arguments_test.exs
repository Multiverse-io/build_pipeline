defmodule BuildPipeline.Run.CommandLineArgumentsTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.CommandLineArguments

  @usage_instructions """
  usage: ./bp run [--cwd ./path/to/directory/to/use] [--verbose or --debug] [--sr] [--ff]

  --verbose  - prints output from successful as well as failed build steps to the terminal. Cannot be set with --debug
  --debug    - build steps run one at a time and their output is printed to the terminal in real time. Cannot be set with --verbose
  --cwd path - the path in which to look for the build_pipeline config.json and build scripts. Defaults to "."
  --sr       - save-result: saves the results of this run to "<cwd>/previous_run_result.json"
  --ff       - from-failed: sets save-result (--sr) and also if "<cwd>/previous_run_result.json" exists, then only build steps that were either failed or not started from the previous build will run. Previously successful build steps will not be run. If no previous_run_result.json file is found then I exit and tell you I couldn't do as you asked.
  """

  describe "parse/1" do
    test "with no args returns the default setup" do
      assert {:ok, %{cwd: ".", mode: :normal, save_result: false, run_from_failed: false}} ==
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
      assert {:error, {:bad_cmd_args, @usage_instructions}} =
               CommandLineArguments.parse(["--debug", "--verbose"])

      assert {:error, {:bad_cmd_args, @usage_instructions}} =
               CommandLineArguments.parse(["--verbose", "--debug"])

      assert {:error, {:bad_cmd_args, @usage_instructions}} =
               CommandLineArguments.parse(["--verbose", "--debug", "--cwd", "cool/path"])
    end

    test "with --sr result, returns save_result: true" do
      assert {:ok, %{save_result: true}} = CommandLineArguments.parse(["--sr"])
    end

    test "with --ff from failed, returns save_result: true" do
      assert {:ok, %{save_result: true}} = CommandLineArguments.parse(["--ff"])
    end
  end
end
