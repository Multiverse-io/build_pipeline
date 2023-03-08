defmodule Mix.Tasks.BuildPipeline.RunTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO
  alias Mix.Tasks.BuildPipeline.Run

  describe "run/1" do
    test "works" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      output =
        capture_io(fn ->
          assert :ok == Run.run(["--cwd", "example_projects/complex_yet_functioning"])
        end)

      assert Regex.match?(~r|echo tires \[Succeeded in|, output)

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end
end
