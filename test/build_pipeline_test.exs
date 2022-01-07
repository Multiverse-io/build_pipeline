defmodule BuildPipelineTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO
  alias BuildPipeline

  describe "main" do
    # TODO test what to do in each error case (in preflight checks htat is)
    test "runs commands for a simple working case & prints command output if the flag is passed" do
      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main([
                     "--cwd",
                     "./test/example_projects/complex_yet_functioning",
                     "--print-cmd-output"
                   ])
        end)

      assert output =~ "tires"
      assert output =~ "fuel"
      assert output =~ "car works"
      assert output =~ "drive"
      assert output =~ "walk over"
      assert output =~ "hello"
    end

    test "does not print command output if print-cmd-output is not set" do
      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main([
                     "--cwd",
                     "./test/example_projects/complex_yet_functioning"
                   ])
        end)

      refute output =~ "tires"
      refute output =~ "fuel"
      refute output =~ "car works"
      refute output =~ "drive"
      refute output =~ "walk over"
      refute output =~ "hello"
    end
  end
end
