defmodule BuildPipelineTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO
  alias BuildPipeline

  describe "main" do
    # TODO test what to do in each error case (in preflight checks htat is)
    test "runs commands for a simple working case" do
      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main(["--cwd", "./test/example_projects/complex_yet_functioning"])
        end)

      assert output =~ "tires"
      assert output =~ "fuel"
      assert output =~ "car works"
      assert output =~ "drive"
      assert output =~ "walk over"
      assert output =~ "hello"
    end

    test "x" do
      assert :ok ==
               BuildPipeline.main(["--cwd", "./test/example_projects/complex_yet_functioning"])
    end
  end
end
