defmodule BuildPipelineTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO
  alias BuildPipeline

  describe "main" do
    # TODO test what to do in each error case
    test "runs commands for a simple working case" do
      capture_io(fn ->
        assert {:ok, _pid} =
                 BuildPipeline.main(["--cwd", "./test/example_projects/simple_and_functioning"])
      end)
      |> IO.inspect()
    end
  end
end
