defmodule BuildPipelineTest do
  use ExUnit.Case, async: true
  alias BuildPipeline
  import ExUnit.CaptureIO

  describe "main/1" do
    test "given run & args for run, it runs Run.main/1" do
      assert :ok ==
               BuildPipeline.main([
                 "run",
                 "--cwd",
                 "./example_projects/complex_yet_functioning"
               ])
    end
  end
end
