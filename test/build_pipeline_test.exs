defmodule BuildPipelineTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO
  alias BuildPipeline

  describe "main" do
    # TODO test what to do in each error case (in preflight checks htat is)
    test "can show runner output on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      # assert :ok ==
      #         BuildPipeline.main([
      #           "--cwd",
      #           "./test/example_projects/complex_yet_functioning"
      #         ])

      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main([
                     "--cwd",
                     "./test/example_projects/complex_yet_functioning"
                   ])
        end)

      assert output =~ "echo tires [Pending]"
      assert output =~ "echo tires [Running]"
      assert output =~ "echo tires [Finished in"

      assert output =~ "echo fuel [Pending]"
      assert output =~ "echo fuel [Running]"
      assert output =~ "echo fuel [Finished in"

      assert output =~ "echo car works [Pending]"
      assert output =~ "echo car works [Running]"
      assert output =~ "echo car works [Finished in"

      assert output =~ "echo walk over [Pending]"
      assert output =~ "echo walk over [Running]"
      assert output =~ "echo walk over [Finished in"

      assert output =~ "echo hello [Pending]"
      assert output =~ "echo hello [Running]"
      assert output =~ "echo hello [Finished in"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end
end
