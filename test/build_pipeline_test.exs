defmodule BuildPipelineTest do
  use ExUnit.Case, async: false
  use Mimic
  import ExUnit.CaptureIO
  alias BuildPipeline
  alias BuildPipeline.TerminalWidth.TputCols
  alias BuildPipeline.Mocks.TputCols.{NotOnSystem, NonsenseResult}

  describe "main" do
    test "can show runner output on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
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

    test "returns error if config file parsing fails" do
      output =
        capture_io(fn ->
          assert :error ==
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/bad_config"
                   ])
        end)

      assert output =~ "I failed to parse the build_pipeline_config"
    end

    test "returns error if given nonsense CLI args" do
      output =
        capture_io(fn ->
          assert :error ==
                   BuildPipeline.main([
                     "--nonesense"
                   ])
        end)

      assert output =~ "There was at least one bad argument in the command line"
    end

    test "returns error if the config file is not found" do
      output =
        capture_io(fn ->
          assert :error ==
                   BuildPipeline.main([
                     "--cwd",
                     "./not/a_real/directory"
                   ])
        end)

      assert output =~ "I failed to find a config.json file"
    end

    test "returns error if the config is invalid JSON" do
      output =
        capture_io(fn ->
          assert :error ==
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/invalid_json"
                   ])
        end)

      assert output == "I failed to parse the config.json because it was not valid JSON\n"
    end

    test "returns error if we can't determine the terminals width because tput can't be run" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &NotOnSystem.run/0)

      output =
        capture_io(fn ->
          assert :error ==
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
                   ])
        end)

      assert output ==
               "I tried to run 'tput cols' but it failed because it looks like I'm not able to run the 'tput' binary?\n"
    end

    test "returns error if we can't determine the terminals width because tput returns nonsense" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &NonsenseResult.run/0)

      output =
        capture_io(fn ->
          assert :error ==
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
                   ])
        end)

      assert output ==
               "I tried to run 'tput cols' but it returned a result I couldn't parse! Damn\n"
    end
  end
end
