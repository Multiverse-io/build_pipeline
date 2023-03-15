defmodule BuildPipelineTest do
  use ExUnit.Case, async: false
  use Mimic
  import ExUnit.CaptureIO
  alias BuildPipeline
  alias BuildPipeline.TerminalWidth.TputCols
  alias BuildPipeline.Mocks.TputCols.{NotOnSystem, NonsenseResult}

  @moduletag timeout: 1_000

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
      assert output =~ "echo tires [Succeeded in"

      assert output =~ "echo fuel [Pending]"
      assert output =~ "echo fuel [Running]"
      assert output =~ "echo fuel [Succeeded in"

      assert output =~ "echo car works [Pending]"
      assert output =~ "echo car works [Running]"
      assert output =~ "echo car works [Succeeded in"

      assert output =~ "echo walk over [Pending]"
      assert output =~ "echo walk over [Running]"
      assert output =~ "echo walk over [Succeeded in"

      assert output =~ "echo hello [Pending]"
      assert output =~ "echo hello [Running]"
      assert output =~ "echo hello [Succeeded in"

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "with mode = debug, can show runner output on the screen & run to completion" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      output =
        capture_io(fn ->
          assert :ok ==
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning",
                     "--debug"
                   ])
        end)

      assert output =~ "echo tires [Pending]"
      assert output =~ "echo tires [Running]"
      assert output =~ "echo tires [Succeeded in"

      assert output =~ "echo fuel [Pending]"
      assert output =~ "echo fuel [Running]"
      assert output =~ "echo fuel [Succeeded in"

      assert output =~ "echo car works [Pending]"
      assert output =~ "echo car works [Running]"
      assert output =~ "echo car works [Succeeded in"

      assert output =~ "echo walk over [Pending]"
      assert output =~ "echo walk over [Running]"
      assert output =~ "echo walk over [Succeeded in"

      assert output =~ "echo hello [Pending]"
      assert output =~ "echo hello [Running]"
      assert output =~ "echo hello [Succeeded in"

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

    test "when save-result (sr) is set, when all build steps succeed - writes the result of each step to file" do
      previous_run_result_file =
        "./example_projects/complex_yet_functioning/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok =
        BuildPipeline.main([
          "--cwd",
          "./example_projects/complex_yet_functioning",
          "--sr"
        ])

      previous_run_result =
        previous_run_result_file
        |> File.read!()
        |> Jason.decode!()

      assert previous_run_result == [
               %{
                 "buildStepName" => "tiresNotSlashed",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "enoughFuel",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "carWorks",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "driveToOffice",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "approachHuman",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "sayHello",
                 "result" => "successful"
               }
             ]

      File.rm(previous_run_result_file)
    end

    test "when save-result (sr) is NOT set, we don't save the result to file" do
      previous_run_result_file =
        "./example_projects/complex_yet_functioning/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok =
        BuildPipeline.main([
          "--cwd",
          "./example_projects/complex_yet_functioning"
        ])

      assert File.exists?(previous_run_result_file) == false
    end

    test "when save-result (sr) is set, when a build step fails - writes the result of each step to file" do
      previous_run_result_file =
        "./example_projects/complex_and_failing/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok =
        BuildPipeline.main([
          "--cwd",
          "./example_projects/complex_and_failing",
          "--sr"
        ])

      previous_run_result =
        previous_run_result_file
        |> File.read!()
        |> Jason.decode!()

      assert previous_run_result == [
               %{
                 "buildStepName" => "tiresNotSlashed",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "enoughFuel",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "carWorks",
                 "result" => "successful"
               },
               %{
                 "buildStepName" => "driveToOffice",
                 "result" => "failed"
               },
               %{
                 "buildStepName" => "approachHuman",
                 "result" => "not started"
               },
               %{
                 "buildStepName" => "sayHello",
                 "result" => "not started"
               }
             ]
    end

    test "when from failed (ff) is set, save the result to file just like with save result (sr)" do
      previous_run_result_file =
        "./example_projects/complex_and_failing/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok =
        BuildPipeline.main([
          "--cwd",
          "./example_projects/complex_and_failing",
          "--sr"
        ])

      :ok =
        BuildPipeline.main([
          "--cwd",
          "./example_projects/complex_and_failing",
          "--ff"
        ])

      previous_run_result =
        previous_run_result_file
        |> File.read!()
        |> Jason.decode!()

      assert previous_run_result == [
               %{
                 "buildStepName" => "tiresNotSlashed",
                 "result" => "skipped"
               },
               %{
                 "buildStepName" => "enoughFuel",
                 "result" => "skipped"
               },
               %{
                 "buildStepName" => "carWorks",
                 "result" => "skipped"
               },
               %{
                 "buildStepName" => "driveToOffice",
                 "result" => "failed"
               },
               %{
                 "buildStepName" => "approachHuman",
                 "result" => "not started"
               },
               %{
                 "buildStepName" => "sayHello",
                 "result" => "not started"
               }
             ]

      File.rm(previous_run_result_file)
    end

    test "when from failed (ff) is set, and a previous_run_result file exists, then previously successful build steps are skipped" do
      previous_run_result_file =
        "./example_projects/complex_and_failing/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok = BuildPipeline.main(["--cwd", "./example_projects/complex_and_failing", "--sr"])

      :ok = BuildPipeline.main(["--cwd", "./example_projects/complex_and_failing", "--ff"])

      previous_run_result = previous_run_result_file |> File.read!() |> Jason.decode!()

      assert previous_run_result == [
               %{
                 "buildStepName" => "tiresNotSlashed",
                 "result" => "skipped"
               },
               %{
                 "buildStepName" => "enoughFuel",
                 "result" => "skipped"
               },
               %{
                 "buildStepName" => "carWorks",
                 "result" => "skipped"
               },
               %{
                 "buildStepName" => "driveToOffice",
                 "result" => "failed"
               },
               %{
                 "buildStepName" => "approachHuman",
                 "result" => "not started"
               },
               %{
                 "buildStepName" => "sayHello",
                 "result" => "not started"
               }
             ]

      File.rm(previous_run_result_file)
    end

    test "when from failed (ff) is set, but the file can't be found, returns an error" do
      previous_run_result_file =
        "./example_projects/complex_and_failing/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      output =
        capture_io(fn ->
          assert :error =
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/complex_and_failing",
                     "--ff"
                   ])
        end)

      assert output =~
               "You asked me to run only the steps that failed last time, and I tried to look in \n#{previous_run_result_file}\nfor a file containing the results of the last run, but there was nothing there, so I'm crashing now *death noise*"
    end

    test "when from failed (ff) is set, but with invalid JSON in the file, returns an error" do
      output =
        capture_io(fn ->
          assert :error =
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/invalid_previous_run_result_json",
                     "--ff"
                   ])
        end)

      assert output =~
               "I failed to parse the previous_run_result.json because it was not valid JSON. I suggest you delete it and run the full build from scratch"
    end

    test "when from failed (ff) is set, but with valid JSON we can't parse, returns an error" do
      output =
        capture_io(fn ->
          assert :error =
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/valid_nonsense_previous_run_result_json",
                     "--ff"
                   ])
        end)

      assert output =~
               "I couldn't parse the result the previous run!\n\nI need a JSON list containing a list of only {buildStepName, result},\nbut I was given {\"nonsense\": \"json\"}\n.\n\nI suggest you delete your previous_run_result.json file & run the whole build from scratch...\n\n"
    end

    test "when from failed (ff) is set, but with valid JSON but containing a result we can't parse, returns an error" do
      output =
        capture_io(fn ->
          assert :error =
                   BuildPipeline.main([
                     "--cwd",
                     "./example_projects/bad_build_step_name_previous_run_result_json",
                     "--ff"
                   ])
        end)

      assert output =~
               "I couldn't parse the result the previous run!\n\nI need a JSON list containing a list of only {buildStepName, result},\nbut I was given a result I didn't recognise of \"nonsense\"\n\nI suggest you delete your previous_run_result.json file & run the whole build from scratch...\n\n"
    end
  end
end
