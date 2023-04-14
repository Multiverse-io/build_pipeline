defmodule BuildPipeline.RunTest do
  use ExUnit.Case, async: false
  use Mimic
  import ExUnit.CaptureIO
  alias BuildPipeline.Run
  alias BuildPipeline.Run.Support.EnvVarsSystemMock
  alias BuildPipeline.Run.TerminalWidth.TputCols
  alias BuildPipeline.Run.Mocks.TputCols.{NotOnSystem, NonsenseResult}

  @moduletag timeout: 3_000

  describe "main/1" do
    test "can show runner output on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)
      EnvVarsSystemMock.setup()

      output =
        capture_io(fn ->
          assert :ok ==
                   Run.main([
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
      EnvVarsSystemMock.setup()
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)

      output =
        capture_io(fn ->
          assert :ok ==
                   Run.main([
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
                   Run.main([
                     "--cwd",
                     "./example_projects/bad_config"
                   ])
        end)

      assert output =~ "I failed to parse the build_pipeline_config"
    end

    test "returns error if given nonsense CLI args" do
      output = capture_io(fn -> assert :error == Run.main(["--nonesense"]) end)

      assert output =~ "I was given some arguments I don't understand."
    end

    test "returns error if the config file is not found" do
      output =
        capture_io(fn ->
          assert :error ==
                   Run.main([
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
                   Run.main([
                     "--cwd",
                     "./example_projects/invalid_json"
                   ])
        end)

      assert output =~ "I failed to parse the config.json because it was not valid JSON\n"
    end

    test "returns error if we can't determine the terminals width because tput can't be run" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &NotOnSystem.run/0)

      output =
        capture_io(fn ->
          assert :error ==
                   Run.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
                   ])
        end)

      assert output =~
               "I tried to run 'tput cols' but it failed because it looks like I'm not able to run the 'tput' binary?\n"
    end

    test "returns error if we can't determine the terminals width because tput returns nonsense" do
      Mimic.copy(TputCols)
      Mimic.stub(TputCols, :run, &NonsenseResult.run/0)

      output =
        capture_io(fn ->
          assert :error ==
                   Run.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
                   ])
        end)

      assert output ==
               "I tried to run 'tput cols' but it returned a result I couldn't parse! Damn\n"
    end

    test "when save-result (sr) is NOT set, we don't save the result to file" do
      EnvVarsSystemMock.setup()

      previous_run_result_file =
        "./example_projects/complex_yet_functioning/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok =
        Run.main([
          "--cwd",
          "./example_projects/complex_yet_functioning"
        ])

      assert File.exists?(previous_run_result_file) == false
    end

    test "when from failed (ff) is set, save the result to file, when running it again, read the file & skip the previously successful build steps" do
      EnvVarsSystemMock.setup()

      previous_run_result_file =
        "./example_projects/complex_and_failing/build_pipeline/previous_run_result.json"

      File.rm(previous_run_result_file)

      :ok =
        Run.main([
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

      :ok =
        Run.main([
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

    test "when from failed (ff) is set, but with invalid JSON in the file, returns an error" do
      output =
        capture_io(fn ->
          assert :error =
                   Run.main([
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
                   Run.main([
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
                   Run.main([
                     "--cwd",
                     "./example_projects/bad_build_step_name_previous_run_result_json",
                     "--ff"
                   ])
        end)

      assert output =~
               "I couldn't parse the result the previous run!\n\nI need a JSON list containing a list of only {buildStepName, result},\nbut I was given a result I didn't recognise of \"nonsense\"\n\nI suggest you delete your previous_run_result.json file & run the whole build from scratch...\n\n"
    end

    test "running with --stats puts some stats on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)
      EnvVarsSystemMock.setup()

      output =
        capture_io(fn ->
          assert :ok ==
                   Run.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning",
                     "--stats"
                   ])
        end)

      assert Enum.all?(stats_regexes(), fn regex -> Regex.match?(regex, output) end)

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "running without --stats puts no stats on the screen" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)
      EnvVarsSystemMock.setup()

      output =
        capture_io(fn ->
          assert :ok ==
                   Run.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning"
                   ])
        end)

      assert Enum.all?(stats_regexes(), fn regex -> Regex.match?(regex, output) == false end)

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end

    test "--analyse-self-worth mode!" do
      original_env = Application.get_env(:build_pipeline, :print_runner_output)

      Application.put_env(:build_pipeline, :print_runner_output, true)
      EnvVarsSystemMock.setup()

      output =
        capture_io(fn ->
          assert :ok ==
                   Run.main([
                     "--cwd",
                     "./example_projects/complex_yet_functioning",
                     "--analyse-self-worth"
                   ])
        end)

      build_pipeline_regex = ~r|build_pipeline runtime with parallelism = [0-9].*|
      serial_regex = ~r|build_pipeline runtime without parallelism = [0-9].*|

      assert Regex.match?(build_pipeline_regex, output)
      assert Regex.match?(serial_regex, output)

      Application.put_env(:build_pipeline, :print_runner_output, original_env)
    end
  end

  defp stats_regexes do
    [
      ~r|Branch 1 - [0-9]+ .*|,
      ~r|├── .*echo tires \[[0-9]+ .*\]|,
      ~r|├── .*echo car works \[[0-9]+ .*\]|,
      ~r|├── .*echo drive \[[0-9]+ .*\]|,
      ~r|├── .*echo walk over \[[0-9]+ .*\]|,
      ~r|└── .*echo hello \[[0-9]+ .*\]|,
      ~r|Branch 2 - [0-9]+ .*|
    ]
  end
end
