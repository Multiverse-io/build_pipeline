defmodule BuildPipeline.Run.PreviousRunResultTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.Builders.{RunnersBuilder, PreviousRunResultBuilder, ServerSetupBuilder}
  alias BuildPipeline.Run.{Const, PreviousRunResult}

  describe "read/1" do
    test "with run_from_failed: false, do nothing, read no files & return the config that was passed in" do
      Mimic.copy(File)
      Mimic.reject(File, :read, 1)

      setup = ServerSetupBuilder.build() |> ServerSetupBuilder.with_run_from_failed(false)
      assert PreviousRunResult.read(setup) == {:ok, setup}
    end

    test "with run_from_failed: true, read the file & return its contents along with the passed in config" do
      Mimic.copy(File)
      Mimic.stub(File, :read, fn _file_name -> {:ok, "some cool file contents"} end)

      setup = ServerSetupBuilder.build() |> ServerSetupBuilder.with_run_from_failed(true)
      assert PreviousRunResult.read(setup) == {:ok, {"some cool file contents", setup}}
    end

    test "with run_from_failed: true, but no file can be found, return error" do
      Mimic.copy(File)
      Mimic.stub(File, :read, fn _file_name -> {:error, :enoent} end)

      setup =
        ServerSetupBuilder.build()
        |> ServerSetupBuilder.with_run_from_failed(true)
        |> ServerSetupBuilder.with_cwd("cool/dir")

      assert PreviousRunResult.read(setup) ==
               {:error,
                {:previous_run_result_file_not_found,
                 "cool/dir/build_pipeline/previous_run_result.json"}}
    end
  end

  describe "parse_and_validate/2" do
    test "given valid previous result json, the build pipeline is updated with any steps that should be skipped" do
      results = [Const.successful(), Const.not_started(), Const.failed(), Const.skipped()]

      [
        %{
          build_step_name: successful_build_step_name,
          previous_result: successful_previous_result,
          build_step: successful_build_step
        },
        %{
          build_step_name: not_started_build_step_name,
          previous_result: not_started_previous_result,
          build_step: not_started_build_step
        },
        %{
          build_step_name: failed_build_step_name,
          previous_result: failed_previous_result,
          build_step: failed_build_step
        },
        %{
          build_step_name: skipped_build_step_name,
          previous_result: skipped_previous_result,
          build_step: skipped_build_step
        }
      ] = build_step_with_previous_results(results)

      build_pipeline = [
        successful_build_step,
        not_started_build_step,
        failed_build_step,
        skipped_build_step
      ]

      previous_results = [
        successful_previous_result,
        not_started_previous_result,
        failed_previous_result,
        skipped_previous_result
      ]

      server_setup =
        ServerSetupBuilder.build() |> ServerSetupBuilder.with_build_pipeline(build_pipeline)

      previous_result_json = Jason.encode!(previous_results)

      assert {:ok,
              %{
                build_pipeline: [
                  %{build_step_name: ^successful_build_step_name, skip: true},
                  %{build_step_name: ^not_started_build_step_name, skip: false},
                  %{build_step_name: ^failed_build_step_name, skip: false},
                  %{build_step_name: ^skipped_build_step_name, skip: true}
                ]
              }} = PreviousRunResult.parse_and_validate({previous_result_json, server_setup})
    end

    test "given invalid JSON returns an error" do
      server_setup = ServerSetupBuilder.build()

      assert {:error, {:previous_run_result, %Jason.DecodeError{}}} =
               PreviousRunResult.parse_and_validate({"bad json", server_setup})
    end

    test "given valid JSON with nonsense contents returns an error" do
      server_setup = ServerSetupBuilder.build()

      error = """
      I couldn't parse the result the previous run!

      I need a JSON list containing a list of only {buildStepName, result},
      but I was given {"a": "b"}.

      I suggest you delete your previous_run_result.json file & run the whole build from scratch...
      """

      assert {:error, {:previous_run_result, error}} ==
               PreviousRunResult.parse_and_validate({~s|{"a": "b"}|, server_setup})
    end

    test "given valid JSON with an unknown result, returns an error" do
      %{
        build_step_name: _nonsense_build_step_name,
        previous_result: nonsense_previous_result,
        build_step: nonsense_build_step
      } = build_step_with_previous_results("nonsense_result")

      build_pipeline = [nonsense_build_step]

      previous_results = [nonsense_previous_result]

      server_setup =
        ServerSetupBuilder.build() |> ServerSetupBuilder.with_build_pipeline(build_pipeline)

      previous_result_json = Jason.encode!(previous_results)

      error = """
      I couldn't parse the result the previous run!

      I need a JSON list containing a list of only {buildStepName, result},
      but I was given a result I didn't recognise of "nonsense_result"

      I suggest you delete your previous_run_result.json file & run the whole build from scratch...
      """

      assert {:error, {:previous_run_result, error}} ==
               PreviousRunResult.parse_and_validate({previous_result_json, server_setup})
    end

    test "given valid json with a buildStepName that doesn't exist in the build_pipeline, errors" do
      build_step_name_1 = RunnersBuilder.build_step_name()
      build_step_name_2 = RunnersBuilder.build_step_name()

      previous_result =
        PreviousRunResultBuilder.build()
        |> PreviousRunResultBuilder.with_build_step_name(build_step_name_1)

      build_step =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_build_step_name(build_step_name_2)

      server_setup =
        ServerSetupBuilder.build() |> ServerSetupBuilder.with_build_pipeline([build_step])

      previous_result_json = Jason.encode!([previous_result])

      error = """
      Something's wrong with my saved build_pipeline/previous_run_result.json!

      It contains a buildStepName #{build_step_name_1}
      but there isn't a build step of that name in build_pipeline/config.json!

      I suggest you delete your previous_run_result.json file & run the whole build from scratch...
      """

      assert {:error, {:previous_run_result, error}} ==
               PreviousRunResult.parse_and_validate({previous_result_json, server_setup})
    end
  end

  defp build_step_with_previous_results(results) when is_list(results) do
    results
    |> Enum.with_index()
    |> Enum.map(fn {result, order} ->
      build_step_name = RunnersBuilder.build_step_name()

      previous_result =
        PreviousRunResultBuilder.build()
        |> PreviousRunResultBuilder.with_result(result)
        |> PreviousRunResultBuilder.with_build_step_name(build_step_name)

      build_step =
        RunnersBuilder.build_incomplete()
        |> RunnersBuilder.with_build_step_name(build_step_name)
        |> RunnersBuilder.with_order(order)

      %{
        result: result,
        build_step_name: build_step_name,
        previous_result: previous_result,
        build_step: build_step
      }
    end)
  end

  defp build_step_with_previous_results(result) do
    [build_step_and_friends] = build_step_with_previous_results([result])
    build_step_and_friends
  end
end
