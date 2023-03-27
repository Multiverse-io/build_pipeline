defmodule BuildPipeline.Run.PreviousRunResultFileWriterTest do
  use ExUnit.Case, async: true
  use Mimic
  alias BuildPipeline.Run.{PreviousRunResultFileWriter, Const}
  alias BuildPipeline.Run.Builders.{RunnersBuilder, ServerStateBuilder}

  describe "write/3" do
    test "given a server state, and a runner_pid and exit code for a runner that's completed, writes the result to a file" do
      Mimic.copy(File)
      Mimic.stub(File, :write!, fn file_name, content -> {file_name, content} end)

      runner_pid = RunnersBuilder.pid()

      runner =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_exit_code(0)
        |> RunnersBuilder.with_build_step_name("hi_mom")
        |> RunnersBuilder.with_command("echo hi mom")
        |> RunnersBuilder.with_output("hi mom\n")

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_cwd("cool_dir")
        |> ServerStateBuilder.with_runners(%{runner_pid => runner})
        |> ServerStateBuilder.with_save_result(true)

      {file, content} = PreviousRunResultFileWriter.write(server_state, runner_pid, 0)

      assert file == "cool_dir/build_pipeline/previous_run_result.json"

      assert Jason.decode!(content) == [
               %{
                 "buildStepName" => "hi_mom",
                 "result" => Const.successful()
               }
             ]
    end

    test "given a server state with save_result = false, then save nothing to file" do
      Mimic.copy(File)
      Mimic.reject(File, :write!, 2)

      runner_pid = RunnersBuilder.pid()

      runner =
        RunnersBuilder.build_complete()
        |> RunnersBuilder.with_exit_code(0)
        |> RunnersBuilder.with_build_step_name("hi_mom")
        |> RunnersBuilder.with_command("echo hi mom")
        |> RunnersBuilder.with_output("hi mom\n")

      server_state =
        ServerStateBuilder.build()
        |> ServerStateBuilder.with_cwd("cool_dir")
        |> ServerStateBuilder.with_runners(%{runner_pid => runner})

      assert :noop = PreviousRunResultFileWriter.write(server_state, runner_pid, 0)
    end
  end
end
