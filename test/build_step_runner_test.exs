defmodule BuildPipeline.BuildStepRunnerTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.BuildStepRunner
  alias BuildPipeline.Builders.BuildStepBuilder

  @build_step BuildStepBuilder.build("noop")
  @cwd "."

  describe "start_link/2" do
    test "starts up a build step runner process with the status 'waiting'" do
      assert {:ok, pid} = BuildStepRunner.start_link(@build_step, self(), @cwd)
      assert is_pid(pid)

      assert %{build_step: @build_step, server_pid: self(), status: :waiting, opts: [], cwd: @cwd} ==
               :sys.get_state(pid)
    end

    test "stores optional options in the genserver state" do
      assert {:ok, pid} =
               BuildStepRunner.start_link(@build_step, self(), @cwd,
                 print_cmd_output_as_it_comes: false
               )

      assert is_pid(pid)

      assert %{opts: [print_cmd_output_as_it_comes: false]} = :sys.get_state(pid)
    end
  end

  describe "handle_cast/2 - run" do
    test "can run shell commands" do
      build_step =
        BuildStepBuilder.build()
        |> BuildStepBuilder.with_shell_command("true")

      {:ok, pid} = BuildStepRunner.start_link(build_step, self(), @cwd)

      GenServer.cast(pid, :run)

      assert_receive {:"$gen_cast", {:runner_finished, ^pid, %{exit_code: 0, output: ""}}}
    end
  end
end
