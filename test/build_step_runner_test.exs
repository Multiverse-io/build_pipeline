defmodule BuildPipeline.BuildStepRunnerTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.BuildStepRunner
  alias BuildPipeline.Builders.BuildStepBuilder

  @build_step BuildStepBuilder.build()
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
               BuildStepRunner.start_link(@build_step, self(), @cwd, print_cmd_output: false)

      assert is_pid(pid)

      assert %{opts: [print_cmd_output: false]} = :sys.get_state(pid)
    end
  end

  describe "run_if_waiting/2" do
    test "if status = waiting, it runs the command" do
      build_step =
        BuildStepBuilder.build()
        |> BuildStepBuilder.with_shell_command("true")

      {:ok, pid} = BuildStepRunner.start_link(build_step, self(), @cwd)

      GenServer.cast(pid, :run_if_waiting)

      assert_receive {:"$gen_cast", {:runner_finished, ^pid, %{exit_code: 0, output: ""}}}
    end

    test "if status = waiting, the status is set to running" do
      build_step =
        BuildStepBuilder.build()
        |> BuildStepBuilder.with_shell_command("true")

      {:ok, pid} = BuildStepRunner.start_link(build_step, self(), @cwd)

      state = :sys.get_state(pid)

      {:noreply, new_state, {:continue, :run}} =
        BuildStepRunner.handle_cast(:run_if_waiting, state)

      assert new_state == Map.put(state, :status, :running)
    end

    test "if status != waiting, then the state is unchnaged" do
      build_step =
        BuildStepBuilder.build()
        |> BuildStepBuilder.with_shell_command("true")

      {:ok, pid} = BuildStepRunner.start_link(build_step, self(), @cwd)

      state = :sys.get_state(pid)
      state = Map.put(state, :status, :running)

      assert {:noreply, state} ==
               BuildStepRunner.handle_cast(:run_if_waiting, state)
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
