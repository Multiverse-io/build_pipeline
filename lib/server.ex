defmodule BuildPipeline.Server do
  use GenServer
  alias BuildPipeline.BuildStepRunner

  @default_genserver_options []

  def child_spec(pipeline_tree, parent_pid, genserver_options \\ @default_genserver_options) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [{pipeline_tree, parent_pid}, genserver_options]}
    }
  end

  def start_link({pipeline_tree, parent_pid}, genserver_options \\ @default_genserver_options) do
    GenServer.start_link(__MODULE__, {pipeline_tree, parent_pid}, genserver_options)
  end

  @impl true
  def init({pipeline_tree, parent_pid}) do
    start_runners()
    runners = init_waiting_runners(pipeline_tree)

    {:ok, %{runners: runners, completed_runners: MapSet.new([]), parent_pid: parent_pid}}
  end

  @impl true
  def handle_call(:start_runners, _from, state) do
    start_runners_if_able(state)
    {:noreply, state}
  end

  # TODO make this module a supervisor maybe?
  @impl true
  def handle_cast({:runner_finished, runner_pid, _result}, state) do
    # IO.inspect(Map.fetch!(state.runners, runner_pid).build_step_name)
    # IO.inspect(result)

    # TODO in case of any failure: kill all runners, put failure message, kill this server, send death msg to parent
    # TODO in case of any success: print success on screen with timings
    state
    |> update_completed_runners(runner_pid)
    |> start_runners_if_able()
    |> finished_if_all_runners_done()
  end

  @impl true
  def terminate(:normal, state) do
    send(state.parent_pid, :server_done)
  end

  defp update_completed_runners(state, completed_runner_pid) do
    completed_build_step_name =
      state.runners
      |> Map.fetch!(completed_runner_pid)
      |> Map.fetch!(:build_step_name)

    %{state | completed_runners: MapSet.put(state.completed_runners, completed_build_step_name)}
  end

  defp finished_if_all_runners_done(state) do
    all_runners_by_name =
      MapSet.new(state.runners, fn {_runner_pid, %{build_step_name: build_step_name}} ->
        build_step_name
      end)

    if MapSet.equal?(all_runners_by_name, state.completed_runners) do
      {:stop, :normal, state}
    else
      {:noreply, state}
    end
  end

  defp start_runners_if_able(state) do
    Enum.each(state.runners, fn {runner_pid, _build_step} ->
      GenServer.cast(runner_pid, {:run_if_able, state.completed_runners})
    end)

    state
  end

  defp start_runners do
    server_pid = self()
    spawn_link(fn -> GenServer.call(server_pid, :start_runners) end)
  end

  defp init_waiting_runners(pipeline_tree) do
    Enum.reduce(pipeline_tree, %{}, fn build_step, runners ->
      {:ok, runner_pid} = BuildStepRunner.start_link({build_step, self()})
      Map.put(runners, runner_pid, build_step)
    end)
  end
end
