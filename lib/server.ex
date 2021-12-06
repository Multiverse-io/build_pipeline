defmodule BuildPipeline.Server do
  use GenServer

  @process_name :server
  @default_genserver_options [name: @process_name]

  def child_spec(pipeline_tree, genserver_options \\ @default_genserver_options) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [pipeline_tree, genserver_options]}
    }
  end

  def start_link(pipeline_tree, genserver_options \\ @default_genserver_options) do
    GenServer.start_link(__MODULE__, pipeline_tree, genserver_options)
  end

  @impl true
  def init(_command_line_args) do
    {:ok, %{}}
  end
end
