defmodule BuildPipeline.CommandLineArguments do
  @moduledoc false
  @default_setup %{cwd: ".", mode: :normal}
  @cwd "--cwd"
  @verbose "--verbose"
  @usage_instructions """
  usage ./build_pipline [--cwd ./path/to/directory/to/use] [--verbose]
  """

  def parse(command_line_args) do
    parse(@default_setup, command_line_args)
  end

  def parse(setup, []) do
    {:ok, setup}
  end

  def parse(setup, [@cwd, cwd | rest]) do
    parse(Map.put(setup, :cwd, cwd), rest)
  end

  def parse(setup, [@verbose | rest]) do
    parse(Map.put(setup, :mode, :verbose), rest)
  end

  def parse(_step, nonsense) do
    {:error, {:bad_cmd_args, Enum.join(nonsense, " "), @usage_instructions}}
  end
end
