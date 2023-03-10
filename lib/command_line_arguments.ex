defmodule BuildPipeline.CommandLineArguments do
  @moduledoc false
  @default_setup %{cwd: ".", mode: :normal}
  @cwd "--cwd"
  @verbose "--verbose"
  @debug "--debug"
  @usage_instructions """
  usage ./build_pipline [--cwd ./path/to/directory/to/use] [--verbose or --debug]

  note: only --debug or --verbose can be set. It's one or the other
  """

  # TODO update the readme about debug mode

  def parse(command_line_args) do
    if @debug in command_line_args and @verbose in command_line_args do
      {:error, {:bad_cmd_args, Enum.join(command_line_args, " "), @usage_instructions}}
    else
      parse(@default_setup, command_line_args)
    end
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

  def parse(setup, [@debug | rest]) do
    parse(Map.put(setup, :mode, :debug), rest)
  end

  def parse(_step, nonsense) do
    {:error, {:bad_cmd_args, Enum.join(nonsense, " "), @usage_instructions}}
  end
end
