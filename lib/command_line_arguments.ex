defmodule BuildPipeline.CommandLineArguments do
  @default_setup %{cwd: ".", print_cmd_output: false, verbose: false}
  @cwd "--cwd"
  @verbose "--verbose"
  @print_cmd_output "--print-cmd-output"
  @usage_instructions """
  usage ./build_pipline [--cwd ./path/to/directory/to/use] [--print-cmd-output] [--verbose]
  """

  def parse(command_line_args) do
    parse(@default_setup, command_line_args)
  end

  def parse(setup, []) do
    {:ok, setup}
  end

  def parse(setup, [@print_cmd_output | rest]) do
    parse(Map.put(setup, :print_cmd_output, true), rest)
  end

  def parse(setup, [@cwd, cwd | rest]) do
    parse(Map.put(setup, :cwd, cwd), rest)
  end

  def parse(setup, [@verbose | rest]) do
    parse(Map.put(setup, :verbose, true), rest)
  end

  def parse(_step, nonsense) do
    {:error, {:bad_cmd_args, Enum.join(nonsense, " "), @usage_instructions}}
  end
end
