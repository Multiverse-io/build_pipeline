defmodule BuildPipeline.CommandLineArguments do
  @default_setup %{
    cwd: "."
  }

  @cwd "--cwd"
  @usage_instructions """
  usage ./build_pipline [--cwd ./path/to/directory/to/use]
  """

  def parse([@cwd, cwd]) do
    {:ok, %{cwd: cwd}}
  end

  def parse([]) do
    {:ok, @default_setup}
  end

  def parse(nonsense) do
    {:error, {:bad_cmd_args, Enum.join(nonsense, " "), @usage_instructions}}
  end
end
