defmodule BuildPipeline do
  alias BuildPipeline.{Run, Init}

  @run "run"
  @init "init"
  @version "--version"
  @help """
  I only accept arguments of
    - --version
    - run [run args]
    - init [run args]

  If you're unsure of what to do, read the README
  or try running:
    - run help
    - init help
  """

  def main([@run | run_args]) do
    Run.main(run_args)
  end

  def main([@init | init_args]) do
    Init.main(init_args)
  end

  def main([@version]) do
    IO.puts(version())
  end

  def main(_) do
    IO.puts(@help)
  end

  def version, do: "0.0.13"
end
