defmodule BuildPipeline do
  alias BuildPipeline.{Run, Init}

  @run "run"
  @init "init"
  @help """
  I only accept arguments of
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

  def main(_) do
    IO.puts(@help)
  end
end
