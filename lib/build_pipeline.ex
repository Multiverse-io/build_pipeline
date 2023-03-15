defmodule BuildPipeline do
  alias BuildPipeline.{Run, Init}

  @run "run"
  @init "init"

  def main([@run | run_args]) do
    Run.main(run_args)
  end

  def main([@init | init_args]) do
    Init.main(init_args)
  end
end
