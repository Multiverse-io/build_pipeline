defmodule BuildPipeline.Mocks.TputCols.GoodResult do
  @cols 156
  def run, do: {"#{@cols}\n", 0}
  def cols, do: @cols
end

defmodule BuildPipeline.Mocks.TputCols.ResultUnexpectedError do
  @output "something unexpected went wrong"
  def run, do: {@output, 1}
  def output, do: @output
end

defmodule BuildPipeline.Mocks.TputCols.DecimalResult do
  @output "123.4323\n"
  def run, do: {@output, 0}
  def output, do: @output
end

defmodule BuildPipeline.Mocks.TputCols.NonsenseResult do
  @output "nonsense"
  def run, do: {@output, 0}
  def output, do: @output
end

defmodule BuildPipeline.Mocks.TputCols.NotOnSystem do
  def run do
    System.cmd("missing-non-existant-binary-omg", ["redundant-arg"])
  end
end
